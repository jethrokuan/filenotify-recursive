;;; filenotify-recursive.el --- filenotify, but recursive -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2020-2021 Jethro Kuan <jethrokuan95@gmail.com>

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; URL: https://github.com/jethrokuan/filenotify-recursive
;; Keywords: emacs, filenotify
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;;  This is an extension of the filenotify library, making it apply recursively
;;
;;; Code:
(require 'filenotify)
(require 'cl-lib)

;;; Variables
(defvar fnr-descriptors (make-hash-table :test 'equal)
  "Hash table for registered fnr descriptors.
A key in this hashtable is a uuid. The value in the hash table is
a `fnr--watch' struct.")

;;; Utilities
(defun fnr--uuid ()
  "Return string with random (version 4) UUID."
  (let ((rnd (md5 (format "%s%s%s%s%s%s"
                          (random)
                          (user-uid)
                          (emacs-pid)
                          (user-full-name)
                          user-mail-address
                          (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring rnd 0 8)
            (substring rnd 8 12)
            (substring rnd 13 16)
            (format "%x"
                    (logior
                     #b10000000
                     (logand
                      #b10111111
                      (string-to-number (substring rnd 16 18) 16))))
            (substring rnd 18 20)
            (substring rnd 20 32))))

(defun fnr--subdirectories-recursively (dir &optional regexp predicate follow-symlinks)
  "Return list of subdirectories under directory DIR.
This function works recursively.  Files are returned in \"depth
first\" order, and files from each directory are sorted in
alphabetical order.  Each file name appears in the returned list
in its absolute form.


If REGEXP, when the directory matches REGEXP it is skipped.

PREDICATE can be either nil (which means that all subdirectories
of DIR are descended into), t (which means that subdirectories that
can't be read are ignored), or a function (which is called with
the name of each subdirectory, and should return non-nil if the
subdirectory is to be descended into).

If FOLLOW-SYMLINKS is non-nil, symbolic links that point to
directories are followed.  Note that this can lead to infinite
recursion."
  (let* ((result nil)
         (dir (directory-file-name dir)))
    (dolist (file (sort (file-name-all-completions "" dir)
                        'string<))
      (unless (or (member file '("./" "../"))
                  (and regexp
                       (string-match regexp file)))
        (when (directory-name-p file)
          (let* ((leaf (substring file 0 (1- (length file))))
                 (full-file (concat dir "/" leaf)))
            ;; Don't follow symlinks to other directories.
            (when (and (or (not (file-symlink-p full-file))
                           (and (file-symlink-p full-file)
                                follow-symlinks))
                       ;; Allow filtering subdirectories.
                       (or (eq predicate nil)
                           (eq predicate t)
                           (funcall predicate full-file)))
              (let ((sub-files
                     (if (eq predicate t)
                         (ignore-error file-error
                           (fnr--subdirectories-recursively
                            full-file predicate follow-symlinks))
                       (fnr--subdirectories-recursively
                        full-file predicate follow-symlinks))))
                (setq result (nconc result sub-files))))))))
    (cons dir result)))

(cl-defstruct (fnr--watch (:constructor fnr--watch-create)
                          (:copier nil))
  "Internal struct for managing filenotify recursive watchers."
  uuid
  descs
  flags
  callback)

;;;
(defun fnr-add-watch (dir flags callback &optional regexp)
  "Create a new recursive watcher for filesystem events to DIR.
Use `fnr-rm-watch' to cancel the watch.

The returned value is a UUID. If the file cannot be watched for
some reason, this function signals a `file-notify-error' error.

FLAGS is a list of conditions to set what will be watched for. It
can include the following symbols:

  `change'           -- watch for file changes
  `attribute-change' -- watch for file attributes changes, like
                        permissions or modification time


When any event happens, Emacs will call the CALLBACK function passing
it a single argument EVENT, which is of the form

  (DESCRIPTOR ACTION FILE [FILE1])

DESCRIPTOR is the same object as the one returned by this function.
ACTION is the description of the event.  It could be any one of the
following:

  `created'           -- FILE was created
  `deleted'           -- FILE was deleted
  `changed'           -- FILE has changed
  `renamed'           -- FILE has been renamed to FILE1
  `attribute-changed' -- a FILE attribute was changed
  `stopped'           -- watching FILE has been stopped

FILE is the name of the file whose event is being reported.

If REGEXP is non-nil, do not watch directories matching REGEXP."
  (let* ((uuid (fnr--uuid))
         (all-dirs (fnr--subdirectories-recursively dir regexp))
         (wrapped-callback (fnr--wrap-callback uuid callback))
         (descs (mapcar (lambda (dir)
                          (file-notify-add-watch dir flags wrapped-callback))
                        all-dirs))
         (watcher (fnr--watch-create :uuid uuid
                                     :flags flags
                                     :descs descs
                                     :callback wrapped-callback)))
    (puthash uuid watcher fnr-descriptors)
    uuid))

(defun fnr-rm-watch (uuid)
  "Remove recursive watcher by UUID."
  (let ((watcher (or (gethash uuid fnr-descriptors)
                     (user-error "No watcher with id %s" uuid))))
    (dolist (desc (fnr--watch-descs watcher))
      (file-notify-rm-watch desc))
    (remhash uuid fnr-descriptors)
    uuid))

(defun fnr--wrap-callback (uuid callback)
  "Wraps the user-provided CALLBACK to include keeping track of new change.
UUID is the uuid of the fnr-watcher."
  (lambda (event)
    (funcall #'fnr--callback uuid event)
    (funcall callback event)))

(defun fnr--callback (uuid event)
  "Callback that handles watching new directories.
UUID is the id corresponding to the recursive watcher. This ID
should be present in `fnr-descriptors.' EVENT is an event from
`filenotify'."
  (pcase event
    (`(,_ created ,file)
     (when (file-directory-p file)
       (let* ((watcher (gethash uuid fnr-descriptors))
              (new-desc (file-notify-add-watch file
                                               (fnr--watch-flags watcher)
                                               (fnr--watch-callback watcher))))
         (setf (fnr--watch-descs watcher) (push new-desc (fnr--watch-descs watcher)))
         (puthash uuid watcher fnr-descriptors))))
    (`(,desc stopped ,_)
     (let ((watcher (gethash uuid fnr-descriptors)))
       (setf (fnr--watch-descs watcher) (delete desc (fnr--watch-descs watcher)))
       (puthash uuid watcher fnr-descriptors)))))

(defun fnr-clear-all ()
  "Clear all recursive filenotify watches."
  (interactive)
  (maphash (lambda (_uuid watcher)
             (dolist (desc (fnr--watch-descs watcher))
               (file-notify-rm-watch desc))) fnr-descriptors)
  (setq fnr-descriptors (make-hash-table :test 'equal)))

(provide 'filenotify-recursive)
;;; filenotify-recursive.el ends here
