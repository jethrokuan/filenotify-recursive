;;; test-fnr.el --- Tests for filenotify-recursive -*- lexical-binding: t; -*-
;; Copyright (C) 2021 Jethro Kuan

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; Package-Requires: ((buttercup) (f.el))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;; Code:

(require 'buttercup)
(require 'filenotify-recursive)

(defvar test-fnr-root-directory nil
  "Test root directory.")

(defvar test-fnr-event-counter (make-hash-table :test 'equal)
  ".")

(defun test-fnr-counter-increment (f)
  "."
  (message "HERE")
  (if-let ((count (gethash f test-fnr-event-counter)))
      (puthash f (1+ count) test-fnr-event-counter)
    (puthash f 1 test-fnr-event-counter)))

(defun test-fnr-counter-reset ()
  "."
  (setq test-fnr-event-counter (make-hash-table :test 'equal)))

(defun test-fnr-random-alnum ()
  "."
  (let* ((alnum "abcdefghijklmnopqrstuvwxyz0123456789")
           (i (% (abs (random)) (length alnum))))
      (substring alnum i (1+ i))))

(defun test-fnr-random-string (n)
  "Generate a slug of n random alphanumeric characters."
  (if (= 0 n)
      ""
    (concat (test-fnr-random-alnum) (test-fnr-random-string (1- n)))))

(defun test-fnr-create-test-directory ()
  (let ((test-root (expand-file-name (format "%s/" (test-fnr-random-string 5)) temporary-file-directory)))
    (make-directory test-root 'parents)
    (make-directory (expand-file-name "a/a1" test-root) 'parents)
    (make-directory (expand-file-name "b" test-root) 'parents)
    (setq test-fnr-root-directory test-root)))

(defun test-fnr-touch (f)
     "Touches F."
     (interactive)
     (shell-command (concat "touch " (shell-quote-argument f))))

(describe "fnr--subdirectories-recursively"
  (before-each
    (test-fnr-create-test-directory)
    (test-fnr-counter-reset)
    (fnr-clear-all))

  (it "correctly lists subdirectories"
    (expect (mapcar #'directory-file-name (fnr--subdirectories-recursively test-fnr-root-directory))
            :to-have-same-items-as
            (mapcar #'directory-file-name `(,test-fnr-root-directory
                                           ,(expand-file-name "a" test-fnr-root-directory)
                                           ,(expand-file-name "a/a1" test-fnr-root-directory)
                                           ,(expand-file-name "b" test-fnr-root-directory)))))

  (xit "correctly tracks items"
    (let* ((id (fnr-add-watch test-fnr-root-directory
                              '(change attribute-change) (lambda (e)
                                                           (message "%s" e)
                                                           (pcase e
                                                             (`(,_ ,_ ,f)
                                                              (test-fnr-counter-increment f))))))
           (watcher (gethash id fnr-descriptors)))
      (expect (length (fnr--watch-descs watcher)) :to-equal 4)
      (let ((test-new-file (expand-file-name "a/foo" test-fnr-root-directory)))
        (test-fnr-touch test-new-file)
        (message "%s" test-fnr-event-counter)
        (expect (gethash test-new-file test-fnr-event-counter) :to-equal 1)))))

(provide 'test-fnr)
;;; test-fnr.el ends here
