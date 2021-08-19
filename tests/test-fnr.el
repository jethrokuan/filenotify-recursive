;;; test-fnr.el --- Tests for filenotify-recursive -*- lexical-binding: t; -*-
;; Copyright (C) 2021 Jethro Kuan

;; Author: Jethro Kuan <jethrokuan95@gmail.com>
;; Package-Requires: ((buttercup))

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

(defvar test-fnr--temp-testdir nil
  "Test root directory.")

(defvar test-fnr-event-counter (make-hash-table :test 'equal)
  ".")

(defun test-fnr-counter-increment (f)
  "."
  (if-let ((count (gethash f test-fnr-event-counter)))
      (puthash f (1+ count) test-fnr-event-counter)
    (puthash f 1 test-fnr-event-counter)))

(defun test-fnr-counter-reset ()
  "."
  (setq test-fnr-event-counter (make-hash-table :test 'equal)))

(defun fnr--make-temp-dir ()
  "Create a temporary file directory for test."
  (unless (stringp test-fnr--temp-testdir)
    (setq test-fnr--temp-testdir
          (expand-file-name
           (make-temp-name "fnr-test") temporary-file-directory)))
  (unless (file-directory-p test-fnr--temp-testdir)
    (make-directory test-fnr--temp-testdir)))

(defun test-fnr-create-test-directory ()
  (fnr--make-temp-dir)
  (make-directory (expand-file-name "a/a1" test-fnr--temp-testdir) 'parents)
  (make-directory (expand-file-name "b" test-fnr--temp-testdir) 'parents))

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
    (expect (mapcar #'directory-file-name (fnr--subdirectories-recursively test-fnr--temp-testdir))
            :to-have-same-items-as
            (mapcar #'directory-file-name `(,test-fnr--temp-testdir
                                           ,(expand-file-name "a" test-fnr--temp-testdir)
                                           ,(expand-file-name "a/a1" test-fnr--temp-testdir)
                                           ,(expand-file-name "b" test-fnr--temp-testdir))))))

(provide 'test-fnr)
;;; test-fnr.el ends here
