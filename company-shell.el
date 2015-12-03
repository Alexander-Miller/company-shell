;;; company-shell.el --- Company mode backend for shell functions -*- lexical-binding: t -*-

;; Copyright (C) 2015 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Keywords: company, shell

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Backend for company mode to complete binaries found on your $PATH
;; and fish shell functions.

;;; Code:

(require 'company)
(require 'dash)
(require 'cl-lib)

(defvar company-shell--cache nil
  "Cache of all possible completions. Invoke `company-shell-create-completion-list' to rebuild manually.")

(defun company-shell-create-completion-list ()
  "Builds the cache of all completions found on the $PATH and optionally all fish functions."
  (interactive)
  (setq company-shell--cache
        (sort (append
               (company-shell--fetch-fish-functions)
               (company-shell--fetch-path-functions))
              'string-lessp)))

(defun company-shell--fetch-candidates ()
  (when (null company-shell--cache) (company-shell-create-completion-list))
  company-shell--cache)

(defun company-shell--fetch-path-functions ()
  (-mapcat
   (lambda (dir)
     (-map
      (lambda (f) (propertize f 'origin dir))
      (directory-files dir)))
   (-> (getenv "PATH") (split-string ":"))))

(defun company-shell--fetch-fish-functions ()
  (when (executable-find "fish")
    (-->
     (shell-command-to-string "fish -c functions")
     (split-string it "\n")
     (-map (lambda (f) (propertize f 'origin "Fish Function")) it))))

(defun company-shell--doc-buffer (arg)
  (company-doc-buffer
   (let ((man-page (shell-command-to-string (format "man %s" arg))))
     (if (or
          (null man-page)
          (string-empty-p man-page)
          (string-prefix-p "No manual entry" man-page))
         (or
            (shell-command-to-string (format "%s --help" arg))
            (shell-command-to-string (format "%s -h" arg)))
       man-page))))

(defun company-shell (command &optional arg &rest ignored)
  "Company mode backend for binaries found on the $PATH and fish shell functions."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-shell))
    (prefix      (company-grab-symbol))
    (sorted      t)
    (duplicates  t)
    (ignore-case nil)
    (no-cache    nil)
    (annotation  (get-text-property 0 'origin arg))
    (doc-buffer  (company-shell--doc-buffer arg))
    (meta        (car (split-string (shell-command-to-string (format "whatis %s" arg)) "\n")))
    (candidates  (cl-remove-if-not
                  (lambda (candidate) (string-prefix-p arg candidate))
                  (company-shell--fetch-candidates)))))

(provide 'company-shell)
