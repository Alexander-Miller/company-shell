;;; company-shell.el --- Company mode backend for shell functions

;; Copyright (C) 2015 Alexander Miller

;; Author: Alexander Miller <alexanderm@web.de>
;; Package-Requires: ((company "0.8.12") (dash "2.12.0") (cl-lib "0.5"))
;; Homepage: https://github.com/Alexander-Miller/company-shell
;; Version: 1.0
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
  "Cache of all possible $PATH completions. Automatically built when nil. Invoke `company-shell-rebuild-cache' to rebuild manually.")

(defvar company-shell--fish-cache nil
  "Cache of all possible fish shell function completions. Automatically built when nil. Invoke `company-shell-rebuild-cache' to rebuild manually.")

(defvar company-shell-delete-duplicates t
  "If non-nil the list of completions will be purged of duplicates. Duplicates in this context means any two
string-equal entries, regardless where they have been found. This would prevent a completion candidate
appearing twice because it is found in both /usr/bin/ and /usr/local/bin.

For a change to this variable to take effect the cache needs to be rebuilt via `company-shell-create-completion-list'.")

(defvar company-shell-modes '(sh-mode fish-mode)
  "List of major modes where `company-shell' will be providing completions if it is part of `company-backends'.
All modes not on this list will be ignored. Set value to nil to enable company-shell regardless of current major-mode.")

(defvar company-fish-shell-modes '(fish-mode)
  "List of major modes where `company-fish-shell' will be providing completions if it is part of `company-backends'.
All modes not on this list will be ignored. Set value to nil to enable company-fish-shell regardless of current major-mode.")

(defun company-shell--fetch-candidates ()
  (when (null company-shell--cache) (company-shell--build-cache))
  company-shell--cache)

(defun company-shell--fetch-fish-candidates ()
  (when (null company-shell--fish-cache) (company-shell--build-fish-cache))
  company-shell--fish-cache)

(defun company-shell--build-cache ()
  (let ((completions (-mapcat
                      (lambda (dir)
                        (-map
                         (lambda (file) (propertize file 'origin dir))
                         (directory-files dir)))
                      (-filter 'file-readable-p exec-path))))
    (setq company-shell--cache (sort
                                (if company-shell-delete-duplicates
                                    (delete-dups completions)
                                  completions)
                                'string-lessp))))

(defun company-shell--build-fish-cache ()
  (when (executable-find "fish")
    (setq company-shell--fish-cache
          (->
           (shell-command-to-string "fish -c \"functions -a\"")
           (split-string "\n")
           (sort 'string-lessp)))))

(defun company-shell--prefix (mode-list)
  (when (or (null mode-list)
          (-contains? mode-list major-mode))
      (company-grab-symbol)))

(defun company-shell--doc-buffer (arg)
  (company-doc-buffer
   (let ((man-page (shell-command-to-string (format "man %s" arg))))
     (if (or
          (null man-page)
          (string= man-page "")
          (string-prefix-p "No manual entry" man-page))
         (shell-command-to-string (format "%s --help" arg))
       man-page))))

(defun company-shell--meta-string (arg)
  (let ((meta (-> (format "whatis %s" arg)
                  (shell-command-to-string)
                  (split-string "\n")
                  (car))))
    (when (not (string-equal meta (format "%s: nothing appropriate." arg)))
      (second (split-string meta " - ")))))

;;;###autoload
(defun company-shell-rebuild-cache ()
  "Builds the cache of all completions found on the $PATH and all fish functions."
  (interactive)
  (company-shell--build-cache)
  (company-shell--build-fish-cache))

;;;###autoload
(defun company-fish-shell (command &optional arg &rest ignored)
  "Company backend for fish shell functions."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-fish-shell))
    (prefix      (company-shell--prefix company-fish-shell-modes))
    (sorted      t)
    (duplicates  nil)
    (ignore-case nil)
    (no-cache    nil)
    (annotation  "Fish Function")
    (doc-buffer  (company-shell--doc-buffer arg))
    (meta        (company-shell--meta-string arg))
    (candidates  (cl-remove-if-not
                  (lambda (candidate) (string-prefix-p arg candidate))
                  (company-shell--fetch-fish-candidates)))))

;;;###autoload
(defun company-shell (command &optional arg &rest ignored)
  "Company mode backend for binaries found on the $PATH."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-shell))
    (prefix      (company-shell--prefix company-shell-modes))
    (sorted      t)
    (duplicates  nil)
    (ignore-case nil)
    (no-cache    nil)
    (annotation  (get-text-property 0 'origin arg))
    (doc-buffer  (company-shell--doc-buffer arg))
    (meta        (company-shell--meta-string arg))
    (candidates  (cl-remove-if-not
                  (lambda (candidate) (string-prefix-p arg candidate))
                  (company-shell--fetch-candidates)))))

(provide 'company-shell)
;;; company-shell.el ends here
