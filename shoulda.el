;;; shoulda.el --- Shoulda test support for ruby

;; Copyright (C) 2014 Marcwebbie

;; Author: Marcwebbie <marcwebbie@gmail.com>
;; Version: 0.1
;; Keywords: ruby tests shoulda

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ruby-mode)

(defvar *project-roots*
  '(".git" ".hg" "Rakefile" "Makefile" "README" "build.xml" ".emacs-project" "Gemfile")
  "The presence of any file/directory in this list indicates a project root.")
(defvar *project-root* nil
  "Used internally to cache the project root.")

(defun root-match(root names)
  (member (car names) (directory-files root)))

(defun root-matches(root names)
  (if (root-match root names)
      (root-match root names)
    (if (eq (length (cdr names)) 0)
'nil
      (root-matches root (cdr names))
      )))

(defun project-root ()
  "Returns the current project root."
  (when (or
         (null *project-root*)
         (not (string-match *project-root* default-directory)))
    (let ((root (find-project-root)))
      (if root
          (setq *project-root* (expand-file-name (concat root "/")))
        (setq *project-root* nil))))
  *project-root*)

(defun find-project-root (&optional root)
  "Determines the current project root by recursively searching for an indicator."
  (when (null root) (setq root default-directory))
  (cond
   ((root-matches root *project-roots*)
    (expand-file-name root))
   ((equal (expand-file-name root) "/") nil)
   (t (find-project-root (concat (file-name-as-directory root) "..")))))

;;;###autoload
(defun shoulda-run-should-at-point ()
  "Run Shoulda should test at point"
  (interactive)
  (save-excursion
    (ruby-end-of-block)
    (let* ((name-regex "\\(\\(:[a-z0-9_]+\\)\\|\\([\"']\\([a-z0-9_ ]+\\)[\"']\\)\\)")
           (name-match (lambda () (or (match-string-no-properties 2) (match-string-no-properties 4))))
           (should (when (search-backward-regexp (concat "[ \t]*should +" name-regex "[ \t]+do") nil t)
                     (funcall name-match)))
           (context (when (search-backward-regexp (concat "[ \t]*context +" name-regex "[ \t]+do") nil t)
                      (funcall name-match))))
      (when (and should context)
        (compilation-start (concat "cd " (project-root) " && bundle exec ruby -I'lib:test' " (shell-quote-argument (buffer-file-name)) " -n /'"  should "'/"))))))

;;;###autoload
(defun shoulda-run-context-at-point ()
  "Run Shoulda context test at point"
  (interactive)
  (save-excursion
    (ruby-end-of-block)
    (let* ((name-regex "\\(\\(:[a-z0-9_]+\\)\\|\\([\"']\\([a-z0-9_ ]+\\)[\"']\\)\\)")
           (name-match (lambda () (or (match-string-no-properties 2) (match-string-no-properties 4))))
           (should (when (search-backward-regexp (concat "[ \t]*should +" name-regex "[ \t]+do") nil t)
                     (funcall name-match)))
           (context (when (search-backward-regexp (concat "[ \t]*context +" name-regex "[ \t]+do") nil t)
                      (funcall name-match))))
      (when (and should context)
        (compilation-start (concat "cd " (project-root) " && bundle exec ruby -I'lib:test' " (shell-quote-argument (buffer-file-name)) " -n /'"  context "'/"))))))


(provide 'shoulda)
;;; shoulda.el ends here
