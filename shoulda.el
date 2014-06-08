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

(defun shoulda:run-should-at-point ()
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
        (compilation-start (concat "cd " (projectile-project-root) " && bundle exec -- ruby " (buffer-file-name) " -n /'"  should "'/"))))))

(defun shoulda:run-context-at-point ()
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
        (compilation-start (concat "cd " (projectile-project-root) " && bundle exec -- ruby " (buffer-file-name) " -n /'"  context "'/"))))))


(provide 'shoulda)
;;; shoulda.el ends here
