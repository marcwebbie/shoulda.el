;; copied from textmate.el

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

(provide 'project-root)
