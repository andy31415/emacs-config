;;
;; Handles project file opening.
;;
;; A project is a directory with any type of files and we use
;; grizzl to have a nice auto-complete for the files we open
;;

(need-package 'flx) ;; our fuzzy search provider
(need-package 'flx-ido) ;; our fuzzy search provider
(need-package 'ido-vertical-mode) ;; our fuzzy search provider

(flx-ido-mode 1)
(setq ido-use-faces nil) ;; disable ido faces to see flx highlights.

(ido-vertical-mode 1)

(setq gc-cons-threshold 100000000)

(defvar project/directory nil "Directory path for project open")
(defvar project/files-index nil "Compiled index of files in the project")

(defun project/accept-dir (dir-name)
  "Find out if DIR-NAME is something we want to check for project data"
  (not (or (member dir-name '("." ".." ".git"))
           (string-prefix-p ".git5" dir-name)
           (string-prefix-p ".completions-" dir-name)
           (string-prefix-p "blaze-" dir-name))))

(defun project/directory-files-recursive (dir &optional prefix)
  "Find all files in DIR (without recursing into non `accept-dir' directories"
  (unless (file-directory-p dir)
    (error "Not a directory '%s'" dir))
  (let* ((dir (directory-file-name dir))
         (items '())
         (file-names (directory-files dir nil nil t)))
    (dolist (file-name file-names)
      (let ((full-name (concat dir "/" file-name)))
        (if (file-directory-p full-name)
            (if (project/accept-dir file-name)
                (progn
                  (message "Recursing into %s%s" (or prefix "") file-name)
                  (setq items (append (project/directory-files-recursive
                                       full-name
                                       (concat prefix file-name "/"))
                                      items))))
          (setq items (cons (concat prefix file-name) items)))))
    items))

(defun project/set-directory ()
  (interactive)
  (setq project/directory
        (expand-file-name (read-file-name "Project Directory: "
                                          project/directory nil t
                                          "" 'file-directory-p)))
  (setq project/files-list (project/directory-files-recursive project/directory)))

(defun project/select-file ()
  (interactive)
  (if (not project/directory)
      (error "No project defined. Use project/set-directory"))
  (find-file (expand-file-name
              (concat project/directory "/" (ido-completing-read "File:" project/files-list)))))

(setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right)

;; Nice helper functions
(defun project-grep ()
  (interactive)
  (helm-do-grep-1 (let ((search-dir
                         (if (string-match (regexp-quote "google3") project/directory)
                             project/directory
                           (concat project/directory "google3"))))
                    (loop for sd in '("java" "javatests" "isp" "production")
                          collect (concat search-dir sd)))
                  t nil '("*")))
