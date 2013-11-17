;;
;; Handles project file opening.
;;
;; A project is a directory with any type of files and we use
;; grizzl to have a nice auto-complete for the files we open
;;

(require 'grizzl) ;; our fuzzy search provider

(setq project/directory nil) ;; where the current project is located

(defun project/accept-dir (dir-name)
  "Find out if DIR-NAME is something we want to check for project data"
  (not (or (member dir-name '("." ".." ".git"))
           (string-prefix-p ".git5" dir-name)
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
                (setq items (append (project/directory-files-recursive
                                     full-name
                                     (concat (if (consp prefix) prefix "") file-name "/"))
                                    items)))
          (setq items (cons (concat prefix file-name) items)))))
    items))

(defun project/set-directory ()
  (interactive)
  (setq project/directory
        (expand-file-name (read-file-name "Project Directory: "
                                          project/directory nil t
                                          nil 'file-directory-p)))
  (setq project/files-index
        (grizzl-make-index (project/directory-files-recursive project/directory))))

(defun project/select-file ()
  (interactive)
  (if (not project/directory)
      (error "No project defined. Use project/set-directory"))
  (switch-to-buffer
   (create-file-buffer (concat project/directory "/"
                               (grizzl-completing-read "File: " project/files-index)))))
