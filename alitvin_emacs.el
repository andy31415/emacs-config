;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I use a few packages here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defun need-package (package-name)
  "Require PACKAGE-NAME. If not available, do package-install on it and then require."
  (unless (require package-name nil 'noerror)
    (message "Need to load %s" package-name)
    (package-install package-name)
    (require package-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Key bindings - I really like/need these
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(need-package 'evil)
(setq evil-default-cursor "ivory") ;; Don't want black cursor
(evil-mode 1)
(blink-cursor-mode 0) ;; No blinking is better

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color theme + A theme similar to sublime text 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(need-package 'color-theme)
(defun sublime-text-2 ()
  (interactive)
  (color-theme-install
   '(sublime-text-2
      ((background-color . "#171717")
      (background-mode . dark)
      (border-color . "#1a1a1a")
      (cursor-color . "#fce94f")
      (foreground-color . "#cfbfad")
      (mouse-color . "ivory"))
     (cursor ((t (:background "ivory"))))
     (fringe ((t (:background "#1a1a1a"))))
     (mode-line ((t (:foreground "#eeeeec" :background "#555753"))))
     (region ((t (:foreground "#404040" :background "#CC9900"))))
     (font-lock-builtin-face ((t (:foreground "#52e3f6"))))
     (font-lock-comment-face ((t (:foreground "#6495ed"))))
     (font-lock-function-name-face ((t (:foreground "#edd400"))))
     (font-lock-keyword-face ((t (:foreground "#ff007f"))))
     (font-lock-string-face ((t (:foreground "#ece47e"))))
     (font-lock-type-face ((t (:foreground"#8ae234"))))
     (font-lock-variable-name-face ((t (:foreground "#8ae234"))))
     (minibuffer-prompt ((t (:foreground "#729fcf" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))
(sublime-text-2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tab handling and such
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default indent-tabs-mode nil) ;; I like space-indent, 2 spaces
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)

(set-default-font "Inconsolata-14") ;; hopefully this is installed

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Icy mode - this completion is awesome
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(need-package 'icicles)
(icy-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(need-package 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Company-mode: better auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(need-package 'company)
(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; powerline: eye-candy for the status bar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(need-package 'powerline)
(add-hook 'post-command-hook 
	  (lambda ()
	    (when (not (minibuffer-selected-window))
	      (setq powerline-selected-window (selected-window)))))
(powerline-default-theme)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FlyMake: syntax checking on the fly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(need-package 'flymake)
(add-hook 'java-mode-hook 'flymake-mode-on)

;; ELISP
(defun flymake-elisp-init ()
  (unless (string-match "^ " (buffer-name))
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name))))
      (list
       (expand-file-name invocation-name invocation-directory)
       (list
        "-Q" "--batch" "--eval" 
        (prin1-to-string
         (quote
          (dolist (file command-line-args-left)
            (with-temp-buffer
              (insert-file-contents file)
              (condition-case data
                  (scan-sexps (point-min) (point-max))
                (scan-error
                 (goto-char(nth 2 data))
                 (princ (format "%s:%s: error: Unmatched bracket or quote\n"
                                file (line-number-at-pos)))))))
          )
         )
        local-file)))))
(push '("\\.el$" flymake-elisp-init) flymake-allowed-file-name-masks)
(add-hook 'emacs-lisp-mode-hook
          ;; workaround for (eq buffer-file-name nil)
          (function (lambda () (if buffer-file-name (flymake-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
