;;; Some generic settings to start
(setq make-backup-files nil)
(set-default 'truncate-lines t)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-o") 'icicle-other-window-or-frame)

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
(setq x-stretch-cursor t)

;; make it back after insert
(setq-default cursor-type 'box)
(add-hook 'post-command-hook
          (lambda () (setq cursor-type 'box)))

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
     (whitespace-space ((t (:foreground "grey23"))))
     (whitespace-newline ((t (:foreground "grey21" :background "#171717"))))
     (whitespace-tab ((t (:foreground "grey21" :background "#171717"))))
     (whitespace-indentation ((t (:foreground "grey21" :background "#272727"))))
     (whitespace-empty ((t (:foreground "grey27"))))
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

; general settings
(when (display-graphic-p)
        (set-face-attribute 'default nil :font "Inconsolata-14"))
(setq inhibit-startup-message t)

(add-hook 'java-mode-hook  '(lambda () (font-lock-set-up-width-warning 100)))
(add-hook 'python-mode-hook  '(lambda () (font-lock-set-up-width-warning 80)))
(add-hook 'js-mode-hook  '(lambda () (font-lock-set-up-width-warning 80)))

(tool-bar-mode -1)

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

;; Javascript
;; FROM: http://www.emacswiki.org/emacs/FlymakeJavaScript
;; Make sure you have "jslint":
;;   1. Install node.js
;;   2. npm -g install jslint
(need-package 'flymake-easy)
(need-package 'flymake-jslint)
(add-hook 'js-mode-hook 'flymake-jslint-load)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MMM-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)
(need-package 'mmm-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple-cursors: similar to Sublime Text 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(need-package 'multiple-cursors)

(global-set-key (kbd "M-D") 'mc/mark-previous-like-this) ;; instead of kill-word
(global-set-key (kbd "M-d") 'mc/mark-next-like-this) ;; instead of kill-word

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SR-speedbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(need-package 'sr-speedbar)
(global-set-key "\M-s" 'sr-speedbar-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(need-package 'less-css-mode)
(need-package 'whitespace)
(need-package 'magit)

(global-whitespace-mode t)
(setq whitespace-line-column 9999) ; I don't like this highlighting

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(need-package 'grizzl)
(message "File name: %s" load-file-name)
(let ((script-dir (file-name-directory load-file-name)))
  (load (expand-file-name "project_files.el" script-dir)))

(global-set-key (kbd "M-p s") 'project/set-directory)
(global-set-key (kbd "M-p M-s") 'project/set-directory)
(global-set-key (kbd "M-p f") 'project/select-file)
(global-set-key (kbd "M-p M-f") 'project/select-file)

(setq *grizzl-read-max-results* 40)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; More key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c C-b") 'ibuffer)
(global-set-key (kbd "M-<up>") 'evil-window-up)
(global-set-key (kbd "M-<down>") 'evil-window-down)
(global-set-key (kbd "M-<left>") 'evil-window-left)
(global-set-key (kbd "M-<right>") 'evil-window-right)

;; terminal keys: alt up/down is page-up/down for some reason.
;; I use vim, so C-f/C-b, C-d/C-u are better
(global-set-key (kbd "<prior>") 'evil-window-up)
(global-set-key (kbd "<next>") 'evil-window-down)

;; "jk" means "ESC"
(need-package 'key-chord)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
(key-chord-mode 1)

;; fast project rgrep
(defun rgrep-project ()
  "Do a rgrep on all files in the current directory"
  (interactive)
  (rgrep (read-string (format "Search in '%s'\nRegex: " project/directory))
         (read-string "Files: " nil 'rgrep-project-history "*")
         project/directory))
(global-set-key (kbd "M-p g") 'rgrep-project)

;; fast magit on a project
(defun magit-project ()
  "Run magit status on a project"
  (interactive)
  (magit-status project/directory))
(global-set-key (kbd "M-p c") 'magit-project)
