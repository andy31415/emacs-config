;;; Some generic settings to start
(setq make-backup-files nil)
(set-default 'truncate-lines t)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-0") 'icicle-delete-window)
(global-set-key (kbd "M-o") 'icicle-other-window-or-frame)
(setq scss-compile-at-save nil)
(setq redisplay-dont-pause t)

;; always end a file with a newline
(setq require-final-newline t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I use a few packages here
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
  '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(defun need-package (package-name)
  "Require PACKAGE-NAME. If not available, do package-install on it and then require."
  (condition-case nil
      (unless (require package-name nil 'noerror)
        (message "Need to load %s" package-name)
        (package-install package-name)
        (require package-name))
    ((debug error) (message "!! FAILED %s !!" package-name))))


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

(defun flymake-on ()
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
  (add-hook 'js-mode-hook 'flymake-jslint-load))

(need-package 'js2-mode)

;; With closure, find the require/provide and assume available
(add-hook 'js2-post-parse-callbacks
          (lambda ()
            (let ((buf (buffer-string))
                  (index 0))
              (while (string-match "\\(goog\\.require\\|goog\\.provide\\)('\\([^'.]*\\)" buf index)
                (setq index (+ 1 (match-end 0)))
                (add-to-list 'js2-additional-externs (match-string 2 buf))))))
  
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
;; SR-peedbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(need-package 'sr-speedbar)
(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)
(custom-set-variables '(speedbar-show-unknown-files t))

(require 'neotree)
(global-set-key (kbd "<f8>") 'neotree-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(need-package 'helm)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c f") 'helm-find-files)
(global-set-key (kbd "C-c g") 'helm-do-grep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ECLIM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(need-package 'emacs-eclim)
(require 'eclim)
(require 'eclimd)
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)

(add-hook 'java-mode-hook '(lambda ()
        (define-key java-mode-map (kbd "C-/") 'eclim-complete)
        (define-key undo-tree-map (kbd "C-/") 'eclim-complete)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(need-package 'less-css-mode)
(need-package 'whitespace)
(need-package 'magit)
(need-package 'ace-jump-mode)
(need-package 'dired+)

(autoload 'ace-jump-mode "ace-jump-mode" "Emacs quick move minor mode" t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(autoload 'ace-jump-mode-pop-mark "ace-jump-mode" "Ace jump back:-)" t)
(eval-after-load "ace-jump-mode" '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

(need-package 'tabbar)
(tabbar-mode)
(defun my-tabbar-buffer-groups () ;; customize to show all normal files in one group
   "Returns the name of the tab group names the current buffer belongs to.
 There are two groups: Emacs buffers (those whose name starts with '*', plus
 dired buffers), and the rest.  This works at least with Emacs v24.2 using
 tabbar.el v1.7."
   (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
               ((eq major-mode 'dired-mode) "emacs")
               (t "user"))))
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)
(define-key global-map (kbd "M-S-<left>") 'tabbar-backward)
(define-key global-map (kbd "M-S-<right>") 'tabbar-forward)
(define-key global-map (kbd "M-w") '(kill-buffer (current-buffer)))

(need-package 'recentf)
(recentf-mode 1)
(define-key global-map (kbd "C-c C-r") 'icicle-recent-file)


(global-whitespace-mode t)
(setq whitespace-line-column 9999) ; I don't like this highlighting

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extra scripts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "File name: %s" load-file-name)
(let ((script-dir (file-name-directory load-file-name)))
  (load (expand-file-name "project_files.el" script-dir)))


(global-set-key (kbd "M-p s") 'project/set-directory)
(global-set-key (kbd "M-p M-s") 'project/set-directory)

(global-set-key (kbd "M-p f") 'project/select-file)
(global-set-key (kbd "M-p M-f") 'project/select-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; More key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c e f") 'first-error)
(global-set-key (kbd "M-g f") 'first-error)
(global-set-key (kbd "M-g M-f") 'first-error)
(global-set-key (kbd "M-g r") 'recompile)
(global-set-key (kbd "C-l") 'redraw-display)

(global-set-key (kbd "C-c C-b") 'ibuffer)
(global-set-key (kbd "M-<up>") 'evil-window-up)
(global-set-key (kbd "M-<down>") 'evil-window-down)
(global-set-key (kbd "M-<left>") 'evil-window-left)
(global-set-key (kbd "M-<right>") 'evil-window-right)

; ChromeOS terminal has some odd mappings
(global-set-key (kbd "ESC M-[ d") 'evil-window-left)
(global-set-key (kbd "ESC M-[ c") 'evil-window-right)

(global-set-key (kbd "M-h") 'evil-window-left)
(global-set-key (kbd "M-l") 'evil-window-right)
(global-set-key (kbd "M-k") 'evil-window-up)
(global-set-key (kbd "M-j") 'evil-window-down)

(defun rebind-navigation-c ()
   (define-key c-mode-base-map (kbd "M-h") 'evil-window-left)
   (define-key c-mode-base-map (kbd "M-l") 'evil-window-right)
   (define-key c-mode-base-map (kbd "M-k") 'evil-window-up)
   (define-key c-mode-base-map (kbd "M-j") 'evil-window-down))
(add-hook 'c-initialization-hook 'rebind-navigation-c)

;; make ibuffer behave more "evil"-like
(defun rebind-navigation-ibuffer ()
   (define-key ibuffer-mode-map (kbd "j") 'next-line)
   (define-key ibuffer-mode-map (kbd "k") 'previous-line)
   (define-key ibuffer-mode-map (kbd "M-h") 'evil-window-left)
   (define-key ibuffer-mode-map (kbd "M-l") 'evil-window-right)
   (define-key ibuffer-mode-map (kbd "M-k") 'evil-window-up)
   (define-key ibuffer-mode-map (kbd "M-j") 'evil-window-down))
(add-hook 'ibuffer-mode-hooks 'rebind-navigation-ibuffer)

(global-set-key (kbd "M-i") 'ibuffer)

(defun page-up-handler () (interactive) (if (display-graphic-p) (evil-scroll-page-up 1) (evil-window-up 1)))
(defun page-down-handler () (interactive) (if (display-graphic-p) (evil-scroll-page-down 1) (evil-window-down 1)))

;; terminal keys: alt up/down is page-up/down for some reason.
;; I use vim, so C-f/C-b, C-d/C-u are better
(global-set-key (kbd "<prior>") 'page-up-handler)
(global-set-key (kbd "<next>") 'page-down-handler)

;; "jk" means "ESC"
(need-package 'key-chord)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map  "jk" 'evil-normal-state)
(key-chord-mode 1)

;; fast project rgrep
(global-set-key (kbd "M-p g") 'project-grep)

;; fast magit on a project
(defun magit-project ()
  "Run magit status on a project"
  (interactive)
  (magit-status project/directory))
(global-set-key (kbd "M-p c") 'magit-project)

;;;;;;;;;;;;; Sending a region to shell ;;;;;;;;;;;;;
(defun sh-send-line-or-region (&optional step)
  (interactive ())
  (let ((proc (get-process "shell"))
        pbuf min max command)
    (unless proc
      (let ((currbuff (current-buffer)))
        (shell)
        (switch-to-buffer currbuff)
        (setq proc (get-process "shell"))
        ))
    (setq pbuff (process-buffer proc))
    (if (use-region-p)
        (setq min (region-beginning)
              max (region-end))
      (setq min (point-at-bol)
            max (point-at-eol)))
    (setq command (concat (buffer-substring min max) "\n"))
    (with-current-buffer pbuff
      (goto-char (process-mark proc))
      (insert command)
      (move-marker (process-mark proc) (point))
      ) ;;pop-to-buffer does not work with save-current-buffer -- bug?
    (process-send-string  proc command)
    (display-buffer (process-buffer proc) t)
    (when step
      (goto-char max)
      (next-line))
    ))

(defun sh-send-line-or-region-and-step ()
  (interactive) (sh-send-line-or-region t))
(defun sh-switch-to-process-buffer ()
  (interactive) (pop-to-buffer (process-buffer (get-process "shell")) t))

(global-set-key (kbd "M-g s") 'sh-send-line-or-region-and-step)
(global-set-key (kbd "M-g e") 'sh-switch-to-process-buffer)

(modify-syntax-entry ?_ "w")
(modify-syntax-entry ?_ "w" java-mode-syntax-table)

(setq vc-ignore-dir-regexp
                (format "\\(%s\\)\\|\\(%s\\)"
                        vc-ignore-dir-regexp
                        tramp-file-name-regexp))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
