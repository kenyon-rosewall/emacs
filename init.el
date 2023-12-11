;;
;; ELPACA
;;
(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))
;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))
;; Block until current queue processed.
(elpaca-wait)
;;When installing a package which modifies a form used at the top-level
;;(e.g. a package which adds a use-package key word),
;;use `elpaca-wait' to block until that package has been installed/configured.
;;For example:
;;(use-package general :demand t)
;;(elpaca-wait)
;;Turns off elpaca-use-package-mode current declartion
;;Note this will cause the declaration to be interpreted immediately (not deferred).
;;Useful for configuring built-in emacs features.
;;(use-package emacs :elpaca nil :config (setq ring-bell-function #'ignore))
;; Don't install anything. Defer execution of BODY
;;(elpaca nil (message "deferred"))

;;
;; THEMES
;;
(use-package material-theme
  :init
  (load-theme 'material t))

;;
;; DIMMER
;;
(use-package dimmer
  :init
  (dimmer-configure-which-key)
  (dimmer-configure-magit)
  (dimmer-configure-gnus)
  (dimmer-mode t)
  :config
  (setq dimmer-fraction 0.5))
;;
;; ALL THE ICONS
;;
(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :hook
  (dired-mode . (lambda () (all-the-icons-dired-mode t)))
  (dired-sidebar-mode . (lambda () (all-the-icons-dired-mode))))

;;
;; VSCODE ICONS
;;
(use-package vscode-icon
  :ensure t
  :commands (vscode-icon-for-file))

;;
;; FONTS
;;
(set-face-attribute 'default nil
		                :font "JetBrains Mono"
		                :height 110
		                :weight 'medium)
(set-face-attribute 'variable-pitch nil
		                :font "Ubuntu"
		                :height 120
		                :weight 'medium)
(set-face-attribute 'fixed-pitch nil
		                :font "JetBrains Mono"
		                :height 110
		                :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
		                :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		                :slant 'italic)
;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "JetBrains Mono-11"))
;; Uncomment the following line if line spacing needs adjusting.
(setq-default line-spacing 0.12)

;;
;; UI TWEAKS
;;
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq scroll-step 3)
(display-time)
(global-display-line-numbers-mode 1)
(global-visual-line-mode 1)
(global-hl-line-mode 1)
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . js-jsx-mode))

;;
;; INDENTATION
;;
(setq custom-indent-width 2)
(defun disable-tabs () (setq indent-tabs-mode nil))
(add-hook 'prog-mode-hook 'disable-tabs)
(add-hook 'typescript-mode-hook 'disable-tabs)
(setq-default js-indent-level custom-indent-width)
(setq-default js-jsx-indent-level custom-indent-width)
(setq-default typescript-indent-level custom-indent-width)
(setq-default web-mode-markup-indent-offset custom-indent-width)
(setq-default web-mode-css-indent-offset custom-indent-width)
(setq-default web-mode-code-indent-offset custom-indent-width)
(setq-default js2-indent-level custom-indent-width)
(setq-default evil-shift-width custom-indent-width)
;;; (setq-default electric-indent-inhibit t)
;;; Show whitespace characters
(setq whitespace-style '(face tabs tab-mark trailing))
(custom-set-faces
 '(whitespace-tab ((t (:foreground "#636363")))))
(setq whitespace-display-mappings
  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
(global-whitespace-mode) ; Enable whitespace mode everywhere

;;
;; IVY
;;
(use-package counsel
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1))
(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))
(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :diminish
  :custom
  (ivy-virtual-abbreviate 'full
			                    ivy-rich-switch-buffer-align-virtual-buffer t
			                    ivy-rich-path-style 'abbrev))

;;
;; ENV VARIABLES
;;
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;;
;; EVIL MODE
;;
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-move-beyond-eol t)
  (evil-mode))
(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))
(use-package evil-tutor)

;;
;; DIMINISH
;;
(use-package diminish)

;;
;; LANGUAGE SUPPORT
;;
(use-package haskell-mode)
(use-package lua-mode)
(use-package typescript-mode)

;;
;; PROJECTILE
;;
(use-package projectile
  :bind (("M-p" . projectile-command-map))
  :init
  (projectile-mode 1))

;;
;; DIRED-SIDEBAR
;;
(use-package dired-sidebar
  :bind (("C-`" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
	          (lambda ()
	            (unless (file-remote-p default-directory)
	              (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t)
  (setq dired-sidebar-use-custom-font t))

;;
;; TELEPHONE LINE
;;
(use-package telephone-line
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-gradient
        telephone-line-secondary-left-separator 'telephone-line-nil
        telephone-line-primary-right-separator 'telephone-line-gradient
        telephone-line-secondary-right-separator 'telephone-line-nil)
  (setq telephone-line-height 24
        telephone-line-evil-use-short-tag t) 
  (telephone-line-mode 1))

;;
;; CENTAUR TABS
;;
;; (use-package centaur-tabs
;;   :demand
;;   :hook
;;   (dired-mode . centaur-tabs-local-mode)
;;   (dashboard-mode . centaur-tabs-local-mode)
;;   (term-mode . centaur-tabs-local-mode)
;;   (calendar-mode . centaur-tabs-local-mode)
;;   (org-agenda-mode . centaur-tabs-local-mode) 
;;   (magit-mode . centaur-tabs-local-mode)
;;   :config
;;   (defun centaur-tabs-hide-tab (x)
;;     "Do not show buffr X in tabs."
;;     (let ((name (format "%s" x)))
;;       (or
;;        (window dedicated-p (selected-window))
;;        (string-prefix-p "*" name)
;;        (string-prefix-p "magit" name))))
;;   (setq centaur-tabs-style "bar"
;;         centaur-tabs-height 32
;;         centaur-tabs-show-new-tab-button nil
;;         centaur-tabs-set-icons t
;;         centaur-tabs-gray-out-icons 'buffer
;;         centaur-tabs-set-bar 'over
;;         centaur-tabs-set-modified-marker t
;;         centaur-tabs-modified-marker "●"
;;         centaur-tabs-set-close-button nil)
;;   (centaur-tabs-headline-match)
;;   (centaur-tabs-group-by-projectile-project)
;;   (defun centaur-tabs-buffer-groups ()
;;     "Use as few groups as possible."
;;     (list (cond ((string-equal "*" (substring (buffer-name) 0 1))
;;                  (cond ((string-equal "eglot" (downcase (substring (buffer-name) 1 6)))
;;                         "Eglot")
;;                        (t
;;                         "Tools")))
;;                 ((string-equal "magit" (downcase (substring (buffer-name) 0 5)))
;;                  "Magit")
;;                 (t
;;                  "Default"))))
;;   (centaur-tabs-mode t))

;;
;; ESHELL
;;
(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))
;; eshell-syntax-highlighting -- adds fish/zsh-like syntax highlighting.
;; eshell-rc-script -- your profile for eshell; like a bashrc for eshell.
;; eshell-aliases-file -- sets an aliases file for the eshell.
(setq eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "fish" "htop" "ssh" "top" "zsh"))

;;
;; MAGIT
;;
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

;;
;; CONSULT
;;
(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
  (consult-customize
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-file-register
   consult--source-recent-file consult--source-project-recent-file
   :preview-key '(:debounce 0.4 any))
  (setq consult-narrow-key "<"))

;;
;; DASHBOARD
;;
(use-package dashboard
  :ensure t 
  :init
  (setq initial-buffer-choice 'dashboard-open)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Emacs Is More Than A Text Editor!")
  (setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
  ;;(setq dashboard-startup-banner "/home/dt/.config/emacs/images/dtmacs-logo.png")  ;; use custom image as banner
  (setq dashboard-center-content nil) ;; set to 't' for centered content
  (setq dashboard-items '((recents . 5)
                          (agenda . 5 )
                          (bookmarks . 3)
                          (projects . 3)
                          (registers . 3)))
  :custom 
  (dashboard-modify-heading-icons '((recents . "file-text")
				                            (bookmarks . "book")))
  :config
  (dashboard-setup-startup-hook))

;;
;; MOVE-TEXT/WINDMOVE
;;
(use-package move-text
  :config
  (move-text-default-bindings))
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;;
;; COPILOT
;;
(elpaca (copilot
         :type git :host github
         :repo "zerolfx/copilot.el" :branch "main"
         :files ("dist" "*.el")))
(add-hook 'prog-mode-hook 'copilot-mode)

;;
;; FUNCTIONS
;;
(defun replace-in-region (old-word new-word)
  "Perform a replace-string in the current region."
  (interactive "sReplac: \nsReplace: %s  With: ")
  (save-excursion (save-restriction
		                (narrow-to-rgion (mark) (point))
		                (beginning-of-buffer)
		                (replace-string old-word new-word))))
;;;
(defun insert-line-above ()
  (interactive)
  (evil-previous-line)
  (evil-end-of-visual-line)
  (newline-and-indent)
  (evil-insert 1))
(defun insert-line-below ()
  (interactive)
  (evil-end-of-visual-line)
  (newline-and-indent)
  (evil-insert 1))
;;;
(defun scroll-up-hold-cursor ()
  (interactive)
  (scroll-up-command 1))
(defun scroll-down-hold-cursor ()
  (interactive)
  (scroll-down-command 1))
;;;
(defun transpose-backwords ()
  (interactive)
  (transpose-words -1))
;;;
(defun open-config-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
;;;
(defun reload-config-file ()
  (interactive)
  (load-file user-init-file))
;;;
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))
;;;
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

;;
;; GENERAL/CUSTOM KEYBINDINGS
;;
(use-package general
  :config
  (general-evil-setup)
  ;; Set up 'SPC' as the global leader key
  (general-create-definer kr/leader-keys
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "M-SPC")
  (general-create-definer kr/goto-keys
    :states 'normal
    :keymaps 'override
    :prefix "g")
  (kr/leader-keys
    "," '(find-file :wk "Find file")
    "'" '(counsel-recentf :wk "Find recent files")
    "." '(find-file-other-window :wk "Find file other window")
    "c c" '(open-config-file :wk "Open config")
    "c l" '((lambda () (interactive) (load-file user-init-file) (load-file user-inite-file)) :wk "Reload emacs config"))
  (kr/leader-keys
    "b" '(:ignore t :wk "buffer")
    "b b" '(switch-to-buffer :wk "Switch buffer")
    "b i" '(ibuffer :wk "Ibuffer")
    "b k" '(kill-this-buffer :wk "Kill this buffer")
    "b n" '(next-buffer :wk "Next buffer")
    "b p" '(previous-buffer :wk "Previous buffer")
    "b r" '(revert-buffer :wk "Reload buffer")
    "b s" '(save-buffer :wk "Save buffer")
    "b c" '(save-buffers-kill-terminal :wk "Save and close"))
  (kr/leader-keys
    "e" '(:ignore t :wk "Eshell/Evaluate")
    "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
    "e f" '(eval-defun :wk "Evaluate function containing point")
    "e e" '(eval-expression :wk "Evaluate an elisp expression")
    "e h" '(counsel-esh-history :wk "Eshell history")
    "e s" '(eshell :wk "Eshell"))
  (kr/goto-keys
    "l" '(consult-goto-line :wk "consult-goto-line"))
  (kr/leader-keys
    "h" '(:ignore t :wk "Help")
    "h f" '(describe-function :wk "Describe function")
    "h k" '(describe-key :wk "Describe keybinding")
    "h c" '(describe-key-briefly :wk "Describe keybinding brief")
    "h v" '(describe-variable :wk "Describe variable"))
  (kr/leader-keys
    "p" '(:ignore t :wk "Projectile")
    "p a" '(projectile-find-other-file :wk "Find other file")
    "p b" '(projectile-switch-to-buffer :wk "Switch buffer")
    "p c" '(projectile-compile-project :wk "Compile project")
    "p f" '(projectile-find-file :wk "Find file in project")
    "p g" '(projectile-grep :wk "Grep project")
    "p k" '(projectile-kill-buffers :wk "Kill buffers")
    "p p" '(projectile-switch-project :wk "Switch project")
    "p r" '(projectile-replace :wk "Replace in project")
    "p t" '(projectile-toggle-between-implementation-and-test :wk "Toggle between implementation and test")
    "p T" '(projectile-test-project :wk "Test project")
    "p u" '(projectile-run-project :wk "Run project"))
  (kr/leader-keys
    "r" '(:ignore t :wk "Register")
    "r s" '(consult-register-store :wk "Register store")
    "r l" '(consult-register-load :wk "Register load")
    "r r" '(consult-register :wk "Register view"))
  (kr/leader-keys
    "t" '(:ignore t :wk "Toggle")
    "t l" '(display-line-numbers-mode :wk "Toggle line numbers")
    "t t" '(toggle-truncate-lines :wk "Toggle word wrap")
    "t d" '(dired-sidebar-toggle-sidebar :wk "Toggle sidebar")
    "t c" '(copilot-mode :wk "Toggle copilot"))
  (kr/leader-keys
    "w" '(:ignore t :wk "Windows")
    "w c" '(evil-window-delete :wk "Close window")
    "w n" '(evil-window-new :wk "New window")
    "w s" '(evil-window-split :wk "Horizontal split")
    "w v" '(evil-window-vsplit :wk "Vertical split")
    "w w" '(delete-other-windows :wk "Next window"))
  (general-define-key
   :states '(normal visual)
   ;;; Movement
   "h" 'left-char
   "t" 'evil-next-line
   "c" 'evil-previous-line
   "n" 'right-char
   "o" 'evil-first-non-blank
   "e" 'scroll-up-hold-cursor
   "." 'scroll-down-hold-cursor
   "u" 'evil-end-of-line
   "C-h" 'left-word
   "C-t" 'evil-forward-paragraph
   "C-c" 'evil-backward-paragraph
   "C-n" 'right-word
   "C-o" 'beginning-of-buffer
   "C-e" 'evil-scroll-page-down
   "C-." 'evil-scroll-page-up
   "C-u" 'end-of-buffer
   "M-t" 'move-text-down
   "M-c" 'move-text-up
   "C-M-h" 'transpose-backwords
   "C-M-n" 'transpose-words
   ;;; Editing
   "a" 'evil-insert
   "A" 'evil-insert-line
   "i" 'evil-append
   "I" 'evil-append-line
   "," 'evil-open-below
   "<" 'evil-open-above
   ";" 'evil-undo
   "q" 'kill-region
   "j" 'kill-ring-save
   "k" 'yank
   "/" 'consult-line
   "?" 'consult-line-multi
   "C-/" 'query-replace
   "C-?" 'query-replace-regexp
   ;;; Other
   "C-=" 'text-scale-increase
   "C--" 'text-scale-decrease
   ;; "<M-right>" 'centaur-tabs-forward
   ;; "<M-left>" 'centaur-tabs-backward
   )
  (general-define-key
   :keymaps '(global cc-mode)
   "<backtab>" 'indent-for-tab-command
   "TAB" 'copilot-accept-completion
   "<C-tab>" 'dabbrev-expand
   "C-s" 'save-buffer
   "C-'" 'save-buffers-kill-terminal
   "C-;" 'evil-undo
   "C-q" 'kill-region
   "C-j" 'kill-ring-save
   "C-k" 'yank
   "C-o" 'evil-first-non-blank
   "C-u" 'evil-end-of-line
   "C-\\" 'comment-or-uncomment-region-or-line
   "C-c C-c" 'evil-normal-state
   "<C-return>" 'insert-line-below
   "C-<S-return>" 'insert-line-above
   ))

;;
;; VARIABLES
;;
(setq auto-save-default nil)
(setq auto-save-interval 0)
(setq auto-save-list-file-prefix nil)
(setq auto-save-timeout 0)
(setq auto-show-mode t)
(setq delete-auto-save-files nil)
(setq delete-old-versions 'other)
(setq make-backup-file-name-function 'ignore)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq selection-mode 1)

;;
;; WHICH KEY
;;
(use-package which-key
  :init
    (which-key-mode 1)
  :diminish
  :config
  (setq which-key-side-window-location 'bottom
	  which-key-sort-order #'which-key-key-order
	  which-key-allow-imprecise-window-fit nil
	  which-key-sort-uppercase-first nil
	  which-key-add-column-padding 1
	  which-key-max-display-columns nil
	  which-key-min-display-lines 6
	  which-key-side-window-slot -10
	  which-key-side-window-max-height 0.25
	  which-key-idle-delay 0.8
	  which-key-max-description-length 25
	  which-key-allow-imprecise-window-fit nil
	  which-key-separator " → " ))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values
   '((projectile-project-test-cmd)
     (projectile-project-compilation-cmd)
     (projectile-project-run-cmd . "ruby main.rb"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
