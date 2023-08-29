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
;; Or, if you use `use-package', do something like this:
(use-package ample-theme
  :init (progn (load-theme 'ample t t)
               (enable-theme 'ample))
  :defer t
  :ensure t)
(with-eval-after-load "ample-theme"
  ;; add one of these blocks for each of the themes you want to customize
  (custom-theme-set-faces
   'ample
    ;; this will overwride the color of strings just for ample-theme
   '(font-lock-string-face ((t (:foreground "#bdba81"))))))

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
(global-visual-line-mode t)
(global-hl-line-mode 1)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
(defun nil-bell ())
(setq ring-bell-function 'nil-bell)
(setq tab-width 2
      indent-tabs-mode nil)

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
			  ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

;;
;; ENV VARIABLES
;;
(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;;; Adding a new comment here

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
  :config
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
;; COMPANY
;;
(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

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
  :bind (
	 ("C-x b" . consult-buffer)
	 ("M-g g" . consult-goto-line)
	 ("C-s" . consult-line))
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
  (previous-line)
  (end-of-line)
  (newline-and-indent))
(defun insert-line-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
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

;;
;; GENERAL/CUSTOM KEYBINDINGS
;;
(use-package general
  :config
  (general-define-key
   "C-o" 'find-file
   ;;; Buffers
   "M-r" 'revert-buffer
   "M-k" 'kill-this-buffer
   "M-s" 'save-buffer
   ;;;
   "C-c C-c" 'open-config-file
   ;;;
   "<C-return>" 'insert-line-below
   "C-<S-return>" 'insert-line-above
   "C-9" 'scroll-up-hold-cursor
   "C-0" 'scroll-down-hold-cursor
   ;;;
   "C-=" 'transpose-words
   "C-/" 'transpose-backwords
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

