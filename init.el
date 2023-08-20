;;
;; Install Elpaca
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
(split-window-horizontally)
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

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1) ;; this gets us descriptions in M-x.
  :custom
  (ivy-virtual-abbreviate 'full
			  ivy-rich-switch-buffer-align-virtual-buffer t
			  ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

;;
;; MAGIT
;;
(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

;;
;; MOVE TEXT
;;
(use-package move-text
  :config
  (move-text-default-bindings))
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

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
;; FUNCTIONS
;;
(defun replace-in-region (old-word new-word)
  "Perform a replace-string in the current region."
  (interactive "sReplac: \nsReplace: %s  With: ")
  (save-excursion (save-restriction
		    (narrow-to-rgion (mark) (point))
		    (beginning-of-buffer)
		    (replace-string old-word new-word))))

(defun insert-line-above ()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))
(defun insert-line-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun scroll-up-hold-cursor ()
  (interactive)
  (scroll-up-command 1))
(defun scroll-down-hold-cursor ()
  (interactive)
  (scroll-down-command 1))

(defun transpose-backwords ()
  (interactive)
  (transpose-words -1))

;;
;; GENERAL/CUSTOM KEYBINDINGS
;;
(use-package general
  :config
  (general-define-key
   "C-o" 'find-file
   ;;"C-s" 'save-buffer

   "<C-return>" 'insert-line-below
   "C-<S-return>" 'insert-line-above
   "C-9" 'scroll-up-hold-cursor
   "C-0" 'scroll-down-hold-cursor

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

