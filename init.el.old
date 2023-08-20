(require 'package)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;; Stop Emacs from losing undo information by
;; setting very high limits for undo buffers
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)

;; Determine the underlying operating system
(setq kenyon-osx nil)
(setq kenyon-linux nil)
(setq kenyon-windows t)

(when kenyon-windows
  (set-frame-size (selected-frame) 200 50)
)

(when kenyon-osx 
  (cua-mode 0) 
  (setq mac-command-modifier 'meta)
  (setq x-select-enable-clipboard t)
  (setq aquamacs-save-options-on-quit 0)
  (setq special-display-regexps nil)
  (setq special-display-buffer-names nil)
  (define-key function-key-map [return] [13])
  (setq mac-command-key-is-meta t)
  (scroll-bar-mode nil)
  (setq mac-pass-command-to-system nil)
)

(when kenyon-linux
  (display-battery-mode 1)
)

(load-library "view")

(use-package counsel
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) "))

(use-package projectile
  :ensure t
  :bind-keymap ("s-p" . projectile-command-map)
  :init
  (setq projectile-mode-line-function '(lambda () (format " [%s]" (projectile-project-name))))
  :config
  (projectile-mode +1))

(use-package dracula-theme
  :config
  (add-to-list 'custom-theme-load-path "~/.emacs.d/elpa")
  (load-theme 'dracula t))

(defun frame-center ()
  "Center the current frame."
  (interactive)
  (let* ((dw (display-pixel-width))
         (dh (display-pixel-height))
         (f  (selected-frame))
         (fw (frame-pixel-width f))
         (fh (frame-pixel-height f))
         (x  (- (/ dw 2) (/ fw 2)))
         (y  (- (/ dh 2) (/ fh 2))))
    (message (format "dw %d dh %d fw %d fh %d x %d y %d" dw dh fw fh x y))
    (set-frame-position f x y)))
(frame-center)
(split-window-horizontally)

(menu-bar-mode -1)
(tool-bar-mode -1)
(display-time)
(global-hl-line-mode 1)
(scroll-bar-mode -1)
(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)

(defun nil-bell ())
(setq ring-bell-function 'nil-bell)
(setq scroll-step 3)

(setq tab-width 2
      indent-tabs-mode nil)
(global-unset-key [mouse-2])

(defun kenyon-replace-in-region (old-word new-word)
  "Perform a replace-string in the current region."
  (interactive "sReplace: \nsReplace: %s  With: ")
  (save-excursion (save-restriction
		    (narrow-to-region (mark) (point))
		    (beginning-of-buffer)
		    (replace-string old-word new-word)
		    ))
  )
(define-key global-map "\el" 'kenyon-replace-in-region)

(define-key global-map "\e/" 'start-kbd-macro)
(define-key global-map "\e=" 'end-kbd-macro)
(define-key global-map "\e-" 'call-last-kbd-macro)

(define-key global-map "\ef" 'find-file)
(define-key global-map "\eF" 'find-file-other-window)

(define-key global-map "\er" 'revert-buffer)
(define-key global-map "\ek" 'kill-this-buffer)
(define-key global-map "\es" 'save-buffer)

(define-key global-map "\t" 'dabbrev-expand)
(define-key global-map [C-tab] 'indent-for-tab-command)
(define-key global-map [M-tab] 'indent-region)

(defun insert-line-below ()
  (interactive)
  (end-of-line)
  (newline-and-indent))  
(defun insert-line-above ()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "<C-return>") 'insert-line-below)
(global-set-key (kbd "C-<S-return>") 'insert-line-above)

(defun kb-scroll-up-hold-cursor ()
  (interactive)
  (scroll-up-command 1))
(defun kb-scroll-down-hold-cursor ()
  (interactive)
  (scroll-down-command 1))
(global-set-key (kbd "C-9") 'kb-scroll-up-hold-cursor)
(global-set-key (kbd "C-0") 'kb-scroll-down-hold-cursor)

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

(use-package move-text
  :config
  (move-text-default-bindings))

(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun transpose-backwords ()
  "Transpose words backwords"
  (interactive)
  (transpose-words -1))
(global-set-key (kbd "C-=") 'transpose-words)
(global-set-key (kbd "C-/") 'transpose-backwords)

(use-package magit
  :init
  (message "Loading Magit!")
  :config
  (message "Loaded Magit!")
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(auto-save-interval 0)
 '(auto-save-list-file-prefix nil)
 '(auto-save-timeout 0)
 '(auto-show-mode t t)
 '(delete-auto-save-files nil)
 '(delete-old-versions 'other)
 '(imenu-auto-rescan t)
 '(imenu-auto-rescan-maxout 500000)
 '(kept-new-versions 5)
 '(kept-old-versions 5)
 '(make-backup-file-name-function 'ignore)
 '(make-backup-files nil)
 '(mouse-wheel-follow-mouse nil)
 '(mouse-wheel-progressive-speed nil)
 '(mouse-wheel-scroll-amount '(15))
 '(package-selected-packages '(move-text counsel projectile dracula-theme))
 '(version-control nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
