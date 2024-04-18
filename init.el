(load "hooks")

;; minimalistic ui
(setq-default inhibit-startup-message t            ; disable startup screen
              ring-bell-function 'ignore           ; disable sounds
              make-backup-files nil                ; disable backup files
              tab-width 4
              indent-tabs-mode nil                 ; use spaces instead of tabs
              display-line-numbers-type 'relative) ; use relative line numbers
(scroll-bar-mode -1)   ; disable visible scrollbar
(tool-bar-mode -1)     ; disable the toolbar
(tooltip-mode -1)      ; disable tooltips
(menu-bar-mode -1)     ; disable the menu bar
(set-fringe-mode 15)   ; add some horizontal padding
(electric-pair-mode t) ; auto pairs

;; changing to tree sitter modes
;;(setq major-mode-remap-alist
;;     '((javascript-mode . js-ts-mode)))

;; fonts
(set-face-attribute 'default nil :font "JetBrains Mono" :height 160)

;; line and column numbers
(column-number-mode)                 ; enable column numbers
(global-display-line-numbers-mode t) ; enable line numbers globally
(dolist (mode '(term-mode-hook       ; disable line numbers on some modes
                shell-mode-hook
                eshell-mode-hook))
        (add-hook mode (lambda () (display-line-numbers-mode nil))))

;; packages
;; initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;; load use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package)) ; install it if it isn't already installed
(require 'use-package)
(setq use-package-always-ensure t)

;; icons
(use-package all-the-icons
  :if (display-graphic-p))

;; neotree
(use-package neotree)
(global-set-key (kbd "C-x f") 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'classic))

;; doom themes
(use-package doom-themes
  :ensure t
  :config
  ;; global settings (defaults)
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
    doom-themes-enable-italic t)  ; if nil, italics is universally disabled
  (load-theme 'doom-moonlight t)

  ;; enable custom neotree theme
  (doom-themes-neotree-config)
  ;; corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode t)
  :custom ((doom-modeline-height 15)))

;; match parenthesis
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
  ; prog-mode is the base mode for any programming language mode

;; counsel
(use-package counsel
  :bind (("M-x" . counsel-M-x)
    ("C-x b" . counsel-ibuffer)
    ("C-x C-f" . counsel-find-file)
    :map minibuffer-local-map
    ("C-r" . 'counsel-minibuffer-history))
  :config (setq ivy-initial-inputs-alist nil)) ; don't start searches with ^

;; ivy
(use-package swiper :ensure t) ; swiper sometimes doesn't come included in ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
    :map ivy-minibuffer-map
    ("TAB" . ivy-alt-done)
    ("C-l" . ivy-alt-done)
    ("C-j" . ivy-next-line)
    ("C-k" . ivy-previous-line)
    :map ivy-switch-buffer-map
    ("C-k" . ivy-previous-line)
    ("C-l" . ivy-done)
    ("C-d" . ivy-switch-buffer-kill)
    :map ivy-reverse-i-search-map
    ("C-k" . ivy-previous-line)
    ("C-d" . ivy-reverse-i-search-kill))
  :config (ivy-mode t))      ; config happens after the mode is loaded

;; adds extra info when listing commands with ivy
(use-package ivy-rich
    :init (ivy-rich-mode t))

;; pops up a panel when using a bind, explaining what you can do with it
(use-package which-key
  :init (which-key-mode) ; init happens before the package is loaded
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5))

;; better help functions
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . counsel-describe-variable))

;; define keybinds in a more concise way
(use-package general
  :config
  (general-evil-setup t)

  ;; making C-SPC the new leader key
  (general-create-definer pluto/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (pluto/leader-keys
    "t" '(:ignore t :which-key "toggles")))

;; better undo and redo
(use-package undo-fu)

;; vim keybindings
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode t)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;use C-g instead of ESC to go back to normal mode

  ;; when a line wraps
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; evil keybindings for different emacs modes
(use-package evil-collection
  :after evil ;; load this package after evil is loaded
  :config
  (evil-collection-init))

;; temporary bindings for repetitive actions
(use-package hydra)

;; managing projects
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/dev"))
    (setq projectile-project-search-path '("~/dev")))

;; improve projectile and ivy integration
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;;git integration
(use-package magit
  :custom
  (magit-display-buffer-function
  #'magit-display-buffer-same-window-except-diff-v1))

;; github integration (open prs, see issues, etc.)
(use-package forge)

;; key bindings
(defhydra hydra-text-scale nil
    "zoom"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("f" nil "finished" :exit t))

(pluto/leader-keys "tz" '(hydra-text-scale/body :which-key "zoom"))

(evil-define-key '(normal visual) 'global
  "L" 'evil-end-of-line
  "H" 'evil-beginning-of-line)

(evil-define-key 'normal eglot-mode-map
  (kbd "C-.") 'xref-find-definitions
  (kbd "C-,") 'xref-go-back)

;; hooks
(add-hook 'neotree-mode-hook 'pluto/neotree-hook)
(add-hook 'prog-mode-hook 'pluto/whitespaces-hook)
