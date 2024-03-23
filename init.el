;; Minimalistic UI
(setq-default inhibit-startup-message t            ; Disable startup screen
              ring-bell-function 'ignore           ; Disable sounds
              make-backup-files nil                ; Disable backup files
              tab-width 4
              indent-tabs-mode nil                 ; Use spaces instead of tabs
              display-line-numbers-type 'relative) ; Use relative line numbers

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(menu-bar-mode -1)   ; Disable the menu bar
(set-fringe-mode 15) ; Add some horizontal padding

;; Fonts
(set-face-attribute 'default nil :font "JetBrains Mono" :height 160)

;; Line and column numbers
(column-number-mode)                 ; Enable column numbers, show up on modeline
(global-display-line-numbers-mode t) ; Enable line numbers globally
(dolist (mode '(term-mode-hook       ; Disable line numbers on some modes
                shell-mode-hook
                eshell-mode-hook))
        (add-hook mode (lambda () (display-line-numbers-mode nil))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Packages
;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
    (package-refresh-contents))
;; Load use-package
(unless (package-installed-p 'use-package)
    (package-install 'use-package)) ; Install it if it isn't already installed
(require 'use-package)
(setq use-package-always-ensure t)

;; Icons
(use-package all-the-icons
  :if (display-graphic-p))

;; Neotree
(use-package neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'classic))

;; DOOM themes
(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-moonlight t)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; DOOM modeline
(use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode t)
    :custom ((doom-modeline-height 15)))

;; Rainbow delimiters (match parenthesis)
(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode)) ; prog-mode is the base mode for any programming language mode

;; Counsel
(use-package counsel
    :bind (("M-x" . counsel-M-x)
           ("C-x b" . counsel-ibuffer)
           ("C-x C-f" . counsel-find-file)
           :map minibuffer-local-map
           ("C-r" . 'counsel-minibuffer-history))
    :config (setq ivy-initial-inputs-alist nil)) ; Don't start searches with ^

;; Ivy
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
(use-package swiper :ensure t) ; Swiper sometimes doesn't come included in Ivy

;; Ivy rich, adds extra info when listing commands with Ivy
(use-package ivy-rich
    :init (ivy-rich-mode t))

;; Which key (pops up a panel when using a bind, explaining what you can do with it)
(use-package which-key
    :init (which-key-mode) ; init happens before the package is loaded
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 0.5))

;; Helpful, better help functions
(use-package helpful
    :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
    :bind
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-function] . counsel-describe-variable))
