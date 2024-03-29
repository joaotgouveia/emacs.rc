;; minimalistic ui
(setq-default inhibit-startup-message t            ; disable startup screen
              ring-bell-function 'ignore           ; disable sounds
              make-backup-files nil                ; disable backup files
              tab-width 4
              indent-tabs-mode nil                 ; use spaces instead of tabs
              display-line-numbers-type 'relative) ; use relative line numbers

(scroll-bar-mode -1) ; disable visible scrollbar
(tool-bar-mode -1)   ; disable the toolbar
(tooltip-mode -1)    ; disable tooltips
(menu-bar-mode -1)   ; disable the menu bar
(set-fringe-mode 15) ; add some horizontal padding

;; fonts
(set-face-attribute 'default nil :font "JetBrains Mono" :height 160)

;; line and column numbers
(column-number-mode)                 ; enable column numbers, show up on modeline
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
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'classic))

;; doom themes
(use-package doom-themes
  :ensure t
  :config
  ;; global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
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

;; rainbow delimiters, match parenthesis
(use-package rainbow-delimiters
    :hook (prog-mode . rainbow-delimiters-mode)) ; prog-mode is the base mode for any programming language mode

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

;; ivy rich, adds extra info when listing commands with ivy
(use-package ivy-rich
    :init (ivy-rich-mode t))

;; which key, pops up a panel when using a bind, explaining what you can do with it
(use-package which-key
    :init (which-key-mode) ; init happens before the package is loaded
    :diminish which-key-mode
    :config
    (setq which-key-idle-delay 0.5))

;; helpful, better help functions
(use-package helpful
    :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
    :bind
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-function] . counsel-describe-variable))

;; general, define keybinds in a more concise way
(use-package general
    :config
    (general-evil-setup t)

    ;; Making C-SPC the new leader key
    (general-create-definer pluto/leader-keys
        :keymaps '(normal insert visual emacs)
        :prefix "SPC"
        :global-prefix "C-SPC"))

;; disable evil keybindings in these modes, C-z to activate
(defun pluto/evil-hook ()
    dolist (mode '(custom-mode
                   eshell-mode
                   git-rebase-mode
                   erc-mode
                   circe-server-mode
                   circe-chat-mode
                   circe-query-mode
                   sauron-mode
                   term-mode))
    add-to-list 'evil-emacs-state-modes mode)

;; evil mode, vim keybinds
(use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-i-jump nil)
    :hook (evil-mode . pluto/evil-hook)
    :config
    (evil-mode t)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) ;; use "C-g" instead of ESC to go back to normal mode

    ;; when a line wraps
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal))

;; evil collection, evil keybindings for different emacs modes
(use-package evil-collection
    :after evil ;; load this package after evil is loaded, since this depends on it
    :config
    (evil-collection-init))

;; key bindings

(general-define-key
    "<escape>" 'keyboard-escape-quit ;; make esc quit prompts
    "C-M-j" 'counsel-switch-buffer)


;; making neotree work with evil mode
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
