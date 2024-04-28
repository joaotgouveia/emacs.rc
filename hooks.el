;; disable evil keybindings in these modes, C-z to activate
(defun jg/evil-hook ()
    (dolist (mode '(custom-mode
                   eshell-mode
                   git-rebase-mode
                   erc-mode
                   circe-server-mode
                   circe-chat-mode
                   circe-query-mode
                   sauron-mode
                   term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; making neotree work with evil mode
(defun jg/neotree-hook ()
  (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
  (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
  (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
  (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
  (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
  (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
  (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
  (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)
  (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle))

;; whitespace handling (strongly inspired by tsoding's config)
(defun jg/whitespaces-hook ()
  (whitespace-mode t)
  (setq whitespace-style '(face
    trailing         ; trailing whitespaces
   ;lines            ; long lines (> 80)
    empty            ; empty lines at the beginning or end
    space-before-tab
    space-after-tab
    indentation
    missing-newline-at-eof))
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

;; whitespace handling for org files
(defun jg/org-whitespaces-hook ()
  (whitespace-mode t)
  (setq whitespace-style '(face
    trailing         ; trailing whitespaces
    empty            ; empty lines at the beginning or end
    space-before-tab
    space-after-tab
    indentation
    missing-newline-at-eof))
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

;; org mode settings (strongly inspired by SystemCrafter's config)
(defun jg/org-hook ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

;; auto tangle config file on save (strongly inspired by SystemCrafter's config)
(defun jg/org-tangle-hook ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/emacs.rc/config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(defun jg/org-visual-fill-hook ()
  (setq visual-fill-column-width 125
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
