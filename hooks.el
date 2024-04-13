;; disable evil keybindings in these modes, C-z to activate
(defun pluto/evil-hook ()
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
(defun pluto/neotree-hook ()
        (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
        (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
        (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
        (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
        (define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
        (define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
        (define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
        (define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
        (define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle))

