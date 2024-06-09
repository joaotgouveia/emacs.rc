(defun jg/evil-hook ()
  "Disable evil keybindings in these modes."
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

(defun jg/whitespaces-hook ()
  "Whitespace handling (strongly inspired by tsoding's config)."
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

(defun jg/org-whitespaces-hook ()
  "Whitespace handling for org files."
  (whitespace-mode t)
  (setq whitespace-style '(face
    trailing         ; trailing whitespaces
    empty            ; empty lines at the beginning or end
    space-before-tab
    space-after-tab
    indentation
    missing-newline-at-eof))
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

(defun jg/org-hook ()
  "Org mode settings (strongly inspired by SystemCrafter's config)."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(defun jg/org-tangle-hook ()
  "Auto tangle config file on save (strongly inspired by SystemCrafter's config)."
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/emacs.rc/config.org"))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(defun jg/org-visual-fill-hook ()
  "Center text on screen."
  (setq visual-fill-column-width 75
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(defun jg/org-present-hook ()
  "Customize org appearence when displaying slides."
  (setq-local face-remapping-alist '(
              (default (:height 1.5) variable-pitch)
              (header-line (:height 4.0) variable-pitch)
              (org-document-title (:height 1.75) org-document-title)
              (org-code (:height 1.55) org-code)
              (org-verbatim (:height 1.55) org-verbatim)
              (org-block (:height 1.25) org-block)
              (org-block-begin-line (:height 0.7) org-block)))
  (visual-line-mode 1))

(provide 'hooks)
