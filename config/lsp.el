
(add-to-list 'major-mode-remap-alist '(cmake-mode . cmake-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
(add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))

(require 'treesit)
(setq treesit-language-source-alist
      '((cmake . ("https://github.com/uyha/tree-sitter-cmake"))
	(c . ("https://github.com/tree-sitter/tree-sitter-c"))
	(cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))


	)
      )
(use-package lsp-mode
  :hook
  (c-ts-mode . lsp)
  (cmake-ts-mode . lsp)
  (rust-mode . lsp)
  (c++-ts-mode . lsp)
  :commands lsp
  :custom
  (lsp-enable-symbol-highlighting nil)
  (lsp-idle-dely 0.2)
  )
; This does not seem to work
;(evil-define-minor-mode-key 'normal lsp-mode (kbd "<leader>l") lsp-command-map)

(with-eval-after-load 'lsp-mode
;  (lsp-unregister-client 'cmake-language-server)
  (setq lsp-semantic-token-modifier-faces '(()))
  (setq lsp-semantic-token-faces '(("comment" . lsp-face-semhl-comment)))
  (add-to-list 'lsp-language-id-configuration '(cmake-ts-mode . "cmake-new"))
  (lsp-register-client (make-lsp-client
                      :new-connection (lsp-stdio-connection '("neocmakelsp" "--stdio"))
                      :activation-fn (lsp-activate-on "cmake-new")
                      :server-id 'neocmakels)))
(use-package lsp-ui
  :ensure t)
(setq lsp-ui-doc-delay 0)
(setq lsp-ui-doc-position 'at-point)
(setq lsp-ui-doc-show-with-mouse nil)
;(setq lsp-ui-doc-max-height 50)


(defun my/show-lsp-ui ()
  (interactive)
  (let ((lsp-ui-cleanup-fn t))
   (lsp-ui-doc-show)
   (setq lsp-ui-cleanup-fn
	 (lambda ()
	   (lsp-ui-doc-hide)
	   (remove-hook 'pre-command-hook lsp-ui-cleanup-fn t)))
   (add-hook 'pre-command-hook lsp-ui-cleanup-fn t))
   )
(evil-define-key 'normal 'global (kbd "K") #'my/show-lsp-ui)
(evil-define-key 'normal 'global (kbd "gi") #'lsp-find-implementation)
(with-eval-after-load 'lsp-mode
  (evil-define-key 'normal 'global (kbd "K") #'my/show-lsp-ui)
  (evil-define-key 'normal 'global (kbd "<leader>la") #'lsp-execute-code-action))
;; performance things
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq lsp-inlay-hint-enable t)
(add-hook 'lsp-mode-hook #'lsp-inlay-hints-mode)

(setq lsp-rust-analyzer-display-parameter-hints t)
