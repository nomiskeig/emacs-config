
(add-to-list 'major-mode-remap-alist '(cmake-mode . cmake-ts-mode))
(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))

(use-package lsp-mode
  :hook
  (c-ts-mode . lsp)
  (cmake-ts-mode . lsp)
  :commands lsp
  :custom
  (lsp-enable-symbol-highlighting nil)
  (lsp-semantic-tokens-enable nil)
  )
; This does not seem to work
;(evil-define-minor-mode-key 'normal lsp-mode (kbd "<leader>l") lsp-command-map)

(with-eval-after-load 'lsp-mode
;  (lsp-unregister-client 'cmake-language-server)
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
(evil-define-key 'normal 'global (kbd "K") #'lsp-ui-doc-show)
