;; see https://www.gnu.org/software/emacs/manual/html_mono/eglot.html#Setting-Up-LSP-Servers
;(add-hook 'emacs-lisp-mode-hook #'flymake-mode)
;(require 'treesit)
;(setq treesit-language-source-alist
;      '((cmake . ("https://github.com/uyha/tree-sitter-cmake"))
;	(c . ("https://github.com/tree-sitter/tree-sitter-c"))
;	(cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))


;	)
;      )

;;(mapc #'treesit-install-language-grammar
 ;     (mapcar #'car treesit-language-source-alist))
;(add-to-list 'major-mode-remap-alist '(cmake-mode . cmake-ts-mode))
;(add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
;(use-package cmake-ts-mode
;  :config
;  (add-hook 'cmake-ts-mode-hook
;    (defun setup-neocmakelsp ()

;      (require 'eglot)
;      (add-to-list 'eglot-server-programs `((cmake-ts-mode) . ("neocmakelsp" "--stdio")))
;      (eglot-ensure))))


;(require 'eglot)
;(add-to-list 'eglot-server-programs `((c-ts-mode) . ("clangd")))
;(add-to-list 'eglot-server-programs `((nix-mode) . ("nil")))
;(add-hook 'c-ts-mode-hook 'eglot-ensure)
;(add-hook 'nix-mode-hook 'eglot-ensure)
