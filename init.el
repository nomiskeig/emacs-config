;; -- lexial-binding t
					; Set up package.el to work with MELPA
(require 'package)
(require 'dired)
(eval-when-compile
  (require 'org)
  (require 'org-agenda)
  (require 'org-capture)
					;(require 'pdf-view)
  (require 'flymake)
					;(require 'magit)
  )
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/")
	     t)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
;(package-refresh-contents)
(require 'use-package)
(setq use-package-always-ensure t)
;; (add-to-list 'load-path "~/.emacs.d/themes/")
;;(load-theme 'timu-spacegrey t)
;; see https://www.emacswiki.org/emacs/DisplayTime
(display-time-mode 1)
(setq display-time-24hr-format t)
(setq display-time-default-load-average nil)
(defface egoge-display-time
  '((((type x w32 mac))
     ;; #060525 is the background colour of my default face.
     (:foreground "#060525" :inherit bold))
    (((type tty))
     (:foreground "blue")))
  "Face used to display the time in the mode line.")
;; This causes the current time in the mode line to be displayed in
;; `egoge-display-time-face' to make it stand out visually.
(setq display-time-string-forms
      '((propertize (concat " " 24-hours ":" minutes " ")
 		    'face 'egoge-display-time)))


(setq lazy-highlight-cleanup nil)
(setq lazy-highlight-max-at-a-time nil)
(use-package nix-mode
  :mode
  ("\\.nix\\'" "\\.nix.in\\'"))

(use-package envrc
  :hook (after-init . envrc-global-mode))
(use-package rust-mode
  :ensure t)
(require 'rust-mode)
(use-package corfu
  :ensure
  t
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  :bind
  (:map corfu-map
	("C-j" . corfu-next)
	("C-k" . corfu-previous)
	("RET" . nil)
	))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  )
(use-package cmake-mode
  :ensure
  t)

(use-package org-download
  :ensure t
  :after org
  :hook 
  (org-mode . org-download-enable)
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "images")
  (org-download-heading-lvl nil)
 (org-download-timestamp "%Y%m%d-%H%M%S_"))
	    
(use-package org-noter
  :ensure t)
(require 'cmake-mode)
(use-package posframe
  :ensure
  t
  )
(use-package magit
  :ensure
  t)
(use-package evil
  :ensure
  t
  :functions (evil-mode evil-set-leader evil-define-key)
  :defines (evil-want-keybinding)
  :init
  (setq evil-want-keybinding nil)
  :config

  (evil-mode 1))
(use-package evil-collection
  :ensure
  t)

(use-package timu-spacegrey-theme
  :ensure
  t
  :config
  (load-theme 'timu-spacegrey t))
(use-package vertico
  :bind
  (:map vertico-map
	("C-k" . vertico-previous)
	("C-j" . vertico-next))
  :init
  (vertico-mode))
(setq evil-want-keybinding nil)
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil)
  (completion-pcm-leading-wildcard t))
(use-package consult
  :ensure
  t)
(use-package marginalia
  :bind
  (:map minibuffer-local-map
	("M-A" . marginalia-cycle))
  :init (marginalia-mode))
(require 'evil)
(when (require 'evil-collection nil t)
  (evil-collection-init '(calendar magit)))
(use-package eldoc-box
  :ensure
  t)
					;:hook (eglot-managed-mode . eldoc-box-hover-mode))
					; :hook (emacs-lisp-mode . eldoc-box-hover-mode))
					; :bind (:map emacs-lisp-mode-map
					;  ("K" . eldoc-box-help-at-point)))
(setq help-window-select t)

(use-package evil-escape
  :ensure
  t
  :after evil
  :defines (evil-escape-key-sequence evil-escape-delay)
  :functions (evil-escape-mode)
  :init (setq evil-escape-key-sequence "jk"
	      evil-escape-delay 0.1)
  :config (evil-escape-mode 1))

(use-package pdf-tools
  :ensure
  t
  :config (pdf-tools-install))
(setq pdf-links-enable-links t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(citar-file-open-functions
   '(("pdf" . citar-file-open-external)
     ("html" . citar-file-open-external) (t . find-file)))
 '(citar-open-entry-function 'citar-open-entry-in-zotero)
 '(format-all-show-errors 'never)
 '(inhibit-startup-screen t)
 '(lsp-idle-delay 0.5)
 '(org-agenda-files '("/home/simon/org/inbox.org"))
 '(package-selected-packages
   '(cape citar cmake-mode consult corfu dired-subtree direnv eldoc-box
	  envrc evil-collection evil-escape format-all lsp-ui magit
	  marginalia nerd-icons-dired nix-mode orderless org-download
	  org-noter org-ref pdf-tools posframe rust-mode
	  timu-spacegrey-theme vertico zotxt)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(defun my/display-line-numbers ()
  (display-line-numbers-mode 1)
  )
(add-hook 'prog-mode-hook 'my/display-line-numbers)
(setq scroll-conservatively 10
      scroll-margin 15)
(set-frame-font "FiraCode Nerd Font Mono-12" nil t)
(evil-set-leader 'normal (kbd "SPC"))
(setq magit-display-buffer-function
      #'magit-display-buffer-fullframe-status-v1)

					;(setq help-window-select t)
(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "C-k") nil))
(evil-define-key 'normal 'global
  (kbd "<leader>gc")
  (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(evil-define-key 'normal 'global
  (kbd "<leader>gn")
  (lambda () (interactive) (dired "~/org/notes")))

(evil-define-key 'normal 'global (kbd "<leader>w") #'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>gg") #'magit-status)
(evil-define-key 'normal 'global
  (kbd "<leader>et") #'org-set-tags-command)
(evil-define-key 'normal 'global (kbd "C-j") #'windmove-down)
(evil-define-key 'normal 'global (kbd "C-k") #'windmove-up)
(evil-define-key 'normal 'global (kbd "C-h") #'windmove-left)
(evil-define-key 'normal 'global (kbd "C-l") #'windmove-right)
(evil-define-key 'normal 'global
  (kbd "<leader>hf") #'describe-function)
(evil-define-key 'normal 'global
  (kbd "<leader>hv") #'describe-variable)
(evil-define-key 'normal 'global (kbd "<leader>hk") #'describe-key)
(evil-define-key 'normal 'global
  (kbd "<leader>la") #'eglot-code-action-quickfix)
					;(evil-define-key 'normal 'global (kbd "K") #'eldoc-box-help-at-point)
					;(setq eldoc-box-only-multi-line t)

					;(setq eldoc-box-hover-display-frame-above-point t)
					;(setq eldoc-documentation-strategy #'eldoc-documentation-compose)


(with-eval-after-load 'help
  (evil-define-key 'normal help-mode-map (kbd "q") #'quit-window))


(defun my/safe-apply-function (fn &rest args)
  (if (and (symbolp fn) (not (fboundp fn)))
      (format "%S returnined above" fn)
					; return an empty string if fn is void
    (condition-case err
	(apply fn args)
      (void-function (format "%S is a void function" fn))
      (error ""))))

(defun my/wrap-eldoc-provider (fn)
  (lambda (&rest args)
    (let ((result (my/safe-apply-function fn args)))
      (format "Provider %S returned: %S" fn result))))


(evil-define-key 'normal 'global
  (kbd "<leader>d")
  (lambda () (interactive)
    ;(dired (file-name-directory buffer-file-name))))
    (my/dired-explorer)))
(evil-define-key 'normal 'global
  (kbd "<leader>D")
  (lambda () (interactive)
    (dired-other-window (file-name-directory buffer-file-name))))
(defun my/compile-in-project-root ()
  "Run compile from project root."
  (interactive)
  (let
      ((default-directory
	(or  (project-root (project-current t)) default-directory)))
    (call-interactively 'compile)))
					;(setq eldoc-documentation-functions
					;      (mapcar #'my/wrap-eldoc-provider eldoc-documentation-functions))
(setq consult-fd-args "fd --hidden --full-path")
(evil-define-key 'normal 'global
  (kbd "<leader>F") (lambda () (interactive) (project-find-file)))
(evil-define-key 'normal 'global
  (kbd "<leader>G")
  (lambda () (interactive) (consult-fd "/home/simon")))
(evil-define-key 'normal 'global
  (kbd "<leader>f") (lambda () (interactive) (consult-ripgrep)))
(evil-define-key 'normal 'global
  (kbd "<leader>jc") #'my/compile)
(evil-define-key 'normal 'global
  (kbd "<leader>jt") #'eshell)
(evil-define-key 'normal 'global (kbd "<leader>jr") #'recompile)
(evil-define-key 'normal 'gloabl
  (kbd "<leader>jk") #'kill-compilation)
(evil-define-key 'normal 'global (kbd "C-.") #'completion-at-point)
(defun my/compile-minibuffer-keys ()
  "Use C-j/C-k to scroll through the compile history"
  (when (eq this-command 'compile)
					;(local-set-key (kbd "C-k") #'previous-history-element)
					;(local-set-key (kbd "C-j") #'next-history-element)))
    ))

(add-hook 'minibuffer-setup-hook #'my/compile-minibuffer-keys)

(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map
    (kbd "o") #'dired-find-file)
  (evil-define-key 'normal dired-mode-map
    (kbd "v") #'dired-find-file-other-window)
  (evil-define-key 'normal dired-mode-map
    (kbd "d") #'dired-flag-file-deletion)
  (evil-define-key 'normal dired-mode-map
    (kbd "x") #'dired-do-flagged-delete)
  (evil-define-key 'normal dired-mode-map
    (kbd "u") #'dired-unmark
    (evil-define-key 'normal dired-mode-map
      (kbd "g") #'revert-buffer)
    (evil-define-key 'normal dired-mode-map
      (kbd "r") #'dired-do-rename)
    (evil-define-key 'normal dired-mode-map
      (kbd "a") #'find-file)))
					;  (kbd "o") (message "pressed o")))

(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.3)
               (inhbit-same-window . nil)
               ))
(add-to-list 'display-buffer-alist
             '("\\*xref\\*"
               (display-buffer-reuse-window display-buffer-at-bottom)
               (window-height . 0.3)
               (inhbit-same-window . nil)
               ))


(add-hook 'compilation-start-hook
          (lambda (buf) (switch-to-buffer-other-window buf)))
(add-hook 'compilation-mode-hook
          (lambda ()
            (setq-local shell-file-name (executable-find "zsh"))
            (setq-local shell-command-switch "-ic")))

(with-eval-after-load 'evil
  (evil-set-initial-state 'help-mode 'normal))

(defun reload-config ()
  "Reloads the config."
  (interactive)
  (load-file (expand-file-name "~/.emacs.d/init.el")))

(evil-define-key 'normal 'global (kbd "<leader>r") #'reload-config)
(defun my/kill-function-popup (name)
  "Kill the function popup"
  (interactive)
  (message "called the kill function")
  (Remove-hook 'pre-command-hook 'my/kill-function-popup t)
  (posframe-delete name)
  )

(defun my/describe-function-popup ()
  "Show function help in popup"
  (interactive)
  (let (
	(name "*Symbol info*")
	(sym (symbol-at-point))
	)
    (posframe-show
     name
     :string (with-current-buffer eldoc--doc-buffer (buffer-string))
     :position (point)

     :width 80
     :height 20

     )
    (message name)
					;(add-hook 'pre-command-hook (lambda () (my/kill-function-popup name)))
    (set-tran () sient-map
	      (let ((map (make-sparse-keymap)))
		(define-key map (kbd "j")
			    (lambda () (interactive)
			      (posframe-delete name)
			      (call-interactively #'evil-next-line))
			    (define-key map (kbd "k")
					(lambda () (interactive) (posframe-delete name)))
			    (define-key map (kbd "h")
					(lambda () (interactive) (posframe-delete name)))
			    (define-key map (kbd "l")
					(lambda () (interactive) (posframe-delete name)))
			    map))

	      )))

					;(evil-define-key 'normal 'global (kbd "K") #'my/describe-function-popup)




(defun my/flymake-show-line-diagnostics-posframe ()
  "Show popup containing the flymake diagnostics at that point."
  (interactive)
  (let* ((beg (line-beginning-position))
         (end (line-end-position))
         (diags (flymake-diagnostics beg end)))
    (if (null diags)
        (message "No diagnostics on this line.")
      (let ((msg (mapconcat
                  (lambda (d)
		    (let* ((type (flymake-diagnostic-type d))
			   (text (flymake-diagnostic-text d))
			   (face (cond
				  ((eq type :error) 'error)
				  ((eq type :warning) 'warning)
				  ((eq type :note) 'warning)))
			   (type-str (propertize (symbol-name type)
				   'face face))
			   (text-str (propertize text
				   'face face)))
                    (format "%s: %s"
                            type-str
                            text-str)))
                  diags
                  "\n"))
	    cleanup-fn)
        (posframe-show " *flymake*"
                       :string msg
                       :position (point)
		       :background-color "#1f2329"
		       ;:foreground-color "#ff00ff"
		       :border-color "#ffffff"
		       :border-width 1
                       )

	(setq cleanup-fn
	      (lambda ()
		(posframe-hide " *flymake*")
		(remove-hook 'pre-command-hook cleanup-fn t)))
	(add-hook 'pre-command-hook cleanup-fn t)
	))))

;;(add-hook 'post-command-hook
;;         (lambda ()
					; (posframe-hide " *flymake*")
;;          (remove-hook 'post-command-hook #'posframe-hide))
;;       nil t)))))

;; If you use Evil, a nice normal-mode binding for programming buffers:
(with-eval-after-load 'evil
  (evil-define-key 'normal prog-mode-map
    (kbd "gl") #'my/flymake-show-line-diagnostics-posframe))
(global-hl-line-mode 1)
(set-face-background 'hl-line "#293440")

(defun load-directory (directory)
  "Load recursively all `.el' files in DIRECTORY."
  (dolist
      (element (directory-files-and-attributes directory nil nil nil))
    (let* ((path (car element))
           (fullpath (concat directory "/" path))
           (isdir (car (cdr element)))
           (ignore-dir (or (string= path ".") (string= path ".."))))
      (cond
       ((and (eq isdir t) (not ignore-dir))
        (load-directory fullpath))
       ((and (eq isdir nil) (string= (substring path -3) ".el"))
        (load (file-name-sans-extension fullpath)))))))
(evil-set-initial-state 'pdf-view-mode 'normal)
(evil-set-initial-state 'compilation-mode 'normal)
					;(evil-set-initial-state 'pdf-view-mode 'emacs)
(with-eval-after-load 'pdf-view
  (evil-define-key 'normal pdf-view-mode-map
    (kbd "j") 'pdf-view-next-line-or-next-page
    (kbd "k") 'pdf-view-previous-line-or-previous-page
    (kbd "h") 'image-backward-hscroll
    (kbd "l") 'image-forward-hscroll
    (kbd "+") (lambda () (interactive) (pdf-view-enlarge 1.25))
    (kbd "-") (lambda () (interactive) (pdf-view-shrink 1.25))
    (kbd "H") 'pdf-view-fit-height-to-window
    (kbd "W") 'pdf-view-fit-width-to-window
    (kbd "P") 'pdf-view-fit-page-to-window
    (kbd "gg") 'pdf-view-first-page
    (kbd "G")  'pdf-view-last-page))

(add-hook 'pdf-view-mode 'pdf-view-fit-page-to-window)
(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map
    (kbd "<tab>") 'org-cycle
    ))

(evil-define-key 'normal compilation-mode-map
  (kbd "q") 'evil-window-delete)

(evil-define-key 'normal compilation-mode-map
  (kbd "n") 'next-error)
(evil-define-key 'normal compilation-mode-map
  (kbd "p") 'previous-error)
(load-directory "~/.emacs.d/config")
(setopt treesit-font-lock-level 5)
					;(setq compilation-auto-jump-to-first-error t)
(with-eval-after-load 'compile
  (setq compilation-scroll-output 'first-error)
  (setq compilation-skip-threshold 2)
  )


					;(global-treesit-mode)
					;(add-hook 'treesit-after-on-hook #'treesit-hl-mode)
(add-to-list  'custom-theme-load-path "~/.emacs.d/config/")
(load-theme 'onedark-darker t)

(use-package format-all
  :commands format-all-mode
  :config
  (setq-default format-all-formatters
		'(("Rust"  (rustfmt)))))

(defun my/compile ()
  (interactive)
  (let* ((proj (project-current))
	 (default-directory (project-root proj)))
    (call-interactively 'compile)))
(evil-define-key 'normal 'global (kbd "<leader>lf") 'format-all-region-or-buffer)

(defun my/split-window-right ()
  (interactive)
  (split-window-right)
  (other-window 1))


(evil-define-key 'normal 'global (kbd "<leader>v") #'my/split-window-right)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(with-eval-after-load 'ediff
  (add-hook 'ediff-startup-hook
	    (lambda ()
	      (evil-emacs-state ediff-control-buffer))))

(setq-default display-line-numbers-width 4)

(electric-pair-mode 1)
(use-package dired-subtree
  :after dired)
(with-eval-after-load 'dired
  (evil-define-key 'normal dired-mode-map (kbd "TAB") 'dired-subtree-toggle))
(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(setq dired-kill-when-opening-new-dired-buffer nil)
(defun my/dired-explorer ()
  "Open or reuse a Dired buffer for the project root."
  (interactive)
  (let* ((root (or (when-let ((pr (project-current))) (project-root pr))
                   default-directory))
         (buf  (dired-noselect root)))
    (switch-to-buffer buf)))

(setq dired-subtree-line-prefix "    ")
(setq dired-omit-files
      "*\\~"
      )
(add-hook 'dired-mode-hook #'dired-omit-mode)
(with-eval-after-load 'dired-subtree
  (defun my/dired-subtree-apply-omit (&rest _)
    (when (bound-and-true-p dired-omit-mode)
      (let ((inhibit-read-only t))
        (save-excursion
          (dired-omit-expunge)))))
  (advice-add 'dired-subtree-insert :after #'my/dired-subtree-apply-omit))

(evil-define-key 'normal xref--button-map (kbd "q") 'quit-window)
(evil-define-key 'normal xref--button-map (kbd "o") 'xref-quit-and-goto-xref)

(evil-define-key 'normal 'global (kbd "<leader>b") 'consult-buffer)
(evil-define-key 'normal 'global (kbd "<leader>k") 'consult-recent-file)

(setq compilation-ask-about-save nil)


(require 'recentf)
(recentf-mode 1)
(add-hook 'buffer-list-update-hook #'recentf-track-opened-file)



(use-package citar
  :hook
  (org-mote . citar-capf-setup)
  :custom
  (citar-bibliography '("~/org/notes/refs.bib")))

(setq citar-notes-paths '("~/org/notes/literature/"))






(evil-define-key 'normal 'global (kbd "<leader>ac") 'citar-insert-citation)

(evil-define-key 'normal 'global (kbd "<leader>on") 'citar-open-note)
(evil-define-key 'normal 'global (kbd "<leader>of") 'citar-open-files)
(evil-define-key 'normal 'global (kbd "<leader>oe") 'citar-open-entry)
