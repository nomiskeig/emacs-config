;; config/org.el --- -*- lexical-binding: t; -*-
;;; Commentary

					; Contains code and ideas from https://github.com/jethrokuan/dots/blob/master/.doom.d/config.el
;;; Ensure that the window layout persists when the agenda is opened
(setq org-agenda-restore-windows-after-quit t)
					;(defvar my/agenda-window-config nil
					; "The window configuration when opening the agenda view")
					;(advice-add 'org-agenda :before (lambda (&rest _)  (setq my/agenda-window-config (current-window-configuration))) )
					;(add-hook 'org-agenda-quit-hook (lambda () (when (window-configuration-p my/agenda-window-config) (set-window-configuration my/agenda-window-config))))
;;; Keymaps
(evil-define-key 'normal 'global
  (kbd "<leader>gi")
  (lambda () (interactive) (find-file "~/org/inbox.org")))
(evil-define-key 'normal 'global
  (kbd "<leader>gp")
  (lambda () (interactive)
    (find-file "~/org/projects.org")))
(evil-define-key 'normal 'global
  (kbd "<leader>ge")
  (lambda () (interactive) (find-file "~/org/errands.org")))
(evil-define-key 'normal 'global
  (kbd "<leader>oa") (lambda () (interactive) (org-agenda nil " ")))
					;(evil-define-key 'normal 'global (kbd "<leader>c") #'org-capture)
(with-eval-after-load 'org-agenda
  (evil-define-key 'emacs org-agenda-mode-map
    (kbd "j") #'org-agenda-next-line
    (kbd "k") #'org-agenda-previous-line
    (kbd "r") #'jethro/org-process-inbox
    ;(kbd "d") #'org-agenda-kill
    (kbd "u") #'org-agenda-undo
    (kbd "gd") #'org-agenda-switch-to
    (kbd "gg") #'org-agenda-redo-all
    ))
					;(kbd "gd") #'org-agenda-switch-to  ))

(with-eval-after-load 'org-agenda
  (evil-define-key 'normal org-agenda-mode-map
    (kbd "q") #'org-agenda-quit))

;;; Config
;;(setq org-capture-templates
					;     '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
					; "** TODO %? %i \n Timestamp: %T, Source: %a")
					;("s" "Subtask"  entry (function (lambda () (point)))
					; "*** TODO %? %i \n Timestamp: %T, Source: %a")
					;("E" "Errrand" entry (file+headline "~/org/errands.org" "Errands")
					; "** TODO %? %i \n Timestamp: %T")
					;("i" "Inbox" entry (file "~/org/inbox.org")
					; "* %i")
					;'))
(setq jethro/org-agenda-directory "~/org/todo/")
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)" "|"
		  "Cancelled(c)")))
(defun my/capture-end-function ()
  (when (and (not org-note-abort)
             (string= (org-capture-get :key) "t") ; change "i" to your template key
             (marker-buffer org-capture-last-stored-marker))
    (with-current-buffer (marker-buffer org-capture-last-stored-marker)
      (goto-char org-capture-last-stored-marker)
      (org-refile))))
(add-hook 'org-capture-after-finalize-hook #'my/capture-end-function)
(setq org-capture-templates
      `(("i" "Inbox" entry  (file "~/org/todo/inbox.org")
         ,(concat "* TODO %?\n"
                  "Entered on %U"))
        ("s" "Slipbox" entry  (file "~/org/notes/inbox.org")
         "* %?\n")
        ("t" "Task" entry  (file "~/org/todo/inbox.org")
         "* TODO %?\n
Entered on %U"  )
        ))

(setq org-tag-alist
      '((:startgroup . nil) ("@work" . ?w) ("@home" . ?h)
	("@uni" . ?u) ("@errand" . ?e) ("@read" . ?r ) (:endgroup . nil)))
(setq org-agenda-files
      '("~/org/todo/" "~/org/notes/"))
(setq org-refile-targets '(("next.org" :level . 0)
                           ("someday.org" :level . 0)
                           ("reading.org" :level . 1)
                           ("projects.org" :maxlevel . 1)))
(setq org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)
(setq org-refile-allow-creating-parent-nodes 'confirm)
(setq org-agenda-window-setup 'only-window)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)

(defvar jethro/org-agenda-bulk-process-key ?f
  "Default key for bulk processing inbox items.")

(defun jethro/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (my/org-agenda-bulk-mark-regexp-file "inbox\\.org")
					; (org-agenda-bulk-mark-regexp "~/org/todo/inbox.org")
  (jethro/bulk-process-entries))

(defvar jethro/org-current-effort "1:00"
  "Current effort for agenda items.")

(defun jethro/my-org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " jethro/org-current-effort) nil nil jethro/org-current-effort)))
  (setq jethro/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil jethro/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun jethro/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'jethro/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun jethro/bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'jethro/org-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

(defun jethro/org-inbox-capture ()
  (interactive)
  "Capture a task in agenda mode."
  (org-capture nil "i"))

(setq org-agenda-bulk-custom-functions `((,jethro/org-agenda-bulk-process-key jethro/org-agenda-process-inbox-item)))

(define-key org-agenda-mode-map "i" 'org-agenda-clock-in)
(define-key org-agenda-mode-map "r" 'jethro/org-process-inbox)
(define-key org-agenda-mode-map "R" 'org-agenda-refile)
(define-key org-agenda-mode-map "c" 'jethro/org-inbox-capture)
(setq jethro/org-agenda-todo-view
      `(" " "Agenda"
        ((agenda ""
                 ((org-agenda-span 'day)
                  (org-deadline-warning-days 365)))
         (todo "TODO"
               ((org-agenda-overriding-header "To Refile")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "inbox.org")))))
					;(todo "TODO"
					;      ((org-agenda-overriding-header "Emails")
					;       (org-agenda-files '(,(concat jethro/org-agenda-directory "emails.org")))))
         (todo "NEXT"
               ((org-agenda-overriding-header "In Progress")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "someday.org")
                                    ,(concat jethro/org-agenda-directory "projects.org")
                                    ,(concat jethro/org-agenda-directory "next.org")))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "projects.org")))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "One-off Tasks")
                (org-agenda-files '(,(concat jethro/org-agenda-directory "next.org")))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
         (todo "TODO"
         ((org-agenda-overriding-header "To read")
          (org-agenda-files '(,(concat jethro/org-agenda-directory "reading.org")))))

         nil)))
(add-to-list 'org-agenda-custom-commands `,jethro/org-agenda-todo-view)
(setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
(setq org-agenda-block-separator nil)
(setq org-agenda-start-with-log-mode t)
(defun jethro/style-org ()
  (setq line-spacing 0.2)
  (variable-pitch-mode +1)
  (mapc
   (lambda (face) ;; Other fonts with fixed-pitch.
     (set-face-attribute face nil :inherit 'fixed-pitch))
   (list 'org-code
         'org-block
         'org-table
         'org-verbatim
         'org-block-begin-line
         'org-block-end-line
         'org-meta-line
         'org-document-info-keyword)))

					;(add-hook 'org-mode-hook #'jethro/style-org)

(defun my/agenda-todo-prefix ()
  "Show the file name if there is no parent and breadcrumbs if there are parents"
  (let ((x (nth 0 (org-get-outline-path))))
    (if x
        (concat "[ " (org-format-outline-path (list x)) " ]")
					; the line below is basically %c, see https://emacs.stackexchange.com/questions/80509/org-agenda-prefix-format-change-file-name
      ;; (file-name-nondirectory (directory-file-name (file-name-directory (buffer-file-name)))))))
      (file-name-sans-extension (file-name-nondirectory (buffer-file-name))))))

(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s") (todo . " %i %-20(my/agenda-todo-prefix) %s")
				 (tags . " %i %-12:c") (search . " %i %-12:c")))
(setq org-log-done 'time
      org-log-into-drawer t
      org-log-state-notes-insert-after-drawers nil)
(add-hook 'org-agenda-after-action-hook #'org-save-all-org-buffers)
(add-hook 'org-after-refile-insert-hook #'org-save-all-org-buffers)

(defun my/org-agenda-bulk-mark-regexp-file (regexp &optional fullpath)
  "Mark agenda entries whose SOURCE FILE matches REGEXP.
With C-u, match the absolute path; otherwise match the basename."
  (interactive "sFile regexp (e.g. inbox\\.org): \nP")
  (let ((n 0))
    (save-excursion
      (goto-char (or (next-single-property-change (point-min) 'org-hd-marker)
                     (point-min)))
      (while (not (eobp))
        (unless (get-char-property (point) 'invisible)
          (let* ((m (or (org-get-at-bol 'org-hd-marker)
                        (org-get-at-bol 'org-marker)))
                 (f (and m (org-with-point-at m (buffer-file-name))))
                 (key (and f (if fullpath f (file-name-nondirectory f)))))
            (if (and key (string-match-p regexp key))
		(progn (setq n (1+ n))
		       (org-agenda-bulk-mark))
	      (forward-line 1)
	      ))
          )))
    (message "Marked %d entries (file ~ %s)" n regexp)))
(with-eval-after-load 'org
  (defun my/org-save-after-refile (&rest _)
    (org-save-all-org-buffers))
  (advice-add 'org-refile :after #'my/org-save-after-refile))



(use-package org-modern)
;; Option 1: Per buffer
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
(add-hook 'org-mode-hook #'org-modern-mode)

(setq org-startup-indented t)
					;(use-package org-modern-indent
					;  :load-path "~/.emacs.d/org-modern-indent-main/"
					;  :config
					;  (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
					;  )

(use-package org-roam)
(setq org-roam-directory (file-truename "~/org/zettelkasten"))
(with-eval-after-load 'org-roam
  (org-roam-db-autosync-mode)


  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.33))))



(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))
(setq citar-org-roam-note-title-template "${author} - ${title}")


(defun my/citar-date-fragment ()
  (if (and (boundp 'citar-date) citar-date)
      "test"
					;(format " (%s)" citar-date)
    ""))


(setq org-roam-capture-templates
      '(("p" "permanent" plain
         "%?"
         :target
         (file+head
          "permanent/${slug}.org"
					;"#+title: ${title}\n")
          "#+title: ${title}
#+created: %U
#+filetags: :permanent%(let* ((tags (completing-read-multiple
                           \"Tags: \"
                           (org-roam-tag-completions) nil))
                    (s (string-join tags \":\")))
               (if (string-empty-p s) \"\" (concat \":\" s \":\")))
               \n\n\n")
         :unnarrowed t)
        ("n" "literature note" plain
         "%?"
         :target
         (file+head
          "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/literature/${citar-citekey}.org"
	  "#+title: ${citar-citekey} %(if (and (boundp 'citar-date) citar-date)
                                (format \"(%s)\" citar-date)
                              \"\") ${note-title}
#+created: %U
#+filetags: :literature%(let* ((tags (completing-read-multiple
                           \"Tags:  \"
                           (org-roam-tag-completions) nil))
                    (s (string-join tags \":\")))
               (if (string-empty-p s) \"\" (concat \":\" s \":\")))

               \n\n\n")

         :unnarrowed t)

      ("c" "concept" plain
       
       "\n* Sources\n\n
%?"
         :target
         (file+head
          "concepts/${slug}.org"
					;"#+title: ${title}\n")
          "#+title: ${title}
#+created: %U
#+filetags: :concept%(let* ((tags (completing-read-multiple
                           \"Tags:  \"
                           (org-roam-tag-completions) nil))
                    (s (string-join tags \":\")))
               (if (string-empty-p s) \"\" (concat \":\" s \":\")))
               \n\n\n")
         :unnarrowed t)

      ("s" "specific project" plain
       
       "%?"
         :target
         (file+head
          "projects/${slug}.org"
					;"#+title: ${title}\n")
          "#+title: ${title}
#+created: %U
#+filetags: :projects%(let* ((tags (completing-read-multiple
                           \"Tags:  \"
                           (org-roam-tag-completions) nil))
                    (s (string-join tags \":\")))
               (if (string-empty-p s) \"\" (concat \":\" s \":\")))
               \n\n\n")
         :unnarrowed t)
        ))
(setq org-roam-node-display-template
      (concat "${title:*}" (propertize "${tags:30}" 'face 'org-tag)))
(setq citar-org-roam-capture-template-key "n")
(setq org-roam-dailies-directory "daily/")
(setq org-roam-dailies-capture-templates
      '(("d" "default" plain
         "* %<%H:%M>:\n%?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

(defun my/org-newline-same-indent ()
  "Open a new line below with the same indentation, no list magic."
  (interactive)
  (let ((col (current-indentation)))
    (end-of-line)
    (insert "\n")
    (indent-line-to col)
    (evil-insert 1)
    ))

(with-eval-after-load 'org
  (evil-define-key 'normal org-mode-map
    (kbd "o") #'my/org-newline-same-indent))

(defun my/org-roam-capture-permanent ()
  (interactive)
  (let* ((title (read-string "Title: "))
	 (node  (org-roam-node-create)))
    (setf (org-roam-node-title node) title)
    (org-roam-capture- :node node :keys "p")
    )
  )
(defun my/org-roam-capture-concept ()
  (interactive)
  (let* ((title (read-string "Title: "))
	 (node  (org-roam-node-create)))
    (setf (org-roam-node-title node) title)
    (org-roam-capture- :node node :keys "c")
    )
  )
(defun my/org-roam-capture-project ()
  (interactive)
  (let* ((title (read-string "Title: "))
	 (node  (org-roam-node-create)))
    (setf (org-roam-node-title node) title)
    (org-roam-capture- :node node :keys "s")
    )
  )
(defun my/capture-dispatch ()
  "Ein zentrales Capture-Menü für TODO, Roam, Daily etc."
  (interactive)
  (pcase (read-key "d: Daily p: Permanent  n: Citar-Note  d: Daily i: Inbox c: Concept t: Task")
    (?i (org-capture nil "i"))              ; z.B. Inbox
    (?d (org-roam-dailies-capture-today))
    (?p (my/org-roam-capture-permanent))                  ; <- direkt Roam-Template „p“
    (?c (my/org-roam-capture-concept))                  ; <- direkt Roam-Template „p“
    (?s (my/org-roam-capture-project))
    (?n (citar-open-notes))                  ; z.B. permanentes Note-Template „n)
    (?t (org-capture nil "t"))
    ))

(evil-define-key 'normal 'global (kbd "<leader>c") #'my/capture-dispatch)
(evil-define-key 'normal 'global (kbd "<leader>od") #'org-roam-dailies-goto-today)
(setq org-roam-tag-sources '(prop all-directories))
(defun my/org-roam-capf ()
  (add-to-list 'completion-at-point-functions
               #'org-roam-complete-link-at-point))

(add-hook 'org-mode-hook #'my/org-roam-capf)

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(evil-define-key 'normal 'org-mode-map (kbd "gd") #'org-open-at-point)
(setq org-link-frame-setup
      '((file . find-file)
        (id   . find-file)
        (http . browse-url)))
(defcustom my/zotero-storage-dirs
  (list
   ;; adjust if your path differs
   (expand-file-name "~/Zotero/storage/"))
  "Candidate Zotero storage directories (must end with /storage/)."
  :type '(repeat directory))

(defun my/zotero-attachment-key-from-path (file)
  "If FILE is in a Zotero storage dir, return the 8-char attachment key, else nil."
  (let* ((f (expand-file-name file))
         (match
          (cl-loop for dir in my/zotero-storage-dirs
                   for d = (file-name-as-directory (expand-file-name dir))
                   when (string-prefix-p d f)
                   return (substring f (length d)))))
    (when (and match (string-match "\\`\\([A-Z0-9]\\{8\\}\\)/" match))
      (match-string 1 match))))

(defun my/open-file-in-zotero (file)
  "Open PDF FILE. If it lives in Zotero storage, open via Zotero's internal PDF reader."
  (let ((key (my/zotero-attachment-key-from-path file)))
    (if key
        (browse-url (format "zotero://open-pdf/library/items/%s" key))
      ;; fallback: whatever you normally want
      (call-process "xdg-open" nil 0 nil (expand-file-name file)))))

;; Tell citar to use our opener for pdf files:
(with-eval-after-load 'citar
  (setf (alist-get "pdf" citar-file-open-functions nil nil #'string-equal)
        #'my/open-file-in-zotero))
(with-eval-after-load 'citar
  (setf (alist-get "html" citar-file-open-functions nil nil #'string-equal)
        #'my/open-file-in-zotero))
