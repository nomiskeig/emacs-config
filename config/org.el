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
  (kbd "<leader>ga") (lambda () (interactive) (org-agenda nil " ")))
(evil-define-key 'normal 'global (kbd "<leader>c") #'org-capture)
(with-eval-after-load 'org-agenda
  (evil-define-key 'emacs org-agenda-mode-map
    (kbd "j") #'org-agenda-next-line
    (kbd "k") #'org-agenda-previous-line
    (kbd "r") #'jethro/org-process-inbox
    (kbd "d") #'org-agenda-kill
    (kbd "u") #'org-agenda-undo
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
(setq org-capture-templates
      `(("i" "Inbox" entry  (file "~/org/todo/inbox.org")
         ,(concat "* TODO %?\n"
                  "/Entered on %U"))
        ("s" "Slipbox" entry  (file "~/org/notes/inbox.org")
         "* %?\n")))

(setq org-tag-alist
      '((:startgroup . nil) ("@work" . ?w) ("@home" . ?h)
	("@uni" . ?u) ("@errand" . ?e) (:endgroup . nil)))
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

(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t% s") (todo . " %i %-20(my/agenda-todo-prefix)")
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
;  :config
;  (add-hook 'org-mode-hook #'org-modern-indent-mode 90)
;  )

(use-package org-roam)
(setq org-roam-directory (file-truename "~/org/zettelkasten"))
(org-roam-db-autosync-mode)

(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))
(setq citar-org-roam-note-title-template "${author} - ${title}")
(setq org-roam-capture-templates
      '(("p" "project" plain
         "%?"
         :target
         (file+head
          "%<%Y%m%d%H%M%S>-${slug}.org"
          "#+title: ${note-title}\n")
         :unnarrowed t)
        ("n" "literature note" plain
         "%?"
         :target
         (file+head
          "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/literature/${citar-citekey}.org"
          "#+title: ${citar-citekey} (${citar-date}). ${note-title}.\n#+created: %U\n\n\n")
         :unnarrowed t)))
(setq citar-org-roam-capture-template-key "n")
(setq org-roam-dailies-directory "daily/")
;(use-package org-journal
;  :bind
;  ("C-c n j" . org-journal-new-entry)
;  :custom
;  (org-journal-date-prefix "#+title: ")
;  (org-journal-file-format "%Y-%m-%d.org")
;  (org-journal-dir (file-truename "~/org/zettelkasten/journal"))
;  (org-journal-file-type 'daily)
;  (org-journal-date-format "%A, %d %B %Y"))

;(defun my/org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
;  (org-journal-new-entry t)
;  (unless (eq org-journal-file-type 'daily)
;    (org-narrow-to-subtree))
;  (goto-char (point-max)))

;(add-to-list 'org-capture-templates '("j" "Journal entry" plain (function my/org-journal-find-location)
;                               "** %(format-time-string org-journal-time-format)%^{Title}\n%i%?"
;                                ))
