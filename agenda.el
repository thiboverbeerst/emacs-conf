;; agenda.el configuration
(require 'workspace)
(require 'workspace-status)

(defun my-set-org-agenda-from-workspace ()
  "Dynamically set `org-agenda-files` based on the current workspace.
If a workspace is found (using `my-find-workspace-root`), then look for Org files in
the \"org\" subdirectory of the workspace root. If that folder doesn't exist, search the entire workspace.
If no workspace is found, do nothing."
  (let* ((current-dir (or (when (buffer-file-name)
                            (file-name-directory (buffer-file-name)))
                          default-directory))
         (ws-root (my-find-workspace-root current-dir)))
    (if ws-root
        (let* ((org-dir (expand-file-name "org" ws-root))
               (agenda-files (if (file-directory-p org-dir)
                               (directory-files-recursively org-dir "\\.org$")
                             (directory-files-recursively ws-root "\\.org$"))))
          (setq org-agenda-files agenda-files))
      ;; (message "org-agenda-files set to: %s" org-agenda-files)
      (message "Workspace not found; org-agenda-files not set."))))

(defun my-agenda-should-activate-p ()
  "Check if agenda should be activated based on current workspace."
  (let* ((ws-root (my-find-workspace-root default-directory)))
    (when ws-root
      (let ((org-dir (expand-file-name "org" ws-root)))
        (or 
         ;; Check if there's a dedicated org directory with files
         (and (file-directory-p org-dir)
              (directory-files-recursively org-dir "\\.org$"))
         ;; Check if there are .org files in workspace root
         (directory-files-recursively ws-root "\\.org$")
         ;; Also activate if org-agenda-files is already set and non-empty
         (and (boundp 'org-agenda-files) 
              org-agenda-files 
              (> (length org-agenda-files) 0)))))))

(defun my-agenda-service-status ()
  "Return agenda service status for workspace-status."
  (when (my-agenda-should-activate-p)
    (let* ((ws-root (my-find-workspace-root default-directory))
           (org-dir (expand-file-name "org" ws-root))
           (file-count (length (or org-agenda-files '()))))
      (cond
       ;; If we have agenda files, show the count
       ((> file-count 0)
        (if (and (file-directory-p org-dir)
                 (cl-some (lambda (file) 
                           (string-prefix-p (expand-file-name org-dir) 
                                          (expand-file-name file)))
                         org-agenda-files))
            (format "agenda:%d" file-count)
          (format "agenda:%d*" file-count))) ; * indicates files from workspace root
       ;; If org directory exists but no files yet
       ((file-directory-p org-dir)
        "agenda:0")
       ;; Fallback
       (t "agenda")))))

(defun my-agenda-get-directory ()
  "Get agenda directory if we should activate, nil otherwise."
  (when (my-agenda-should-activate-p)
    (let* ((ws-root (my-find-workspace-root default-directory))
           (org-dir (expand-file-name "org" ws-root)))
      (if (file-directory-p org-dir)
          org-dir
        ws-root))))

(defun my-agenda-status ()
  "Show current agenda configuration status."
  (interactive)
  (message "agenda status:")
  (message " Should activate: %s" (my-agenda-should-activate-p))
  (message " Agenda directory: %s" (my-agenda-get-directory))
  (message " Agenda files count: %d" (length (or org-agenda-files '())))
  (message " Files: %s" (mapconcat #'file-name-nondirectory 
                                   (or org-agenda-files '()) ", ")))

(defun my-agenda-force-configure ()
  "Force agenda configuration refresh."
  (interactive)
  (my-set-org-agenda-from-workspace)
  (my-agenda-status))

(defun my-workspace-debug-agenda ()
  "Debug agenda activation status."
  (interactive)
  (let* ((ws-root (my-find-workspace-root default-directory))
         (org-dir (when ws-root (expand-file-name "org" ws-root)))
         (should-activate (my-agenda-should-activate-p)))
    (message "Debug agenda:")
    (message "  Current dir: %s" default-directory)
    (message "  Workspace root: %s" ws-root)
    (message "  Org dir: %s" org-dir)
    (message "  Org dir exists: %s" (when org-dir (file-directory-p org-dir)))
    ;; (message "  Should activate: %s" should-activate)
    (message "  Current agenda files: %d" (length (or org-agenda-files '())))
    (message "  Service status: %s" (my-agenda-service-status))))

;; Auto-configuration
(defun my-agenda-configure ()
  "Configure agenda for current workspace context."
  (my-set-org-agenda-from-workspace))

;; Set up the agenda when entering Org mode or changing directories
(add-hook 'org-mode-hook 'my-agenda-configure)
(add-hook 'find-file-hook 'my-agenda-configure)
(add-hook 'dired-mode-hook 'my-agenda-configure)

;; Timer to periodically check for workspace changes
(run-with-timer 5 15 #'my-agenda-configure)

;; Register agenda service with workspace-status
(my-workspace-register-service 'agenda #'my-agenda-service-status)

;; Optionally, run once at startup (or call manually as needed)
(my-agenda-configure)