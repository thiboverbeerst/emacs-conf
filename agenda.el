;; agenda.el configuration
(require 'workspace)

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

;; Set up the agenda when entering Org mode
(add-hook 'org-mode-hook 'my-set-org-agenda-from-workspace)

;; Optionally, run once at startup (or call manually as needed)
(my-set-org-agenda-from-workspace)

