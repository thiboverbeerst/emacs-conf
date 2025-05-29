;; org-mode.el
(require 'workspace)

;; Your existing org-mode configuration goes here
;; ... (whatever you already have) ...

;; org-id configuration (part of org-mode)
(require 'org-id)

(defun my-org-id-get-locations-file ()
  "Get org-id-locations file path for current workspace."
  (let* ((ws-root (my-find-workspace-root default-directory))
         (id-locations-path (when ws-root
                             (my-workspace-get-package-property ws-root "org-mode" "idLocationsPath"))))
    (if (and ws-root id-locations-path)
        id-locations-path
      (locate-user-emacs-file ".org-id-locations"))))

(defun my-org-id-configure ()
  "Set org-id-locations-file based on current workspace."
  (let ((id-locations-file (my-org-id-get-locations-file)))
    (setq org-id-locations-file id-locations-file)
    
    ;; Ensure the directory exists
    (let ((id-dir (file-name-directory id-locations-file)))
      (unless (file-directory-p id-dir)
        (make-directory id-dir t)))))

;; Configure on startup and file changes
(my-org-id-configure)
(add-hook 'find-file-hook #'my-org-id-configure)