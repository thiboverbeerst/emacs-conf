;; org-mode.el
(require 'workspace)

;; Upgrade org-mode to latest version from GNU ELPA
(use-package org
  :ensure t
  :pin gnu)

;; Your existing org-mode configuration goes here
;; ... (whatever you already have) ...

;; PROPERTY DRAWER SETTINGS
;; Hide property drawers by default (but keep headings visible)
(setq org-startup-folded 'content)  ; Show headings, fold content
(setq org-hide-drawer-startup t)    ; Hide all drawers (including properties) on startup
(setq org-startup-truncated nil)    ; Don't truncate lines

;; Alternative: Only hide specific drawer types
;; (add-to-list 'org-drawers "PROPERTIES")  ; Make sure PROPERTIES is recognized as a drawer

;; Optional: Customize what gets hidden/shown on startup
;; (setq org-startup-folded 'overview)    ; Only show top-level headings
;; (setq org-startup-folded 'content)     ; Show headings up to level 2, fold content
;; (setq org-startup-folded 'showall)     ; Show everything expanded

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