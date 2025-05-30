;;; citations.el --- Simplified citations with workspace integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Simplified citation configuration that works with just org-cite (built-in)
;; and optionally citar if available. No complex dependencies.

;;; Code:

(require 'workspace) ; Your workspace management functions

(defun my-citations-should-activate-p ()
  "Check if citations should be activated based on current location."
  (and (my-find-workspace-root default-directory)
       (my-citations-get-bibliography-file)))

(defun my-citations-get-bibliography-file ()
  "Get the bibliography file path from workspace configuration."
  (when-let ((ws-root (my-find-workspace-root default-directory)))
    (my-workspace-get-location-path ws-root "zotero-bib")))

(defun my-citations-get-csl-styles-dir ()
  "Get the CSL styles directory from workspace configuration."
  (when-let ((ws-root (my-find-workspace-root default-directory)))
    (my-workspace-get-location-path ws-root "csl-styles")))

(defun my-citations-get-csl-locales-dir ()
  "Get the CSL locales directory from workspace configuration."
  (when-let ((ws-root (my-find-workspace-root default-directory)))
    (my-workspace-get-location-path ws-root "csl-locales")))

(defun my-citations-get-default-csl-style ()
  "Get the default CSL style file path."
  (when-let ((styles-dir (my-citations-get-csl-styles-dir)))
    (let ((apa-style (expand-file-name "apa.csl" styles-dir)))
      (if (file-exists-p apa-style)
          apa-style
        (when-let ((csl-files (directory-files styles-dir t "\\.csl$")))
          (car csl-files))))))

(defun my-citations-configure-org-cite ()
  "Configure org-cite with workspace-specific settings."
  (when (my-citations-should-activate-p)
    (let ((bib-file (my-citations-get-bibliography-file))
          (styles-dir (my-citations-get-csl-styles-dir))
          (default-style (my-citations-get-default-csl-style))
          (locales-dir (my-citations-get-csl-locales-dir)))
      
      (when bib-file
        (setq org-cite-global-bibliography (list bib-file))
        (message "Citations: Bibliography set to %s" bib-file))
      
      (when styles-dir
        (setq org-cite-csl-styles-dir styles-dir)
        (message "Citations: CSL styles directory set to %s" styles-dir))
      
      (when default-style
        (setq org-cite-csl-style default-style)
        (message "Citations: Default CSL style set to %s" default-style))
      
      (when locales-dir
        (setq org-cite-csl-locales-dir locales-dir))
      
      ;; Configure export processors
      (setq org-cite-export-processors
            '((latex biblatex)
              (odt csl)
              (html csl)
              (t csl))))))

(defun my-citations-try-install-citar ()
  "Try to install citar packages if not present."
  (condition-case err
      (progn
        ;; Install dependencies first
        (unless (package-installed-p 'embark)
          (package-install 'embark))
        (unless (package-installed-p 'parsebib)
          (package-install 'parsebib))
        (unless (package-installed-p 'citeproc)
          (package-install 'citeproc))
        
        ;; Install citar
        (unless (package-installed-p 'citar)
          (package-install 'citar))
        
        ;; Try to install citar-embark (optional)
        (condition-case embark-err
            (unless (package-installed-p 'citar-embark)
              (package-install 'citar-embark))
          (error (message "Could not install citar-embark: %s" embark-err)))
        
        (message "Citar packages installed successfully"))
    (error (message "Could not install citar packages: %s" err))))

(defun my-citations-configure-citar ()
  "Configure citar if available."
  (when (and (my-citations-should-activate-p)
             (or (featurep 'citar) (require 'citar nil t)))
    (let ((bib-file (my-citations-get-bibliography-file)))
      (when bib-file
        (setq citar-bibliography (list bib-file))
        (message "Citar: Bibliography set to %s" bib-file)))))

(defun my-citations-setup ()
  "Main setup function for citations."
  (interactive)
  
  ;; Always configure org-cite (built-in)
  (require 'oc)
  (require 'oc-csl)
  (require 'oc-biblatex)
  (my-citations-configure-org-cite)
  
  ;; Try to install and configure citar
  (condition-case err
      (progn
        ;; Try to install if needed
        (my-citations-try-install-citar)
        
        ;; Configure citar
        (my-citations-configure-citar)
        
        ;; Add completion if citar is available
        (when (fboundp 'citar-capf)
          (add-to-list 'completion-at-point-functions #'citar-capf))
        
        ;; Try embark integration
        (condition-case embark-err
            (when (require 'citar-embark nil t)
              (citar-embark-mode 1))
          (error (message "Citar-embark not available: %s" embark-err)))
        
        (message "Citations setup complete with citar"))
    (error 
     (message "Citations setup complete with org-cite only: %s" err))))

(defun my-citations-maybe-reconfigure ()
  "Reconfigure citations if we've entered a different workspace."
  (when (and (derived-mode-p 'org-mode)
             (my-citations-should-activate-p))
    (my-citations-configure-org-cite)
    (my-citations-configure-citar)))

;; Setup citations when this file is loaded
(my-citations-setup)

;; Add hook for workspace switching
(add-hook 'org-mode-hook #'my-citations-maybe-reconfigure)

;; Provide some helpful commands
(defun my-citations-status ()
  "Show current citations configuration."
  (interactive)
  (message "=== Citations Status ===")
  (message "Workspace: %s" (or (my-find-workspace-root default-directory) "None"))
  (message "Bibliography: %s" (or (my-citations-get-bibliography-file) "None"))
  (message "CSL Styles: %s" (or (my-citations-get-csl-styles-dir) "None"))
  (message "Default Style: %s" (or (my-citations-get-default-csl-style) "None"))
  (message "Org-cite active: %s" (if (boundp 'org-cite-global-bibliography) "Yes" "No"))
  (message "Citar available: %s" (if (featurep 'citar) "Yes" "No"))
  (message "Citar-embark available: %s" (if (featurep 'citar-embark) "Yes" "No")))

(provide 'citations)
;;; citations.el ends here