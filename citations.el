;;; citations.el --- Org-cite + Citar setup with workspace integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Dynamic citation configuration based on workspace.xml
;; Requirements:
;; - Emacs 29+ (for native org-cite)
;; - citar (with org-code = oc), citar-embark, citar-capf
;; - Better BibTeX plugin in Zotero
;; - workspace.el loaded
;; - Properly configured workspace.xml with zotero-bib, csl-styles, csl-locales paths

;;; Code:

(require 'workspace) ; Your workspace management functions

;; Cache for workspace-specific citation configurations
(defvar my-citations-config-cache (make-hash-table :test 'equal)
  "Cache for citation configurations per workspace root.")

(defun my-citations-should-activate-p ()
  "Check if citations should be activated based on current location.
Only activate if we're inside the workspace."
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
  "Get the default CSL style file path.
Defaults to APA style if available in the styles directory."
  (when-let ((styles-dir (my-citations-get-csl-styles-dir)))
    (let ((apa-style (expand-file-name "apa.csl" styles-dir)))
      (if (file-exists-p apa-style)
          apa-style
        ;; Fallback to first .csl file found
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
        (setq org-cite-global-bibliography (list bib-file)))
      
      (when styles-dir
        (setq org-cite-csl-styles-dir styles-dir))
      
      (when default-style
        (setq org-cite-csl-style default-style))
      
      (when locales-dir
        (setq org-cite-csl-locales-dir locales-dir))
      
      ;; Configure export processors
      (setq org-cite-export-processors
            '((latex biblatex)
              (odt csl)
              (html csl)
              (t csl))))))

(defun my-citations-configure-citar ()
  "Configure citar with workspace-specific settings."
  (when (my-citations-should-activate-p)
    (let ((bib-file (my-citations-get-bibliography-file)))
      (when bib-file
        (setq citar-bibliography (list bib-file))))))


;; Configuration functions for use-package or direct setup
(defun my-citations-setup-org-cite ()
  "Setup function for org-cite configuration."
  (require 'oc-csl)
  (require 'oc-biblatex)
  (my-citations-configure-org-cite))

(defun my-citations-setup-citar ()
  "Setup function for citar configuration."
  (my-citations-configure-citar)
  
  ;; Configure citar symbols if nerd-icons is available
  (when (featurep 'nerd-icons)
    (setq citar-symbols
          `((file ,(nerd-icons-octicon "nf-oct-file_pdf") . " ")
            (note ,(nerd-icons-octicon "nf-oct-pencil") . " ")
            (link ,(nerd-icons-octicon "nf-oct-link") . " "))
          citar-symbol-separator "  ")))

;; Hook function to reconfigure when entering workspace directories
(defun my-citations-maybe-reconfigure ()
  "Reconfigure citations if we've entered a different workspace."
  (when (and (derived-mode-p 'org-mode)
             (my-citations-should-activate-p))
    (my-citations-configure-org-cite)
    (my-citations-configure-citar)))

;; Use-package configurations (optional - can be used directly)
(with-eval-after-load 'oc
  (my-citations-setup-org-cite))

(with-eval-after-load 'citar
  (my-citations-setup-citar))

;; Hook to reconfigure when switching buffers/directories
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