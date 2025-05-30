;;; citar-install.el --- Installation script for citar and dependencies -*- lexical-binding: t; -*-

;;; Commentary:
;; This script handles the proper installation of citar and its dependencies.
;; Run this before loading citations.el

;;; Code:

;; Ensure package system is initialized
(require 'package)
(unless package-archive-contents
  (package-refresh-contents))

(defun citar-clean-install ()
  "Clean install of citar and dependencies."
  (interactive)
  
  ;; Clean up any broken installations
  (message "Cleaning up existing citar installations...")
  (dolist (pkg '(citar citar-embark citar-capf))
    (when (package-installed-p pkg)
      (condition-case err
          (package-delete (cadr (assoc pkg package-alist)))
        (error (message "Could not delete %s: %s" pkg err)))))
  
  ;; Refresh package contents
  (message "Refreshing package archives...")
  (package-refresh-contents)
  
  ;; Install dependencies in correct order
  (message "Installing embark...")
  (unless (package-installed-p 'embark)
    (package-install 'embark))
  
  (message "Installing parsebib...")
  (unless (package-installed-p 'parsebib)
    (package-install 'parsebib))
  
  (message "Installing citeproc...")
  (unless (package-installed-p 'citeproc)
    (package-install 'citeproc))
  
  ;; Install citar
  (message "Installing citar...")
  (unless (package-installed-p 'citar)
    (package-install 'citar))
  
  ;; Install citar extensions
  (message "Installing citar-embark...")
  (unless (package-installed-p 'citar-embark)
    (package-install 'citar-embark))
  
  (message "Citar installation complete!")
  
  ;; Verify installation
  (message "Verifying installation...")
  (require 'citar)
  (message "Citar version: %s" (if (boundp 'citar-version) citar-version "unknown"))
  (message "Citar-org-setup available: %s" (fboundp 'citar-org-setup))
  (message "Citar-capf available: %s" (fboundp 'citar-capf))
  
  (when (featurep 'citar-embark)
    (message "Citar-embark loaded successfully"))
  
  (message "Installation verification complete."))

;; Alternative: Manual installation from source
(defun citar-install-from-source ()
  "Install citar from source if package installation fails."
  (interactive)
  
  (let ((site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
        (citar-dir (expand-file-name "site-lisp/citar" user-emacs-directory)))
    
    ;; Create site-lisp directory if it doesn't exist
    (unless (file-exists-p site-lisp-dir)
      (make-directory site-lisp-dir t))
    
    ;; Clone or update citar repository
    (if (file-exists-p citar-dir)
        (progn
          (message "Updating citar from source...")
          (shell-command (format "cd %s && git pull" citar-dir)))
      (progn
        (message "Cloning citar from source...")
        (shell-command (format "git clone https://github.com/emacs-citar/citar.git %s" citar-dir))))
    
    ;; Add to load path
    (add-to-list 'load-path citar-dir)
    
    ;; Load citar
    (require 'citar)
    (message "Citar loaded from source successfully")))

;; Diagnostic function
(defun citar-diagnose ()
  "Diagnose citar installation issues."
  (interactive)
  
  (message "=== Citar Diagnostic Report ===")
  
  ;; Check Emacs version
  (message "Emacs version: %s" emacs-version)
  (when (version< emacs-version "27.1")
    (message "WARNING: Citar requires Emacs 27.1 or higher"))
  
  ;; Check package installation
  (message "Package installation status:")
  (dolist (pkg '(citar citar-embark parsebib citeproc embark org))
    (message "  %s: %s" pkg 
             (if (package-installed-p pkg) "INSTALLED" "NOT INSTALLED")))
  
  ;; Check features loaded
  (message "Features loaded:")
  (dolist (feature '(citar citar-embark citar-capf oc oc-csl))
    (message "  %s: %s" feature 
             (if (featurep feature) "LOADED" "NOT LOADED")))
  
  ;; Check functions available
  (message "Functions available:")
  (dolist (func '(citar-org-setup citar-capf citar-embark-mode org-cite-insert))
    (message "  %s: %s" func 
             (if (fboundp func) "AVAILABLE" "NOT AVAILABLE")))
  
  ;; Check load path
  (message "Citar in load-path: %s" 
           (seq-some (lambda (path) (string-match-p "citar" path)) load-path))
  
  (message "=== End Diagnostic Report ==="))

(provide 'citar-install)
;;; citar-install.el ends here