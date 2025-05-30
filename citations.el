;;; citations.el --- Clean citations with workspace integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Simplified citation configuration that works with org-cite (built-in)
;; and optionally citar. Only activates within proper workspaces.

;;; Code:

(require 'workspace)

;;; Configuration and State

(defvar citations--update-timer nil
  "Global timer for debounced sidebar updates.")

(defvar citations--initialized nil
  "Whether citations have been initialized.")

;;; Core Predicates

(defun citations--should-activate-p ()
  "Check if citations should be activated for current buffer."
  (and (citations--in-workspace-p)
       (citations--has-bibliography-p)
       (citations--in-supported-mode-p)))

(defun citations--in-workspace-p ()
  "Check if current buffer is in a workspace."
  (not (null (my-find-workspace-root default-directory))))

(defun citations--has-bibliography-p ()
  "Check if current workspace has a bibliography file."
  (and (citations--get-bibliography-file)
       (file-exists-p (citations--get-bibliography-file))))

(defun citations--in-supported-mode-p ()
  "Check if current buffer is in a supported mode."
  (or (derived-mode-p 'org-mode)
      (derived-mode-p 'markdown-mode)
      (derived-mode-p 'latex-mode)
      (derived-mode-p 'text-mode)))

;;; Workspace Integration

(defun citations--get-bibliography-file ()
  "Get bibliography file path from current workspace."
  (when-let ((ws-root (my-find-workspace-root default-directory)))
    (my-workspace-get-location-path ws-root "zotero-bib")))

(defun citations--get-csl-styles-dir ()
  "Get CSL styles directory from current workspace."
  (when-let ((ws-root (my-find-workspace-root default-directory)))
    (my-workspace-get-location-path ws-root "csl-styles")))

(defun citations--get-csl-locales-dir ()
  "Get CSL locales directory from current workspace."
  (when-let ((ws-root (my-find-workspace-root default-directory)))
    (my-workspace-get-location-path ws-root "csl-locales")))

(defun citations--get-default-csl-style ()
  "Get default CSL style file path."
  (when-let ((styles-dir (citations--get-csl-styles-dir)))
    (let ((apa-style (expand-file-name "apa.csl" styles-dir)))
      (if (file-exists-p apa-style)
          apa-style
        (when-let ((csl-files (directory-files styles-dir t "\\.csl$")))
          (car csl-files))))))

;;; Org-cite Configuration

(defun citations--configure-org-cite ()
  "Configure org-cite with workspace-specific settings."
  (when (citations--should-activate-p)
    (let ((bib-file (citations--get-bibliography-file))
          (styles-dir (citations--get-csl-styles-dir))
          (default-style (citations--get-default-csl-style))
          (locales-dir (citations--get-csl-locales-dir)))
      
      (when bib-file
        (setq org-cite-global-bibliography (list bib-file)))
      
      (when styles-dir
        (setq org-cite-csl-styles-dir styles-dir))
      
      (when default-style
        (setq org-cite-csl-style default-style))
      
      (when locales-dir
        (setq org-cite-csl-locales-dir locales-dir))
      
      ;; Configure processors
      (setq org-cite-activate-processor 'basic
            org-cite-follow-processor 'basic
            org-cite-insert-processor 'basic)
      
      ;; Use CSL for export if available
      (when (require 'oc-csl nil t)
        (setq org-cite-follow-processor 'csl
              org-cite-export-processors
              '((latex biblatex)
                (odt csl)
                (html csl)
                (t csl)))))))

;;; Citar Configuration

(defun citations--try-install-citar ()
  "Attempt to install citar and dependencies."
  (condition-case err
      (progn
        (dolist (pkg '(embark parsebib citeproc citar))
          (unless (package-installed-p pkg)
            (package-install pkg)))
        
        ;; Optional packages
        (condition-case nil
            (unless (package-installed-p 'citar-embark)
              (package-install 'citar-embark))
          (error nil))
        
        t) ; Success
    (error 
     (message "Could not install citar packages: %s" err)
     nil)))

(defun citations--configure-citar ()
  "Configure citar if available."
  (when (and (citations--should-activate-p)
             (require 'citar nil t))
    (let ((bib-file (citations--get-bibliography-file)))
      (when bib-file
        (setq citar-bibliography (list bib-file))
        
        ;; Add completion
        (when (fboundp 'citar-capf)
          (add-to-list 'completion-at-point-functions #'citar-capf))
        
        ;; Try embark integration
        (when (require 'citar-embark nil t)
          (citar-embark-mode 1))))))

;;; Visual Formatting

(defun citations--enable-visual-formatting ()
  "Enable citation highlighting in current buffer."
  (when (citations--should-activate-p)
    (cond
     ;; Org-mode: use org-cite
     ((derived-mode-p 'org-mode)
      (setq-local org-cite-activate-processor 'basic
                  org-cite-follow-processor 'basic)
      
      (when (fboundp 'org-cite-activate)
        (condition-case nil
            (org-cite-activate (point-max))
          (error nil))))
     
     ;; LaTeX mode: add font-lock rules for citation commands
     ((or (derived-mode-p 'latex-mode) (derived-mode-p 'LaTeX-mode))
      (font-lock-add-keywords
       nil
       '(("\\\\\\(?:auto\\|text\\|paren\\|foot\\)?cite\\(?:\\[[^]]*\\]\\)?\\(?:\\[[^]]*\\]\\)?{\\([^}]+\\)}"
          (0 'font-lock-function-name-face)
          (1 'font-lock-constant-face)))))
     
     ;; Markdown mode: highlight Pandoc citations
     ((derived-mode-p 'markdown-mode)
      (font-lock-add-keywords
       nil
       '(("\\[@[^]]+\\]" . 'font-lock-function-name-face)
         ("\\(?:^\\|\\s-\\)\\(@[[:alnum:]_:-]+\\)\\(?:\\s-\\|$\\|[.,;]\\)" 
          1 'font-lock-constant-face)))))
    
    ;; Refresh font-lock
    (when (fboundp 'font-lock-flush)
      (font-lock-flush))))

;;; Bibliography Sidebar

(defun citations--get-cited-keys ()
  "Get all citation keys used in current buffer."
  (let ((keys '()))
    (save-excursion
      (goto-char (point-min))
      (cond
       ;; Org-mode citations: [cite:@key] or [cite:@key1;@key2]
       ((derived-mode-p 'org-mode)
        (while (re-search-forward "\\[cite:[^]]*\\]" nil t)
          (let ((cite-block (match-string 0)))
            (with-temp-buffer
              (insert cite-block)
              (goto-char (point-min))
              (while (re-search-forward "@\\([[:alnum:]_:-]+\\)" nil t)
                (let ((key (match-string 1)))
                  (unless (member key keys)
                    (push key keys))))))))
       
       ;; LaTeX citations: \cite{key}, \autocite{key}, \textcite{key}, etc.
       ((or (derived-mode-p 'latex-mode) (derived-mode-p 'LaTeX-mode))
        (while (re-search-forward "\\\\\\(?:auto\\|text\\|paren\\|foot\\)?cite\\(?:\\[[^]]*\\]\\)?\\(?:\\[[^]]*\\]\\)?{\\([^}]+\\)}" nil t)
          (let ((cite-keys (match-string 1)))
            ;; Handle multiple keys separated by commas
            (dolist (key (split-string cite-keys "," t "[ \t\n]+"))
              (let ((clean-key (string-trim key)))
                (unless (or (string-empty-p clean-key) (member clean-key keys))
                  (push clean-key keys)))))))
       
       ;; Markdown citations: [@key] or @key
       ((derived-mode-p 'markdown-mode)
        ;; Pandoc-style citations [@key1; @key2]
        (while (re-search-forward "\\[@[^]]+\\]" nil t)
          (let ((cite-block (match-string 0)))
            (with-temp-buffer
              (insert cite-block)
              (goto-char (point-min))
              (while (re-search-forward "@\\([[:alnum:]_:-]+\\)" nil t)
                (let ((key (match-string 1)))
                  (unless (member key keys)
                    (push key keys)))))))
        ;; Also look for standalone @key citations
        (goto-char (point-min))
        (while (re-search-forward "\\(?:^\\|\\s-\\)@\\([[:alnum:]_:-]+\\)\\(?:\\s-\\|$\\|[.,;]\\)" nil t)
          (let ((key (match-string 1)))
            (unless (member key keys)
              (push key keys)))))
       
       ;; Generic text mode - look for common patterns
       (t
        ;; Look for @key patterns (common in many formats)
        (while (re-search-forward "\\(?:^\\|\\s-\\)@\\([[:alnum:]_:-]+\\)\\(?:\\s-\\|$\\|[.,;]\\)" nil t)
          (let ((key (match-string 1)))
            (unless (member key keys)
              (push key keys)))))))
    (nreverse keys)))

(defun citations--parse-bib-file (bib-file)
  "Parse BibTeX file and return entries as alist."
  (let ((entries '()))
    (when (file-exists-p bib-file)
      (with-temp-buffer
        (insert-file-contents bib-file)
        (goto-char (point-min))
        (while (re-search-forward "@\\w+{\\([^,]+\\)," nil t)
          (let ((key (match-string 1))
                (entry '())
                (entry-end (save-excursion
                            (when (re-search-forward "^}\\s-*$" nil t)
                              (point)))))
            (when entry-end
              (while (and (< (point) entry-end)
                          (re-search-forward "^\\s-*\\(\\w+\\)\\s-*=\\s-*[{\"]\\(.*?\\)[}\"]" entry-end t))
                (let ((field (downcase (match-string 1)))
                      (value (match-string 2)))
                  (push (cons field value) entry))))
            (when entry
              (push (cons key entry) entries))))))
    entries))

(defun citations--format-reference (key bib-entries)
  "Format a single reference in APA-like style."
  (when-let ((entry (cdr (assoc key bib-entries))))
    (let ((author (cdr (assoc "author" entry)))
          (title (cdr (assoc "title" entry)))
          (year (cdr (assoc "year" entry)))
          (journal (cdr (assoc "journal" entry)))
          (booktitle (cdr (assoc "booktitle" entry))))
      
      (concat
       (when author
         (concat (replace-regexp-in-string " and " ", " author) ". "))
       (when year (concat "(" year "). "))
       (when title (concat title ". "))
       (cond
        (journal (concat "*" journal "*"))
        (booktitle (concat "In *" booktitle "*"))
        (t ""))))))

;;; Auto-update System

(defun citations--schedule-update ()
  "Schedule a debounced update of the bibliography sidebar."
  (when (and (get-buffer-window "*Bibliography*")
             (citations--should-activate-p))
    
    (when citations--update-timer
      (cancel-timer citations--update-timer))
    
    (setq citations--update-timer
          (run-with-idle-timer 1.5 nil #'citations--safe-auto-update))))

(defun citations--safe-auto-update ()
  "Safely update bibliography sidebar."
  (condition-case err
      (when (and (buffer-live-p (current-buffer))
                 (get-buffer-window "*Bibliography*")
                 (citations--should-activate-p)
                 (not (string= (buffer-name) "*Bibliography*")))
        (setq citations--update-timer nil)
        (citations-show-bibliography-sidebar))
    (error 
     (message "Citations auto-update failed: %s" err)
     (setq citations--update-timer nil))))

(defun citations--setup-auto-update ()
  "Set up automatic sidebar updates for current buffer."
  (when (citations--should-activate-p)
    (add-hook 'after-change-functions 
              (lambda (&rest _) (citations--schedule-update))
              nil t)))

(defun citations--cleanup-auto-update ()
  "Clean up auto-update system."
  (remove-hook 'after-change-functions 
               (lambda (&rest _) (citations--schedule-update)) t)
  (when citations--update-timer
    (cancel-timer citations--update-timer)
    (setq citations--update-timer nil)))

;;; Buffer Management

(defun citations--maybe-reconfigure ()
  "Reconfigure citations when entering a workspace buffer."
  (condition-case err
      (when (and (citations--should-activate-p)
                 (not (string= (buffer-name) "*Bibliography*")))
        (citations--configure-org-cite)
        (citations--configure-citar)
        (citations--enable-visual-formatting)
        (citations--setup-auto-update))
    (error (message "Citations reconfigure failed: %s" err))))

;;; Public Commands

;;;###autoload
(defun citations-setup ()
  "Set up citations system with full features."
  (interactive)
  (unless citations--initialized
    ;; Configure org-cite (always available)
    (condition-case err
        (progn
          (require 'oc)
          (require 'oc-csl nil t)
          (require 'oc-biblatex nil t)
          (citations--configure-org-cite)
          (message "Org-cite configured"))
      (error (message "Could not configure org-cite: %s" err)))
    
    ;; Try to set up citar (optional)
    (when (called-interactively-p 'any)
      (when (citations--try-install-citar)
        (citations--configure-citar)
        (message "Citar configured")))
    
    ;; Set up hooks
    (dolist (hook '(org-mode-hook markdown-mode-hook latex-mode-hook text-mode-hook))
      (add-hook hook #'citations--maybe-reconfigure))
    
    (setq citations--initialized t)
    (message "Citations setup complete")))

;;;###autoload
(defun citations-show-bibliography-sidebar ()
  "Show bibliography sidebar for current buffer."
  (interactive)
  (if-let ((bib-file (citations--get-bibliography-file)))
      (let* ((source-buffer (current-buffer))
             (source-name (buffer-name source-buffer))
             (buf (get-buffer-create "*Bibliography*"))
             (cited-keys (citations--get-cited-keys))
             (bib-entries (citations--parse-bib-file bib-file)))
        
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)
            (if cited-keys
                (progn
                  (insert (format "References cited in %s:\n\n" source-name))
                  (let ((counter 1))
                    (dolist (key cited-keys)
                      (let ((formatted-ref (citations--format-reference key bib-entries)))
                        (insert (format "[%d] %s\n\n" counter 
                                       (or formatted-ref (format "%s (not found)" key))))
                        (setq counter (1+ counter))))))
              (insert (format "No citations found in %s." source-name)))
            
            (goto-char (point-min))
            (org-mode)
            (read-only-mode 1)))
        
        (display-buffer-in-side-window 
         buf 
         '((side . right) 
           (window-width . 0.25)
           (slot . 1))))
    (message "No bibliography file found in current workspace")))

;;;###autoload
(defun citations-hide-bibliography-sidebar ()
  "Hide bibliography sidebar."
  (interactive)
  (when-let ((buf (get-buffer "*Bibliography*")))
    (delete-windows-on buf)))

;;;###autoload
(defun citations-toggle-bibliography-sidebar ()
  "Toggle bibliography sidebar."
  (interactive)
  (if (get-buffer-window "*Bibliography*")
      (citations-hide-bibliography-sidebar)
    (citations-show-bibliography-sidebar)))

;;;###autoload
(defun citations-refresh ()
  "Refresh citation formatting and sidebar."
  (interactive)
  (when (citations--should-activate-p)
    (citations--enable-visual-formatting)
    (when (get-buffer-window "*Bibliography*")
      (citations-show-bibliography-sidebar))
    (message "Citations refreshed")))

;;;###autoload
(defun citations-status ()
  "Show current citations configuration."
  (interactive)
  (let ((ws-root (my-find-workspace-root default-directory)))
    (message "=== Citations Status ===")
    (message "Workspace: %s" (or ws-root "None"))
    (message "Bibliography: %s" (or (citations--get-bibliography-file) "None"))
    (message "CSL Styles: %s" (or (citations--get-csl-styles-dir) "None"))
    (message "Should activate: %s" (if (citations--should-activate-p) "Yes" "No"))
    (message "Org-cite available: %s" (if (featurep 'oc) "Yes" "No"))
    (message "Citar available: %s" (if (featurep 'citar) "Yes" "No"))))

;;;###autoload
(defun citations-test ()
  "Insert test citation appropriate for current mode."
  (interactive)
  (if (citations--should-activate-p)
      (let ((test-citation 
             (cond
              ((derived-mode-p 'org-mode) "[cite:@test2023]")
              ((or (derived-mode-p 'latex-mode) (derived-mode-p 'LaTeX-mode)) "\\cite{test2023}")
              ((derived-mode-p 'markdown-mode) "[@test2023]")
              (t "@test2023"))))
        (insert test-citation)
        (citations--enable-visual-formatting)
        (message "Inserted test citation: %s" test-citation)
        (when (get-buffer-window "*Bibliography*")
          (citations-show-bibliography-sidebar)))
    (message "Citations not available in current buffer (no workspace or bibliography file)")))

;;;###autoload
(defun citations-debug ()
  "Show debug information about citation detection."
  (interactive)
  (let ((keys (citations--get-cited-keys)))
    (message "=== Citation Debug ===")
    (message "Buffer: %s" (buffer-name))
    (message "Mode: %s" major-mode)
    (message "Should activate: %s" (if (citations--should-activate-p) "Yes" "No"))
    (message "Found citation keys: %s" (if keys (mapconcat 'identity keys ", ") "None"))
    (message "Bibliography file: %s" (citations--get-bibliography-file))))

;;; Initialization

(condition-case err
    (progn
      ;; Set up mode hooks for auto-configuration
      (dolist (hook '(org-mode-hook markdown-mode-hook latex-mode-hook text-mode-hook))
        (add-hook hook #'citations--maybe-reconfigure))
      (message "Citations: Auto-configuration hooks installed"))
  (error (message "Citations initialization failed: %s" err)))

(provide 'citations)
;;; citations.el ends here