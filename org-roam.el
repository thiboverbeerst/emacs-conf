(require 'workspace)
(require 'workspace-status)

(defun my-org-roam-should-activate-p ()
  "Check if org-roam should be activated based on current location.
Only activate if we're in or under the lexicon directory."
  (let* ((ws-root (my-find-workspace-root default-directory))
         (lexicon-dir (when ws-root 
                       (my-workspace-get-location-path ws-root "lexicon"))))
    (and lexicon-dir
         (file-exists-p lexicon-dir)
         (string-prefix-p (expand-file-name lexicon-dir)
                         (expand-file-name default-directory)))))

(defun my-org-roam-get-directory ()
  "Get org-roam directory if we should activate, nil otherwise."
  (when (my-org-roam-should-activate-p)
    (let ((ws-root (my-find-workspace-root default-directory)))
      (my-workspace-get-location-path ws-root "lexicon"))))

(defun my-org-roam-get-db-location ()
  "Get org-roam database location if we should activate, nil otherwise."
  (when (my-org-roam-should-activate-p)
    (let ((ws-root (my-find-workspace-root default-directory)))
      (my-workspace-get-db-path ws-root "org-roam"))))

(defvar my-org-roam-last-config nil
  "Track last org-roam configuration to avoid repeated messages.")

(defun my-org-roam-configure ()
  "Configure org-roam for current workspace context."
  (when (my-org-roam-should-activate-p)
    (let ((lexicon-dir (my-org-roam-get-directory))
          (db-location (my-org-roam-get-db-location)))
      (when (and lexicon-dir db-location)
        (let ((new-config (cons lexicon-dir db-location)))
          (unless (equal new-config my-org-roam-last-config)
            (setq org-roam-directory lexicon-dir)
            (setq org-roam-db-location db-location)
            (unless org-roam-db-autosync-mode
              (org-roam-db-autosync-enable))
            (setq my-org-roam-last-config new-config)
            (message "org-roam configured for: %s" lexicon-dir)))))))

(defun my-org-roam-deconfigure ()
  "Deconfigure org-roam when leaving workspace context."
  (when (and (boundp 'org-roam-directory) org-roam-directory)
    (unless (my-org-roam-should-activate-p)
      (when org-roam-db-autosync-mode
        (org-roam-db-autosync-disable))
      (setq org-roam-directory nil)
      (setq org-roam-db-location nil)
      (message "org-roam deconfigured"))))

(defun my-org-roam-status ()
  "Show current org-roam configuration status."
  (interactive)
  (message "org-roam status:")
  (message "  Should activate: %s" (my-org-roam-should-activate-p))
  (message "  Directory: %s" (when (boundp 'org-roam-directory) org-roam-directory))
  (message "  DB location: %s" (when (boundp 'org-roam-db-location) org-roam-db-location))
  (message "  Autosync active: %s" (when (boundp 'org-roam-db-autosync-mode) org-roam-db-autosync-mode)))

(defun my-org-roam-force-configure ()
  "Force org-roam configuration refresh."
  (interactive)
  (my-org-roam-configure)
  (my-org-roam-status))

;; Hook om org-roam te configureren bij directory changes
(add-hook 'find-file-hook #'my-org-roam-configure)
(add-hook 'dired-mode-hook #'my-org-roam-configure)

;; Timer om periodiek te checken (voor directory changes via cd etc.)
(run-with-timer 2 10 #'my-org-roam-configure)

(use-package org-roam
  :ensure t
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  ;; Initial configuration
  (my-org-roam-configure)
  
  ;; Lexiconleemmaatje Template
  (setq org-roam-capture-templates
        '(("l" "Lexiconleemmaatje" plain
           "%?"
           :target (file+head "${slug}.org"
                              "#+title: ${title}
#+ROAM_TAGS: lexicon
#+FILETAGS: :${tags}:
#+PROPERTY: DOMAINE ${domaine}
#+PROPERTY: LEVEL ${level}
#+PROPERTY: REGISTER ${register}
#+PROPERTY: KEYWORDS ${keywords}

* Definitie

* Voorbeeld

* Verwant

* Tags
:${tags}:\n")
           :unnarrowed t))))