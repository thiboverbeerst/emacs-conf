(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory (file-truename (concat (getenv "INFORMATIEBANK_DIR") "/3-kennisbank/lexicon")))
  (org-roam-db-location (expand-file-name ".emacs/org-roam/org-roam.db" (getenv "INFORMATIEBANK_DIR")))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-enable)
  ;; Lexiconleemmaatje Template
  (setq org-roam-capture-templates
        '(("l" "Lexiconleemmaatje" plain
           "%?"
           :target (file+head "${slug}.org"
                              "#+title: ${title}
#+ROAM_TAGS: lexicon ${domaine}
:PROPERTIES:
:ID:       %(org-id-new)
:DOMAINE:  ${domaine}
:LEVEL:    ${level}
:REGISTER: ${register}
:KEYWORDS: ${keywords}
:END:

* Definitie

* Voorbeeld

* Verwant

* Tags
:${tags}:")
           :unnarrowed t))))
