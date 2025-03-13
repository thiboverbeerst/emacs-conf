
;; Load workspace functionality first
(load-file (expand-file-name "workspace.el" user-emacs-directory))

;; Then load backup functionality
(load-file (expand-file-name "backup.el" user-emacs-directory))

(load-file (expand-file-name "todos.el" user-emacs-directory))

(load-file (expand-file-name "agenda.el" user-emacs-directory))

;; Optionally load your custom settings file (GUI customizations)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))
