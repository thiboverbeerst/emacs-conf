;; Package system initialization and MELPA setup
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Ensure use-package is installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Set custom file BEFORE loading anything else
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Load workspace functionality first
(load-file (expand-file-name "workspace.el" user-emacs-directory))
(load-file (expand-file-name "workspace-status.el" user-emacs-directory)) ; optioneel

;; Then load other functionality
(load-file (expand-file-name "backup.el" user-emacs-directory))
(load-file (expand-file-name "todos.el" user-emacs-directory))
(load-file (expand-file-name "agenda.el" user-emacs-directory))

;; Load org-mode and its dependencies
(load-file (expand-file-name "org-mode.el" user-emacs-directory))

;; Load org-roam (now it can find workspace)
(load-file (expand-file-name "org-roam.el" user-emacs-directory))

;; Load custom settings file at the END
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; Signal that init is complete (optional but helpful for debugging)
(message "Init file loaded successfully")