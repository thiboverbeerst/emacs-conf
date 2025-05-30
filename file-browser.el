;;; file-browser.el --- NeoTree configuration with workspace integration -*- lexical-binding: t; -*-

;;; Commentary:
;; File browser configuration using NeoTree with intelligent workspace detection.
;; Provides an Obsidian-like file browser with Evil mode integration.

;;; Code:

(require 'workspace nil t) ; Soft dependency on workspace.el

;;; Core NeoTree Configuration

(use-package neotree
  :ensure t
  :commands (neotree-toggle neotree-show neotree-hide neotree-dir neotree-find)
  :init
  ;; Set theme before loading to avoid flicker
  (setq neo-theme (cond
                   ((and (display-graphic-p) 
                         (or (featurep 'all-the-icons) 
                             (package-installed-p 'all-the-icons))) 'icons)
                   ((and (display-graphic-p) 
                         (or (featurep 'nerd-icons) 
                             (package-installed-p 'nerd-icons))) 'nerd-icons)
                   ((display-graphic-p) 'arrow)
                   (t 'classic)))
  :config
  ;; Core settings
  (setq neo-smart-open t
        neo-show-hidden-files t
        neo-window-width 0.2
        neo-window-fixed-size nil
        neo-create-file-auto-open t
        neo-banner-message nil
        neo-show-updir-line nil
        neo-mode-line-type 'neotree
        neo-click-changes-root nil
        neo-auto-indent-point t
        neo-modern-sidebar t
        neo-vc-integration nil
        neo-autorefresh t)
  
  ;; File filtering
  (setq neo-hidden-regexp-list '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.DS_Store$"))
  
  :bind
  (("M-0"     . my-neotree-toggle-smart)
   ("C-x t t" . my-neotree-toggle-smart)
   ("C-x t d" . neotree-dir)
   ("C-x t f" . neotree-find)
   ("C-x t r" . my-neotree-smart-root)))

;;; Icon Support

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package nerd-icons
  :ensure t
  :if (display-graphic-p))

;;; Evil Mode Integration

(defun my-neotree-setup-evil-bindings ()
  "Set up Evil mode keybindings for NeoTree."
  (when (and (featurep 'evil) (fboundp 'evil-normal-state-local-map))
    (let ((map evil-normal-state-local-map))
      (define-key map (kbd "TAB") 'neotree-enter)
      (define-key map (kbd "SPC") 'neotree-quick-look)
      (define-key map (kbd "q")   'neotree-hide)
      (define-key map (kbd "RET") 'neotree-enter)
      (define-key map (kbd "g")   'neotree-refresh)
      (define-key map (kbd "n")   'neotree-next-line)
      (define-key map (kbd "p")   'neotree-previous-line)
      (define-key map (kbd "A")   'neotree-stretch-toggle)
      (define-key map (kbd "H")   'neotree-hidden-file-toggle)
      (define-key map (kbd "R")   'neotree-change-root)
      (define-key map (kbd "C")   'neotree-create-node)
      (define-key map (kbd "D")   'neotree-delete-node)
      (define-key map (kbd "r")   'neotree-rename-node))))

(with-eval-after-load 'evil
  (with-eval-after-load 'neotree
    (add-hook 'neotree-mode-hook #'my-neotree-setup-evil-bindings)))

;;; Workspace Integration

(defun my-neotree-find-root (directory)
  "Find the appropriate root directory for DIRECTORY.
Uses workspace detection if available, otherwise falls back to git root."
  (let ((dir (if (file-directory-p directory)
                 directory
               (file-name-directory directory))))
    (or
     ;; Try workspace root first
     (when (fboundp 'my-find-workspace-root)
       (my-find-workspace-root dir))
     ;; Fall back to git root
     (locate-dominating-file dir ".git")
     ;; Default to the directory itself
     dir)))

(defun my-neotree-visible-p ()
  "Check if NeoTree window is currently visible."
  (and (featurep 'neotree)
       (or (and (fboundp 'neo-global--window-exists-p)
                (neo-global--window-exists-p))
           (and (boundp 'neo-buffer-name)
                (get-buffer neo-buffer-name)
                (get-buffer-window neo-buffer-name)))))

(defun my-neotree-smart-root (&optional directory)
  "Open NeoTree with intelligent root detection.
Uses DIRECTORY if provided, otherwise uses current file's directory."
  (interactive)
  (let* ((target-dir (or directory 
                         buffer-file-name 
                         default-directory))
         (root-dir (my-neotree-find-root target-dir)))
    (when root-dir
      (message "Opening NeoTree at: %s" root-dir)
      (neotree-dir root-dir))))

(defun my-neotree-toggle-smart ()
  "Toggle NeoTree with smart root detection."
  (interactive)
  (if (my-neotree-visible-p)
      (neotree-hide)
    (my-neotree-smart-root)))

;;; Utility Functions

(defun my-neotree-refresh-on-workspace-change ()
  "Refresh NeoTree when workspace changes."
  (when (my-neotree-visible-p)
    (my-neotree-smart-root)))

(defun my-neotree-goto-current-file ()
  "Navigate NeoTree to show the current file."
  (interactive)
  (when buffer-file-name
    (if (my-neotree-visible-p)
        (neotree-find)
      (progn
        (my-neotree-smart-root)
        (neotree-find)))))

(defun my-neotree-force-refresh ()
  "Force refresh NeoTree and clear caches."
  (interactive)
  (when (my-neotree-visible-p)
    (with-current-buffer (get-buffer neo-buffer-name)
      (neotree-refresh))))

;;; Debug and Troubleshooting

(defun my-neotree-debug ()
  "Display debug information about NeoTree configuration."
  (interactive)
  (let ((info (list
               (format "Display graphic: %s" (display-graphic-p))
               (format "NeoTree loaded: %s" (featurep 'neotree))
               (format "NeoTree visible: %s" (my-neotree-visible-p))
               (format "Current directory: %s" default-directory)
               (format "Buffer file: %s" (or buffer-file-name "none"))
               (format "Detected root: %s" (my-neotree-find-root default-directory)))))
    (message "=== NeoTree Debug ===\n%s" (string-join info "\n"))))

(defun my-neotree-reset ()
  "Reset NeoTree state and reopen with fresh configuration."
  (interactive)
  (when (my-neotree-visible-p)
    (neotree-hide))
  ;; Clear any internal state if needed
  (when (boundp 'neo-buffer-name)
    (when (get-buffer neo-buffer-name)
      (kill-buffer neo-buffer-name)))
  (my-neotree-smart-root))

;;; Additional Keybindings

(with-eval-after-load 'neotree
  ;; Additional global bindings for convenience
  (global-set-key (kbd "C-x t g") #'my-neotree-goto-current-file)
  (global-set-key (kbd "C-x t R") #'my-neotree-force-refresh)
  (global-set-key (kbd "C-x t D") #'my-neotree-debug)
  (global-set-key (kbd "C-x t X") #'my-neotree-reset))

;;; Hook Integration

(defun my-neotree-maybe-refresh ()
  "Conditionally refresh NeoTree when switching files."
  (when (and (my-neotree-visible-p)
             buffer-file-name
             (not (string-match-p "/\\.git/" buffer-file-name)))
    (run-with-idle-timer 0.5 nil #'my-neotree-force-refresh)))

;; Optional: Auto-refresh on file changes (disabled by default)
;; (add-hook 'find-file-hook #'my-neotree-maybe-refresh)

;;; Directory Opening Integration

(defun my-neotree-handle-directory-open (directory)
  "Handle opening a directory by showing it in NeoTree instead of dired.
DIRECTORY is the directory path to open."
  (let ((abs-dir (expand-file-name directory)))
    (when (file-directory-p abs-dir)
      ;; Open NeoTree with the directory as root
      (my-neotree-smart-root abs-dir)
      ;; Find and open a README file if it exists
      (let ((readme-files '("README.md" "README.org" "README.txt" "README" "readme.md" "Readme.md")))
        (catch 'found
          (dolist (readme readme-files)
            (let ((readme-path (expand-file-name readme abs-dir)))
              (when (file-exists-p readme-path)
                (find-file readme-path)
                (throw 'found t))))
          ;; No README found, create an info buffer
          (let ((buffer-name (format "*%s*" (file-name-nondirectory 
                                            (directory-file-name abs-dir)))))
            (with-current-buffer (get-buffer-create buffer-name)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert (format "Directory: %s\n" abs-dir))
                (insert (make-string 50 ?=) "\n\n")
                (insert "This directory is open in NeoTree (left panel).\n\n")
                (insert "Quick commands:\n")
                (insert "  M-0 or C-x t t  - Toggle NeoTree\n")
                (insert "  C-x t g         - Go to current file in tree\n")
                (insert "  C-x t r         - Refresh tree root\n")
                (insert "  C-x C-f         - Find file\n")
                (insert "  C-x d           - Open dired for this directory\n\n")
                (insert "NeoTree Evil bindings (when in tree):\n")
                (insert "  RET/TAB         - Open file/expand folder\n")
                (insert "  SPC             - Quick preview\n")
                (insert "  q               - Hide tree\n")
                (insert "  g               - Refresh\n")
                (insert "  H               - Toggle hidden files\n")
                (insert "  C               - Create new file/folder\n")
                (insert "  D               - Delete\n")
                (insert "  r               - Rename\n")
                (special-mode))
              (switch-to-buffer (current-buffer)))))))))

;; Hook into command-line file opening
(defun my-handle-command-line-directory ()
  "Handle directories passed via command line."
  (when command-line-args-left
    (let ((first-arg (car command-line-args-left)))
      (when (and first-arg 
                 (file-exists-p first-arg)
                 (file-directory-p first-arg))
        ;; Remove the directory from command-line-args-left to prevent default handling
        (setq command-line-args-left (cdr command-line-args-left))
        ;; Handle it with our function
        (my-neotree-handle-directory-open first-arg)
        ;; Prevent further processing
        t))))

;; Add to command-line-functions early to intercept directory arguments
(add-hook 'command-line-functions #'my-handle-command-line-directory)

;; Also handle interactive directory opening
(defun my-dired-neotree-advice (orig-fun dirname &optional switches)
  "Advice for dired to optionally use NeoTree instead.
ORIG-FUN is the original dired function.
DIRNAME is the directory name.
SWITCHES are dired switches."
  (if (and (called-interactively-p 'interactive)
           (y-or-n-p "Open with NeoTree instead of dired? "))
      (my-neotree-handle-directory-open dirname)
    (funcall orig-fun dirname switches)))

;; Uncomment to enable dired interception
;; (advice-add 'dired :around #'my-dired-neotree-advice)

;; Alternative command to explicitly open directory with NeoTree
(defun my-neotree-open-directory (directory)
  "Open DIRECTORY with NeoTree explicitly."
  (interactive "DDirectory: ")
  (my-neotree-handle-directory-open directory))

;;; Initialization

(with-eval-after-load 'neotree
  (message "NeoTree configuration loaded successfully"))

;; Add keybindings
(global-set-key (kbd "C-x t o") #'my-neotree-open-directory)

(provide 'file-browser)

;;; file-browser.el ends here