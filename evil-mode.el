;; evil-mode.el - Vim emulation for Emacs

;; Suppress compilation warnings for third-party packages
(setq byte-compile-warnings '(not docstrings obsolete))

;; Evil mode configuration
(use-package evil
  :ensure t
  :init
  ;; Pre-load configuration
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  ;; Enable evil mode
  (evil-mode 1)
  
  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; Set initial state for certain modes
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Evil collection for better integration with Emacs packages
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Undo tree for better undo/redo functionality
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history/"))))

;; Evil commentary for easy commenting
(use-package evil-commentary
  :after evil
  :ensure t
  :config
  (evil-commentary-mode))

;; Evil surround for manipulating surrounding characters
(use-package evil-surround
  :after evil
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Evil matchit for better matching of parentheses, brackets, etc.
(use-package evil-matchit
  :after evil
  :ensure t
  :config
  (global-evil-matchit-mode 1))

;; Custom key bindings
(with-eval-after-load 'evil
  ;; Use escape to quit minibuffers
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
  
  ;; Make C-g work like escape in evil modes
  (define-key evil-normal-state-map (kbd "C-g") 'evil-escape)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-escape)
  (define-key evil-visual-state-map (kbd "C-g") 'evil-escape))

;; Make sure Evil works well with org-mode
(with-eval-after-load 'org
  ;; Don't let evil override org-mode's tab behavior
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "S-TAB") 'org-shifttab)
  
  ;; Better movement in org-mode
  (evil-define-key 'normal org-mode-map (kbd "g h") 'outline-up-heading)
  (evil-define-key 'normal org-mode-map (kbd "g j") 'outline-forward-same-level)
  (evil-define-key 'normal org-mode-map (kbd "g k") 'outline-backward-same-level)
  (evil-define-key 'normal org-mode-map (kbd "g l") 'outline-next-visible-heading))

(message "Evil mode configuration loaded")
