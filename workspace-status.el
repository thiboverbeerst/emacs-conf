(require 'workspace)

;; Registry voor workspace services
(defvar my-workspace-services '()
  "List of registered workspace services with their status functions.")

(defun my-workspace-register-service (name status-fn)
  "Register a workspace service with NAME and STATUS-FN.
STATUS-FN should return nil if inactive, or a string/plist with status info."
  (setq my-workspace-services 
        (cons (cons name status-fn)
              (assq-delete-all name my-workspace-services))))

(defun my-workspace-get-status ()
  "Get status of all workspace services."
  (let ((ws-root (my-find-workspace-root default-directory))
        (services-status '()))
    (if ws-root
        (progn
          (push (cons 'workspace ws-root) services-status)
          (dolist (service my-workspace-services)
            (let* ((name (car service))
                   (status-fn (cdr service))
                   (status (funcall status-fn)))
              (when status
                (push (cons name status) services-status))))
          services-status)
      '((workspace . nil)))))

(defun my-workspace-status-string ()
  "Return a compact string showing active workspace services."
  (let ((status (my-workspace-get-status))
        (active-services '()))
    (dolist (item status)
      (let ((name (car item))
            (value (cdr item)))
        (cond
         ((eq name 'workspace)
          (when value
            (push (format "WS:%s" (file-name-nondirectory value)) active-services)))
         (value
          (push (format "%s" name) active-services)))))
    (if active-services
        (format "[%s]" (string-join (reverse active-services) " "))
      "[no workspace]")))

;; Modeline integration
(defvar my-workspace-modeline-format
  '(:eval (my-workspace-status-string))
  "Modeline format for workspace status.")

(defun my-workspace-modeline-enable ()
  "Add workspace status to modeline."
  (unless (member my-workspace-modeline-format mode-line-format)
    (setq mode-line-format
          (append mode-line-format (list " " my-workspace-modeline-format)))))

(defun my-workspace-modeline-disable ()
  "Remove workspace status from modeline."
  (setq mode-line-format
        (remove my-workspace-modeline-format mode-line-format)))

;; Interactive commands
(defun my-workspace-show-status ()
  "Show detailed workspace status in minibuffer."
  (interactive)
  (let ((status (my-workspace-get-status)))
    (if (assq 'workspace status)
        (let ((ws-root (cdr (assq 'workspace status)))
              (active-services (remove 'workspace (mapcar #'car status))))
          (if ws-root
              (message "Workspace: %s | Active services: %s"
                      ws-root
                      (if active-services
                          (string-join (mapcar #'symbol-name active-services) ", ")
                        "none"))
            (message "No workspace active")))
      (message "No workspace detected"))))

(defun my-workspace-toggle-modeline ()
  "Toggle workspace status in modeline."
  (interactive)
  (if (member my-workspace-modeline-format mode-line-format)
      (progn
        (my-workspace-modeline-disable)
        (message "Workspace modeline disabled"))
    (progn
      (my-workspace-modeline-enable)
      (message "Workspace modeline enabled"))))

(defun my-workspace-list-services ()
  "List all registered workspace services."
  (interactive)
  (if my-workspace-services
      (message "Registered services: %s" 
               (string-join (mapcar (lambda (service) (symbol-name (car service))) 
                                   my-workspace-services) ", "))
    (message "No services registered")))

;; Enable modeline by default
(my-workspace-modeline-enable)

(provide 'workspace-status)