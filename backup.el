(require 'xml)  ;; Safe to require; workspace.el has already been loaded

;; Cache for backup configuration per workspace
(defvar my-workspace-backup-config-cache (make-hash-table :test 'equal)
  "Cache mapping of workspace roots to the backupDir configuration.")

(defun my-read-backup-dir-from-workspace (workspace-root)
  "Read the backupDir value from the workspace config (.assets/workspace.xml) in WORKSPACE-ROOT.
Returns NIL on error."
  (or (gethash workspace-root my-workspace-backup-config-cache)
      (let* ((xml-file (expand-file-name ".assets/workspace.xml" workspace-root)))
        (if (file-exists-p xml-file)
            (let* ((xml-data (condition-case nil
                                 (xml-parse-file xml-file)
                               (error nil))))
              (if xml-data
                  (let* ((ws (car xml-data))
                         (tools (car (xml-get-children ws 'tools)))
                         (emacs (car (xml-get-children tools 'emacs)))
                         (backup (car (xml-get-children emacs 'backupDir))))
                    (when backup
                      (let ((backup-str (string-trim (car (xml-node-children backup)))))
                        (puthash workspace-root backup-str my-workspace-backup-config-cache)
                        backup-str)))
                nil))
          nil))))

(defun my-default-backup-file-name (file)
  "Fallback default backup file name function."
  (concat file "~"))

(defun my-workspace-backup-file-name (file)
  "Determine the backup file name for FILE.
If FILE is inside a workspace, use the backupDir from the workspace config relative to FILE's directory.
Otherwise, use the default backup method."
  (let* ((file-dir (file-name-directory file))
         (workspace-root (my-find-workspace-root file-dir)))
    (if workspace-root
        (let* ((backup-rel (or (my-read-backup-dir-from-workspace workspace-root)
                               ".assets/emacs/backups"))
               (backup-dir (expand-file-name backup-rel file-dir)))
          (unless (file-directory-p backup-dir)
            (condition-case err
                (make-directory backup-dir t)
              (error (message "[CUSTOM ERROR] Unable to create backup directory: %s" err))))
          (expand-file-name (concat (file-name-nondirectory file) "~") backup-dir))
      (my-default-backup-file-name file))))

(setq make-backup-file-name-function 'my-workspace-backup-file-name)
(setq make-backup-files t)
