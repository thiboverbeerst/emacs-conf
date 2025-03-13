(require 'xml)

;; Cache for workspace roots
(defvar my-workspace-root-cache (make-hash-table :test 'equal)
  "Cache mapping of directory paths to their workspace-root.")

(defun my-find-workspace-root (dir)
  "Search upward from DIR for a directory containing .assets/workspace.xml.
Return NIL if no workspace is found."
  (let ((abs-dir (expand-file-name dir)))
    (or (gethash abs-dir my-workspace-root-cache)
        (let ((current abs-dir) found)
          (while (and current (not found))
            (if (file-exists-p (expand-file-name ".assets/workspace.xml" current))
                (setq found current)
              (let ((parent (file-name-directory (directory-file-name current))))
                (setq current (if (or (null parent) (equal current parent))
                                  nil
                                parent)))))
          (puthash abs-dir found my-workspace-root-cache)
          found))))
