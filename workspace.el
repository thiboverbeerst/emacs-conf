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

;; Workspace XML helpers
(defvar my-workspace-xml-cache (make-hash-table :test 'equal)
  "Cache for parsed workspace.xml data per workspace root.")

(defun my-workspace-xml-parse (xml-file)
  "Parse workspace.xml and cache the result per workspace root."
  (let ((abs-file (expand-file-name xml-file)))
    (or (gethash abs-file my-workspace-xml-cache)
        (condition-case err
            (let ((parsed (with-temp-buffer
                            (insert-file-contents abs-file)
                            (car (xml-parse-region (point-min) (point-max))))))
              (puthash abs-file parsed my-workspace-xml-cache)
              parsed)
          (error (message "[workspace] ERROR parsing %s: %s" abs-file err)
                 nil)))))

(defun my-workspace-xml-find-by-id (xml-root tag id)
  "Recursively search XML-ROOT for a node with TAG and id=ID."
  (let ((result nil))
    (cl-labels ((search (node)
                  (when (and (listp node)
                             (eq (car node) tag)
                             (equal (xml-get-attribute node 'id) id))
                    (setq result node))
                  (dolist (child (xml-node-children node))
                    (when (listp child) (search child)))))
      (search xml-root))
    result))

(defun my-workspace-xml-get-property (node property)
  "Get PROPERTY (as symbol) from NODE's children."
  (let ((child (car (xml-get-children node property))))
    (when child
      (car (xml-node-children child)))))

(defun my-workspace-get-location-target-by-id (ws-xml id)
  "Return the <target> value for a <path> with given ID in <locations>."
  (let* ((structure (car (xml-get-children ws-xml 'structure)))
         (locations (car (xml-get-children structure 'locations)))
         (paths (xml-get-children locations 'path))
         (node (seq-find (lambda (p) (equal (xml-get-attribute p 'id) id)) paths)))
    (when node (my-workspace-xml-get-property node 'target))))

;; New high-level convenience functions
(defun my-workspace-get-location-path (workspace-root location-id)
  "Get absolute path for LOCATION-ID in WORKSPACE-ROOT."
  (let* ((xml-file (expand-file-name ".assets/workspace.xml" workspace-root))
         (ws-xml (my-workspace-xml-parse xml-file))
         (target (my-workspace-get-location-target-by-id ws-xml location-id)))
    (when (and target (not (string-empty-p target)))
      (expand-file-name (string-trim target) workspace-root))))

(defun my-workspace-get-package-property (workspace-root package-id property-name)
  "Get PROPERTY-NAME value for PACKAGE-ID in WORKSPACE-ROOT.
Returns absolute path if the property value is a relative path, otherwise returns the raw value."
  (let* ((xml-file (expand-file-name ".assets/workspace.xml" workspace-root))
         (ws-xml (my-workspace-xml-parse xml-file))
         (configuration (car (xml-get-children ws-xml 'configuration)))
         (tools (car (xml-get-children configuration 'tools)))
         (editor (seq-find (lambda (e) (equal (xml-get-attribute e 'id) "emacs"))
                          (xml-get-children tools 'editor)))
         (packages (car (xml-get-children editor 'packages)))
         (package (seq-find (lambda (p) (equal (xml-get-attribute p 'id) package-id))
                           (xml-get-children packages 'package)))
         (prop-value (when package (my-workspace-xml-get-property package (intern property-name)))))
    (when (and prop-value (not (string-empty-p prop-value)))
      (let ((trimmed-value (string-trim prop-value)))
        (if (file-name-absolute-p trimmed-value)
            trimmed-value
          (expand-file-name trimmed-value workspace-root))))))

(provide 'workspace)