
;; count todo items and auto update on change
(setq org-hierarchical-todo-statistics nil)
(add-hook 'org-after-todo-statistics-hook 'org-update-parent-todo-statistics)
