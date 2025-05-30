;; Font configuration - Academic/Nostalgic fonts
;; You can customize these settings

;; CLASSIC ACADEMIC FONTS (choose one):

;; 1. Computer Modern (LaTeX default) - most academic/nostalgic
(add-to-list 'default-frame-alist '(font . "Latin Modern Mono-11"))

;; 2. Times-style academic serif (for variable-pitch text)
;; (set-face-attribute 'variable-pitch nil :font "Liberation Serif-12")

;; 3. Classic monospace fonts
;; (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11"))
;; (add-to-list 'default-frame-alist '(font . "Liberation Mono-11"))
;; (add-to-list 'default-frame-alist '(font . "Courier New-11"))

;; MIXED SETUP (recommended for academic work):
;; Monospace for code, serif for text
(set-face-attribute 'fixed-pitch nil :font "Latin Modern Mono-11")
(set-face-attribute 'variable-pitch nil :font "DejaVu Serif-12")

;; For org-mode: use serif fonts for prose, mono for code blocks
(add-hook 'org-mode-hook 'variable-pitch-mode)

;; Optional: Configure font fallbacks
;; (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)
