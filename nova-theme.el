;; Copyright 2018 Muir Manders.  All rights reserved.
;; Use of this source code is governed by a BSD-style
;; license that can be found in the LICENSE file.

(require 'color)

(deftheme nova
  "A dark theme using Trevord Miller's Nova color scheme.
See <https://trevordmiller.com/projects/nova>.")

(defvar nova-base-colors
  '((cyan "#7FC1CA")
    (blue "#83AFE5")
    (purple "#9A93E1")
    (pink "#D18EC2")
    (red "#DF8C8C")
    (orange "#F2C38F")
    (yellow "#DADA93")
    (green "#A8CE93")
    (gray0 "#1E272C")
    (gray1 "#3C4C55")
    (gray2 "#556873")
    (gray3 "#6A7D89")
    (gray4 "#899BA6")
    (gray5 "#C5D4DD")
    (gray6 "#E6EEF3")

    (bg gray1)
    (fg gray5)
    (constant cyan)
    (identifier blue)
    (statement yellow)
    (type green)
    (global purple)
    (emphasis pink)
    (special orange)
    (trivial gray4)

    (user-action-needed red)
    (user-current-state cyan)
    (background-shade gray0)

    (added green)
    (modified orange)
    (removed red)
    (renamed blue)

    (light-pink (nova-lighten pink 0.5))))

(defun nova--build-face (face)
  (let ((name (car face)) (attrs (cdr face)))
    `(list ',name (list (list t ,@attrs)))))

(defmacro nova-set-faces (&rest faces)
  (declare (indent defun))
  `(nova-with-colors
     (custom-theme-set-faces
      'nova
      ,@(mapcar #'nova--build-face faces))))

(defun nova-darken (color alpha)
  (nova-blend "#000000" color alpha))

(defmacro nova-with-colors (&rest body)
  (declare (indent defun))
  `(let* ,nova-base-colors ,@body))

(defun nova-blend (c1 c2 a)
  (apply
   'color-rgb-to-hex
   (mapcar*
    (lambda (c1 c2) (+ (* a c1) (* (- 1 a) c2)))
    (color-name-to-rgb c1)
    (color-name-to-rgb c2))))

(defun nova-lighten (color alpha)
  (nova-blend "#FFFFFF" color alpha))

(nova-set-faces
  ;; basic faces (faces.el)
  (default :foreground fg :background bg)
  (region :background gray3 :distant-foreground nil)
  (highlight :background user-current-state :foreground gray0)
  (cursor :background user-current-state)
  (fringe :foreground gray3)
  (success :foreground green)
  (warning :foreground yellow)
  (error :foreground user-action-needed)
  (escape-glyph :foreground special)
  (button :foreground constant :inherit 'underline)
  (minibuffer-prompt :foreground orange)
  (trailing-whitespace :background user-action-needed)
  (show-paren-match :foreground (nova-darken green 0.4) :background green)
  (show-paren-mismatch :background (nova-darken red 0.4) :foreground red)
  (header-line :background bg)
  (mode-line-inactive :box nil)
  (mode-line :box nil)
  (link :foreground cyan :underline t)

  ;; font lock faces
  (font-lock-function-name-face :foreground identifier)
  (font-lock-constant-face :foreground constant)
  (font-lock-string-face :foreground constant)
  (font-lock-keyword-face :foreground global)
  (font-lock-builtin-face :foreground global)
  (font-lock-variable-name-face :foreground identifier)
  (font-lock-type-face :foreground type)
  (font-lock-warning-face :foreground yellow)
  (font-lock-comment-face :foreground trivial)
  (font-lock-negation-char-face :foreground emphasis)

  ;; powerline faces
  (powerline-active0 :background (nova-blend blue bg 0.6) :foreground fg)
  (powerline-active1 :background (nova-blend purple bg 0.6) :foreground fg)
  (powerline-active2 :background gray3 :foreground fg)
  (powerline-inactive0 :background (nova-blend blue bg 0.3) :foreground (nova-darken fg 0.2))
  (powerline-inactive1 :background (nova-blend purple bg 0.3) :foreground (nova-darken fg 0.2))
  (powerline-inactive2 :background gray2 :foreground (nova-darken fg 0.2))

  ;; search faces
  (match :background orange :foreground gray0)
  (isearch :inherit 'match)
  (lazy-highlight :background (nova-darken orange 0.3) :foreground gray0)

  ;; ivy faces
  (ivy-current-match :background purple :foreground gray0)
  (ivy-remote :foreground yellow)
  (ivy-modified-buffer :foreground modified)
  (ivy-highlight-face :foreground emphasis)
  (ivy-match-required-face :foreground user-action-needed)
  (ivy-minibuffer-match-face-2 :background orange :foreground gray0)

  ;; magit faces
  (magit-tag :foreground yellow)
  (magit-filename :foreground purple)
  (magit-branch-current :foreground special :box t)
  (magit-branch-local :foreground cyan)
  (magit-branch-remote :foreground green)
  (magit-dimmed :foreground trivial)
  (magit-hash :foreground trivial)
  (magit-process-ng :inherit 'error)
  (magit-process-ok :inherit 'success)
  (magit-section-highlight :background gray2)
  (magit-section-heading :foreground blue)
  (magit-diff-hunk-heading :foreground gray0 :background (nova-blend purple bg 0.5))
  (magit-diff-hunk-heading-highlight :foreground gray0 :background (nova-blend purple bg 0.8))
  (magit-diff-lines-heading :background orange)
  (magit-diff-added :foreground added :background (nova-blend added bg 0.1))
  (magit-diff-added-highlight :foreground added :background (nova-blend added bg 0.2))
  (magit-diff-removed :foreground removed :background (nova-blend removed bg 0.1))
  (magit-diff-removed-highlight :foreground removed :background (nova-blend removed bg 0.2))
  (magit-diff-context :foreground trivial :background bg)
  (magit-diff-context-highlight :foreground trivial :background bg)
  (magit-diffstat-added :foreground added)
  (magit-diffstat-removed :foreground removed)
  (magit-log-author :foreground red)
  (magit-log-date :foreground blue)
  (magit-log-graph :foreground trivial)

  ;; vc faces
  (diff-context :inherit 'magit-diff-context)
  (diff-added :inherit 'magit-diff-added-highlight)
  (diff-removed :inherit 'magit-diff-removed-highlight)
  (diff-file-header :inherit 'magit-diff-file-heading)
  (diff-header :inherit 'magit-section)
  (diff-function :foreground identifier)
  (diff-hunk-header :foreground purple)

  (company-tooltip :foreground bg :background blue)
  (company-tooltip-selection :background purple)
  (company-tooltip-common :background 'unspecified)
  (company-preview-common :background purple)
  (company-scrollbar-bg :background (nova-darken blue 0.2))
  (company-scrollbar-fg :background (nova-darken blue 0.4))
  (company-template-field :background orange :foreground bg)

  (web-mode-doctype-face :foreground trivial)
  (web-mode-html-tag-face :foreground global)
  (web-mode-html-tag-bracket-face :foreground global)
  (web-mode-html-attr-name-face :foreground identifier)
  (web-mode-block-attr-name-face :foreground constant)
  (web-mode-html-entity-face :foreground cyan :inherit 'italic)
  (web-mode-block-control-face :foreground emphasis)

  (js2-warning :underline yellow)
  (js2-error :underline user-action-needed)
  (js2-function-param :foreground type)
  (js2-function-call :foreground identifier)
  (js2-jsdoc-tag :foreground trivial)
  (js2-jdsoc-type :foreground type)
  (js2-jdoc-value :foreground identifier)
  (js2-external-variable :foreground special))

(nova-with-colors
  (custom-theme-set-variables
   'nova
   `(vc-annotate-very-old-color ,(nova-darken purple 0.2))
   `(vc-annotate-color-map
     `((20 . ,,red)
       (40 . ,,(nova-blend red orange 0.8))
       (60 . ,,(nova-blend red orange 0.6))
       (80 . ,,(nova-blend red orange 0.4))
       (100 . ,,(nova-blend red orange 0.2))
       (120 . ,,orange)
       (140 . ,,(nova-blend orange yellow 0.8))
       (160 . ,,(nova-blend orange yellow 0.6))
       (180 . ,,(nova-blend orange yellow 0.4))
       (200 . ,,(nova-blend orange yellow 0.2))
       (220 . ,,yellow)
       (240 . ,,(nova-blend yellow green 0.8))
       (260 . ,,(nova-blend yellow green 0.6))
       (280 . ,,(nova-blend yellow green 0.4))
       (300 . ,,(nova-blend yellow green 0.2))
       (320 . ,,green)
       (340 . ,,(nova-blend green blue 0.8))
       (360 . ,,(nova-blend green blue 0.6))
       (380 . ,,(nova-blend green blue 0.4))
       (400 . ,,(nova-blend green blue 0.2))
       (420 . ,,blue)
       (440 . ,,(nova-blend blue purple 0.8))
       (460 . ,,(nova-blend blue purple 0.6))
       (480 . ,,(nova-blend blue purple 0.4))
       (500 . ,,(nova-blend blue purple 0.2))
       (520 . ,,purple)))))

(provide-theme 'nova)
