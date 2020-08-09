;;; monokai-pro-theme.el --- A simple theme based on the Monokai Pro Sublime color schemes

;; Copyright (C) 2019-2020  Kaleb Elwert

;; Author: Kaleb Elwert <belak@coded.io>
;; Maintainer: Kaleb Elwert <belak@coded.io>
;; Version: 0.1
;; Package-Version: 20200525.1430
;; Package-Commit: d1bc669200bf5753cf1963e5e65269e0d60648d5
;; URL: https://github.com/belak/emacs-monokai-pro-theme

;;; Commentary:

;; This theme is a simple theme based on the monokai-pro colors.

;;; Code:

(defvar monokai-pro-theme-colors
  '(;; Background and foreground colors
    :bg     "#2d2a2e"
    :bg+1   "#353236"
    :bg+2   "#403e41"
    :fg-4   "#5b595c"
    :fg-3   "#727072"
    :fg-2   "#939293"
    :fg-1   "#c1c0c0"
    :fg     "#fcfcfa"

    ;; General colors
    :white  "#ffffff"
    :red    "#ff6188"
    :orange "#fc9867"
    :yellow "#ffd866"
    :green  "#a9dc76"
    :blue   "#78dce8"
    :purple "#ab9df2"
    :pink   "#ff6188"

    ;; Colors from the original Monokai colorschemes. Some of these are used
    ;; rarely as highlight colors. They should be avoided if possible.
    :orig-red     "#f92672"
    :orig-orange  "#fd971f"
    :orig-yellow  "#e6db74"
    :orig-green   "#a6e22e"
    :orig-cyan    "#a1efe4"
    :orig-blue    "#66d9ef"
    :orig-violet  "#ae81ff"
    :orig-magenta "#fd5ff0"))

(defun monokai-pro-theme-transform-spec (spec colors)
  "Transform a theme `SPEC' into a face spec using `COLORS'."
  (let ((output))
    (while spec
      (let* ((key       (car  spec))
             (value     (cadr spec))
             (color-key (if (symbolp value) (intern (concat ":" (symbol-name value))) nil))
             (color     (plist-get colors color-key)))

        ;; Prepend the transformed element
        (cond
         ((and (memq key '(:box :underline)) (listp value))
          (push (monokai-pro-theme-transform-spec value colors) output))
         (color
          (push color output))
         (t
          (push value output)))

      ;; Prepend the actual key
      (push key output)

      ;; Go to the next element in the list
      (setq spec (cddr spec))))

    ;; Return the transformed spec
    output))

(defun monokai-pro-theme-transform-face (spec colors)
  "Transform a face `SPEC' into an Emacs theme face definition using `COLORS'."
  (let* ((face             (car spec))
         (definition       (cdr spec)))

    (list face `((t ,(monokai-pro-theme-transform-spec definition colors))))))

(defun monokai-pro-theme-set-faces (theme-name colors faces)
  "Define the important part of `THEME-NAME' using `COLORS' to map the `FACES' to actual colors."
  (apply 'custom-theme-set-faces theme-name
         (mapcar #'(lambda (face)
                     (monokai-pro-theme-transform-face face colors))
                 faces)))

(deftheme monokai-pro)
(monokai-pro-theme-set-faces
 'monokai-pro
 monokai-pro-theme-colors

 '(
;;; Built-in

;;;; basic colors
   ;;(border                                       :background bg+2)
   (cursor                                       :background fg)
   (default                                      :foreground fg :background bg)

   ;; TODO: bg matches what's in the sublime theme here, not bg+2
   (fringe                                       :background bg+2)

   ;;(gui-element                                  :background bg+1)
   (header-line                                  :background nil :inherit mode-line)

   ;; TODO: This matches highlight and findHighlight, but we may want
   ;; to look at findHighlightForeground which is simply bg.
   (highlight                                    :foreground fg-3 :background bg+1)

   (link                                         :foreground blue :underline t)
   (link-visited                                 :foreground purple :underline t)

   (minibuffer-prompt                            :foreground fg)
   (region                                       :background bg+2)
   (secondary-selection                          :background bg+2)
   (trailing-whitespace                          :foreground fg :background red)
   (whitespace-trailing                          :inherit trailing-whitespace)
   (widget-button                                :underline t)
   (widget-field                                 :background fg-1 :box (:line-width 1 :color bg+2))

   (error                                        :foreground red    :weight bold)
   (warning                                      :foreground orange :weight bold)
   (success                                      :foreground green  :weight bold)

;;;; font-lock
   (font-lock-builtin-face                       :foreground purple)
   (font-lock-comment-delimiter-face             :foreground fg-3)
   (font-lock-comment-face                       :foreground fg-3 :slant italic)
   (font-lock-constant-face                      :foreground purple)
   (font-lock-doc-face                           :foreground fg-3)
   (font-lock-doc-string-face                    :foreground fg-3)
   (font-lock-function-name-face                 :foreground green)
   (font-lock-keyword-face                       :foreground pink)
   ;;(font-lock-negation-char-face                 :foreground fg-1)
   ;;(font-lock-preprocessor-face                  :foreground fg-1)
   ;;(font-lock-regexp-grouping-backslash          :foreground fg-1)
   ;;(font-lock-regexp-grouping-construct          :foreground fg)
   (font-lock-string-face                        :foreground yellow)
   (font-lock-type-face                          :foreground blue)
   (font-lock-variable-name-face                 :foreground fg)
   (font-lock-warning-face                       :foreground orange)

;;;; isearch
   (match                                        :foreground yellow :background bg :inverse-video t)

   ;; TODO: Revisit this - doesn't seem to map properly onto tmThemes
   (isearch                                      :foreground bg :background yellow :weight bold)
   (lazy-highlight                               :foreground fg-1 :inverse-video t)
   (isearch-fail                                 :foreground red :background fg)

;;;; line-numbers
   (line-number                                  :foreground fg-2)
   (line-number-current-line                     :foreground fg :background bg+2)

;;;; linum-mode
   (linum                                        :foreground fg-3 :inherit fringe)
   (linum-highlight-face                         :foreground bg+2 :background fg-2)

;;;; mode-line
   (mode-line                                    :foreground fg-2 :background bg+1)
   (mode-line-buffer-id                          :foreground yellow :background nil)
   (mode-line-emphasis                           :foreground fg-1)
   (mode-line-highlight                          :foreground fg :box nil :weight bold)
   (mode-line-inactive                           :foreground fg-2 :background bg+2)

;;; Third-party

;;;; anzu-mode
   ;;    (anzu-mode-line                               :foreground yellow)

;;;; company-mode
   ;; TODO: These don't feel quite right
   (company-tooltip                              :background bg+2 :inherit default)
   (company-scrollbar-bg                         :background bg+1)
   (company-scrollbar-fg                         :background fg-1)
   (company-tooltip-annotation                   :foreground red)
   (company-tooltip-common                       :foreground yellow)
   (company-tooltip-selection                    :background bg+1)
   (company-preview-common                       :foreground blue :background bg+2)

;;;; diff-hl-mode
   (diff-hl-change                               :foreground blue)
   (diff-hl-delete                               :foreground red)
   (diff-hl-insert                               :foreground green)

;;;; diff-mode
   (diff-added                                   :foreground green)
   (diff-changed                                 :foreground purple)
   (diff-removed                                 :foreground red)
   (diff-header                                  :background bg)
   (diff-file-header                             :background bg+1)
   (diff-hunk-header                             :foreground pink :background bg)

;;;; flycheck-mode
   (flycheck-error                               :underline (:style wave :color red))
   (flycheck-info                                :underline (:style wave :color yellow))
   (flycheck-warning                             :underline (:style wave :color orange))

;;;; flyspell-mode
   (flyspell-duplicate                           :underline (:style wave :color orange))
   (flyspell-incorrect                           :underline (:style wave :color red))

;;;; git-gutter-mode
   (git-gutter:added                             :foreground green)
   (git-gutter:deleted                           :foreground red)
   (git-gutter:modified                          :foreground purple)
   (git-gutter:separator                         :foreground blue)
   (git-gutter:unchanged                         :background yellow)

;;;; hl-line-mode
   (hl-line                                      :background bg+1)

;;;; hl-todo-mode
   (hl-todo                                      :slant italic :weight bold)

;;;; ido-mode
   ;; TODO: These don't feel quite right
   (ido-subdir                                   :foreground fg-2)
   (ido-first-match                              :foreground orange)
   (ido-only-match                               :foreground green)
   (ido-indicator                                :foreground red :background bg+2)
   (ido-virtual                                  :foreground fg-2)

;;;; ido-vertical-mode
   (ido-vertical-match-face                      :foreground fg-1)

;;; org-mode
   (org-level-1                                  :foreground orange)
   (org-level-2                                  :foreground green)
   (org-level-3                                  :foreground blue)
   (org-level-4                                  :foreground yellow)
   (org-level-5                                  :foreground orig-cyan)
   (org-level-6                                  :foreground green)
   (org-level-7                                  :foreground red)
   (org-level-8                                  :foreground blue)

;;;; show-paren-mode
   (show-paren-match                             :foreground fg :background blue)
   (show-paren-mismatch                          :background red :inverse-video t)

;;;; selectrum
   (selectrum-current-candidate                  :foreground orange)
   (selectrum-completion-annotation              :foreground blue)

   ))

;; Anything leftover that doesn't fall neatly into a face goes here.
(let ((bg      (plist-get monokai-pro-theme-colors :bg))
      (fg      (plist-get monokai-pro-theme-colors :fg))
      (red     (plist-get monokai-pro-theme-colors :red))
      (green   (plist-get monokai-pro-theme-colors :green))
      (yellow  (plist-get monokai-pro-theme-colors :yellow))
      (blue    (plist-get monokai-pro-theme-colors :blue))
      (magenta (plist-get monokai-pro-theme-colors :purple))
      (cyan    (plist-get monokai-pro-theme-colors :orig-cyan)))
  (custom-theme-set-variables
   'monokai-pro
   `(ansi-color-names-vector
     ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
     [,bg ,red ,green ,yellow ,blue ,magenta ,cyan ,fg])
   `(ansi-term-color-vector
     ;; black, base08, base0B, base0A, base0D, magenta, cyan, white
     [unspecified ,bg ,red ,green ,yellow ,blue ,magenta ,cyan ,fg])))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'monokai-pro)

(provide 'monokai-pro-theme)

;;; monokai-pro-theme.el ends here
