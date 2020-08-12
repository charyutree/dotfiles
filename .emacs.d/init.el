;; -*- mode: elisp -*-

;; enable melpa stable
(require 'package)
;; Internet repositories for new packages.
(setq package-archives '(("org"       . "https://orgmode.org/elpa/")
                         ("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ;; Maintainer is AWOL.
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ))
(package-initialize)

;; Unless it's already installed, update the packages archives,
;; then install the most recent version of “use-package”.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable Transient mark mode
(transient-mark-mode 1)

;;add custom theme path
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;;;;Org mode configuration
;; Enable Org mode
;(require 'org)
(use-package org :ensure org-plus-contrib)
;; Make Org mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen
(require 'org-habit)
(set 'org-habit-graph-column 60)


;; Agenda file list
(setq org-agenda-files '("~/Dropbox/org-files/"))

;; Custom org todo keywords 
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "SITE(s)")
              (sequence "DRAFTING(w@/!)" "HOLD(h@/!)" "CHECKING(c@/!)" "FEEDBACK(f@/!)" "|" "MEETING"))))

(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("SITE" :foreground "blue" :weight bold)	     
              ("DRAFTING" :foreground "orange" :weight bold)
              ("Hold" :foreground "magenta" :weight bold)
              ("CHECKING" :foreground "orange" :weight bold)
              ("FEEDBACK" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

;; Associate .xls, .xlsx, .doc, .docx files with system defaults in org mode
(add-to-list 'org-file-apps '("\\.xls\\'" . default))
(add-to-list 'org-file-apps '("\\.xlsx\\'" . default))
(add-to-list 'org-file-apps '("\\.doc\\'" . default))
(add-to-list 'org-file-apps '("\\.docx\\'" . default))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#ffffff" "#f36c60" "#8bc34a" "#fff59d" "#4dd0e1" "#b39ddb" "#81d4fa" "#263238"))
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#a1efe4" "#fcfcfa"])
 '(beacon-color "#c82829")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#689d6a")
 '(cua-normal-cursor-color "#a89984")
 '(cua-overwrite-cursor-color "#d79921")
 '(cua-read-only-cursor-color "#98971a")
 '(custom-enabled-themes (quote (gruvbox-dark-hard)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "5846b39f2171d620c45ee31409350c1ccaddebd3f88ac19894ae15db9ef23035" "a37d20710ab581792b7c9f8a075fcbb775d4ffa6c8bce9137c84951b1b453016" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" "b89ae2d35d2e18e4286c8be8aaecb41022c1a306070f64a66fd114310ade88aa" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "cb8d0e6e2dbf542e29a2485526b7f1c474f8587d96daade6163273a90d34d928" "342566c9967d82f479cf8ca929ba7d23263ec67a334198d0cd8babec00a2e09b" default)))
 '(elpy-shell-echo-input nil)
 '(fci-rule-color "#37474f")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote light))
 '(highlight-changes-colors (quote ("#d3869b" "#b16286")))
 '(highlight-symbol-colors
   (quote
    ("#522a41fa2b3b" "#3821432637ec" "#5bbe348b2bf5" "#483d36c73def" "#43c0418329b9" "#538f36232679" "#317a3ddc3e5d")))
 '(highlight-symbol-foreground-color "#bdae93")
 '(highlight-tail-colors
   (quote
    (("#32302f" . 0)
     ("#747400" . 20)
     ("#2e7d33" . 30)
     ("#14676b" . 50)
     ("#a76e00" . 60)
     ("#a53600" . 70)
     ("#9f4d64" . 85)
     ("#32302f" . 100))))
 '(hl-bg-colors
   (quote
    ("#a76e00" "#a53600" "#b21b0a" "#9f4d64" "#8b2a58" "#14676b" "#2e7d33" "#747400")))
 '(hl-fg-colors
   (quote
    ("#282828" "#282828" "#282828" "#282828" "#282828" "#282828" "#282828" "#282828")))
 '(hl-paren-colors (quote ("#689d6a" "#d79921" "#458588" "#b16286" "#98971a")))
 '(hl-sexp-background-color "#efebe9")
 '(lsp-ui-doc-border "#bdae93")
 '(nrepl-message-colors
   (quote
    ("#fb4933" "#d65d0e" "#d79921" "#747400" "#b9b340" "#14676b" "#689d6a" "#d3869b" "#b16286")))
 '(org-agenda-files
   (quote
    ("~/Dropbox/org-files/master.org" "~/Dropbox/org-files/personal.org")))
 '(org-agenda-window-setup (quote current-window))
 '(org-contacts-files (quote ("~/Dropbox/org-files/contacts.org")))
 '(package-selected-packages
   (quote
    (color-theme-sanityinc-tomorrow monokai-pro-theme hc-zenburn-theme gruvbox-theme solarized-theme org-super-agenda powershell orgtbl-ascii-plot jedi use-package helm magit pyenv-mode material-theme exec-path-from-shell elpy better-defaults)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#32302f")
 '(pos-tip-foreground-color "#bdae93")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#98971a" "#32302f" 0.2))
 '(term-default-bg-color "#282828")
 '(term-default-fg-color "#a89984")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#282828" "#32302f" "#b21b0a" "#fb4933" "#747400" "#98971a" "#a76e00" "#d79921" "#14676b" "#458588" "#9f4d64" "#d3869b" "#2e7d33" "#689d6a" "#a89984" "#282828")))
 '(window-divider-mode nil)
 '(xterm-color-names
   ["#32302f" "#fb4933" "#98971a" "#d79921" "#458588" "#d3869b" "#689d6a" "#a89984"])
 '(xterm-color-names-bright
   ["#282828" "#d65d0e" "#7c6f64" "#282828" "#a89984" "#b16286" "#bdae93" "#fbf1c7"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Follow links in same window
(setq org-link-frame-setup '((file . find-file)))

;; enable org indent mode
(setq org-startup-indented t)
(setq org-hide-leading-stars t)

;; set desktop save location
(desktop-change-dir "~/.emacs.d/")

;; add git executable path for magit
(add-to-list 'exec-path "C:/Users/RossB/Downloads/cmder/vendor/git-for-windows/mingw64/libexec/git-core")

(use-package org-contacts
  :ensure nil
  :after org
  :custom (org-contacts-files '("~/Dropbox/org-files/contacts.org")))

;; add capture template
(setq org-capture-templates
      '
	(("n" "Project Refiling" entry (file+olp "~/Dropbox/org-files/master.org" "Work" "LOA" "Refiling")
	 "* TODO %^{Job Number|xx-xxx} // %^{Description} 
:PROPERTIES:
:ADDED: %U
:CLIENT: %^{Client|NA}
:END: 
\n
- %?
" :empty-lines 0)
	 ("e" "Email Entry" entry (file+olp "~/Dropbox/org-files/master.org" "Work" "LOA" "Emails")
	 "* TODO %x \n  %U" :empty-lines 0)
	("r" "Reading" entry (file+olp "~/Dropbox/org-files/personal.org" "Misc" "Books")
         "* %^{Title}
:PROPERTIES: 
:AUTHOR: %^{Author}
:RATING: %^{Rating}
:END:")
	("c" "Contacts" entry (file "~/Dropbox/org-files/contacts.org")
         "* %(org-contacts-template-name)
 :PROPERTIES:
 :ORGANISATION: %^{Organisation}
 :PHONE: %^{Work/Home Phone}
 :MOBILE: %^{Mobile}
 :EMAIL: %(org-contacts-template-email)
 :END:")
	("s" "Shopping List" checkitem (file+headline "~/Dropbox/org-files/shopping.org" "Main") " [ ] %?\n\n" :append t :kill-buffer t)

	("b" "Bill" entry (file+olp "~/Dropbox/org-files/personal.org" "Tasks" "Bills")
	 "* TODO %^{Description} \n - %?")

	("p" "Personal Refiling" entry (file+olp "~/Dropbox/org-files/master.org" "Work" "LOA" "Refiling") "* TODO %^{Description} \n - %?")
	
))
	

;; keybinding for capture mode 
(define-key global-map "\C-cc" 'org-capture)

;; keybinding for org-agenda
(define-key global-map "\C-ca" 'org-agenda)

;; Allow refiling 3 layers deep
(setq org-refile-targets '((org-agenda-files :maxlevel . 4)))

;; Enable helm and configure
(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)

(helm-mode 1)

;; Enable note and timestamp when TODO item closed
(setq org-log-done 'note)

;; Install and enable elpy (Python IDE)
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

;; Disable Elpy indentation highlighting
(add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))

;; Install and enable Jedi (Python autocomplete)
(use-package jedi
:ensure t
:init
(add-hook 'python-mode-hook 'jedi:setup)
(add-hook 'python-mode-hook 'jedi:ac-setup))


(put 'upcase-region 'disabled nil)

;; Enable elisp outlook script
(add-to-list 'load-path "~/.emacs.d/elisp/")
(require 'org-outlook)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;;define function to unfold document to 4 levels
(defun unfold-4()
  (interactive)
  (outline-hide-sublevels 4))

(defun unfold-3()
  (interactive)
  (outline-hide-sublevels 3))

;;bind unfold-4 to C-c e
(global-set-key (kbd "C-c e") 'unfold-4)
(global-set-key (kbd "C-c w") 'unfold-3)

;;Super-agenda
(use-package org-super-agenda
  :ensure t )

(setq org-agenda-custom-commands
      '(("z" "Custom  view"
         ((agenda "" ((org-agenda-span 'day)
		      (org-deadline-warning-days 90)
                      (org-super-agenda-groups
                       '((:log t)
			 (:name "Scheduled Today"
                                :time-grid t
                                :and(:scheduled today :not(:deadline today))
                                )
			 (:name "Due Today"
				:and(
				     :not(:tag "Bills" :tag "Reminders" :todo "HOLD" :todo "FEEDBACK")
				:deadline today
				))
			 (:name "Scheduled Inspections"
				  :tag "Inspections"
				  :order 5)
			 (:name "Overdue"
			        :and(
				     :not(:tag "Bills" :tag "Reminders" :todo "HOLD" :todo "FEEDBACK")
				:deadline past
			        )
				:and(
				     :not(:tag "Bills" :tag "Reminders" :todo "HOLD" :todo "FEEDBACK")
				:scheduled past))
			 (:name "On Hold / Waiting for Feedback"
				:todo "HOLD"
				:todo "FEEDBACK")
			   (:name "Upcoming Deadlines"
                                  :and(
				       :deadline future
						 :not(:tag "Bills" :todo "HOLD" :todo "FEEDBACK")   ))
			   (:name "Bills/Reminders"
				  :and(
				       :deadline t
						 :tag ("Bills"))

				  :and(
				       :deadline t
						 :tag ("Reminders"))
				  
				  )))))
			 
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(
			  (:name "Jobs for Checking"
				 :and(
				      :not(:scheduled t :deadline t)
				  :todo "CHECKING"
				  ))
			  (:name "Jobs being Drafted"
				 :and(
				      :not(:scheduled t :deadline t)
					  :todo "DRAFTING"))
			  (:name "Jobs on Hold"
				 :and(
				      :not(:scheduled t :deadline t)
					  :todo "HOLD"))
			  (:name "Unscheduled Work"
				 :and(
				      :not(:scheduled t :deadline t :tag "Admin" :tag "Reminders")

				  :todo t
				  
				  ))
			  (:name "Admin"
				 :tag "Admin")
			  (:discard (:anything t))
				     
				 ))))))))

(setq org-agenda-start-with-log-mode t)
(setq org-agenda-block-separator nil)
(setq org-agenda-compact-blocks t)
(org-super-agenda-mode) 
