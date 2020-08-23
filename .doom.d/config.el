;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Brent Oar"
      user-mail-address "brentoar@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "DejaVuSansMono" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the

;; `load-theme' function. This is the default:
(setq doom-theme 'doom-vibrant)
;;(doom-themes-org-config)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org-files/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Customise org-super-agenda`
(setq org-agenda-custom-commands
      '(("z" "Custom  view"
         ((agenda "" ((org-agenda-span 'day)
		      (org-deadline-warning-days 90)
                      (org-agenda-start-day "0d")
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

;; Enable note and timestamp when TODO item closed
(setq org-log-done 'note)

;;define function to unfold document to 4 levels
(defun unfold-4()
  (interactive)
  (outline-hide-sublevels 4))

(defun unfold-3()

  (interactive)
  (outline-hide-sublevels 3))

;; Define custom functions to open Windows apps from within WSL
(defun open-xls(filepath)
  (setq cmd_prefix "/mnt/c/Windows/System32/cmd.exe"))

;; Associate .xls, .xlsx, .doc, .docx files with system defaults in org mode
(add-to-list 'org-file-apps '("\\.xls\\'" . default))
(add-to-list 'org-file-apps '("\\.xlsx\\'" . default))
(add-to-list 'org-file-apps '("\\.doc\\'" . default))
(add-to-list 'org-file-apps '("\\.docx\\'" . default))

;; Follow links in same window
(setq org-link-frame-setup '((file . find-file)))

;; Add custom keybindings for Org-Mode

(map! :map evil-org-mode-map
      :leader
      :after org
      :n "n 4" #'unfold-4
      :n "n 3" #'unfold-3
 )

;; set global line spacing
(setq line-spacing 0.2)

(defun my/org-mode-hook ()
  "Stop the org-level headers from increasing in height relative to the other text."
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5))
    (set-face-attribute face nil :weight 'normal :height 1.0)))

(add-hook 'org-mode-hook 'my/org-mode-hook)


;;add org-todo states
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "CHECKING(c)" "|" "DRAFTING(D)" "FEEDBACK(f)" "HOLD(h)"  "DONE(d)" "CANCELLED(C)")
        (sequence "PROJECT(p)" "|" "ISSUED(i)")
        (sequence "MEETING(m)" "SITE(s)" "|" "DONE(d)" )))


(setq org-todo-keyword-faces
      '(("TODO" . "firebrick" )
        ("CHECKING" . "goldenrod")
        ("DRAFTING" . "CornFlowerBlue")
        ("FEEDBACK" . "CadetBlue")
        ("HOLD" . "moccasin")
        ("DONE" . "ForestGreen")
        ("CANCELLED" . "SlateGrey")
        ("PROJECT" . "coral")
        ("ISSUED" . "ForestGreen")
        ("NEXT" . "DeepSkyBlue")
        ("MEETING" . "salmon")
        ("SITE" . "salmon")
        ))

;; Add capture template
(setq org-capture-templates
  (doct '(("LOA Work" :keys "w"
           :children (("Project" :keys "p"
                       :children (("New" :keys "n"
                                   :type entry
                                   :file "~/Dropbox/org-files/master.org"
                                   :olp ("Work" "LOA" "Active Projects / Tasks")
                                   :template ("* PROJECT %^{Job Number} // %^{Description} [%]"
                                              ":PROPERTIES:"
                                              ":Created: %U"
                                              ":CATEGORY: %\\1"
                                              ":END:"
                                              "* Notes"
                                              "%?"
                                              "* Subtasks"))
                                  ("Subtask within existing heading" :keys "t"
                                   :type entry
                                   :function (lambda() (get-org-id-from-heading))
                                   :template ("* TODO %^{Description}"))))
                      ("Task" :keys "t"
                       :type entry
                       :file "~/Dropbox/org-files/master.org"
                       :olp ("Work" "LOA" "Active Projects / Tasks")
                       :template ("* TODO %^{Description}"
                                  ":PROPERTIES:"
                                  ":Created: %U"
                                  ":END:"
                                  "%?"))
                      ))

           ("Quick Refiling Note" :keys "n"
            :type entry
            :file "~/Dropbox/org-files/master.org"
            :olp ("Work" "LOA" "Refiling")
            :template ("* TODO %^{Description}"
                       ":PROPERTIES:"
                       ":Created: %U"
                       ":END:"
                       "%?"))

           ("ASP" :keys "a"
            :children (("Project" :keys "p"
                       :children (("New" :keys "n"
                                    :type entry
                                    :file "~/Dropbox/org-files/master.org"
                                    :olp ("Work" "ASP" "Active Projects / Tasks")
                                    :template ("* PROJECT %^{Job Number} // %^{Description} [%]"
                                               ":PROPERTIES:"
                                               ":Created: %U"
                                               ":CATEGORY: %\\1"
                                               ":END:"
                                               "* Notes"
                                               "%?"
                                               "* Subtasks"))

                                  ("Subtask within existing heading" :keys "t"
                                   :type entry
                                   :function (lambda() (get-org-id-from-heading))
                                   :template ("* TODO %^{Description}"))))

                      ("Task" :keys "t"
                       :type entry
                       :file "~/Dropbox/org-files/master.org"
                       :olp ("Work" "LOA" "Active Projects / Tasks")
                       :template ("* TODO %^{Description}"
                                  ":PROPERTIES:"
                                  ":Created: %U"
                                  ":END:"
                                  "%?"))))
            )))

;; Show matching parenthesis by default
(setq show-paren-mode 1)

;; Define function to interactively locate existing org heading to assign subtasks
(defun get-org-id-from-heading ()
  (interactive)
  (org-id-get-create
   (helm-org-rifle)
  ))

; Enable ledger-mode for all files with .ldg extension
(add-to-list 'auto-mode-alist '("\\.ldg" . ledger-mode))
