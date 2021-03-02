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
(setq doom-theme 'doom-gruvbox)
(setq doom-gruvbox-dark-variant "hard")
(custom-set-faces! '(default :background "gray15"))
(setq doom-gruvbox-brighter-comments nil)
(doom-themes-org-config)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Seafile/org-files/")

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
(after! org
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
                                  :and(
                                       :scheduled t
                                       :tag ("Bills"))
                                  :and(
                                       :scheduled t
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

			 (:name "Scheduled Inspections"
                              :todo "SITE")

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
  )
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

;; Associate .xls, .xlsx, .doc, .docx files with system defaults in org mode
(add-to-list 'org-file-apps '("\\.xls\\'" . default))
(add-to-list 'org-file-apps '("\\.xlsx\\'" . default))
(add-to-list 'org-file-apps '("\\.doc\\'" . default))
(add-to-list 'org-file-apps '("\\.docx\\'" . default))
(add-to-list 'org-file-apps '("\\.pdf\\'" . "cmd.exe \\c \"%s\""))

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
    (set-face-attribute face nil :weight 'normal :height 1.0))
  (setq line-spacing 0.2)
  (unfold-4))

(defun my/agenda-hook ()
  (setq line-spacing 0.2))

(add-hook 'org-mode-hook 'my/org-mode-hook)
(add-hook 'org-agenda-mode-hook 'my/agenda-hook)



;;add org-todo states
(after! org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "CHECKING(c)" "DRAFTING(D)" "FEEDBACK(f)" "HOLD(h)" "|"  "DONE(d)" "CANCELLED(C)")
          (sequence "PROJECT(p)" "|" "ISSUED(i)")
          (sequence "MEETING(m)" "SITE(s)" "|" "DONE(d)" ))))

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
                                   :file "~/Seafile/org-files/master.org"
                                   :olp ("Work" "LOA" "Active Projects / Tasks")
                                   :template ("* PROJECT %^{Job Number} // %^{Description} [%]"
                                              ":PROPERTIES:"
                                              ":Created: %U"
                                              ":CATEGORY: %\\1"
                                              ":END:"
                                              "** Notes"
                                              "%?"
                                              "** Subtasks"))
                                  ("Subtask within existing heading" :keys "t"
                                   :type entry
                                   :function (lambda() (get-org-id-from-heading))
                                   :template ("* TODO %^{Description}"))))
                      ("Task" :keys "t"
                       :type entry
                       :file "~/Seafile/org-files/master.org"
                       :olp ("Work" "LOA" "Active Projects / Tasks")
                       :template ("* TODO %^{Job Number} // %^{Description}"
                                  ":PROPERTIES:"
                                  ":Created: %U"
                                  ":CATEGORY: %\\1"
                                  ":END:"
                                  "%?"))

                      ("Inspection" :keys "i"
                       :type entry
                       :file "~/Seafile/org-files/work-appointments.org"
                       :template ("* SITE %^{Job Number} // %^{Description}"
                                  ":PROPERTIES:"
                                  ":Created: %U"
                                  ":CATEGORY: %\\1"
                                  ":END:"
                                  "%^T"))
                      ))

           ("Quick Refiling Note" :keys "n"
            :type entry
            :file "~/Seafile/org-files/master.org"
            :olp ("Work" "LOA" "Refiling")
            :template ("* TODO %^{Description}"
                       ":PROPERTIES:"
                       ":Created: %U"
                       ":END:"
                       "%?"))
           ("Admin Task" :keys "A"
            :type entry
            :file "~/Seafile/org-files/master.org"
            :olp ("Work" "LOA" "Admin")
            :template ("* TODO %^{Description}"
                       ":PROPERTIES:"
                       ":Created: %U"
                       ":END:"
                       "%?"))
           ("ASP" :keys "a"
            :children (("Project" :keys "p"
                       :children (("New" :keys "n"
                                    :type entry
                                    :file "~/Seafile/org-files/master.org"
                                    :olp ("Work" "ASP" "Active Projects / Tasks")
                                    :template ("* PROJECT %^{Job Number} // %^{Description} [%]"
                                               ":PROPERTIES:"
                                               ":Created: %U"
                                               ":CATEGORY: %\\1"
                                               ":END:"
                                               "** Notes"
                                               "%?"
                                               "** Subtasks"))

                                  ("Subtask within existing heading" :keys "t"
                                   :type entry
                                   :function (lambda() (get-org-id-from-heading))
                                   :template ("* TODO %^{Description}"))))

                      ("Task" :keys "t"
                       :type entry
                       :file "~/Seafile/org-files/master.org"
                       :olp ("Work" "ASP" "Active Projects / Tasks")
                       :template ("* TODO %^{Description}"
                                  ":PROPERTIES:"
                                  ":Created: %U"
                                  ":END:"
                                  "%?"))))
           ("Personal" :keys "p"
            :children (("Bill" :keys "b"
                        :type entry

                        :file "~/Seafile/org-files/personal.org"
                        :olp ("Tasks" "Bills")
                        :template ("* TODO %^{Description}"
                                   "Amount: %^{Amount}"
                                   "%?"))
                       ("Calendar Entry" :keys "c"
                        :type entry
                        :file "~/Seafile/org-files/personal-appointments.org"
                        :template ("* %^{Description}"
                                   "%^T"))))
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

; Reduce lag on leader key help menu
(require 'which-key)
(setq which-key-idle-delay 0.1)

; Set longitude and latitude for org agenda
(setq calendar-latitude -28.016666)
(setq calendar-longitude 153.399994)
;; add sunrise and sunset to agenda
(defun solar-sunrise-string (date &optional nolocation)
  (let ((l (solar-sunrise-sunset date)))
    (format
     "%s (%s hours daylight)"
     (if (car l)
     (concat "Sunrise " (apply 'solar-time-string (car l)))
       "no sunrise")
     (nth 2 l)
     )))
;; To be called from diary-list-sexp-entries, where DATE is bound.
;;;###diary-autoload
(defun diary-sunrise ()
  (or (and calendar-latitude calendar-longitude calendar-time-zone)
      (solar-setup))
  (solar-sunrise-string date))
  (defun solar-sunset-string (date &optional nolocation)
  (let ((l (solar-sunrise-sunset date)))
    (format
     "%s (%s hours daylight)"
     (if (cadr l)
     (concat "Sunset " (apply 'solar-time-string (cadr l)))
       "no sunset")
     (nth 2 l)
     )))
;; To be called from diary-list-sexp-entries, where DATE is bound.
;;;###diary-autoload
(defun diary-sunset ()
  (or (and calendar-latitude calendar-longitude calendar-time-zone)
      (solar-setup))
  (solar-sunset-string date))
  (provide 'sunrise-sunset)
;; custom level 3 font colour(custom-theme-set-faces 'user
`(org-level-3 ((t (:foreground "chocolate1"))))

;; Disable section numbers on org export
(setq org-export-with-section-numbers nil)

;; Org-Caldav Setup for Radicale server
 (setq org-caldav-url "https://charyutree.duckdns.org/radicale/brento/")
      (setq org-caldav-calendars
    '((:calendar-id "47cddf9a-4057-ccc7-a45d-e00121d796c7"
	    	:files ("~/Seafile/org-files/work-appointments.org")
		:inbox "~/Seafile/org-files/work-org-caldav-inbox.org")
	  (:calendar-id "1f49ba50-739d-057d-a062-a967174808fd"
		:files ("~/Seafile/org-files/personal-appointments.org")
		:inbox "~/Seafile/org-files/personal-org-caldav-inbox.org")
		))
(setq org-caldav-delete-calendar-entries 'never)
;; This is the sync on close function; it also prompts for save after syncing so
;; no late changes get lost
  (defun org-caldav-sync-at-close ()
    (org-caldav-sync)
    (save-some-buffers))

;; This is the delayed sync function; it waits until emacs has been idle for
;; "secs" seconds before syncing.  The delay is important because the caldav-sync
;; can take five or ten seconds, which would be painful if it did that right at save.
;; This way it just waits until you've been idle for a while to avoid disturbing
;; the user.
(defvar org-caldav-sync-timer nil
  "Timer that `org-caldav-push-timer' used to reschedule itself, or nil.")
(defun org-caldav-sync-with-delay (secs)
  (when org-caldav-sync-timer
    (cancel-timer org-caldav-sync-timer))
  (setq org-caldav-sync-timer
	      (run-with-idle-timer
	       (* 1 secs) nil 'org-caldav-sync)))
(setq org-icalendar-alarm-time 1)
;; This makes sure to-do items as a category can show up on the calendar
(setq org-icalendar-include-todo t)
;; This ensures all org "deadlines" show up, and show up as due dates
(setq org-icalendar-use-deadline '(event-if-todo event-if-not-todo todo-due))
;; This ensures "scheduled" org items show up, and show up as start times
(setq org-icalendar-use-scheduled '(todo-start event-if-todo event-if-not-todo))
;; Add the delayed save hook with a five minute idle timer
(add-hook 'after-save-hook
	        (lambda ()
	          (when (eq major-mode 'org-mode)
		          (org-caldav-sync-with-delay 300))))
;; Add the close emacs hook
(add-hook 'kill-emacs-hook 'org-caldav-sync-at-close)
