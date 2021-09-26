;;; dgg.el --- dgg's elisp utilities;

;; picked from here
;; URL: https://github.com/benswift/.dotfiles/blob/master/ben-utils.el
;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Visual appearance toggles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (spacemacs/toggle-visual-line-navigation)
(spacemacs/toggle-highlight-current-line-globally-off)


;; General CONFIGURATION

;; Execute cleanup functions when Emacs is closed
(add-hook 'kill-emacs-hook 'mb/kill-emacs-hook)

;; WINDOW CONFIGURATION
;; Automatic buffer resizing based on which split has focus
;; Bias towards splitting horizontally on narrow screens customized to 15 inch MBP
(setq split-width-threshold 168)
(setq fringe-mode 'no-fringes)

;; Spaceline config
(setq spaceline-org-clock-p t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODE CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Force headings to be the same Size. Not sure if I'm crazy...
(add-hook 'org-load-hook #'mb/org-mode-hook)

;; Ensure buffers are saved automatically to prevent sync errors
(add-hook 'auto-save-hook 'org-save-all-org-buffers)

;; Save file (if it exists) when cycling TODO states
(advice-add 'org-todo           :after 'mb/save-buffer-if-file)
(advice-add 'org-deadline       :after 'mb/save-buffer-if-file)
(advice-add 'org-schedule       :after 'mb/save-buffer-if-file)
(advice-add 'org-store-log-note :after 'mb/save-buffer-if-file)

;; Refile notes to top
(setq org-reverse-note-order t)

;; Size images displayed in org buffers to be more reasonable by default
(setq org-image-actual-width 600)

;; Sets custom TODO states
(setq org-todo-keywords
      '((sequence "REPEAT(r)" "TODO(t)" "IN-PROGRESS(i)" "|" "DONE(d)")
        (sequence "PROJECT(p)" "AREA(a)" "|" "COMPLETED(c)")
        (sequence "LATER(l)" "WAITING(w)" "FUTURE(f)" "|" "CANCELLED(x)")
        (sequence "|" "NOTE(n)")))


(setq org-lowest-priority 68)
(setq org-default-priority 67)

;; ORG-AGENDA CONFIGURATION
(setq org-agenda-start-with-follow-mode 't)

(setq org-superstar-headline-bullets-list '("◉" "○" "■" "◆" "▲" "▶"))

;; Org set habit graph columns
;; (setq 'org-habit-graph-column t)

(defun datetree-tomorrow ()
  (org-datetree-find-iso-week-create
   (calendar-gregorian-from-absolute
    (time-to-days (time-add (current-time) (list 0 86400 0))))))


;; org-capture templates and reflile config
(setq org-capture-templates
      `(
        ("n" "Note" entry (file+headline "~/Dropbox/org/inbox.org" "Notes")
         "*** %? :NOTE:\n%U\n%a\n%i\n:WEEK:%u " :prepend t :tree-type week :clock-in t :clock-resume t)
        ("p" "New Project" entry (file+headline "~/Dropbox/org/inbox.org" "Tasks")
         "** PROJECT %? \t\t\n\nEntered on: %U" :empty-lines-after 2)
        ("t" "TODO" entry (file+headline "~/Dropbox/org/inbox.org" "Tasks")
         "** TODO %? \nEntered on: %U\nURL: %c\n\nContext: %a\n%i\n" :clock-resume t :prepend t)
        ("i" "Inter" entry (file+headline "~/Dropbox/org/inbox.org" "Tasks")
         (file "~/Dropbox/org/questions_template.txt") :prepend t :empty-lines-after 2 :clock-resume t)
        ("u" "Tech Tutorial" entry (file+headline "~/Dropbox/org/inbox.org" "Tasks")
         "** TODO %? \t\t\t\t:TECH:TUTORIAL: \nURL:%c\n\nEntered on: %U\nContext:%a\n%i\n" :clock-resume t :prepend t)
        ("j" "Journal" entry (file+olp+datetree "~/Dropbox/org/journal.org")
         "*** %<%H:%M> %U\n\t\tFrom: %a\n%?" :tree-type week :clock-in t :clock-resume t)
        ("m" "Meeting" entry (file+headline "~/Dropbox/org/inbox.org" "Meeting")
         "*** TODO %t MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t :jump-to-captured t)
        ("r" "Reading" entry (file+headline "~/Dropbox/org/inbox.org" "Tasks")
         "** TODO %? :READING:ARTICLES:\nURL: %c\n\nEntered on: %U\nContext:%a\n%i\n" :prepend t :tree-type week :clock-in t :clock-resume t)
        ("h" "Habit" entry (file "~/Dropbox/org/inbox.org")
         "*** TODO %?\n%U\n%a\n:HABIT:\nSCHEDULED: %(format-time-string \"%<<%Y-%m(%B)-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: TODO\n:END:\n")
        ("v" "Videos to watch" entry (file+headline "~/Dropbox/org/inbox.org" "Tasks")
         "*** TODO %? \t\t\t\t\t:VIDEOS:YT: \nURL:%c\n\nEntered on: %U\nContext:%a\n%i\n" :prepend t)

        ("g" "GTD" entry (file+olp+datetree "~/Dropbox/org/gtd.org")
         "*** %? :GTD:\n%U\n%a\n%i\n" :prepend t :tree-type week :clock-in t :clock-resume t)

        ("G" "Tomorrow GTD, cannot be applied for start of week" plain (file+function "~/Dropbox/org/gtd.org" datetree-tomorrow)
         "*** %? :GTD:\n%U\n%a\n%i\n" :prepend t :tree-type week :clock-in t :clock-resume t)

        ("d" "DONE" entry (file+olp+datetree "~/Dropbox/org/done.org")
         "*** %? :DONE:\n%U\n%a\n%i\n" :prepend t :tree-type week :clock-in t :clock-resume t)


        ("D" "Insert to DONE_MAP Question" entry (file+headline "~/Dropbox/org/done_map.org" "Questions")
         (file "~/Dropbox/org/done_map_question.txt") :clock-resume t :immediate-finish t)
        ))


(with-eval-after-load 'ace-window
  (set-face-attribute 'aw-leading-char-face nil :height 2.5))

;; Org babel/programming config
;; Here are some alternatives - to add at the beginning of org-modules:
;;    or at the end:
;; https://lists.gnu.org/archive/html/emacs-orgmode/2010-04/msg00122.html
;; (setq org-modules (append org-modules '(org-habit)))
;; (add-to-list 'org-modules 'org-habit t)
;; (add-to-list 'org-modules 'org-checklist t)
(setq org-modules '(ol-bbdb
                    ol-bibtex
                    ol-docview
                    ol-eww
                    ol-gnus
                    ol-info
                    ol-irc
                    ol-mhe
                    ol-rmail
                    ol-w3m
                    org-checklist
                    org-habit))

(with-eval-after-load 'org
  (with-eval-after-load
      (org-babel-do-load-languages
      'org-babel-load-languages '((C . t)
                                  (java . t)
                                  (js . t)
                                  (plantuml . t)
                                  (python . t)
                                  (shell . t)
                                  (sql . t)
                                  )))
  )

(with-eval-after-load 'org
  (with-eval-after-load 'org-agenda
    (setq org-agenda-files (apply 'append
                                  (mapcar
                                   (lambda (directory)
                                     (directory-files-recursively
                                      directory org-agenda-file-regexp))
                                   '("~/Dropbox/org"
                                     "~/Dropbox/1_Projects"
                                     "~/Dropbox/2_Areas"
                                     ;; "~/Dropbox/3_Resources"
                                     "~/Dropbox/4_Archives"
                                     "~/workdir/inter"))))
    )
)



(setq org-agenda-custom-commands
      '(("d" "Daily agenda and all TODOs"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "" ((org-agenda-span-to-ndays 1)))
          (alltodo ""
                   ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                   (air-org-skip-subtree-if-priority ?A)
                                                   (org-agenda-skip-if nil '(scheduled deadline))))
                    (org-agenda-overriding-header "ALL normal priority tasks:"))))
         ((org-agenda-compact-blocks t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;; eshell and pyenv ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq eshell-path-env path-from-shell) ; for eshell users
    (setq exec-path (split-string path-from-shell path-separator))))

(when window-system (set-exec-path-from-shell-PATH))

(setq eshell-modify-global-environment t)

(defun smf/post-venv-hook ()
  (setq eshell-path-env (mapconcat 'identity exec-path ":")))

(add-hook 'pyvenv-post-activate-hooks #'smf/post-venv-hook)
(add-hook 'pyvenv-post-deactivate-hooks #'smf/post-venv-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eww configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; URL: https://protesilaos.com/dotemacs/#h:524bc702-ff55-4ed9-9a38-26d30d64591d
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'eww
  (add-hook 'eww-after-render-hook #'prot-eww--rename-buffer)
  (advice-add 'eww-back-url :after #'prot-eww--rename-buffer)
  (advice-add 'eww-forward-url :after #'prot-eww--rename-buffer))

(defun prot-eww--rename-buffer ()
  "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
                  (plist-get eww-data :url)
                (plist-get eww-data :title))))
    (rename-buffer (format "*%s # eww*" name) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END EWW CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mb/org-mode-hook ()
  "Keep headings all the same size"
  (interactive)
  (set-face-attribute 'org-level-1 nil :height 1.0))

;; URL: https://github.com/mack1070101/dotfiles/blob/master/.spacemacs#L814
(defun mb/save-buffer-if-file (&rest _rest)
  "Save the buffer if it has an associated file"
  (if (buffer-file-name)
      (save-buffer)))

;; General Emacs functions
(defun mb/kill-emacs-hook()
  "Performs cleanup tasks when quitting emacs"
  ;; Clock out when shutting down to prevent dangling clocks
  (org-clock-out nil t)
  (org-save-all-org-buffers))

;; ORG-MODE helper functions
(defun mb/org-mode-hook ()
  "Keep headings all the same size"
  (set-face-attribute 'org-level-1 nil :height 1.0))

(defun mb/org-babel-run-block ()
  "Run a code block by name"
  (interactive)
  (save-excursion
    (goto-char
     (org-babel-find-named-block
      (completing-read "#+NAME: "
                       (org-babel-src-block-names))))
    (org-babel-execute-src-block-maybe)))

;; narrow to parent of the org mode
(defun mb/org-narrow-to-parent ()
  "Narrow buffer to the current subtree."
  (interactive)
  (widen)
  (org-up-element)
  (save-excursion
    (save-match-data
      (org-with-limited-levels
       (narrow-to-region
        (progn
          (org-back-to-heading t) (point))
        (progn (org-end-of-subtree t t)
               (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
               (point)))))))

(defun prot-eww--rename-buffer ()
  "Rename EWW buffer using page title or URL.
To be used by `eww-after-render-hook'."
  (let ((name (if (eq "" (plist-get eww-data :title))
                  (plist-get eww-data :url)
                (plist-get eww-data :title))))
    (rename-buffer (format "*%s # eww*" name) t)))

(setq org-agenda-custom-commands
  '(("d" "Daily agenda and all TODOs"
    ((tags "PRIORITY=\"A\""
            ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
            (org-agenda-overriding-header "High-priority unfinished tasks:")))
     (agenda "" ((org-agenda-span-to-ndays 1)))
      (alltodo ""
              ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                              (air-org-skip-subtree-if-priority ?A)
                                              (org-agenda-skip-if nil '(scheduled deadline))))
                (org-agenda-overriding-header "ALL normal priority tasks:"))))
    ((org-agenda-compact-blocks t)))))


(defun mb/org-mode-hook ()
  "Keep headings all the same size"
  (interactive)
  (set-face-attribute 'org-level-1 nil :height 1.0))


;; URL: https://github.com/mack1070101/dotfiles/blob/master/.spacemacs#L814
(defun mb/save-buffer-if-file (&rest _rest)
  "Save the buffer if it has an associated file"
  (if (buffer-file-name)
      (save-buffer)))

;; General Emacs functions
(defun mb/kill-emacs-hook()
  "Performs cleanup tasks when quitting emacs"
  ;; Clock out when shutting down to prevent dangling clocks
  (org-clock-out nil t)
  (org-save-all-org-buffers))

;; ORG-MODE helper functions
(defun mb/org-mode-hook ()
  "Keep headings all the same size"
  (set-face-attribute 'org-level-1 nil :height 1.0))

(defun mb/org-babel-run-block ()
  "Run a code block by name"
  (interactive)
  (save-excursion
    (goto-char
     (org-babel-find-named-block
      (completing-read "#+NAME: "
                       (org-babel-src-block-names))))
    (org-babel-execute-src-block-maybe)))

;; narrow to parent of the org mode
(defun mb/org-narrow-to-parent ()
  "Narrow buffer to the current subtree."
  (interactive)
  (widen)
  (org-up-element)
  (save-excursion
    (save-match-data
      (org-with-limited-levels
       (narrow-to-region
        (progn
          (org-back-to-heading t) (point))
        (progn (org-end-of-subtree t t)
               (when (and (org-at-heading-p) (not (eobp))) (backward-char 1))
               (point)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; leuven configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package leuven-theme
  :config
  (setq leuven-scale-org-agenda-structure nil)
  (setq leuven-dark-scale-volatile-highlight nil)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END leuven configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org roam configuration mode
;; - org-roam
;; - org-roam-bibtex
;; - org-noter
;; - org-noter-pdftools
;; - helm-bibtex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Used these instructions
;; https://benswift.me/blog/2020/12/16/configuring-spacemacs-org-roam-org-noter-for-academic-writing-bliss/
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :ensure t
  :hook ((after-init . org-roam-map))
  :custom
  (org-roam-db-update-method 'immediate)
  :config
  (setq org-roam-dailies-directory "scratch/")
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t))
        org-roam-capture-ref-templates
        '(("r" "ref" plain
           "%?"
           :target (file+head "web/${slug}.org"
                              "#+title: ${title}\n#+roam_key: ${ref}\n#+created: %u\n#+last_modified: %U\n\n%(zp/org-protocol-insert-selection-dwim \"%i\")")
           :unnarrowed t)
          ("i" "incremental" plain
           "* %?\n%(zp/org-protocol-insert-selection-dwim \"%i\")"
           :target (file+head "web/${slug}.org"
                              "#+title: ${title}\n#+roam_key: ${ref}\n#+created: %u\n#+last_modified: %U\n\n")
           :unnarrowed t
           :empty-lines-before 1))
        org-roam-dailies-capture-templates
        '(("d" "default" entry
           "* %?"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n\n")
           :add-created t
           ))
        ))

;; (use-package org-roam-bibtex
;;   :after org-roam
;;   :hook (org-roam-mode . org-roam-bibtex-mode)
;;   :custom
;;   (orb-preformat-keywords '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
;;   (orb-file-field-extensions '("pdf" "epub" "html"))

;;   (orb-templates
;;    '(("r" "ref" plain (function org-roam-capture--get-point)
;;       ""
;;       :file-name "${citekey}"
;;       :head "#+TITLE: ${citekey}: ${title}
;; #+ROAM_KEY: ${ref}
;; - tags ::
;; - keywords :: ${keywords}
;; * ${title}
;;   :PROPERTIES:
;;   :Custom_ID: ${citekey}
;;   :URL: ${url}
;;   :AUTHOR: ${author-or-editor}
;;   :NOTER_DOCUMENT: ${file}
;;   :NOTER_PAGE:
;;   :END:"))))

(use-package org-pdftools
  :hook
  (org-mode . org-pdftools-setup-link)
  )

(use-package org-noter
  :after (:any org pdf-view)
  :config
  (require 'org-noter-pdftools)
  :custom
  (org-noter-always-create-frame nil)
  (org-noter-separate-notes-from-heading t)
  (org-noter-insert-note-no-questions t)
  (org-noter-separate-notes-from-heading t)
  (org-noter-save-last-location-property t)
  ;; (org-noter-default-notes-file-names '("notes.org"))
  (org-noter-notes-search-path (list org-roam-directory))
  ;; added this from here
  ;; https://github.com/weirdNox/org-noter/issues/57
  (defun org-noter-init-pdf-view ()
    (pdf-view-fit-page-to-window)
    (pdf-view-auto-slice-minor-mode)
    (run-at-time "0.5 sec" nil #'org-noter))
  (add-hook 'pdf-view-mode-hook 'org-noter-init-pdf-view))

(use-package org-noter-pdftools
  :after org-noter
  :config
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

(use-package helm-bibtex
  :config
  ;; (setq bibtex-completion-bibliography
  ;;       '("~/Dropbox/org/dgg_bib.bib"
  ;;         "~/Dropbox/org/dgg_bib.org"))
  ;; (setq bibtex-completion-notes-path "~/Dropbox/org/dgg_bib.org")
  ;; (setq org-ref-default-bibliography '("~/Dropbox/org/dgg_bib.bib")
  ;;       org-ref-pdf-directory "~/Dropbox/3_Resources/Books_Algo/"
  ;;       org-ref-bibliography-notes "~/Dropbox/3_Resources/Books/")

  (setq bibtex-completion-bibliography '("~/Dropbox/org/dgg_bib.bib"))
  (setq bibtex-completion-pdf-field "File")
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; END ORG ROAM CONFIG
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Disable flyspell by default
;; overriding the init-flyspell function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; +May not be required check at dgg-settings.el+
;; This is required to suppress the flyspell hook for org-mode files
;; Even after adding this to the flyspell config: https://develop.spacemacs.org/layers/+checkers/spell-checking/README.html#disabling-by-default
;;
(defun spacemacs/init-flyspell ()
  (defun dgg-enable-flyspell-mode ()
    (flyspell-mode 1))
  (use-package flyspell
    :defer t
    :init
    (progn
      (setq-default ispell-program-name "aspell")
      (setq-default ispell-dictionary "english")
      (add-hook 'markdown-mode-hook 'dgg-enable-flyspell-mode)
      (add-hook 'text-mode-hook 'dgg-enable-flyspell-mode)
      (add-hook 'dgg-mode-hook 'dgg-enable-flyspell-mode)
      (spacemacs|add-toggle spelling-checking
        :status flyspell-mode
        :on (flyspell-mode)
        :off (flyspell-mode -1)
        :documentation
        "Enable flyspell for automatic spelling checking."
        :evil-leader "ts"))
    :config
    (progn
      (flyspell-prog-mode)
      (spacemacs|diminish flyspell-mode " ⓢ" " s"))))
