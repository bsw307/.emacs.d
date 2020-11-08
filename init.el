(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

(package-initialize)

(defun get-default-height ()
       (/ (- (display-pixel-height) 120)
          (frame-char-height)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(eval-when-compile
  (require 'use-package))

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(setq default-directory "/Users/baltasarsalamonwelwert/")
  
(setq ring-bell-function 'ignore)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

(setq custom-file "~/.emacs.d/emacs-custom.el")

(load custom-file)

(use-package spacemacs-theme
:ensure t
:defer t)

(use-package modus-operandi-theme
:ensure t
:defer t)


(set-frame-font "Cascadia Code 12")
(load-theme 'spacemacs-light t)

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "ETBembo" :height 140))))
 '(fixed-pitch ((t ( :family "Cascadia Code 12" :height 140)))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(turn-on-page-break-lines-mode)

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
;;                        (projects . 5)
                        (agenda . 5)
                        (registers . 5)))
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner 3)
  (dashboard-setup-startup-hook))

(setq-default abbrev-mode t)

(use-package olivetti
  :ensure t)

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package counsel
   :after ivy
   :config (counsel-mode))

(use-package swiper
   :after ivy
   :bind (("C-s" . swiper)
          ("C-r" . swiper)))

(use-package deft
  :commands deft
  :init
  (setq deft-default-extension "org"
        deft-use-filename-as-title nil
        deft-use-filter-string-for-filename t
        deft-auto-save-interval -1.0
        deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))
  :config
  (add-to-list 'deft-extensions "tex")
  )

(use-package ivy-bibtex
   :ensure t
   :bind*
   ("C-c C-r" . ivy-bibtex))

   (setq
    bibtex-completion-notes-path "/Users/baltasarsalamonwelwert/Dropbox/ORG/Sources/Notes/"
    bibtex-completion-bibliography "/Users/baltasarsalamonwelwert/Dropbox/ORG/Sources/lib.bib"
    bibtex-completion-pdf-field "file"
    bibtex-completion-notes-template-multiple-files
    (concat
     "#+TITLE: ${title}\n"
     "#+ROAM_KEY: cite:${=key=}\n"
     "* TODO Notes\n"
     ":PROPERTIES:\n"
     ":Custom_ID: ${=key=}\n"
     ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
     ":AUTHOR: ${author-abbrev}\n"
     ":JOURNAL: ${journaltitle}\n"
     ":DATE: ${date}\n"
     ":YEAR: ${year}\n"
     ":DOI: ${doi}\n"
     ":URL: ${url}\n"
     ":END:\n\n"
     )
)

(use-package org-ref
    :ensure t
    :config
    (setq
         org-ref-completion-library 'org-ref-ivy-cite
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-ivy-bibtex
         org-ref-default-bibliography (list "/Users/baltasarsalamonwelwert/Dropbox/ORG/Sources/lib.bib")
         org-ref-bibliography-notes "/Users/baltasarsalamonwelwert/Dropbox/ORG/Sources/Notes/bibnotes.org"
         org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory "/Users/baltasarsalamonwelwert/Dropbox/ORG/Sources/Notes/"
         org-ref-notes-function 'orb-edit-notes
	 ))

(setq
   org_notes "/Users/baltasarsalamonwelwert/Dropbox/ORG/Sources/ORG-Simuvac/"
   zot_bib "/Users/baltasarsalamonwelwert/Dropbox/ORG/Sources/Notes/bibnotes.org"
   org-directory org_notes
   deft-directory org_notes
   org-roam-directory org_notes
   )

(require 'org)
(setq org-hide-emphasis-markers t)
(setq org-hide-leading-stars 't)
(setq org-log-into-drawer 't)
(setq org-agenda-files (list "/Users/baltasarsalamonwelwert/Dropbox/ORG/Planner"
			     "/Users/baltasarsalamonwelwert/Dropbox/ORG/Classes"))

(setq org-capture-bookmark nil)
;;(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
;;(global-set-key "\C-cb" 'org-iswitchb)

(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'flyspell-mode)

(setq org-cycle-separator-lines 1)

(use-package org-noter
  :ensure t
  :after (:any org pdf-view)
  :config
  (setq
   ;; The WM can handle splits
   org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list org_notes)
   )
  )

(use-package pdf-tools
  :ensure t
  :pin manual
  :config
   (pdf-tools-install)
   (setq-default pdf-view-display-size 'fit-page)
  )
   (setq pdf-view-use-scaling t)
   (setq pdf-view-use-imagick nil)

(use-package company
:ensure t
:defer t
)
