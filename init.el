(defun get-default-height ()
       (/ (- (display-pixel-height) 120)
          (frame-char-height)))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(add-to-list 'default-frame-alist '(width . 180))
;;(add-to-list 'default-frame-alist (cons 'height (get-default-height)))

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

(eval-when-compile
  (require 'use-package))

;Changes backup directory
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;;Turn off sound
(setq ring-bell-function 'ignore)

;;Theme
(set-frame-font "Cascadia Code 12")


(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#21252B" "#E06C75" "#98C379" "#E5C07B" "#61AFEF" "#C678DD" "#56B6C2" "#ABB2BF"])
 '(beacon-color "#cc6666")
 '(custom-enabled-themes (quote (spacemacs-light)))
 '(custom-safe-themes
   (quote
    ("35c096aa0975d104688a9e59e28860f5af6bb4459fd692ed47557727848e6dfe" "f490984d405f1a97418a92f478218b8e4bcc188cf353e5dd5d5acd2f8efd0790" "28a104f642d09d3e5c62ce3464ea2c143b9130167282ea97ddcc3607b381823f" "2d035eb93f92384d11f18ed00930e5cc9964281915689fa035719cab71766a15" "3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "77a70f6056d7d4b505ea99458eaad8bd748a8e0c940467c738f9632f7a97366f" default)))
 '(fci-rule-color "#3E4451")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(ivy-count-format "(%d/%d) ")
 '(ivy-use-virtual-buffers t)
 '(line-spacing 0.2)
 '(org-agenda-files
   (quote
    ("~/Dropbox/ORG/Classes/English 103/Trophic Cascades/Literature Notes.org" "/Users/baltasarsalamonwelwert/Dropbox/ORG/Planner/Book-list.org" "/Users/baltasarsalamonwelwert/Dropbox/ORG/Planner/Books.org" "/Users/baltasarsalamonwelwert/Dropbox/ORG/Planner/Kenyon.org" "/Users/baltasarsalamonwelwert/Dropbox/ORG/Planner/Org-system.org" "/Users/baltasarsalamonwelwert/Dropbox/ORG/Planner/Registration.org" "/Users/baltasarsalamonwelwert/Dropbox/ORG/Planner/Running.org" "/Users/baltasarsalamonwelwert/Dropbox/ORG/Planner/TODO.org" "/Users/baltasarsalamonwelwert/Dropbox/ORG/Planner/Transfer.org" "/Users/baltasarsalamonwelwert/Dropbox/ORG/Planner/Visa-appointment.org" "/Users/baltasarsalamonwelwert/Dropbox/ORG/Planner/Word-list.org" "/Users/baltasarsalamonwelwert/Dropbox/ORG/Planner/test.org" "/Users/baltasarsalamonwelwert/Dropbox/ORG/Classes/Kenyon Courses.org")))
 '(org-journal-dir "~/Dropbox/ORG/Planner/Journal" t)
 '(org-journal-file-type (quote weekly) t)
 '(org-roam-directory "/Users/baltasarsalamonwelwert/Dropbox/ORG/Org-Simuvac")
 '(package-selected-packages
   (quote
    (ivy-bibtex focus ivy-rich counsel org-journal auctex poet-theme org-books olivetti eyebrowse persp-mode color-theme-sanityinc-tomorrow grandshell-theme jedi all-the-icons-dired exec-path-from-shell poetry vscode-icons vscode-icon spacemacs-theme dashboard dired-sidebar elpy use-package)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef")))
 '(tetris-x-colors
   [[229 192 123]
    [97 175 239]
    [209 154 102]
    [224 108 117]
    [152 195 121]
    [198 120 221]
    [86 182 194]])
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#cc6666")
     (40 . "#de935f")
     (60 . "#f0c674")
     (80 . "#b5bd68")
     (100 . "#8abeb7")
     (120 . "#81a2be")
     (140 . "#b294bb")
     (160 . "#cc6666")
     (180 . "#de935f")
     (200 . "#f0c674")
     (220 . "#b5bd68")
     (240 . "#8abeb7")
     (260 . "#81a2be")
     (280 . "#b294bb")
     (300 . "#cc6666")
     (320 . "#de935f")
     (340 . "#f0c674")
     (360 . "#b5bd68"))))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))


;;Hiding menu and toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(turn-on-page-break-lines-mode)


;;Abbrev
(setq-default abbrev-mode t)


;;Package imports
;;(use-package elpy
;;  :ensure t
;;  :init
;;  (elpy-enable))

(use-package olivetti
  :ensure t)

;;Ivy
(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package ivy-bibtex
  :ensure t
  :bind*
  ("C-c C-r" . ivy-bibtex)
  :config
  (setq bibtex-completion-bibliography my/bib-file-location)
  ;; default is to open pdf - change that to insert citation
  (setq ivy-bibtex-default-action #'ivy-bibtex-insert-citation))


;; (use-package ivy-rich
;;   :after ivy
;;   :custom
;;   (ivy-virtual-abbreviate 'full
;;                           ivy-rich-switch-buffer-align-virtual-buffer t
;;                           ivy-rich-path-style 'abbrev)
;;   :config
;;   (ivy-set-display-transformer 'ivy-switch-buffer
;;                                'ivy-rich-switch-buffer-transformer))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

;;Flyspell
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map ("C-;" . flyspell-correct-wrapper)))

(use-package flyspell-correct-ivy
  :after flyspell-correct)

;;Org mode
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

;;Org mode hooks
;;(add-hook 'org-mode-hook 'olivetti-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
;;(add-hook 'org-mode-hook 'variable-pitch-mode)

(setq org-cycle-separator-lines 1)

;;ZZBembo beautifying
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "ETBembo" :height 140 :weight thin))))
 '(fixed-pitch ((t ( :family "Cascadia Code 12" :height 140)))))

;; (let* ((variable-tuple
;;         (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
;;               ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
;;               ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
;;               ((x-list-fonts "Verdana")         '(:font "Verdana"))
;;               ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
;;               (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
;;        (base-font-color     (face-foreground 'default nil 'default))
;;        (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

;;   (custom-theme-set-faces
;;    'user
;;    `(org-level-8 ((t (,@headline ,@variable-tuple :height 1.1))))
;;    `(org-level-7 ((t (,@headline ,@variable-tuple :height 1.1))))
;;    `(org-level-6 ((t (,@headline ,@variable-tuple :height 1.1))))
;;    `(org-level-5 ((t (,@headline ,@variable-tuple :height 1.1))))
;;    `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
;;    `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.1))))
;;    `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.1))))
;;    `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.2))))
;;    `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

;; Latex
(use-package auctex
  :defer t
  :ensure t)

;;Focus
(use-package focus
  :ensure t

  )

(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-theme 'icons)
  (setq dired-use-ls-dired nil))

;; Capture Templates
(setq org-capture-templates
      '(("b" "Book" entry (file "~/Dropbox/ORG/Planner/Book-list.org")
         "%(let* ((url (substring-no-properties (current-kill 0)))
                  (details (org-books-get-details url)))
             (when details (apply #'org-books-format 1 details)))")))

;; ORG-Journal
(use-package org-journal
  :ensure t
  :defer t
  :init
  (setq org-journal-prefix-key "C-c j ")
;;  (setq org-journal-open-next-entry "C-c f ")
  :bind
  ("C-c j" . org-journal-new-entry)
;;  ("C-c f" . org-journal-open-next-entry)
  :custom
  (org-journal-dir "~/Dropbox/ORG/Planner/Journal")
  (org-journal-file-type 'weekly)
  
  )

;; ORG-ROAM
(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "/Users/baltasarsalamonwelwert/Dropbox/ORG/Org-Simuvac")
;;      (org-roam-index-file "/Users/baltasarsalamonwelwert/Dropbox/ORG/Org-Simuvac/index")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
;;               ("C-c n j" . org-roam-jump-to-index)
               ("C-c n b" . org-roam-switch-to-buffer)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

;; ORG-BOOKS
(use-package org-books
  :ensure t)
(setq org-books-file "~/Dropbox/ORG/Planner/Book-list.org")

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

;;(use-package vscode-icon
;;  :ensure t
;;  :commands (vscode-icon-for-file))

;;Eyebrowse
;; (use-package eyebrowse
;;   :ensure t)
;; (eyebrowse-mode t)

;;ALL THE ICONS
(use-package all-the-icons-dired
  :ensure t)
;;  :hook (dired-mode . all-the-icons-dired-mode))

(use-package poetry
 :ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;;(use-package jedi
;;  :ensure t)

;;(add-hook 'python-mode-hook 'jedi:setup)

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(fixed-pitch ((t (:family "Cascadia Code 12" :height 160))))
;;  '(org-document-title ((t (:inherit default :weight bold :foreground "#655370" :font "ETBembo" :height 2.0 :underline nil))))
;;  '(org-level-1 ((t (:inherit default :weight bold :foreground "#655370" :font "ETBembo" :height 1.2))))
;;  '(org-level-2 ((t (:inherit default :weight bold :foreground "#655370" :font "ETBembo" :height 1.1))))
;;  '(org-level-3 ((t (:inherit default :weight bold :foreground "#655370" :font "ETBembo" :height 1.1))))
;;  '(org-level-4 ((t (:inherit default :weight bold :foreground "#655370" :font "ETBembo" :height 1.1))))
;;  '(org-level-5 ((t (:inherit default :weight bold :foreground "#655370" :font "ETBembo" :height 1.1))))
;;  '(org-level-6 ((t (:inherit default :weight bold :foreground "#655370" :font "ETBembo" :height 1.1))))
;;  '(org-level-7 ((t (:inherit default :weight bold :foreground "#655370" :font "ETBembo" :height 1.1))))
;;  '(org-level-8 ((t (:inherit default :weight bold :foreground "#655370" :font "ETBembo" :height 1.1))))
;;  '(variable-pitch ((t (:family "ETBembo" :height 160 :weight thin)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fixed-pitch ((t (:family "Cascadia Code 12" :height 140))))
 '(variable-pitch ((t (:family "ETBembo" :height 140 :weight thin)))))
