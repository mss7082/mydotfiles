;; -*- lexical-binding: t; -*-

(require 'package)

;; optional. makes unpure packages archives unavailable
(setq package-archives nil)

(setq package-enable-at-startup nil)
(package-initialize)

(pdf-tools-install)


;; (setq lpr-command "lpr")
(require 'printing)
(pr-update-menus)

(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq-default indent-tabs-mode nil)

;; Set up the visible bell
(setq visible-bell t)


(setq display-buffer-base-action
      '(display-buffer-reuse-mode-window
        display-buffer-reuse-window
        display-buffer-same-window))

;; If a popup does happen, don't resize windows to be equal-sized
(setq even-window-sizes nil)

;; Window size management
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time


(require 'doom-themes)
(load-theme 'doom-palenight t)
(doom-themes-visual-bell-config)


(save-place-mode 1) 

;; Keyboard-centric user interface
;;(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; enable visual feedback on selections
(setq transient-mark-mode t)

;; TODO Sequence
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; Highlight Matching Braces
(require 'paren)
(set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
(show-paren-mode 1)

;; ESC Cancels All
;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;Auto save changed files
(super-save-mode +1)
(setq super-save-auto-save-when-idle t)
(setq auto-save-default nil)



;; Doom Mode Line
(require 'doom-modeline)
(doom-modeline-mode 1)

(which-key-mode)
(setq which-key-idle-delay 0.3)

;; Ivy Counsel and Swiper
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;;enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythnmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(require 'ivy-prescient)
(require 'counsel)
(ivy-prescient-mode 1)
(setq prescient-filter-method '(literal regexp fuzzy))

(require 'company-prescient)
(require 'company)
(company-prescient-mode 1)

;;Org bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;;Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;Rainbow Delimiters
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;LSP Config
(require 'lsp-mode)
(require 'lsp)
(require 'lsp-haskell)
;; Hooks so haskell and literate haskell major modes trigger LSP setup
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(require 'ormolu)
(add-hook 'haskell-mode 'ormolu-format-on-save-mode)
;;(define-key haskell-mode-map (kbd "C-c r") 'ormolu-format-buffer)

;;Startup Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)

(add-to-list 'load-path "/nix/store/29drn9jg4riar14mw4ndpcw3iz0zrp2w-system-path/share/emacs/site-lisp/mu4e")
(require 'mu4e)
;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)
(setq mu4e-change-filenames-when-moving t ; avoid sync conflicts
      mu4e-update-interval (* 10 60) ; check mail 10 minutes
      mu4e-compose-format-flowed t ; re-flow mail so it's not hard wrapped
      mu4e-get-mail-command "mbsync -a"
      mu4e-maildir "~/mail/proton")

(setq mu4e-drafts-folder "/Drafts"
      mu4e-sent-folder   "/Sent"
      mu4e-refile-folder "/Archive"
      mu4e-trash-folder  "/Trash")

(setq mu4e-maildir-shortcuts
      '(("/inbox"     . ?i)
	("/Sent"      . ?s)
	("/Trash"     . ?t)
	("/Drafts"    . ?d)
	("/Archive"  . ?a)))

;;(setq message-send-mail-function 'smtpmail-send-it
;;      auth-sources '("~/.authinfo") ;need to use gpg version but only local smtp stored for now
;;      smtpmail-smtp-server "localhost"
;;      smtpmail-smtp-service 1025
;;      smtpmail-stream-type  'ssl))

(setq send-mail-function 'smtpmail-send-it)
;; Send mail using SMTP via mail.example.org.
(setq smtpmail-smtp-server "localhost")
;; Send mail using SMTP on the mail submission port 587.
(setq smtpmail-smtp-service 1025)
(setq smtpmail-stream-type 'starttls)

;; general emacs mail settings; used when composing e-mail
;; the non-mu4e-* stuff is inherited from emacs/message-mode
(setq mu4e-compose-reply-to-address "moses@sokabi.me"
      user-mail-address "moses@sokabi.me"
      user-full-name  "Moses S.")
(setq mu4e-compose-signature
   "Regards,\nMoses\n")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)


;;Org Mime
(require 'org-mime)
(setq org-mime-library 'mml)

(add-hook 'message-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c M-o") 'org-mime-htmlize)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (local-set-key (kbd "C-c M-o") 'org-mime-org-buffer-htmlize)))



(add-hook 'org-mime-html-hook
	  (lambda ()
	    (org-mime-change-element-style
	     "pre" (format "color: %s; background-color: %s; padding: 0.5em;"
			   "#E6E1DC" "#232323"))))

;; the following can be used to nicely offset block quotes in email bodies
(add-hook 'org-mime-html-hook
	  (lambda ()
	    (org-mime-change-element-style
	     "blockquote" "border-left: 2px solid gray; padding-left: 4px;")))

(add-hook 'org-mime-html-hook
	  (lambda ()
	    (while (re-search-forward "#\\([^#]*\\)#" nil t)
	      (replace-match "<span style=\"color:red\">\\1</span>"))))

;;Or just setup your export options in org buffer/subtree.
;;org-mime-export-options will override your export options if it’s NOT nil.
(setq org-mime-export-options '(:with-latex dvipng
:section-numbers nil
:with-author nil
:with-toc nil))

;; RSS FEEDS
(global-set-key (kbd "C-x w") 'elfeed)
(setq elfeed-feeds
      '(
        ;; programming
        ("https://www.reddit.com/r/emacs.rss" emacs)

        ("https://www.reddit.com/r/cardanodevelopers.rss" cardanodev)

        ;; networking
        ("https://www.reddit.com/r/networking.rss" networking)
        ("https://feeds.packetpushers.net/packetpushersfatpipe" packetpushers)

        ;; os
        ("https://www.reddit.com/r/nixos.rss" nixos)

        ;;crypto
        ("https://www.reddit.com/r/cardano.rss" cardano)
        

        ;; programming languages
        ("https://www.reddit.com/r/haskell.rss" haskell)
        ("https://www.reddit.com/r/rust.rss" rust)

        ;; cloud
        ("https://www.reddit.com/r/aws.rss" aws)
        ("https://www.reddit.com/r/googlecloud.rss" googlecloud)
        ("https://www.reddit.com/r/azure.rss" azure)

        ;;RnineT
        ("https://www.ninetowners.com/forums/servicing-and-maintainance.661/index.rss" rninetservice)
        ("https://www.ninetowners.com/forums/r-ninet-builds.641/index.rss" rninetbuilds)
        ("https://www.ninetowners.com/forums/bmw-ninet-photos.121/index.rss" rninetphotos)
        ("https://www.ninetowners.com/forums/bmw-ninet-videos.129/index.rss" rninetvideos)
        ("https://www.ninetowners.com/forums/wheels-tires-and-brakes.185/index.rss" rninetwheels)
        ("https://www.ninetowners.com/forums/how-to-tutorials-and-diy-projects.659/index.rss" rninetdiy)
        ("https://www.ninetowners.com/forums/accessories-and-gear.209/index.rss" rninetgear)
        ("https://www.ninetowners.com/forums/electronics.201/index.rss" rninetelectronics)
        ("https://www.ninetowners.com/forums/appearance-body.193/index.rss" rninetbody)
))

(setq-default elfeed-search-filter "@2-days-ago +unread")
(setq-default elfeed-search-title-max-width 100)
(setq-default elfeed-search-title-min-width 100)

;; (use-package org-roam
;;   :ensure t
;;   :init
;;   (setq org-roam-v2-ack t)
;;   :custom
;;   (org-roam-directory "~/orgRoam")
;;   (org-roam-completion-everywhere t)
;;   (org-roam-dailies-capture-templates
;;     '(("d" "default" entry "* %<%I:%M %p>: %?"
;;        :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n i" . org-roam-node-insert)
;;          :map org-mode-map
;;          ("C-M-i" . completion-at-point)
;;          :map org-roam-dailies-map
;;          ("Y" . org-roam-dailies-capture-yesterday)
;;          ("T" . org-roam-dailies-capture-tomorrow))
;;   :bind-keymap
;;   ("C-c n d" . org-roam-dailies-map)
;;   :config
;;   (require 'org-roam-dailies) ;; Ensure the keymap is available
;;   (org-roam-db-autosync-mode))


;;(defun my/org-roam-filter-by-tag (tag-name)
;;  (lambda (node)
;;    (member tag-name (org-roam-node-tags node))))

;;(defun my/org-roam-list-notes-by-tag (tag-name)
;;  (mapcar #'org-roam-node-file
;;          (seq-filter
;;           (my/org-roam-filter-by-tag tag-name)
;;           (org-roam-node-list))))

;;(defun my/org-roam-refresh-agenda-list ()
;;  (interactive)
;;  (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

;; Build the agenda list the first time for the session
;;(my/org-roam-refresh-agenda-list)

(define-key global-map (kbd "C-c a") #'org-agenda)

;; Save & restore sessions
(desktop-save-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(haskell haskell-mode all-th zerodark-theme yaml-mode which-key undo-fu-session telega super-save rainbow-delimiters projectile perspective pdf-tools ormolu org-roam org-mime org-bullets nix-mode nameless magit lsp-ui lsp-haskell ivy-prescient hydra helm-lsp evil elfeed doom-themes doom-modeline dashboard counsel company-prescient beacon auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
