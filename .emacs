(require 'package)
;;(require 'use-package)

;; optional. makes unpure packages archives unavailable
;;(setq package-archives nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)

;; (setq lpr-command "lpr")
(require 'printing)
(pr-update-menus)

;; Make buffer name more meaningful
(add-hook 'exwm-update-class-hook
	(lambda ()
		(exwm-workspace-rename-buffer exwm-class-name)))
(add-hook 'exwm-update-title-hook
	(lambda ()
		(when (or (not exwm-instance-name)
			(string-prefix-p "sun-awt-X11-" exwm-instance-name)
			(string= "gimp" exwm-instance-name))
			(exwm-workspace-rename-buffer exwm-title))))

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


;;(require 'doom-themes)
(use-package doom-themes :defer t)
(load-theme 'doom-palenight t)
(doom-themes-visual-bell-config)


(save-place-mode 1) 

(setq exwm-workspace-number 0)

;; Global keybindings can be defined with `exwm-input-global-keys'.
;; Here are a few examples:
(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-&] . (lambda (command)
		     (interactive (list (read-shell-command "$ ")))
		     (start-process-shell-command command nil command)))))


(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun efs/set-wallpaper ()
  (interactive)
  ;; NOTE: You will need to update this to a valid background path!
  (start-process-shell-command
   "feh" nil  "feh --bg-scale /home/moses/Pictures/motivational-workout-conquer-m1f9vlaf12ukuaky.jpg"))

(defun efs/exwm-init-hook ()
  ;; Make workspace 0 be the one where we land at startup
  (exwm-workspace-switch-create 0)

  ;; Open eshell by default
  ;;(eshell)

  ;; Show battery status in the mode line
  (display-battery-mode 1)

  ;; Show the time and date in modeline
  (setq display-time-day-and-date t)
  (display-time-mode 1)
  ;; Also take a look at display-time-format and format-time-string

  ;; Launch apps that will run in the background
  (efs/run-in-background "dunst")
  (efs/run-in-background "pasystray")
  (efs/run-in-background "blueman-applet"))

;; When EXWM starts up, do some extra confifuration
(add-hook 'exwm-init-hook #'efs/exwm-init-hook)

(exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)


;; Desktop notifications

(defun efs/disable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_PAUSE\""))

(defun efs/enable-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_RESUME\""))

(defun efs/toggle-desktop-notifications ()
  (interactive)
  (start-process-shell-command "notify-send" nil "notify-send \"DUNST_COMMAND_TOGGLE\""))


;; Keyboard-centric user interface
;;(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)

;; enable visual feedback on selections
(setq transient-mark-mode t)

(efs/set-wallpaper)

;; TODO Sequence
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;;Desktop Enviroment for key bindings
(desktop-environment-mode)
(setq desktop-environment-brightness-small-increment "2%+")
(setq desktop-environment-brightness-small-decrement "2%-")
(setq desktop-environment-brightness-normal-increment "5%+")
(setq desktop-environment-brightness-normal-decrement "5%-")

;; Highlight Matching Braces
(require 'paren)
(set-face-attribute 'show-paren-match-expression nil :background "#363e4a")
(show-paren-mode 1)

;; ESC Cancels All
;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;Auto save changed files
;;(super-save-mode +1)
;;(setq super-save-auto-save-when-idle t)
;;(setq auto-save-default nil)
(use-package super-save
  :defer 1
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))



;; Doom Mode Line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Enable evil mode
;;(require 'evil)
;;(evil-mode 1)
;;(evilnc-default-hotkeys)
;;(setq evil-undo-system 'undo-fu)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" default))
 '(package-selected-packages
   '(org-roam mpv elfeed telega use-package-hydra undo-fu undo-tree nix-mode zzz-to-char nerdtab magit lsp-haskell haskell-mode desktop-environment gnus-desktop-notify org-mime dashboard undo-fu-session pdf-tools helm-lsp ormolu rainbow-delimiters evil-nerd-commenter projectile company treemacs-all-the-icons counsel swiper ivy which-key doom-themes exwm doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;(which-key-mode)
(use-package which-key
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

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

;;Org bullets
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))


;; Company
(add-hook 'after-init-hook 'global-company-mode)

;;Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;Rainbow Delimiters
;;(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;;LSP Config
(require 'lsp-mode)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;;(add-hook 'prog-mode-hook 'display-line-numbers-mode)
;;(global-display-line-numbers-mode)
;;(setq display-line-numbers-type 'relative)
(column-number-mode)

;; Enable line numbers for some modes
(dolist (mode '(text-mode-hook
                prog-mode-hook
                conf-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 1))))

;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))



;;Startup Dashboard
(require 'dashboard)
(dashboard-setup-startup-hook)

(require 'exwm-systemtray)
;;(setq exwm-systemtray-height 32)
(exwm-systemtray-enable)

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
        ("https://www.reddit.com/r/devops.rss" devops)
        ("https://www.reddit.com/r/kubernetes.rss" kubernetes)
))

(setq-default elfeed-search-filter "@2-days-ago +unread")
(setq-default elfeed-search-title-max-width 100)
(setq-default elfeed-search-title-min-width 100)

;;Telegram
(setq telega-use-docker t)
;;(setq telega-video-player-command t 'mpv)
(add-hook 'telega-load-hook 'telega-notifications-mode)


(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/orgRoam")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
    '(("d" "default" entry "* %<%I:%M %p>: %?"
       :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         :map org-roam-dailies-map
         ("Y" . org-roam-dailies-capture-yesterday)
         ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(use-package all-the-icons-dired)

(use-package dired-rainbow
    :defer 2
    :config
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package dired-single
    :defer t)

(use-package dired-ranger
    :defer t)

(use-package dired-collapse
    :defer t)

(exwm-enable)
