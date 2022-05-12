(require 'package)

;; optional. makes unpure packages archives unavailable
;;(setq package-archives nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(setq package-enable-at-startup nil)
(package-initialize)


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


(require 'doom-themes)
(load-theme 'doom-palenight t)


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


;;(defun efs/configure-window-by-class ()
;;  (interactive)
;;  (pcase exwm-class-name
;;    ("qutebrowser" (exwm-workspace-move-window 0))
;;    ("discord" (exwm-workspace-move-window 1))
;;    ("TelegramDesktop" (exwm-workspace-move-window 1))
;;    ("vlc" (exwm-workspace-move-window 2))
;;    ("Brave-browser" (exwm-workspace-move-window 4))
;;    ("mpv" (exwm-floating-toggle-floating)
;;     (exwm-layout-toggle-mode-line))))


;; Tab-bar settings.

(defun dipo/tab-bar-switch-or-create (name func)
  (if (dipo/tab-bar-tab-exists name)
      (tab-bar-switch-to-tab name)
    (dipo/tab-bar-new-tab name func)))

(defun dipo/tab-bar-tab-exists (name)
  (member name
	  (mapcar #'(lambda (tab) (alist-get 'name tab))
		        (tab-bar-tabs))))

(defun dipo/tab-bar-new-tab (name func)
  (when (eq nil tab-bar-mode)
    (tab-bar-mode))
  (tab-bar-new-tab)
  (tab-bar-rename-tab name)
  (funcall func))

;;(defun dipo/tab-bar-run-elfeed ()
;;  (interactive)
;;  (dipo/tab-bar-switch-or-create "RSS" #'elfeed))

(defun dipo/tab-bar-run-mail ()
  (interactive)
  (dipo/tab-bar-switch-or-create
   "Mail"
   #'(lambda ()
       ;;(mu4e-context-switch :name "Private") ;; If not set then mu4e will ask for it.
       (mu4e))))

;;(defun dipo/tab-bar-run-irc ()
;;  (interactive)
;;  (dipo/tab-bar-switch-or-create
;;   "IRC"
;;   #'(lambda ()
;;       (dipo/erc-connect)
;;      (sit-for 1) ;; ERC connect takes a while to load and doesn't switch to a buffer itself.
;;       (switch-to-buffer "Libera.Chat"))))

(defun dipo/tab-bar-run-agenda ()
  (interactive)
  (dipo/tab-bar-switch-or-create
   "Agenda"
   #'(lambda ()
       (org-agenda nil "a")))) ;; 'a' is the key of the agenda configuration I currently use.

;;(defun dipo/tab-bar-run-journal ()
;;  (interactive)
;;  (dipo/tab-bar-switch-or-create
;;   "Journal"
;;   #'org-journal-open-current-journal-file))

;;(defun dipo/tab-bar-run-projects ()
;;  (interactive)
;;  (dipo/tab-bar-switch-or-create
;;   "Projects"
;;   #'(lambda ()
;;       (find-file "~/org/projects.org"))))


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
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;;Auto save changed files
(super-save-mode +1)
(setq super-save-auto-save-when-idle t)


;; Doom Mode Line
(require 'doom-modeline)
(doom-modeline-mode 1)

;; Enable evil mode
(require 'evil)
(evil-mode 1)
;;(evilnc-default-hotkeys)
(setq evil-undo-system 'undo-fu)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" default))
 '(package-selected-packages
   '(elfeed telega use-package-hydra undo-fu undo-tree nix-mode zzz-to-char nerdtab magit lsp-haskell haskell-mode desktop-environment gnus-desktop-notify org-mime dashboard undo-fu-session pdf-tools helm-lsp ormolu rainbow-delimiters evil-nerd-commenter projectile company treemacs-all-the-icons counsel swiper ivy which-key doom-themes exwm doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(which-key-mode)

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
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
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
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;LSP Config
(require 'lsp-mode)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-mode-hook #'lsp)
(add-hook 'haskell-literate-mode-hook #'lsp)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;;(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

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

;;(setq telega-server-libs-prefix "/nix/store/jhlvq7axrhzlyrp33jflzc19rqbx7cis-tdlib-1.7.9/")
;;(setq telega-server-libs-prefix "/nix/store/9m67dfjbs0rd9ay4lqdvjwan971hz8af-tdlib-1.8.3/")


;;Fix evil conflicts with elfeed
(add-to-list 'evil-emacs-state-modes 'elfeed-search-mode)
(add-to-list 'evil-emacs-state-modes 'elfeed-show-mode)

(exwm-enable)
