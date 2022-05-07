(require 'package)

;; optional. makes unpure packages archives unavailable
;;(setq package-archives nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

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



(require 'doom-themes)
(load-theme 'doom-palenight)


(setq exwm-workspace-number 4)

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
  ;; Make workspace 1 be the one where we land at startup
  ;;(exwm-workspace-switch-create 1)

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

(defun efs/configure-window-by-class ()
  (interactive)
  (pcase exwm-class-name
    ("qutebrowser" (exwm-workspace-move-window 0))
    ("Brave-browser" (exwm-workspace-move-window 3))
    ("mpv" (exwm-floating-toggle-floating)
     (exwm-layout-toggle-mode-line))))


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

;; Doom Mode Line
(require 'doom-modeline)
(doom-modeline-mode 1)

;; Enable evil mode
;;(require 'evil)
;;(evil-mode 1)
;;(evilnc-default-hotkeys)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" default))
 '(package-selected-packages
   '(lsp-haskell haskell-mode desktop-environment gnus-desktop-notify org-mime dashboard undo-fu-session pdf-tools helm-lsp ormolu rainbow-delimiters evil-nerd-commenter projectile company treemacs-all-the-icons counsel swiper ivy which-key doom-themes exwm doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;Which key mode
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
(setq mu4e-compose-reply-to-address "moses@example.com"
      user-mail-address "moses@example.com"
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

(exwm-enable)
