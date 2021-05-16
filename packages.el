;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)
;;(package! elim)
(package! org-bullets)
(package! elfeed-goodies)
(package! slack)
(package! ovpn-mode)
(package! telega)
(package! excorporate)

;; telegram client for emacs config from:https://sunyour.org/post/doom-emacs%E9%87%8Ctelega%E7%9A%84%E9%85%8D%E7%BD%AE/

;; (use-package! telega
;;   :commands (telega)
;;   :defer t
;;   :bind ("C-c t" . #'telega)
;;   :init
;;   (unless (display-graphic-p) (setq telega-use-images nil))
;;   :hook
;;   ('telega-root-mode . #'evil-emacs-state)
;;   ('telega-chat-mode . #'evil-emacs-state)
;;   ('telega-chat-mode . #'yas-minor-mode)
;;   ('telega-chat-mode . (lambda ()
;;                          (set-company-backend! 'telega-chat-mode
;;                            (append '(telega-company-emoji
;;                                      telega-company-username
;;                                      telega-company-hashtag)
;;                                    (when (telega-chat-bot-p telega-chatbuf--chat)
;;                                      '(telega-company-botcmd))))
;;                          (company-mode 1)))
;;   ('telega-chat-pre-message . #'telega-msg-ignore-blocked-sender)
;;   :config
;;   (setq telega-proxies
;;         (list '(:server "127.0.0.1" :port 1086 :enable t
;;                         :type (:@type "proxyTypeSocks5"))))
;;   (set-popup-rule! "^\\*Telega Root"
;;     :side 'right :size 100 :quit nil :modeline t)
;;   (set-popup-rule! "^◀\\(\\[\\|<\\|{\\).*\\(\\]\\|>\\|}\\)"
;;     :side 'right :size 100 :quit nil :modeline t)
;;   (telega-mode-line-mode 1))


;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
