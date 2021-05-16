(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;;(require 'smtpmail)
(setq user-mail-address "jamesmstone@hotmail.com"
      user-full-name  "James Stone"
      ;; I have my mbsyncrc in a different folder on my system, to keep it separate from the
      ;; mbsyncrc available publicly in my dotfiles. You MUST edit the following line.
      ;; Be sure that the following command is: "mbsync -c ~/.config/mu4e/mbsyncrc -a"
      mu4e-get-mail-command "mbsync -a"
      mu4e-update-interval  300
      mu4e-main-buffer-hide-personal-addresses t
      message-send-mail-function 'smtpmail-send-it
      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.1and1.com" 587 nil nil))
      mu4e-sent-folder "/outlook/Sent"
      mu4e-drafts-folder "/outlook/Drafts"
      mu4e-trash-folder "/outlook/Trash"
      mu4e-maildir-shortcuts
      '(("/outlook/inbox"       . ?i)
        ("/outlook/Sent"        . ?s)
        ("/outlook/Drafts"      . ?d)
        ("/outlook/Deleted"     . ?t)))

(defvar my-mu4e-account-alist
  '(("exchange"
     (mu4e-sent-folder "/exchange/Sent")
     (mu4e-drafts-folder "/exchange/Draft")
     (mu4e-trash-folder "/exchange/Trash")
     (mu4e-compose-signature
       (concat
         "James Stone\n"
         "james@renewabeenergyhub.com.au\n"))
     (user-mail-address "james.stone@traditionasia.com")
     (smtpmail-default-smtp-server "smtp.domain.com")
     (smtpmail-smtp-server "smtp.domain.com")
     (smtpmail-smtp-user "acc1@domain.com")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 587))
    ("gmail"
     (mu4e-sent-folder "/gmail/Sent")
     (mu4e-drafts-folder "/gmail/Drafts")
     (mu4e-trash-folder "/gmail/Trash")
     (mu4e-compose-signature
       (concat
         "John Boy\n"
         "acc3@domain.com\n"))
     (user-mail-address "jamesmstone711@gmail.com")
     (smtpmail-default-smtp-server "smtp.domain.com")
     (smtpmail-smtp-server "smtp.domain.com")
     (smtpmail-smtp-user "acc3@domain.com")
     (smtpmail-stream-type starttls)
     (smtpmail-smtp-service 587))))

(defun my-mu4e-set-account ()
  "Set the account for composing a message."
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))

(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)
