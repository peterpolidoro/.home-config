(use-package mu4e
  ;; :defer 20 ; Wait until 20 seconds after startup
  :config

  ;; Load org-mode integration
  (require 'mu4e-org)

  ;; (require git-email-mu4e)

  (setq mu4e-attachment-dir "~/Downloads")
  (add-to-list 'mu4e-view-actions '("attachments" . mu4e-view-save-attachments) t)

  (require 'gnus-dired)
  ;; make the `gnus-dired-mail-buffers' function also work on
  ;; message-mode derived modes, such as mu4e-compose-mode
  (defun gnus-dired-mail-buffers ()
    "Return a list of active message buffers."
    (let (buffers)
      (save-current-buffer
        (dolist (buffer (buffer-list t))
          (set-buffer buffer)
          (when (and (derived-mode-p 'message-mode)
                     (null message-sent-message-via))
            (push (buffer-name buffer) buffers))))
      (nreverse buffers)))

  (setq gnus-dired-mail-mode 'mu4e-user-agent)
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

  ;; Refresh mail using isync every 5 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-root-maildir "~/Email")
  (add-to-list 'mu4e-view-actions '("browser" . mu4e-action-view-in-browser) t)

  ;; Use Ivy for mu4e completions (maildir folders, etc)
  (setq mu4e-completing-read-function #'ivy-completing-read)

  ;; Make sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)

  ;; All mime parts get buttons to make it easier to save inline images
  (setq gnus-inhibit-mime-unbuttonizing t)

  ;; Ensure text/plain is preferred as a mime type
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext"))

  (setq mu4e-headers-show-threads nil)

  (setq mml-secure-openpgp-signers '("C068EB0AB1B573BDB39FA662A121AF8A1FE021D6"))
  (setq mml-secure-opengpg-sign-with-sender t)
  ;; (setq mm-sign-option 'guided)
  (defun sign-or-encrypt-message ()
    (let ((answer (read-from-minibuffer "Sign or encrypt?\nEmpty to do nothing.\n[s/e]: ")))
      (cond
       ((string-equal answer "s") (progn
                                    (message "Signing message.")
                                    (mml-secure-message-sign-pgpmime)))
       ((string-equal answer "e") (progn
                                    (message "Encrypt and signing message.")
                                    (mml-secure-message-encrypt-pgpmime)))
       (t (progn
            (message "Dont sign or encrypt message.")
            nil)))))
  (add-hook 'message-send-hook 'sign-or-encrypt-message)

  ;; Set up contexts for email accounts
  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "gmail"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
          :vars '((user-full-name . "Peter Polidoro")
                  (user-mail-address . "peterpolidoro@gmail.com")
                  (smtpmail-smtp-user  . "peterpolidoro@gmail.com")
                  (smtpmail-smtp-server  . "smtp.gmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder . "/Gmail/Drafts")
                  (mu4e-sent-folder . "/Gmail/Sent")
                  (mu4e-trash-folder . "/Gmail/Trash")
                  (mu4e-sent-messages-behavior . sent)
                  ))
         (make-mu4e-context
          :name "janelia"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/Janelia" (mu4e-message-field msg :maildir))))
          :vars '((user-full-name . "Peter Polidoro")
                  (user-mail-address . "polidorop@janelia.hhmi.org")
                  (smtpmail-smtp-user  . "polidorop@hhmi.org")
                  (smtpmail-smtp-server  . "smtp.office365.com")
                  (smtpmail-smtp-service . 587)
                  (smtpmail-stream-type  . starttls)
                  (mu4e-drafts-folder . "/Janelia/Drafts")
                  (mu4e-sent-folder . "/Janelia/Sent")
                  (mu4e-trash-folder . "/Janelia/Trash")
                  (mu4e-sent-messages-behavior . sent)
                  ))
         (make-mu4e-context
          :name "polidoro"
          :match-func (lambda (msg)
                        (when msg
                          (string-prefix-p "/Polidoro" (mu4e-message-field msg :maildir))))
          :vars '((user-full-name . "Peter Polidoro")
                  (user-mail-address . "peter@polidoro.io")
                  (smtpmail-smtp-user  . "peter@polidoro.io")
                  (smtpmail-smtp-server  . "smtp.dreamhost.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  (mu4e-drafts-folder . "/Polidoro/Drafts")
                  (mu4e-sent-folder . "/Polidoro/Sent")
                  (mu4e-trash-folder . "/Polidoro/Trash")
                  (mu4e-sent-messages-behavior . sent)
                  ))
         ))
  (setq mu4e-context-policy 'pick-first)

  ;; Prevent mu4e from permanently deleting trashed items
  ;; This snippet was taken from the following article:
  ;; http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
  (defun remove-nth-element (nth list)
    (if (zerop nth) (cdr list)
      (let ((last (nthcdr (1- nth) list)))
        (setcdr last (cddr last))
        list)))
  (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
  (add-to-list 'mu4e-marks
               '(trash
                 :char ("d" . "▼")
                 :prompt "dtrash"
                 :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
                 :action (lambda (docid msg target)
                           (mu4e~proc-move docid
                                           (mu4e~mark-check-target target) "-N"))))

  ;; Display options
  (setq mu4e-view-show-images t)
  (setq mu4e-view-show-addresses t)
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))

  ;; Composing mail
  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-compose-format-flowed t)

  ;; Use mu4e for sending e-mail
  (setq message-send-mail-function 'smtpmail-send-it)

  ;; (See the documentation for `mu4e-sent-messages-behavior' if you have
  ;; additional non-Gmail addresses and want assign them different
  ;; behavior.)

  (add-to-list 'mu4e-bookmarks
               '(:name "All Inboxes"
                 :query "maildir:/Gmail/Inbox OR maildir:/Janelia/Inbox OR maildir:/Polidoro/Inbox"
                 :key ?i))

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  (setq pjp/mu4e-inbox-query
        "(maildir:/Gmail/Inbox OR maildir:/Janelia/Inbox OR maildir:/Polidoro/Inbox) AND flag:unread")

  (setq mu4e-maildir-shortcuts
        '(("/Gmail/Inbox" . ?g)
          ("/Janelia/Inbox" . ?j)
          ("/Polidoro/Inbox" . ?p)
          ))

  ;; (defun pjp/go-to-inbox ()
  ;;   (interactive)
  ;;   (mu4e-headers-search pjp/mu4e-inbox-query))

  (setq mu4e-headers-fields
        '((:human-date . 12)
          (:from-or-to . 20)
          (:subject . 50)
          (:maildir . 16)
          (:flags . 4)
          ))

  ;; Start mu4e
  (call-interactively 'mu4e))

(use-package mu4e-alert
  :after mu4e
  :config
  ;; Show unread emails from all inboxes
  (setq mu4e-alert-interesting-mail-query pjp/mu4e-inbox-query)

  ;; Show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)

  (mu4e-alert-enable-notifications))

(use-package org-mime
  :after mu4e
  :config
  (setq org-mime-export-options '(:section-numbers nil
                                                   :with-author nil
                                                   :with-toc nil))

  (add-hook 'message-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c o") 'org-mime-edit-mail-in-org-mode)))
  (advice-add 'org-mime-edit-src-exit :after
              (defun pjp-org-mime-edit-src-exit ()
                "Add function calls after org-mime-edit-src-exit"
                (org-mime-htmlize)))
  (advice-add 'org-mime-htmlize :after 'message-goto-to)
  (add-hook 'message-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c M-o") 'org-mime-htmlize)))
  (add-hook 'org-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c M-o") 'org-mime-org-buffer-htmlize)))
  )

(provide 'pjp-email)
