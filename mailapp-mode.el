(require 'applescript-contrib)
(require 'mailapp-custom)
(require 'mailapp-utils)
(require 'mailapp-sort)
(require 'mailapp-keymap)
(require 'mm-view)
(require 'sendmail)

(defun mailapp-mode ()
  "Mail function for running Mail.app mode.

\\{mailapp-mode-map}"
  (interactive)
  (switch-to-buffer mailapp-buffer-name)
  (setq buffer-read-only t)
  (kill-all-local-variables)
  (setq major-mode 'mailapp-mode)
  (setq mode-name "Mail.app")
  (mailapp-set-keymap)
  (mailapp-set-send-keymap)
  (use-local-map mailapp-mode-map)
  (setq last-list-sort (mailapp-sort-string->list mailapp-default-sort))
  (get-unread-message-list)
  (message "Mail.app mode loaded!"))

(defun mailapp-mode-quit ()
  "Quits Mail.app mode."
  (interactive)
  (kill-buffer mailapp-buffer-name))

(defun mailapp ()
  "Either opens mailapp or switches to its buffer if already open."
  (interactive)
  (if (get-buffer mailapp-buffer-name)
      (switch-to-buffer mailapp-buffer-name)
    (mailapp-mode)))

;; *****************
;; MESSAGE RETRIEVAL
;; *****************
(defun get-unread-message-list (&optional sort)
  (interactive)
  (unless sort
    (setq sort last-list-sort))
  (mailapp-buffer 
   nil 
   (get-message-list "whose read status is false" "Unread Messages" sort))
  (if (and (equal last-viewed-list "unread")
           last-viewed-message-count)
      (mailapp-list-jump last-viewed-message-count)
    (message-list-next))
  (setq last-viewed-list "unread"))

(defun get-all-message-list (&optional sort)
  (interactive)
  (unless sort
    (setq sort last-list-sort))
  (mailapp-buffer 
   nil
   (get-message-list "" "All Messages" sort))
  (if (and (equal last-viewed-list "all")
           last-viewed-message-count)
      (mailapp-list-jump last-viewed-message-count)
    (message-list-next))
  (setq last-viewed-list "all"))

(defun get-message-list (filter header &optional sort)
  (insert-header (concat header " in " last-viewed-mailbox 
                         (when last-viewed-account 
                           (concat " from " last-viewed-account))))
  (display-message-list 
   "emacs-mail-getmail" 
   `(("LISTFILTER" ,filter)
     ("TARGETBOX" ,(mailapp-get-last-box)))
   sort))

(defun message-view (id)
  (mailapp-buffer nil
   (let ((msg (first (parse-messages "emacs-mail-get-message"
                                     `(("MAILID" ,id)
                                       ("TARGETBOX" 
                                        ,(mailapp-get-last-box))))))
         (attachments (read 
                        (run-script "emacs-mail-get-message-attachments"
                                    `(("MAILID" ,id)
                                      ("TARGETBOX"
                                       ,(mailapp-get-last-box)))))))
     (save-excursion
       (let ((subject (rest (assoc 'subject msg)))
             (from (assoc 'from msg))
             (date (assoc 'date msg))
             (account (assoc 'account msg))
             (mailbox (rest (assoc 'mailbox msg)))
             (content (rest (assoc 'content msg))))
         (insert-header subject)
         (insert-message-part from)
         (insert-message-part date)
         (insert-message-part account)
         (insert (make-string (1- (window-width)) ?-))
         (when (< 0 (length attachments))
           (insert-message-attachments attachments))
         (newline)
         (insert-message-content content)
         (add-text-properties (point-min) (point-max) 
                              `(mailid ,id 
                                       account ,(rest account) 
                                       mailbox ,mailbox 
                                       view "message")))))))

(defun mailapp-goto-last-list (&optional sort)
  (interactive)
  (cond
   ((equal "all" last-viewed-list)
    (get-all-message-list sort))
   ((equal "unread" last-viewed-list)
    (get-unread-message-list sort))))

(defun mailapp-next-message (thisid)
  (mailapp-goto-last-list)
  (mailapp-list-jump last-viewed-message-count)
  (if (equal thisid (mailapp-get-mailid))
      (progn
        (message-list-next)
        (if (equal thisid (mailapp-get-mailid))
            (progn
              (mailapp-goto-last-list)
              (mailapp-list-jump last-viewed-message-count))
          (mailapp-return-press)))
    (mailapp-return-press)))

(defun mailapp-previous-message (thisid)
  (mailapp-goto-last-list)
  (mailapp-list-jump last-viewed-message-count)
  (message-list-previous)
  (if (equal thisid (mailapp-get-mailid))
      (mailapp-list-jump 1)
    (mailapp-return-press)))

;; ***************
;; MESSAGE DISPLAY
;; ***************
(defun display-message-list (script &optional tokens sort)
  (mailapp-buffer t
                  (unless (insert-message-list script tokens sort)
                    (insert "No mail."))
                  (add-text-properties (point-min) (point-max) 
                                       '(view "list"))))

(defun insert-message-list (script &optional tokens sort)
  (let ((msgs (parse-messages script tokens sort))
        (count 1))
    (dolist (msg msgs)
      (let ((prop-start (point))
            (face (mailapp-face count 
                                (when (assoc 'status msg) 
                                  (cdr (assoc 'status msg)))
                                (when (assoc 'junk msg) 
                                  (cdr (assoc 'junk msg))))))
        (insert-count count)
        (insert-message-list-subject (assoc 'subject msg))
        (insert-message-list-part (assoc 'from msg))
        (insert-message-list-part (assoc 'date msg))
        (unless last-viewed-account
          (insert-message-list-part (assoc 'account msg)))
        (add-text-properties prop-start (point) 
                             `(face ,face 
                                    mailid ,(rest (assoc 'id msg)) 
                                    mailbox ,(rest (assoc 'mailbox msg))
                                    account ,(rest (assoc 'account msg))
                                    mailcount ,count 
                                    action "viewmessage")))
      (setq count (1+ count)))
    msgs))

(defun insert-count (count)
     (insert (concat (number-to-string count) ") ")))

(defun insert-message-list-subject (subject)
  (let ((text (cdr subject)))
    (insert-message-list-part-text text)))

(defun insert-message-list-part (part)
  (let ((text (concat "   " (upcase-initials (symbol-name (car part))) ": " 
                      (cdr part))))
    (insert-message-list-part-text text)))

(defun insert-message-list-part-text (text)
  (insert (mailapp-strip-chars text))
  (end-of-line)
  (newline))

(defun insert-message-part (part)
  (insert (concat (upcase-initials (symbol-name (car part))) ": " 
                  (mailapp-strip-chars (cdr part))))
  (newline))

(defun insert-message-content (content)
  (insert
    (with-temp-buffer
      (insert content)
      (let ((handles (mm-dissect-buffer)))
        (if (not handles)
            (progn
              (let ((b64 (buffer-mail-is-base64)))
                (mail-text)
                (delete-region (point-min) (point))
                (if b64
                    (mailapp-strip-chars (base64-decode-string (buffer-string)))
                  (mailapp-strip-chars (buffer-string)))))
          (delete-region (point-min) (point-max))
          (display-mm-tree handles)
          (clean-up-mm-buffers)
          (mailapp-strip-chars (buffer-string)))))))

(defun mm-handle-p (handle)
  (bufferp (first handle)))

(defun display-mm-tree (tree)
  (if (mm-handle-p tree)
      (when (string= (first (mm-handle-type tree)) "text/plain")
        (mm-display-part tree)
        (goto-char (point-max))
        (newline)
        (newline))
    (dolist (branch (rest tree))
      (display-mm-tree branch))))

(defun clean-up-mm-buffers ()
  (dolist (buffer (buffer-list))
    (let ((name (buffer-name buffer)))
      (when (and (>= (length name) 6) 
                 (string= (substring name 0 6) " *mm*<"))
        (kill-buffer buffer)))))

(defun insert-header (text)
  (let ((spacing (number-to-string (/ (- (window-width) (length text)) 2))))
    (insert (format (concat "%-" spacing "s") ""))
    (insert text)
    (newline 2)))

(defun insert-message-attachments (attachments)
  (newline)
  (insert "Attachments:")
  (newline)
  (let ((count 1))
    (dolist (attachment attachments)
      (let ((id (cdr (assoc 'id attachment)))
            (name (cdr (assoc 'name attachment)))
            (size (cdr (assoc 'size attachment)))
            (mime (cdr (assoc 'mime attachment)))
            (downloaded (cdr (assoc 'downloaded attachment))))
        (insert (concat (number-to-string count) ") " name " (" mime
                        ", " (number-to-string (/ size 1024)) "KB) "))
        (add-text-properties 
         (save-excursion
           (beginning-of-line)
           (point))
         (point)
         `(action "viewattachment" attachid ,id))
        (newline)
        (setq count (1+ count)))))
  (insert (make-string (1- (window-width)) ?-)))

;; ******************
;; ATTACHMENT DISPLAY
;; ******************
(defun mailapp-view-attachment (mailid attachmentid)
  (message "View attachment %s for mail %s" attachmentid mailid))

;; ********************
;; MESSAGE MODIFICATION
;; ********************
(defun mailapp-message-marked-p ()
  (get-text-property (point) 'marked))

(defun mailapp-mark-message (&optional unmark)
  (interactive)
  (save-excursion
    (let ((bounds (mailapp-get-message-bounds)))
      (when bounds
        (let ((start (car bounds))
              (end (if (cdr bounds) (cdr bounds) (point-max))))
          (toggle-read-only -1)
          (if unmark
              (when (mailapp-message-marked-p)
                (remove-text-properties start end '(marked nil))
                (goto-char start)
                (delete-char 1))
            (unless (mailapp-message-marked-p)
              (goto-char start)
              (add-text-properties (1- (point)) (point) `(rear-nonsticky t))
              (add-text-properties (point) (1+ (point)) `(front-sticky t))
              (insert-and-inherit "*")
              (remove-text-properties (- (point) 2) (1- (point)) `(rear-nonsticky nil))
              (remove-text-properties (point) (1+ (point)) `(front-sticky nil))
              (add-text-properties start (1+ end) `(marked t)))
            (toggle-read-only 1)))))))

(defun mailapp-unmark-message ()
  (interactive)
  (mailapp-mark-message t))

(defun mailapp-mark-all-messages (&optional unmark)
  (interactive)
  (when (equal "list" (get-text-property (point) 'view))
    (save-excursion
      (end-of-buffer)
      (previous-line)
      (let ((count 1)
            (lastcount (get-text-property (point) 'mailcount)))
      (while (<= count lastcount)
        (mailapp-list-jump count)
        (if unmark
            (mailapp-mark-message t)
          (mailapp-mark-message))
        (setq count (1+ count)))))))

(defun mailapp-unmark-all-messages ()
  (interactive)
  (mailapp-mark-all-messages t))

(defun mailapp-get-message-bounds ()
  (when (equal "list" (get-text-property (point) 'view))
    (let ((id (get-text-property (point) 'mailid)))
      (when id
        (unless (equal id (get-text-property (1- (point)) 'mailid))
          (forward-char))
        (let ((start (previous-single-property-change (point) 'mailid))
              (end (next-single-property-change (point) 'mailid)))
          (cons start end))))))

(defun mailapp-get-marked-messages ()
  (let ((marked '())
        pos)
    (when (equal "list" (get-text-property (point) 'view))
      (save-excursion
        (beginning-of-buffer)
        (while (setq pos (next-single-property-change (point) 'mailid))
          (goto-char pos)
          (when (equal t (get-text-property (point) 'marked))
            (add-to-list 'marked (get-text-property (point) 'mailid))))))
    (when (and (= (length marked) 0)
               (get-text-property (point) 'mailid))
      (add-to-list 'marked (get-text-property (point) 'mailid)))
    (if (> (length marked) 0)
        marked
      (message "No mail message marked or at point!")
      nil)))

(defun mailapp-mark-unread (&optional mark-read dont-change-view)
  (interactive)
  (let ((marked (mailapp-get-marked-messages)))
    (when marked
      (let ((unread (if mark-read "true" "false")))
        (dolist (id marked)
          (run-script 
           "emacs-mail-mark-unread" 
           `(("MAILID" ,id)
             ("TARGETBOX" ,(mailapp-get-last-box))
             ("UNREAD" ,unread))))
        (unless dont-change-view
          (if (not (equal (get-text-property (point) 'action) "viewmessage"))
              (mailapp-next-message (mailapp-get-mailid))
            (setq last-viewed-message-count (get-text-property (point) 'mailcount))
            (mailapp-goto-last-list)))))))

(defun mailapp-mark-read (&optional dont-change-view)
  (interactive)
  (mailapp-mark-unread t dont-change-view))

(defun mailapp-toggle-junk ()
  (interactive)
  (let ((marked (mailapp-get-marked-messages)))
    (when marked
      (dolist (id marked)
        (run-script "emacs-mail-mark-junk" 
                    `(("MAILID" ,id) ("TARGETBOX" ,(mailapp-get-last-box)))))
      (if (equal (get-text-property (point) 'action) "viewmessage")
          (mailapp-next-message (mailapp-get-mailid))
        (setq last-viewed-message-count (get-text-property (point) 'mailcount))
        (mailapp-goto-last-list)))))
    
(defun mailapp-mark-deleted ()
  (interactive)
  (let ((marked (mailapp-get-marked-messages)))
    (when marked
      (let ((mailbox (get-text-property (point) 'mailbox))
            (account (get-text-property (point) 'account)))
        (dolist (id marked)
          (mailapp-move-message id mailbox account "Trash")
          (sleep-for .5)))
      (if (not (equal (get-text-property (point) 'action) "viewmessage"))
          (mailapp-next-message (first marked))
        (setq last-viewed-message-count (get-text-property (point) 'mailcount))
        (mailapp-goto-last-list)))))

(defun mailapp-mark-read-and-deleted ()
  (interactive)
  (let ((marked (mailapp-get-marked-messages)))
    (when marked
      (mailapp-mark-read t)
      (mailapp-mark-deleted))))

(defun mailapp-move-message-using-props ()
  (interactive)
  (let ((marked (get-text-property (point) 'marked)))
    (when marked
      (let ((startbox (mailapp-get-last-box))
            (account (get-text-property (point) 'account))
            (box (get-text-property (point) 'boxname))
            (count (get-text-property (point) 'mailcount)))
        (dolist (id marked)
          (mailapp-move-message id startbox account box)
          (sleep-for .5))
        (if (equal (get-text-property (point) 'view) "message")
            (mailapp-next-message (first marked))
          (mailapp-goto-last-list)
          (mailapp-list-jump count))))))

(defun mailapp-move-message (id startbox account box)
  (run-script "emacs-mail-move-message" `(("MAILID" ,id)
                                          ("STARTBOX" ,startbox)
                                          ("TARGETACCOUNT" ,account)
                                          ("TARGETBOX" ,box))))

;; *********
;; MAILBOXES
;; *********
(defun mailapp-select-mailbox-to-view ()
  (interactive)
  (mailapp-list-mailboxes "Select a Mailbox" "choosebox" "chooseinbox"))

(defun mailapp-select-mailbox-to-move-message ()
  (interactive)
  (let ((marked (mailapp-get-marked-messages)))
    (when marked
      (let ((count (get-text-property (point) 'mailcount))
            (view (get-text-property (point) 'view)))
        (mailapp-list-mailboxes "Move Message to Mailbox" "movetobox")
        (save-excursion
          (mailapp-buffer 
           t
           (beginning-of-buffer)
           (add-text-properties
            (point)
            (save-excursion
              (end-of-buffer)
              (point))
            `(marked ,marked mailcount ,count view ,view))))))))

(defun mailapp-list-mailboxes (header action &optional inbox-action)
  (mailapp-buffer nil
   (insert-header header)
   (when inbox-action
     (add-text-properties
      (point)
      (save-excursion
        (insert "Main Inbox")
        (point))
      `(action ,inbox-action))
     (end-of-line)
     (newline 2))
   (let ((mailboxes (parse-mailboxes "emacs-mail-get-all-mailboxes"))
         (count 1))
     (dolist (boxcons mailboxes)
       (insert (concat "Account: " (first boxcons)))
       (newline 2)
       (dolist (box (second boxcons))
         (add-text-properties
          (point)
          (save-excursion
            (insert (concat (number-to-string count) ") " box))
            (point))
          `(action ,action boxname ,box account ,(first boxcons)))
         (end-of-line)
         (newline)
         (setq count (1+ count)))
       (newline))))
  (goto-line 3)
  (beginning-of-line))

;; *****************
;; OUTGOING MESSAGES
;; *****************
(defun mailapp-new-message (&optional to subject other-headers continue switch-function yank-action send-actions)
  (interactive)
  (let (from)
    (if (equal "message" (get-text-property (point) 'view))
        (setq from (mailapp-get-account-email-address (mailapp-message-get-detail "account")))
      (setq from (mailapp-get-account-email-address)))
    (if from
        (progn
          (let ((other-headers `(("From" . ,from))))
            (compose-mail to subject other-headers continue switch-function yank-action send-actions))
          (use-local-map mailapp-send-map))
      (message "Can't get a from address."))))

(defun mailapp-reply-message ()
  (interactive)
  (if (mailapp-get-mailid)
      (progn
        (when (equal (get-text-property (point) 'view) "list")
          (message-view (get-text-property (point) 'mailid)))
        (let ((details (mailapp-message-get-details)))
          (mailapp-new-message
           (cdr (assoc 'from details))
           (cdr (assoc 'subject details)))
          (end-of-buffer)
          (insert (cdr (assoc 'body details)))
          (beginning-of-buffer)
          (re-search-forward "--text follows this line--")
          (next-line)
          (beginning-of-line)))
    (message "Nothing to reply to.")))

(defun mailapp-message-get-details ()
  (let (details
        (subject (mailapp-message-get-detail "subject")))
    (unless (string-match "^[Rr][Ee]" subject)
      (setq subject (concat "Re: " subject)))
    (add-to-list 'details `(account . ,(mailapp-message-get-detail "account")))
    (add-to-list 'details `(from . ,(mailapp-message-get-detail "from")))
    (add-to-list 'details `(subject . ,subject))
    (add-to-list 'details `(body . ,(mailapp-message-get-detail "body")))
    details))

(defun mailapp-message-get-detail (detail)
  (let (pos pos2)
    (save-excursion
      (beginning-of-buffer)
      (cond
       ((equal detail "account")
        (re-search-forward "^Account: "))
       ((equal detail "from")
        (re-search-forward "^From: "))
       ((equal detail "subject")
        (re-search-forward " *"))
       ((equal detail "body")
        (next-line 5)
        (setq pos (point))
        (end-of-buffer)
        (setq pos2 (point))))
      (unless pos
        (setq pos (point)))
      (unless pos2
        (end-of-line)
        (setq pos2 (point)))
      (buffer-substring-no-properties pos pos2))))
      
(defun mailapp-send-message (&optional show-window)
  (interactive)
  (let ((msg (mailapp-parse-new-message))
        sendto
        (visible "false")
        (activate "")
        (sendcmd "send newMail"))
    (setq sendto (mailapp-new-mail-recipients (cdr (assoc 'to msg))))
    (when show-window
      (setq visible "true")
      (setq activate "tell application \"Mail\" to activate")
      (setq sendcmd (concat "-- " sendcmd)))
    (run-script "emacs-mail-send-message" 
                `(("FROM" ,(cdr (assoc 'from msg)))
                  ("MSGSUBJECT" ,(as-quote (cdr (assoc 'subject msg))))
                  ("BODY" ,(as-quote (cdr (assoc 'body msg))))
                  ("SENDTO" ,sendto)
                  ("SHOWWINDOW" ,visible)
                  ("ACTIVATE" ,activate)
                  ("SENDCMD" ,sendcmd))))
  (mailapp-kill-message))

(defun mailapp-new-mail-recipients (recipients)
  (let ((recString ""))
    (dolist (recipient recipients)
      (setq recString (concat recString "make new to recipient at end of to recipients with properties {address:\"" recipient "\"}")))
    recString))

(defun mailapp-choose-from-email-address ()
  (mailapp-buffer
   nil
   (insert-header "Choose a 'From' Address")
   (let* ((emailString (run-script "emacs-mail-get-all-email-addresses"))
          (emailList (split-string emailString message-list-separator t))
          emailEntry
          (count 1))
     (dolist (email emailList)
       (setq emailEntry (split-string email message-part-separator t))
       (let ((account (first emailEntry))
             (emailAddress (second emailEntry)))
         (add-text-properties
          (point)
          (save-excursion
            (insert (concat (number-to-string count) ") " emailAddress))
            (point))
          `(emailaddress ,emailAddress account ,account action "choosefrom")))
       (end-of-line)
       (newline)
       (setq count (1+ count)))
)))

(defun mailapp-kill-message ()
  (interactive)
  (not-modified)
  (kill-buffer)
  (mailapp-goto-last-list))

;; **********
;; NAVIGATION
;; **********
(defun message-list-next ()
  (interactive)
  (let ((p (point))
        (line (line-number-at-pos (point)))
        (regexp "^\*?[0-9]*\)"))
    (when (re-search-forward regexp nil t)
      (when (equal line (line-number-at-pos (point)))
        (re-search-forward regexp nil t))
      (beginning-of-line)
      t)))

(defun message-list-previous ()
  (interactive)
  (let ((p (point))
        (line (line-number-at-pos (point)))
        (regexp "^\*?[0-9]*\)"))
    (when (re-search-backward regexp nil t)
      (when (equal line (line-number-at-pos (point)))
        (re-search-backward regexp nil t))
      (beginning-of-line)
      t)))

(defun mailapp-list-jump (num)
  (interactive (list (read-number "Jump to Number: ")))
  (let (pos)
    (save-excursion
      (beginning-of-buffer)
      (when (re-search-forward (concat "^\*?" (number-to-string num) ")") nil t)
        (beginning-of-line)
        (setq pos (point))))
    (when pos
      (goto-char pos))))

(provide 'mailapp-mode)