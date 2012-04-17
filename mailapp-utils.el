(defvar script-dir "applescripts/"
  "Directory of the applescripts.")

(defvar mailapp-buffer-name "*Mail.app*"
  "Name of buffer.")

(defvar message-list-separator "|||"
  "Separator token for message lists.")

(defvar message-part-separator "|-|"
  "Separator token for message parts.")

(defvar message-part-id-separator "-"
  "Separator token for message parts.")

(defvar last-viewed-list nil
  "Holds the last viewed inbox list (all, unread, etc.) so the user can be returned to where they were after perfoming an action.")

(defvar last-list-sort nil
  "Holds the last sort used.")

(defvar last-viewed-account nil
  "Holds the last viewed account.")

(defvar last-viewed-mailbox "inbox"
  "Holds the last viewed mailbox.")

(defvar last-viewed-message-count nil
  "Holds the count number of the last viewed message.")

(defmacro mailapp-buffer (appendp &rest body)
  "Sends all output and buffer modifications to *Mail.app* buffer."
  `(with-current-buffer mailapp-buffer-name 
     (setq buffer-read-only nil)
     (if ,appendp
         (end-of-buffer)
       (delete-region (point-min) (point-max)))
     ,@body
     (delete-eob-whitespace)
     (setq buffer-read-only t)
     (when truncate-lines
       (toggle-truncate-lines -1))
     (unless word-wrap
       (toggle-word-wrap t))
     (beginning-of-buffer)
     (use-local-map mailapp-mode-map)))

(defun run-script (filename &optional tokens)
  (load-and-run-applescript 
   (concat mailapp-base-dir "/" script-dir "/" filename ".applescript") tokens))

(defun mailapp-set-inbox ()
  (setq last-viewed-mailbox "inbox")
  (setq last-viewed-account nil))

(defun mailapp-set-box (box account)
  (setq last-viewed-mailbox box)
  (setq last-viewed-account account))

(defun mailapp-get-last-box ()
  (if last-viewed-account
      (concat "item 1 of (mailboxes in item 1 of (accounts whose name is \""
              last-viewed-account
              "\") whose name is \""
              last-viewed-mailbox
              "\")")
    last-viewed-mailbox))

(defun parse-mailboxes (script &optional tokens)
  (let ((mailboxes (run-script script tokens))
        account
        templist
        boxlist)
    (when mailboxes
      (dolist (item (split-string mailboxes message-list-separator t))
        (if account
            (progn
              (setq templist nil)
              (dolist (box (split-string item message-part-separator t))
                (add-to-list 'templist box))
              (add-to-list 'boxlist (cons account (list templist)) t)
              (setq account nil))
          (setq account item))))
    boxlist))

(defun parse-messages (script &optional tokens sort)
  (let ((message-list (run-script script tokens))
        (date-size 24))
    (when message-list
      (setq message-list (read message-list)))

    (when sort
      (setq message-list (mailapp-sort message-list sort)))
    message-list))

(defun mailapp-parse-new-message ()
  (let (msg field content pos pos2)
    (beginning-of-buffer)
    (setq pos (point))
    (while (re-search-forward "^[^ ]*:" nil t)
      (setq pos2 (point))
      (setq field (buffer-substring-no-properties pos pos2))
      (end-of-line)
      (setq pos (point))
      (setq content (buffer-substring-no-properties pos2 pos))
      (cond
       ((equal field "To:")
        (add-to-list 'msg `(to . ,(mailapp-parse-to content)) t))
       ((equal field "From:")
        (add-to-list 'msg `(from . ,content) t))
       ((equal field "Subject:")
        (add-to-list 'msg `(subject . ,content) t)))
      (next-line)
      (beginning-of-line)
      (setq pos (point)))
    (next-line)
    (beginning-of-line)
    (setq pos (point))
    (end-of-buffer)
    (setq pos2 (point))
    (add-to-list 'msg `(body . ,(buffer-substring-no-properties pos pos2)))
    msg))

(defun mailapp-parse-to (content)
  (let (tos stringList)
    (setq stringList (split-string content " " t))
    (dolist (part stringList)
      (when (string-match ".*@.*" part)
        (add-to-list 'tos part t)))
    tos))

(defun mailapp-get-account-email-address (&optional account)
  (unless account
    (setq account last-viewed-account))
  (if account
      (run-script "emacs-mail-get-account-email-address"
                  `(("THEACCOUNT" ,account)))
    (mailapp-choose-from-email-address)
    nil))

(defun mailapp-get-mailid ()
  (get-text-property (point) 'mailid))

(defun mailapp-get-attachmentid ()
  (get-text-property (point) 'attachid))

(defun mailapp-face (count read-p junk-p)
  (let ((even-p (evenp count)))
    (cond
     ((and even-p read-p junk-p)
      'mailapp-junk-message-even-row-face)

     ((and even-p read-p (not junk-p))
      'mailapp-message-even-row-face)

     ((and even-p (not read-p) junk-p)
      'mailapp-unread-junk-message-even-row-face)

     ((and even-p (not read-p) (not junk-p))
      'mailapp-unread-message-even-row-face)

     ((and (not even-p) read-p junk-p)
      'mailapp-junk-message-odd-row-face)

     ((and (not even-p) read-p (not junk-p))
      'mailapp-message-odd-row-face)

     ((and (not even-p) (not read-p) junk-p)
      'mailapp-unread-junk-message-odd-row-face)

     ((and (not even-p) (not read-p) (not junk-p))
      'mailapp-unread-message-odd-row-face))))

(defun buffer-mail-is-base64 ()
  (save-excursion
    (beginning-of-buffer)
    (let ((beg (point)))
      (while (not (looking-at "^$"))
        (forward-line 1))
      (let ((headers 
             (mapcar (lambda (str) 
                       (mapcar (lambda (str)
                                 (replace-regexp-in-string 
                                  "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" 
                                  str)) (split-string str ":")))
                     (split-string (buffer-substring-no-properties 
                                    beg (point)) "\n"))))
        (string= "base64" (second (assoc "Content-Transfer-Encoding" headers)))))))

(defun delete-eob-whitespace ()
  (save-excursion
    (goto-char (point-max))
    (delete-horizontal-space)))

(defun mailapp-strip-chars (str)
  (let ((replace-list '(("" "")
                        ("=0A=" "")
                        (" " " "))))
    (dolist (rep replace-list)
      (setq str (replace-regexp-in-string (first rep) (second rep) str)))
    str))

(provide 'mailapp-utils)