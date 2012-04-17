(defvar mailapp-mode-map nil
  "Keymapping for mailapp-mode.")

(defvar mailapp-send-map nil
  "Keymapping for mailapp-mode when sending messages.")

(defun mailapp-set-keymap ()
  (interactive)
  (setq mailapp-mode-map (make-sparse-keymap))
  (suppress-keymap mailapp-mode-map)
  (define-key mailapp-mode-map "b" 'mailapp-select-mailbox-to-view)
  (define-key mailapp-mode-map "c" 'mailapp-new-message)
  (define-key mailapp-mode-map "d" 'mailapp-mark-read-and-deleted)
  (define-key mailapp-mode-map "D" 'mailapp-mark-deleted)
  (define-key mailapp-mode-map "g" 'get-unread-message-list)
  (define-key mailapp-mode-map "G" 'get-all-message-list)
  (define-key mailapp-mode-map "j" 'mailapp-list-jump)
  (define-key mailapp-mode-map "J" 'mailapp-toggle-junk)
  (define-key mailapp-mode-map "m" 'mailapp-mark-message)
  (define-key mailapp-mode-map "M" 'mailapp-select-mailbox-to-move-message)
  (define-key mailapp-mode-map "n" 'mailapp-n-press)
  (define-key mailapp-mode-map "p" 'mailapp-p-press)
  (define-key mailapp-mode-map "q" 'mailapp-mode-quit)
  (define-key mailapp-mode-map "r" 'mailapp-mark-read)
  (define-key mailapp-mode-map "R" 'mailapp-reply-message)
  (define-key mailapp-mode-map "s" 'mailapp-s-press)
  (define-key mailapp-mode-map "u" 'mailapp-unmark-message)
  (define-key mailapp-mode-map "U" 'mailapp-mark-unread)
  (define-key mailapp-mode-map "*m" 'mailapp-mark-all-messages)
  (define-key mailapp-mode-map "*u" 'mailapp-unmark-all-messages)
  (define-key mailapp-mode-map (kbd "SPC") 'scroll-up)
  (define-key mailapp-mode-map (kbd "S-SPC") 'scroll-down)
  (define-key mailapp-mode-map [tab] 'mailapp-n-press)
  (define-key mailapp-mode-map [S-tab] 'mailapp-p-press)
  (define-key mailapp-mode-map [return] 'mailapp-return-press))

(defun mailapp-set-send-keymap ()
  (interactive)
  (setq mailapp-send-map (make-sparse-keymap))
  (define-key mailapp-send-map (kbd "C-c C-c") 'mailapp-send-message)
  (define-key mailapp-send-map (kbd "C-c C-k") 'mailapp-kill-message))

(defun mailapp-return-press ()
  (interactive)
  (cond
   ((equal (get-text-property (point) 'action) "viewmessage")
    (setq last-viewed-message-count (get-text-property (point) 'mailcount))
    (message-view (mailapp-get-mailid)))

   ((equal (get-text-property (point) 'action) "viewattachment")
    (mailapp-view-attachment (mailapp-get-mailid) (mailapp-get-attachmentid)))

   ((equal (get-text-property (point) 'action) "chooseinbox")
    (mailapp-set-inbox)
    (mailapp-goto-last-list))

   ((equal (get-text-property (point) 'action) "choosebox")
    (mailapp-set-box 
     (get-text-property (point) 'boxname)
     (get-text-property (point) 'account))
    (mailapp-goto-last-list))

   ((equal (get-text-property (point) 'action) "choosefrom")
    (let ((temp last-viewed-account))
      (setq last-viewed-account (get-text-property (point) 'account))
      (mailapp-new-message)
      (setq last-viewed-account temp)))

   ((equal (get-text-property (point) 'action) "movetobox")
    (mailapp-move-message-using-props))))

(defun mailapp-n-press ()
  (interactive)
  (let ((prop (get-text-property (point) 'view)))
    (cond
     ((equal prop "list")
      (message-list-next))
     ((equal prop "message")
      (mailapp-next-message (mailapp-get-mailid))))))

(defun mailapp-p-press ()
  (interactive)
  (let ((prop (get-text-property (point) 'view)))
    (cond
     ((equal prop "list")
      (message-list-previous))
     ((equal prop "message")
      (mailapp-previous-message (mailapp-get-mailid))))))

(defun mailapp-s-press ()
  (interactive)
  (let ((prop (get-text-property (point) 'view)))
    (cond 
     ((equal prop "list")
      (call-interactively 'mailapp-sort-choice)))))

(provide 'mailapp-keymap)