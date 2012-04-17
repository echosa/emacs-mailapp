(defgroup mailapp nil
  "Mail.app mode customization group."
  :group 'external)

(defgroup mailapp-faces nil
  "Faces and colors for Mail.app mode."
  :group 'mailapp)

(defcustom mailapp-base-dir "~/.emacs.d"
  "Directory where maillapp is installed."
  :group 'mailapp
  :type 'directory)

(defcustom mailapp-default-sort ""
  "Default sort string."
  :group 'mailapp
  :type 'string)
  
(defface mailapp-message-odd-row-face
  '((t (:foreground "white" :background "black")))
  "Colors for odd count mail messages."
  :group 'mailapp-faces)

(defface mailapp-message-even-row-face
  '((t (:foreground "white" :background "grey25")))
  "Colors for even count mail messages."
  :group 'mailapp-faces)

(defface mailapp-unread-message-odd-row-face
  '((t (:foreground "white" :background "black" :bold t)))
  "Colors for odd count unread mail messages."
  :group 'mailapp-faces)

(defface mailapp-unread-message-even-row-face
  '((t (:foreground "white" :background "grey25" :bold t)))
  "Colors for even count unread mail messages."
  :group 'mailapp-faces)

(defface mailapp-junk-message-odd-row-face
  '((t (:foreground "orange" :background "black")))
  "Colors for odd count read junk mail messages."
  :group 'mailapp-faces)

(defface mailapp-junk-message-even-row-face
  '((t (:foreground "orange" :background "grey25")))
  "Colors for even count read junk mail messages."
  :group 'mailapp-faces)

(defface mailapp-unread-junk-message-odd-row-face
  '((t (:foreground "orange" :background "black" :bold t)))
  "Colors for odd count unread junk mail messages."
  :group 'mailapp-faces)

(defface mailapp-unread-junk-message-even-row-face
  '((t (:foreground "orange" :background "grey25" :bold t)))
  "Colors for even count unread junk mail messages."
  :group 'mailapp-faces)

(provide 'mailapp-custom)