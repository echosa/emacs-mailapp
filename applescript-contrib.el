(require 'applescript-mode)

(defvar *applescript-mode-debug* nil 
  "Print Applescript to message buffer before sending.")

(defcustom as-osadecompile-command "osadecompile"
  "*decompile AppleScripts and other OSA language scripts."
  :type 'string
  :group 'applescript)

(defun as-execute-line (&optional async)
  "Execute the current line as Applescript"
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'line))
         (p1 (first bounds))
         (p2 (rest bounds)))
    (as-execute-region p1 p2 async)))

(defun load-and-run-applescript (path &optional tokens)
  "Load an Applescript file, replace any tokens with values and execute"
  (interactive "fFile to load: ")
  (with-temp-buffer
    (insert-file-contents path)
    (let ((includes '()))
      (while (re-search-forward "^-- *require *\"\\\(.*\\\)\"" nil t)
        (add-to-list 'includes (match-string 1) t))
      (dolist (inc includes)
        (let ((incpath (concat (file-name-directory path) inc)))
          (when (file-exists-p incpath)
            (insert-file-contents incpath)
            (newline)))))
    (when tokens
      (if (listp tokens)
          (dolist (tok tokens)
            (save-excursion
              (while (re-search-forward (first tok) nil t)
                (replace-match (format "%s" (second tok)) t nil))))
        (error "Invalid token alist.")))
    (if *applescript-mode-debug*
        (message "Applescript:\n%s" (buffer-string)))
    (do-applescript (buffer-string))))

(modify-syntax-entry ?\\ "\\" as-mode-syntax-table)

(push '("\\.scpt$" . applescript-mode) auto-mode-alist)

(defun decompile-scpt ()
  (if (string= (file-name-extension (buffer-file-name)) "scpt")
      (let ((cmd (concat as-osadecompile-command " " 
                         (shell-quote-argument (buffer-file-name)))))
        (let ((script (shell-command-to-string cmd)))
          (if (string= (substring script 0 14) "osadecompile: ")
              (error "Invalid Applescript binary.")
            (delete-region (point-min) (point-max))
            (insert script))))))

(defun compile-scpt ()
  (if (string= (file-name-extension (buffer-file-name)) "scpt")
      (let ((cmd (concat as-osacompile-command " -o " 
                         (shell-quote-argument (buffer-file-name))
                         " "
                         (shell-quote-argument (buffer-file-name)))))
        (message "cmd: %s" cmd)
        (let ((result (shell-command-to-string cmd)))
            (if (> (length result) 0)
                (error "Error compiling Applescript: %s" result)
              (message "Applescript compiled successfully."))))))

(add-hook 'applescript-mode-hook 'decompile-scpt)
(add-hook 'after-save-hook 'compile-scpt)

(defun as-line-break ()
  "Insert the Applescript line continuation character."
  (interactive)
  (if (not (= (char-before) 32))
      (insert " "))
  (insert "¬")
  (newline))

(message "%s" as-mode-map)
(define-key as-mode-map (kbd "C-c RET") 'as-line-break)

(defun as-quote (str)
  (replace-regexp-in-string "\"" "\\\\\\\\\"" 
                            (replace-regexp-in-string "\\\\" "\\\\\\\\" 
                                                      str t) t))

(provide 'applescript-contrib)