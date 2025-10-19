;;; -*- lexical-binding: t; -*-

(defun eml-add-date-at-beginning-of-eml-file ()
   "Add date at the beginning of eml file in dired.
(v2, available in occisn/emacs-utils GitHub repository)"
   (interactive)

   (cl-labels ((replace-linux-slash-with-two-windows-slashes (path)
                 "Return PATH string after having replaced slashes by two backslashes.
For instance: abc/def --> abc\\def"
                 (replace-regexp-in-string  "/" "\\\\" path))
               (english-month-to-number (month)
                 "Jan --> 1, Dec --> 12"
                 (cond ((equal "Jan" month) 1)
                       ((equal "Feb" month) 2)
                       ((equal "Mar" month) 3)
                       ((equal "Apr" month) 4)
                       ((equal "May" month) 5)
                       ((equal "Jun" month) 6)
                       ((equal "Jul" month) 7)
                       ((equal "Aug" month) 8)
                       ((equal "Sep" month) 9)
                       ((equal "Oct" month) 10)
                       ((equal "Nov" month) 11)
                       ((equal "Dec" month) 12)
                       (t (error "Month not recognized: %s" month))))) ; end of labels definitions
     
     (when (not (string-equal major-mode "dired-mode"))
       (error "Trying to burst a PDF file when not in dired-mode."))
     (when (> (length (dired-get-marked-files)) 1)
       (error "Trying to add dates at the beginning of several files."))
     
     (let* ((files-list (dired-get-marked-files))
	    (file-full-name (car files-list))
	    (file-full-name-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes file-full-name))
	    (file-directory (file-name-directory file-full-name))
	    (file-name (file-name-nondirectory file-full-name))
	    (file-name-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes file-name))
	    ;; (file-name-without-extension (file-name-base file-full-name))
            (date-line nil))
       (when (not (or (string= (file-name-extension file-full-name) "eml") (string= (file-name-extension file-full-name) "EML") ))
         (error "Trying to extract date from a non-EML fil: %S" file-full-name))
       (with-temp-buffer
         (insert-file-contents file-full-name-slash-OK-accents-OK)
         (goto-char (point-min))
         (search-forward "Date:")
         (set-mark-command nil)
         (end-of-line)
         (setq date-line (buffer-substring (region-beginning) (region-end))))
       (when (null date-line)
         (error "No 'Date:' line found in file %s" file-name-slash-OK-accents-OK))
       (let ((elements (split-string date-line)))
         (when (< (length elements) 4)
           (message "Not enough elements on DATE-LINE: %s" date-line)
           (message "   New attempt...")
           (with-temp-buffer
             (insert-file-contents file-full-name-slash-OK-accents-OK)
             (goto-char (point-min))
             (search-forward "Date:")
             (search-forward "Date:")
             (set-mark-command nil)
             (end-of-line)
             (setq date-line (buffer-substring (region-beginning) (region-end))))
           (when (null date-line)
             (error "No *2nd* 'Date:' line found in file %s" file-name-slash-OK-accents-OK))
           (setq elements (split-string date-line))
           (when (< (length elements) 4)
             (error "On 2nd attemp, not enough elements on DATE-LINE: %s" date-line)))
         (let* ((day (string-to-number (nth 1 elements)))
                (month (nth 2 elements))                 ; Jan...Dec
                (month2 (english-month-to-number month)) ; 1...12
                (year (string-to-number (nth 3 elements)))
                (yyyy-mm-dd (format "%04d-%02d-%02d" year month2 day))
                (file1-old-name file-name)
                (file1-new-name (concat file-directory yyyy-mm-dd " _" file1-old-name)))
           
           (rename-file file1-old-name file1-new-name)
           (message "Date (%s) added at the beginning of file '%s'" yyyy-mm-dd file1-old-name)
           (revert-buffer)
           (dired-goto-file file1-new-name))))))

;;; end
