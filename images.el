;;; -*- lexical-binding: t; -*-

(defun my/paste-image-from-clipboard-to-file-with-imagemagick (destination-file-with-path &optional wait-p)
  "Paste image from clipboard fo file DESTINATION-FILE-WITH-PATH with ImageMagick.
If WAIT-P is true, wait for the command to be executed before returning (synchroneous). Otherwise, return immediately (asynchroneous).
(v1, available in occisn/emacs-utils GitHub repository)"
  (let* ((imagemagick-convert-program "c:/.../convert.exe")
         (cmd (concat "\"" imagemagick-convert-program "\" " "clipboard: " destination-file-with-path)))
    (message "Pasting image from clipboard to %s with ImageMagick." destination-file-with-path)
    (if wait-p
        (call-process-shell-command cmd) ; synchroneous (wait for completion)
      (call-process-shell-command cmd nil 0) ; asynchroneous
      )))

(defun my/paste-image-from-clipboard-to-here ()
  "Paste image from clipboard to the current Dired buffer as png file.
Uses ImageMagick.
(v1, available in occisn/emacs-utils GitHub repository)"
  (interactive)

  (unless (string-equal major-mode "dired-mode")
    (error "Trying to paste image from clipboard while not in dired-mode"))

  (let* ((imagemagick-convert-program "c:/.../convert.exe")
         (file-short-name (read-string "File name without suffix: "))
	 (suffix ".png")
         (destination-file-with-path1 (concat default-directory file-short-name suffix))
	 (destination-file-with-path2 (concat "\"" destination-file-with-path1 "\"")))

    (cl-labels ((paste-image-from-clipboard-to-file-with-imagemagick (destination-file-with-path)
                  "Paste image from clipboard fo file DESTINATION-FILE-WITH-PATH with ImageMagick.
(v1, available in occisn/emacs-utils GitHub repository + adaptations)"
                  (unless (my-init--file-exists-p *imagemagick-convert-program*)
                    (error "Unable to paste image from clipboard to file, since *imagemagick-convert-program* does not contain valid content: %s" *imagemagick-convert-program*))
                  (let ((cmd (concat "\"" imagemagick-convert-program "\" " "clipboard: " destination-file-with-path)))
                    (message "Pasting image from clipboard to %s with ImageMagick." destination-file-with-path)
                    (call-process-shell-command cmd)))) ; end of labels definition
      
      (paste-image-from-clipboard-to-file-with-imagemagick destination-file-with-path2)
      (revert-buffer)                               ; update dired
      (dired-goto-file destination-file-with-path1) ; cursor on created file
      (message "Image in clipboard pasted to %s" destination-file-with-path1))))

;;; end
