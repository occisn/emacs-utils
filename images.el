;;; -*- lexical-binding: t; -*-

(defun my/paste-image-from-clipboard-to-file-with-imagemagick (destination-file-with-path &optional wait-p)
  "Paste image from clipboard fo file DESTINATION-FILE-WITH-PATH with ImageMagick.
If WAIT-P is true, wait for the command to be executed before returning (synchroneous). Otherwise, return immediately (asynchroneous).
(v1, available in occisn/emacs-utils GitHub repository + adaptation)"
  (let* ((imagemagick-convert-program "c:/.../convert.exe")
         (cmd (concat "\"" imagemagick-convert-program "\" " "clipboard: " destination-file-with-path)))
    (message "Pasting image from clipboard to %s with ImageMagick." destination-file-with-path)
    (if wait-p
        (call-process-shell-command cmd) ; synchroneous
      (call-process-shell-command cmd nil 0) ; asynchroneous
      )))

;;; end
