;;; -*- lexical-binding: t; -*-

(defun my/insert-string-in-clipboard (str)
   "Insert STR (a string) in clipboard.
(v1, available in occisn/emacs-utils GitHub repository)"
   (with-temp-buffer
     (insert str)
     (clipboard-kill-region (point-min) (point-max))))

;;; paste image from clipboard to file: see `images.el`

;;; end
