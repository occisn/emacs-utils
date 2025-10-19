;;; -*- lexical-binding: t; -*-

(defun my/insert-ocr-clipboard ()
  "Insert in current buffer the result of OCR performed on clipboard content (which is supposed to be a snapshot of text).
Uses Tesseract and ImageMagick.
(v1, available in occisn/emacs-utils GitHub repository)"
  (interactive)

  (let ((imagemagick-convert-program "c:/.../ImageMagick-7.0.11-4-portable-Q16-x64/convert.exe"))

    (cl-labels ((paste-image-from-clipboard-to-file-with-imagemagick (destination-file-with-path)
                  "Paste image from clipboard fo file DESTINATION-FILE-WITH-PATH with ImageMagick.
(v1, available in occisn/emacs-utils GitHub repository + adaptations)"
                  (let ((cmd (concat "\"" imagemagick-convert-program "\" " "clipboard: " destination-file-with-path)))
                    (message "Pasting image from clipboard to %s with ImageMagick." destination-file-with-path)
                    (call-process-shell-command cmd nil 0)))) ; end of labels function definitions
      
      (let* ((tesseract-exe "C:/.../Tesseract-OCR/tesseract.exe")
             (tesseract-tessdata-dir "C:/.../Tesseract-OCR/tessdata")
             (temp-directory "C:/Users/.../AppData/Local/Temp/")

             (tmp-file-1 (make-temp-file (concat temp-directory "ocr-")))
             (tmp-file-2 (concat (format "%s" tmp-file-1) ".png"))
             (tmp-file-3 (make-temp-file (concat temp-directory "ocr-output-")))
             (tmp-file-4 (concat (format "%s" tmp-file-3) ".txt"))
             (cmd (format "\"%s\" --tessdata-dir \"%s\" \"%s\" \"%s\" -l fra" tesseract-exe tesseract-tessdata-dir tmp-file-2 tmp-file-3)))

        (paste-image-from-clipboard-to-file-with-imagemagick tmp-file-2)
        (unless (file-exists-p tmp-file-2) (sleep-for 0.5))
        (unless (file-exists-p tmp-file-2) (error "File with pasted image does not exist, even after 0.5 s sleep: %s" tmp-file-2))
        (call-process-shell-command cmd nil t)
        (if (file-exists-p tmp-file-4)
            (insert-file-contents tmp-file-4)
          (error "OCR output file does not exist: %s" tmp-file-4))  
        
        (message "OCR of clipboard via Tesseract")

        (when (file-exists-p tmp-file-1) (delete-file tmp-file-1))
        (when (file-exists-p tmp-file-2) (delete-file tmp-file-2))
        (when (file-exists-p tmp-file-4) (delete-file tmp-file-4))))))

(defun my/scanned-pdf-to-txt ()
  "Convert scanned PDF file on dired line to text buffer.
Uses Imagemagick and Tesseract.
(v2, available in occisn/emacs-utils GitHub repository, v1 as of 2024-08-25)"
  (interactive)
  
  (when (not (string-equal major-mode "dired-mode"))
    (error "Scanned pdf to txt: not in dired mode."))
  (when (> (length (dired-get-marked-files)) 1)
    (error "Scanned pdf to txt: more than 1 file has been selected."))

  (cl-labels ((replace-linux-slash-with-two-windows-slashes (path)
                "Return PATH string after having replaced slashes by two backslashes.
For instance: abc/def --> abc\\def"
                (replace-regexp-in-string  "/" "\\\\" path)))
    
    (let* ((imagemagick-convert-program "c:/.../ImageMagick-7.0.11-4-portable-Q16-x64/convert.exe")
           (temp-directory "C:/Users/.../AppData/Local/Temp/")
           (tesseract-exe "C:/.../Tesseract-OCR/tesseract.exe")
           (tesseract-tessdata-dir "C:/.../Tesseract-OCR/tessdata")
           
           (files-list (dired-get-marked-files))
	   (file-full-name (car files-list))
	   (file-full-name-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes file-full-name))
	   (file-directory (file-name-directory file-full-name))
	   (file-name (file-name-nondirectory file-full-name))
	   (file-name-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes file-name))
	   ;; (file-name-without-extension (file-name-base file-full-name))

           (cmd1 (concat "\"" imagemagick-convert-program "\" " "-density 300x300 " "\"" file-full-name-slash-OK-accents-OK "\"" " " temp-directory "zabcd-%03d.jpg"))
           )

      ;; step 1: convert pdf to jpg with ImageMagick
      (message "Scanned pdf to txt STEP 1: convert pdf to jpg with ImageMagick")
      (call-process-shell-command cmd1 nil 0)

      ;; step 2: convert jpg to txt with Tesseract
      (message "Scanned pdf to txt STEP 2: convert jpg to txt with Tesseract")
      
      (let* ((txt-buffer (generate-new-buffer (format "*Text content of scanned pdf file %s (my/scanned-pdf-to-txt x)*" file-name)))
             (image-files (file-expand-wildcards (concat temp-directory "zabcd-*.jpg")))
             (page-count 0))

        (switch-to-buffer txt-buffer)
        
        (dolist (image-file1 image-files)
          
          (setq page-count (+ page-count 1))
          (insert (format "------ PAGE %s -------\n\n" page-count))
          (let* ((tmp-file-3 (make-temp-file (concat temp-directory "ocr-output-")))
                 (tmp-file-4 (concat (format "%s" tmp-file-3) ".txt"))
                 (cmd2 (format "\"%s\" --tessdata-dir \"%s\" \"%s\" \"%s\" -l fra" tesseract-exe tesseract-tessdata-dir image-file1 tmp-file-3)))

            (call-process-shell-command cmd2 nil t)

            (if (file-exists-p tmp-file-4)
                (progn
                  (insert-file-contents tmp-file-4)
                  (goto-char (point-max)))
              
              (error "my/scanned-pdf-to-txt: OCR output file does not exist: %s" tmp-file-4))

            (when (file-exists-p tmp-file-4) (delete-file tmp-file-4)))) ; end of dolist
        
        (goto-char (point-min)))))) ; end of defun

;;; end
