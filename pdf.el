;;; -*- lexical-binding: t; -*-

(defun my/pdf-burst ()
  "Bursts PDF file on dired line.
(v2, available in occisn/emacs-utils GitHub repository)"
  (interactive)
  (when (not (string-equal major-mode "dired-mode"))
    (error "Trying to burst a PDF file when not in dired-mode."))
  (when (> (length (dired-get-marked-files)) 1)
    (error "Trying to burst several files."))
  (cl-labels ((replace-linux-slash-with-two-windows-slashes (path)
                "Return PATH string after having replaced slashes by two backslashes.
For instance: abc/def --> abc\\def"
                (replace-regexp-in-string  "/" "\\\\" path)))
    (let* ((pdftk-program-name "PDFtk Server")
           (pdftk-program "c:/.../PDFTKBuilderPortable/App/pdftkbuilder/pdftk.exe")
           (files-list (dired-get-marked-files))
	   (file-full-name (car files-list))
	   (file-full-name-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes file-full-name))
	   (file-directory (file-name-directory file-full-name))
	   (file-name (file-name-nondirectory file-full-name))
	   (file-name-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes file-name))
	   ;; (file-name-without-extension (file-name-base file-full-name))
	   (output-directory file-directory)
	   (output-directory-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes output-directory))
	   (cmd (concat "\"" pdftk-program "\""
		        " "
		        "\"" file-full-name-slash-OK-accents-OK "\""
		        " burst output "
		        "\"" output-directory-slash-OK-accents-OK "page_%03d_of_" file-name-slash-OK-accents-OK "\""))
           ;; Example of cmd line: "c:/Users/.../PDFTKBuilderPortable/App/pdftkbuilder/pdftk.exe" "c:/Users/.../Downloads/test.pdf" burst output "c:/Users/.../Downloads/page_%03d_of_XYZ.pdf"
           )
      (when (not (or (string= (file-name-extension file-full-name) "pdf")
                     (string= (file-name-extension file-full-name) "PDF")))
        (error "Trying to burst a non-PDF file: %S" file-full-name))
      (message "Bursting (splitting) PDF file %S with %s" file-name pdftk-program-name)
      (call-process-shell-command cmd nil t)
      (revert-buffer))))

(defun my/pdf-extract ()
  "Extracts pages from a PDF file on dired line.
(v2, available in occisn/emacs-utils GitHub repository)"
  (interactive)
  (when (not (string-equal major-mode "dired-mode"))
    (error "Trying to extract from a PDF file when not in dired-mode."))
  (when (> (length (dired-get-marked-files)) 1)
    (error "Trying to extract from several files."))
  (cl-labels ((replace-linux-slash-with-two-windows-slashes (path)
                "Return PATH string after having replaced slashes by two backslashes.
For instance: abc/def --> abc\\def"
                (replace-regexp-in-string  "/" "\\\\" path)))
    (let* ((pdftk-program-name "PDFtk Server")
           (pdftk-program "c:/.../PDFTKBuilderPortable/App/pdftkbuilder/pdftk.exe")
           (first-page (read-string "First page: "))
	   (last-page (read-string "Last page: "))
	   (files-list (dired-get-marked-files))
	   (file-full-name (car files-list))
	   (file-full-name-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes file-full-name))
	   (file-directory (file-name-directory file-full-name))
	   (file-name (file-name-nondirectory file-full-name))
	   (file-name-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes file-name))
	   ;; (file-name-without-extension (file-name-base file-full-name))
	   (output-directory file-directory)
	   (output-directory-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes output-directory))
	   (cmd (concat "\"" pdftk-program "\""
		        " "
		        "A=\"" file-full-name-slash-OK-accents-OK "\""
		        " cat A" first-page "-" last-page " output "
		        "\"" output-directory-slash-OK-accents-OK "page_" first-page "_to_" last-page "_of_" file-name-slash-OK-accents-OK "\""))
           ;; Example of cmd line: "c:/Users/.../PDFTKBuilderPortable/App/pdftkbuilder/pdftk.exe" A="c:/Users/.../Downloads/test.pdf" cat A2-4 output "c:/Users/.../Downloads/page_2_to_4_of_XYZ.pdf"
           )
      (when (not (or (string= (file-name-extension file-full-name) "pdf")
                     (string= (file-name-extension file-full-name) "PDF")))
        (error "Trying to extract pages from a non-PDF file: %S" file-full-name))
      (message "Extracting page(s) %s to %s of PDF file %S with %s" first-page last-page file-name pdftk-program-name)
      (call-process-shell-command cmd nil t)
      (revert-buffer))))

(defun my/pdf-join ()
  "Concatenates PDF files
Caution: no accent in file names
If necessary : M-x read-only-mode
(v1, available in occisn/emacs-utils GitHub repository)"
  (interactive)
  (when (not (string-equal major-mode "dired-mode"))
    (error "Trying to extract from a PDF file when not in dired-mode."))
  (cl-labels ((replace-linux-slash-with-two-windows-slashes (path)
                "Return PATH string after having replaced slashes by two backslashes.
For instance: abc/def --> abc\\def"
                (replace-regexp-in-string  "/" "\\\\" path)))
    (let* ((pdftk-program-name "PDFtk Server")
           (pdftk-program "c:/.../PDFTKBuilderPortable/App/pdftkbuilder/pdftk.exe")
           (output-file (read-string "Output file (default: output.pdf): " nil nil "output.pdf"))
	   (files-list (cl-sort (dired-get-marked-files) 'string-lessp))
	   (first-file-full-name (car files-list))
	   (files-directory (file-name-directory first-file-full-name))
	   (output-directory files-directory)
	   (output-directory-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes output-directory))
	   (cmd (concat "\"" pdftk-program "\""))
           ;; Example of cmd line: "c:/Users/.../PDFTKBuilderPortable/App/pdftkbuilder/pdftk.exe" "c:/Users/.../Downloads/1.pdf" "c:/Users/.../Downloads/2.pdf" cat output "c:/Users/.../Downloads/output.pdf"
           )
      (dolist (file-full-name files-list)
        (when (not (or (string= (file-name-extension file-full-name) "pdf")
                       (string= (file-name-extension file-full-name) "PDF")))
          (error "Trying to join a non-PDF file: %S" file-full-name))
        (let ((file-full-name-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes file-full-name)))
	  (setq cmd (concat cmd " " "\"" file-full-name-slash-OK-accents-OK "\""))))
      (setq cmd (concat cmd " cat output " "\"" output-directory-slash-OK-accents-OK output-file "\""))
      ;;(message "cmd: %s" cmd)
      (message "Joining PDF files with %s" pdftk-program-name)
      (call-process-shell-command cmd nil t)
      (revert-buffer))))

;;; end
