;;; -*- lexical-binding: t; -*-

(defun my/unzip ()
  "Unzip file on dired line.
(v1, available in occisn/emacs-utils GitHub repository)"
  (interactive)
  (when (not (string-equal major-mode "dired-mode"))
    (error "Trying to unzip when not in dired-mode."))
  (when (> (length (dired-get-marked-files)) 1)
    (error "Trying to unzip several files."))
  (cl-labels ((replace-linux-slash-with-two-windows-slashes (path)
                "Return PATH string after having replaced slashes by two backslashes.
For instance: abc/def --> abc\\def"
                (replace-regexp-in-string  "/" "\\\\" path)))
    (let* ((unzip-program "c:/.../7-ZipPortable/App/7-Zip64/7z.exe")
           (files-list (dired-get-marked-files))
	   (file-full-name (car files-list))
	   (file-full-name-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes file-full-name))
	   (file-directory (file-name-directory file-full-name))
	   (file-name (file-name-nondirectory file-full-name))
	   (file-name-without-extension (file-name-base file-full-name))
	   (output-directory
	    (if (y-or-n-p "Create directory?")
	        (concat file-directory (read-string "create directory: " file-name-without-extension))
	      file-directory))
	   (output-directory-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes output-directory))
	   (cmd (concat "\"" unzip-program "\""
		        " x " "\"" file-full-name-slash-OK-accents-OK "\""
		        " -aou -y"
		        " -o" "\"" output-directory-slash-OK-accents-OK "\""))
           ;; Example of cmd line: 7z e "C:\Users\...\Downloads\emacs-26.2.tar" -y -o"C:\Users\...\Downloads\emacs-26.2"
           ;; https://info.nrao.edu/computing/guide/file-access-and-archiving/7zip/7z-7za-command-line-guide
           )
      (message "Unzipping %s by %s..." file-name (file-name-nondirectory unzip-program))
      (message "%s" (shell-command-to-string cmd))
      ;; (call-process-shell-command cmd nil 0)
      (revert-buffer)
      (message "... unzip finished."))))

(defun my/zip-content-of-current-directory ()
  "Zip content of current directory into a zip archive.
(v1, available in occisn/emacs-utils GitHub repository)"
  (interactive)
  (when (not (string-equal major-mode "dired-mode"))
    (error "Trying to zip when not in dired-mode."))
  (cl-labels ((replace-linux-slash-with-two-windows-slashes (path)
                "Return PATH string after having replaced slashes by two backslashes.
For instance: abc/def --> abc\\def"
                (replace-regexp-in-string  "/" "\\\\" path)))
    (let* ((unzip-program "c:/.../7-ZipPortable/App/7-Zip64/7z.exe")
           (current-directory-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes default-directory))
	   (archive-name (concat (read-string "Archive name (without zip suffix): " "archive") ".zip"))
	   (cmd (concat "\"" unzip-program "\""
		        " a -tzip "
		        "\"" current-directory-slash-OK-accents-OK archive-name "\""
		        " "
		        "\"" current-directory-slash-OK-accents-OK "*.*\""
		        " -r"))
           ;; Example of cmd line: "c:\Users\...\7-ZipPortable\App\7-Zip64\7z.exe" a -tzip "c:\Users\...\Downloads\test\archive.zip" "c:\Users\...\Downloads\test\*.*" -r
           ;; https://info.nrao.edu/computing/guide/file-access-and-archiving/7zip/7z-7za-command-line-guide
           )
      (message "Zipping %S with %s" current-directory-slash-OK-accents-OK (file-name-nondirectory unzip-program))
      ;; (call-process-shell-command cmd nil t)
      (message "%s" (shell-command-to-string cmd))
      (revert-buffer)
      (message "... zip finished."))))

(defun my/list-zip-content ()
  "List the content of zip file on dired line.
(v1, available in occisn/emacs-utils GitHub repository)"
  (interactive)
  (when (not (string-equal major-mode "dired-mode"))
    (error "Trying to list content of zip file when not in dired-mode."))
  (when (> (length (dired-get-marked-files)) 1)
    (error "Trying to list the content of several files."))
  (cl-labels ((replace-linux-slash-with-two-windows-slashes (path)
                "Return PATH string after having replaced slashes by two backslashes.
For instance: abc/def --> abc\\def"
                (replace-regexp-in-string  "/" "\\\\" path)))
    (let* ((unzip-program "c:/.../7-ZipPortable/App/7-Zip64/7z.exe")
           (files-list (dired-get-marked-files))
	   (file-full-name (car files-list))
	   (file-full-name-slash-OK-accents-OK (replace-linux-slash-with-two-windows-slashes file-full-name))
	   (file-name (file-name-nondirectory file-full-name))
	   (cmd (concat "\"" unzip-program "\""
		        " l " "\"" file-full-name-slash-OK-accents-OK "\"")))
      (message "Listing content of %s by %s..." file-name (file-name-nondirectory unzip-program))
      ;; (shell-command-to-string cmd)
      (shell-command cmd nil nil)
      (revert-buffer)
      (message "... zip content listing finished."))))

(defun my/zip-add-to-archive-present-in-same-directory () 
  "Add to archive present in same directory.
Attention: overwrite.
(v1, available in occisn/emacs-utils GitHub repository)"
  (interactive)
  (when (not (string-equal major-mode "dired-mode"))
    (error "Trying to add to zip archive when not in dired-mode."))
  (let ((files-to-add (dired-get-marked-files)))
    (message "marked files: %s" files-to-add)
    (when (null files-to-add)
      (error "Trying to add to zip archive but no file selected."))
    (let* ((unzip-program "c:/.../7-ZipPortable/App/7-Zip64/7z.exe")
           (files-to-add-no-path (mapcar #'file-name-nondirectory files-to-add))
	   (all-zip-files (directory-files default-directory nil "\\.zip$"))
	   (potential-zip-files (cl-set-difference all-zip-files files-to-add-no-path :test #'string=)))
      (message "files to add: %s" files-to-add-no-path)
      (message "all zip files: %s" all-zip-files)
      (message "potential zip files: %s" potential-zip-files)
      (when (null all-zip-files)
	(error "Impossible to add to archive since no zip file in the directory."))
      (when (null potential-zip-files)
	(error "Impossible to add to archive since all zip files are in the list of files to add."))
      (let* ((chosen-zip-file
	      (completing-read
	       "Choose archive file: "
	       potential-zip-files))
	     (target (concat default-directory chosen-zip-file))
	     (target-accents-OK target))
	(message "chosen zip file: %s" chosen-zip-file)
	(message "default directory: %s" default-directory)
	(dolist (file-to-add files-to-add)
	  (let* ((file-to-add-accents-OK file-to-add)
		 (cmd
		  (concat "\"" unzip-program "\""
			  " a -aot "
			  "\"" target-accents-OK "\""
			  " "
			  "\"" file-to-add-accents-OK "\""
			  " -r")))
	    (message "- Adding %s to %s" file-to-add target)
	    (message "%s" (shell-command-to-string cmd))
	    (message "... addition OK.")))
	(revert-buffer)
	(message "All additions finished.")))))

;;; end
