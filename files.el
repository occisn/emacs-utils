;;; -*- lexical-binding: t; -*-

(defun my/copy-file-here ()
   "Copy file as another file (adding ' (2)' at the end) in same dired folder.
(v1, available in occisn/emacs-utils GitHub repository)"
   (interactive)
   (when (not (string-equal major-mode "dired-mode"))
     (error "Trying to copy file when not in dired-mode."))
   (when (> (length (dired-get-marked-files)) 1)
     (error "Trying to copy several files in same folder."))
   (let* ((current-path-and-name (car (dired-get-marked-files)))
	  (current-path (file-name-directory current-path-and-name))
          (current-name (file-name-nondirectory current-path-and-name))
	  (suggested-new-name (concat (file-name-sans-extension current-name) " (2)." (file-name-extension current-name)))
	  (new-name (read-string "Copy into: " suggested-new-name))
          (new-path-and-name (concat current-path new-name)))
     (message "Copying %s into %s within %s." current-name new-name current-path) 
     (copy-file current-path-and-name new-path-and-name)
     (revert-buffer)                     ; to update dired
     (dired-goto-file new-path-and-name) ; cursor on new file
     ))

(defun my/list-big-files-in-current-directory-and-subdirectories ()
  "List big files in current dired directory and its sub-directories.
The list is printed on a separate buffer.
Requires 'f' package.
(v1, available in occisn/emacs-utils GitHub repository)"
  (interactive)
  (unless (string-equal major-mode "dired-mode")
    (error "Not in dired-mode."))
  (cl-labels ((file-size-Mo (filename)
                "Return file size of FILENAME in Mo.
(v1, available in occisn/elisp-utils GitHub repository)"
                (round
                 (/
                  (file-attribute-size
                   (file-attributes filename))
                  1000000)))
              (add-number-grouping (number &optional separator)
                "Return a string corresponding to NUMBER, which each 3-digit group separated by SEPARATOR, by default a comma.
For instance: 123456 as a number--> 123,456 as a string
(v1, available in occisn/elisp-utils GitHub repository)"
                (let ((num (number-to-string number))
	              (op (or separator ",")))
                  (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
                    (setq num (concat 
		               (match-string 1 num) op
		               (match-string 2 num))))
                  num))) ; end of function definitions within cl-labels
    (let ((root default-directory)
	  (size0 (string-to-number (read-string "Minimal size in Mo (default = 50): " "" nil "50")))
	  (results-buffer (generate-new-buffer "*Big files*"))
	  (list1 nil)
	  (list2 nil)
          (start-time (current-time)))
      (f-files root
	       (lambda (file)
	         (when (> (file-size-Mo file) size0)
		   (push (cons file (file-size-Mo file)) list1))
	         nil)
	       t)
      (setq list2 (sort list1 (lambda (a b) (> (cdr a) (cdr b)))))
      (switch-to-buffer results-buffer)
      (newline)
      (insert (format "In %.3f seconds...\n" (float-time (time-since start-time))))
      (newline)
      (cond ((> (length list2) 1)
             (insert (format "%s files found > %s Mo:\n\n" (length list2) size0)))
            ((= (length list2) 1)
             (insert (format "1 file found > %s Mo:\n\n" size0)))
            (t (insert (format "0 file found > %s Mo.\n" size0))))
      (dolist (x list2)
        (let ((start (point)))
          (insert "DIRED")
          (make-text-button start (point)
                            'action (lambda (_button)
                                      (dired (file-name-directory (car x)))
                                      (dired-goto-file (car x))) 
                            'follow-link t
                            'face '(:box (:line-width 2 :color "gray50" :style released-button)
                                         :background "lightgray"
                                         :foreground "black"
                                         :weight bold)
                            'mouse-face '(:box (:line-width 2 :color "gray30" :style pressed-button)
                                               :background "darkgray"
                                               :foreground "black"
                                               :weight bold)
                            'help-echo "Click this button")) ; end of let (button)
        (insert (format " %s Mo = %s\n" (add-number-grouping (cdr x)) (car x))))) ; end of insert
    (goto-char (point-min))))

(defun my/list-directories-with-many-files-or-direct-subdirectories ()
  "List directories with many files or direct sub-directories.
The list is printed on a separate buffer.
Requires 'f' package.
(v1, available in occisn/emacs-utils GitHub repository)"
  (interactive)
  (unless (string-equal major-mode "dired-mode")
    (error "Not in dired-mode."))
  (cl-labels ((nb-of-elements-in-directory (folder)
                "Return number of elements in FOLDER, including sub-folders (no recursive investigation of subdirectories).
(v1, available in occisn/elisp-utils GitHub repository)"
                (- (length (directory-files folder)) 2))
              (add-number-grouping (number &optional separator)
                "Return a string corresponding to NUMBER, which each 3-digit group separated by SEPARATOR, by default a comma.
For instance: 123456 as a number--> 123,456 as a string
(v1, available in occisn/elisp-utils GitHub repository)"
                (let ((num (number-to-string number))
	              (op (or separator ",")))
                  (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
                    (setq num (concat 
		               (match-string 1 num) op
		               (match-string 2 num))))
                  num))) ; end of functions definitions within cl-labels
    (let ((root default-directory)
	  (nb0 (string-to-number (read-string "Minimal number (default = 100): " "" nil "100")))
	  (results-buffer (generate-new-buffer "*Folders with many files or direct subdirectories*"))
	  (list1 nil)
	  (list2 nil)
          (start-time (current-time)))
      (f-directories root
		     (lambda (folder)
		       (when (> (nb-of-elements-in-directory folder) nb0)
		         (push (cons folder (nb-of-elements-in-directory folder)) list1))
		       nil)
		     t)
      (setq list2 (sort list1 (lambda (a b) (> (cdr a) (cdr b)))))
      (switch-to-buffer results-buffer)
      (newline)
      (insert (format "In %.3f seconds...\n" (float-time (time-since start-time))))
      (newline)
      (cond ((> (length list2) 1)
             (insert (format "%s directories found with more than %s files or direct subdirectories:\n\n" (length list2) nb0)))
            ((= (length list2) 1)
             (insert (format "1 directory found with more than %s files or direct subdirectories:\n\n" nb0)))
            (t (insert (format "0 directory found with more than %s files or direct subdirectories.\n" nb0))))
      (dolist (x list2)
        (let ((start (point)))
          (insert "DIRED")
          (make-text-button start (point)
                            'action (lambda (_button)
                                      (dired (car x))) 
                            'follow-link t
                            'face '(:box (:line-width 2 :color "gray50" :style released-button)
                                         :background "lightgray"
                                         :foreground "black"
                                         :weight bold)
                            'mouse-face '(:box (:line-width 2 :color "gray30" :style pressed-button)
                                               :background "darkgray"
                                               :foreground "black"
                                               :weight bold)
                            'help-echo "Click this button")) ; end of let (button)
        (insert (format " %s = %s" (add-number-grouping (cdr x)) (car x)))
        (newline))
      (goto-char (point-min)))))

(defun my/list-directories-of-big-size ()
  "List directories of big size.
The list is printed on a separate buffer.
Requires 'f' package.
(v1, available in occisn/emacs-utils GitHub repository)"
  (interactive)
  (when (not (string-equal major-mode "dired-mode"))
    (error "Not in dired-mode."))
  (let ((root default-directory)
	(minimal-size (string-to-number (read-string "Minimal size in Mo (default = 100): " "" nil "100")))
	(results-buffer (generate-new-buffer "*Folders with big size*"))
	(list1 nil)
	(list2 nil)
        (start-time (current-time)))
    (cl-labels ((file-size-o (filename)
                  "Return file size of FILENAME in o.
(derived from v1, available in occisn/elisp-utils GitHub repository)"
                  (file-attribute-size (file-attributes filename)))
                (list-size-of-directory-and-subdirectories (current-root)
                  "Compute size of CURRENT-ROOT directory by adding size of its files and recursively examining size of subdirectories.
During this process, each time a directory size exceeds MINIMAL-SIZE (bound in englobing environment), add this directory to LIST1 (bound in englobing environment)."
                  (let ((size 0)
                        (files (f-files current-root))
                        (subdirectories (f-directories current-root)))
                    (dolist (file1 files)
                      (setq size (+ size (file-size-o file1))))
                    (dolist (subdir1 subdirectories)
                      (setq size (+ size (list-size-of-directory-and-subdirectories subdir1))))
                    (when (> size (* 1000000 minimal-size))
                      (push (cons current-root (round (/ size 1000000))) list1))
                    size))
                (add-number-grouping (number &optional separator)
                  "Return a string corresponding to NUMBER, which each 3-digit group separated by SEPARATOR, by default a comma.
For instance: 123456 as a number--> 123,456 as a string
(v1, available in occisn/elisp-utils GitHub repository)"
                  (let ((num (number-to-string number))
	                (op (or separator ",")))
                    (while (string-match "\\(.*[0-9]\\)\\([0-9][0-9][0-9].*\\)" num)
                      (setq num (concat 
		                 (match-string 1 num) op
		                 (match-string 2 num))))
                    num))) ; end of functions definition within cl-labels
      (list-size-of-directory-and-subdirectories root)
      (setq list2 (sort list1 (lambda (a b) (> (cdr a) (cdr b)))))
      (switch-to-buffer results-buffer)
      (newline)
      (insert (format "In %.3f seconds...\n" (float-time (time-since start-time))))
      (newline)
      (cond ((> (length list2) 1)
             (insert (format "%s directories weighing more than %s Mo:\n\n" (length list2) minimal-size)))
            ((= (length list2) 1)
             (insert (format "1 directory weighing more than %s Mo:\n\n" minimal-size)))
            (t (insert (format "0 directory weighing more than %s Mo.\n" minimal-size))))
      (dolist (x list2)
        (let ((start (point)))
          (insert "DIRED")
          (make-text-button start (point)
                            'action (lambda (_button)
                                      (dired (car x))) 
                            'follow-link t
                            'face '(:box (:line-width 2 :color "gray50" :style released-button)
                                         :background "lightgray"
                                         :foreground "black"
                                         :weight bold)
                            'mouse-face '(:box (:line-width 2 :color "gray30" :style pressed-button)
                                               :background "darkgray"
                                               :foreground "black"
                                               :weight bold)
                            'help-echo "Click this button"))
        (insert (format " %s Mo = %s\n" (add-number-grouping (cdr x)) (car x))))
      (goto-char (point-min)))))

(defun my/list-directories-containing-zip-files ()
  "List directories with ZIP files.
The list is printed on a separate buffer.
Directories listed in ALREADY-OK-FOLDERS list are not investigated.
Requires 'f' package.
(v1, available in occisn/emacs-utils GitHub repository)"
  (interactive)
  (when (not (string-equal major-mode "dired-mode"))
    (error "Trying to perform an my/list-directories-containing-zip-files when not in dired-mode."))
  (let ((already-OK-folders nil)
        (root default-directory)
	(results-buffer (generate-new-buffer "*Folders with ZIP files*"))
	(suspect-folders nil)
	(sorted-suspect-folders nil)
        (start-time (current-time)))
    (cl-labels ((string-suffix-p (suffix str &optional ignore-case)
                  "Return t if STR finished by SUFFIX.
Ignore case.
(v1, available in occisn/elisp-utils GitHub repository)
Source: https://stackoverflow.com/questions/22403751/check-if-a-string-ends-with-a-suffix-in-emacs-lisp" 
                  (let ((begin2 (- (length str) (length suffix)))
                        (end2 (length str)))
                    (when (< begin2 0) (setq begin2 0))
                    (eq t (compare-strings suffix nil nil
                                           str begin2 end2
                                           ignore-case))))
                (file-size-Mo (filename)
                  "Return file size of FILENAME in Mo.
(v1, available in occisn/elisp-utils GitHub repository)"
                  (round
                   (/ (file-attribute-size
                       (file-attributes filename))
                      1000000)))) ; end of functions definition within cl-labels
      (f-directories root
		     (lambda (folder)
                       (let ((zip-files-and-sizes nil)
                             (sorted-zip-files-and-sizes nil)
                             (biggest-zip-file-and-size nil))
                         (f-files folder
	                          (lambda (file)
                     	            (when (string-suffix-p ".zip" file)
		                      (push (cons file (file-size-Mo file)) zip-files-and-sizes))
	                            nil)
	                          nil ; not recursive
                                  ) 
                         (when (not (null zip-files-and-sizes))
                           (setq sorted-zip-files-and-sizes (sort zip-files-and-sizes (lambda (a b) (> (cdr a) (cdr b)))))
                           (setq biggest-zip-file-and-size (car sorted-zip-files-and-sizes))
                           (if (cl-loop for suffix in already-OK-folders
                                        always (not (string-suffix-p suffix folder)))
                               (push (list folder (cdr biggest-zip-file-and-size) (car biggest-zip-file-and-size)) suspect-folders)
                             (message "ZIP file in %s but skipped" folder)))) ; if
		       nil)
		     t)
      (setq sorted-suspect-folders (sort suspect-folders (lambda (a b) (> (cadr a) (cadr b)))))
      (switch-to-buffer results-buffer)
      (newline)
      (insert (format "In %.3f seconds...\n" (float-time (time-since start-time))))
      (newline)
      (cond ((> (length sorted-suspect-folders) 1)
             (insert (format "%s directories found with ZIP file(s):\n\n" (length sorted-suspect-folders))))
            ((= (length sorted-suspect-folders) 1)
             (insert (format "1 directory found with ZIP file(s):\n\n")))
            (t (insert (format "0 directory found with ZIP file(s).\n"))))
      
      (dolist (x sorted-suspect-folders)
        (insert (format "%s Mo zip in %s" (cadr x) (car x)))
        (newline)
        (insert "      ")
        (let ((start (point)))
          (insert "DIRED")
          (make-text-button start (point)
                            'action (lambda (_button)
                                      (dired (file-name-directory (caddr x)))
                                      (dired-goto-file (caddr x))) 
                            'follow-link t
                            'face '(:box (:line-width 2 :color "gray50" :style released-button)
                                         :background "lightgray"
                                         :foreground "black"
                                         :weight bold)
                            'mouse-face '(:box (:line-width 2 :color "gray30" :style pressed-button)
                                               :background "darkgray"
                                               :foreground "black"
                                               :weight bold)
                            'help-echo "Click this button"))
        (insert (format " %s\n" (file-name-nondirectory (caddr x)))))
      (goto-char (point-min)))))

(cl-defun my/find-files-with-same-size-in-same-subdirectory (&optional (epsilon 0))
  "From current directory, and recursively, list files with same size in same subdirectory.
Presents the results as a dired buffer.
(v2, available in occisn/emacs-utils GitHub repository; v1 as of December 21th, 2021)"
  (interactive)
  (when (not (string-equal major-mode "dired-mode"))
    (error "Trying to perform my/find-files-with-same-size-in-same-subdirectory when not in dired-mode."))
  (cl-labels ((insert-directories-in-file-list (files)
                "Take a list of files, and return the same list with directories intertwined.
For instance :
d1/a.org d1/b.org d2/c.org d3/d.org
-->
d1/ d1/a.org d1/b.org d2/ d2/c.org d3/ d3/d.org
(v1, available in occisn/elisp-utils GitHub repository)"
                (let ((current-dir "")
	              (files-intertwined-with-directories nil))
                  (cl-loop for filename in files
	                   for dir1 = (file-name-directory filename)
	                   do (progn
		                (when (not (string= current-dir dir1))
		                  (push dir1 files-intertwined-with-directories)
		                  (setq current-dir dir1))
		                (push filename files-intertwined-with-directories)))
                  (reverse files-intertwined-with-directories)))) ; end of functions definition within cl-labels
    (let ((start-time (current-time)))
      (aprogn
       ;; start with current dired directory:
       default-directory
       ;; obtain a list of all files and subdirectories, recursively:
       (directory-files-recursively it "" t)
       ;; only keep directories:
       (cl-remove-if-not #'file-directory-p it)
       ;; list all suspect files:
       (cl-loop for directory in it append
                ;; for each directory...
                (aprogn
                 ;; list of files and directories within subdirectory:
                 (directory-files directory t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")
                 ;; remove directories:
                 (cl-remove-if-not #'file-regular-p it)
                 ;; sort by file size:
                 (cl-sort it '< :key #'(lambda (file) (file-attribute-size (file-attributes file))))
                 ;; list of suspects in current directory:
                 (cl-loop for (file1 file2) on it by #'cdr while file2
                          for size1 = (file-attribute-size (file-attributes file1))
                          for size2 = (file-attribute-size (file-attributes file2))
                          when (<= (abs (- size1 size2)) epsilon)
                          append (list file1 file2))))
       ;; intertwin directories within list of suspect files:
       (insert-directories-in-file-list it)
       ;; print nb of suspects and list under dired form:
       (progn
         (message "%s suspects found in %.3f seconds" (length it) (float-time (time-since start-time)))
         (dired (cons "Results (same size within same subdirectory):" it)))))))

;;; end
