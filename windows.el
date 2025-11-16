;;; -*- lexical-binding: t; -*-

(defun add-to-environment-variable (envt-variable-name prog-name directory)
   "Add DIRECTORY corresponding to PROG-NAME to environment variable ENVT-VARIABLE-NAME. DIRECTORY may have a final slash.
(v1, available in occisn/emacs-utils GitHub repository)"
   (let ((envt-variable-content (getenv envt-variable-name)))
     (if (cl-search directory envt-variable-content)
	 (message "No need to add %s to Windows %s environment variable since already in: %s" prog-name envt-variable-name directory)
       (setenv envt-variable-name (concat directory ";" envt-variable-content))
       (message "%s is added to %s environment variable." prog-name envt-variable-name))))

;;; end
