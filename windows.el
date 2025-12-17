;;; -*- lexical-binding: t; -*-

(defun add-to-environment-variable (envt-variable-name prog-name directory)
   "Add DIRECTORY corresponding to PROG-NAME to environment variable ENVT-VARIABLE-NAME. DIRECTORY may have a final slash.
(v1, available in occisn/emacs-utils GitHub repository)"
   (let ((envt-variable-content (getenv envt-variable-name)))
     (if (cl-search directory envt-variable-content)
	 (message "No need to add %s to Windows %s environment variable since already in: %s" prog-name envt-variable-name directory)
       (setenv envt-variable-name (concat directory ";" envt-variable-content))
       (message "%s is added to %s environment variable." prog-name envt-variable-name))))

(defun my/delete-to-recycle-bin (file)
  "Move FILE to Windows Recycle Bin using PowerShell.
Returns t on success, nil on failure.
Note: (setq delete-by-moving-to-trash t) does not seem enough.
(v1, available in occisn/emacs-utils GitHub repository, 2025-12-27)"
  (let* ((file-path (convert-standard-filename file))
         (ps-command (format 
                      "Add-Type -AssemblyName Microsoft.VisualBasic; [Microsoft.VisualBasic.FileIO.FileSystem]::DeleteFile('%s', 'OnlyErrorDialogs', 'SendToRecycleBin')"
                      file-path)))
    (condition-case err
        (progn
          (call-process "powershell.exe" nil nil nil
                        "-NoProfile" "-NonInteractive" "-Command" ps-command)
          (not (file-exists-p file)))
      (error nil))))

(defun my--find-process (process-name)
  "Check if a process with PROCESS-NAME is running on Windows.
Returns t or nil.
Example: (my--find-process 'chrome.exe'
(v1, available in occisn/emacs-utils GitHub repository)"
  (let ((processes (list-system-processes))
        (found nil))
    (dolist (pid processes found)
      (let* ((attrs (process-attributes pid))
             (comm (cdr (assoc 'comm attrs))))
        (when (and comm (string-match-p process-name comm))
          (setq found t))))))

;;; end
