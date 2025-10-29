;;; -*- lexical-binding: t; -*-

;;; === COPY BLOCK

(defun my/org-copy-link-or-inline-code-or-verbatim-or-block ()
  "Copy link, inline code between = or ~ signs in org-mode, or content of org block.
Fallback : current word.
(v2, available in occisn/emacs-utils GitHub repository)
(v1 around Sept.-Oct 2025)"
  (interactive)
  (let ((found nil))
     
    ;; (1) inline code?
    (let ((start-pos (point)))
      (save-excursion
        ;; Find the opening = before cursor
        (when (re-search-backward "=" (line-beginning-position) t)
          (let ((open-pos (point)))
            (goto-char start-pos)
            (when (re-search-forward "=" (line-end-position) t)
              (let ((close-pos (point)))
                (when (and (> start-pos open-pos) (< start-pos close-pos))
                  (let ((content (buffer-substring-no-properties (1+ open-pos) (1- close-pos))))
                    (when (and (> (length content) 0)
                               (not (string-match-p "\n" content)))
                      (kill-new content)
                      (setq found t)
                      (message "Copied inline code: %s" content))))))))))
     
    ;; (2) inline verbatim?
    (unless found
      (let ((start-pos (point)))
        (save-excursion
          ;; Find the opening = before cursor
          (when (re-search-backward "~" (line-beginning-position) t)
            (let ((open-pos (point)))
              (goto-char start-pos)
              (when (re-search-forward "~" (line-end-position) t)
                (let ((close-pos (point)))
                  (when (and (> start-pos open-pos) (< start-pos close-pos))
                    (let ((content (buffer-substring-no-properties (1+ open-pos) (1- close-pos))))
                      (when (and (> (length content) 0)
                                 (not (string-match-p "\n" content)))
                        (kill-new content)
                        (setq found t)
                        (message "Copied inline verbatim: %s" content)))))))))))
     
    ;; (3) org block?
    (unless found
      (let ((content-begin nil)
            (content-end nil)
            (element (org-element-context)))
        ;; (message "element = %s" element)
        ;; (message "(org-element-type element) = %s" (org-element-type element))
        (while (and element
                    (not (memq (org-element-type element)
                               '(src-block example-block export-block quote-block verse-block center-block special-block comment-block))))
          ;; (message "element = %s" element)
          ;; (message "(org-element-type element) = %s" (org-element-type element))
          (setq element (org-element-property :parent element)))
        (when element
          (save-excursion
            (goto-char (org-element-property :begin element))
            (forward-line 1)
            (setq content-begin (point))
            (re-search-forward "^#\\+END_" (org-element-property :end element) t)
            (beginning-of-line)
            (setq content-end (point))
            (kill-ring-save content-begin content-end)
            (setq found t)
            (message "Org block content copied.")))))

    ;; (4) Link in org-mode ?
    (unless found
      (let ((url (thing-at-point 'url t)))
        (when url
          (setq found t)
          (kill-new url)
          (message "Copied link: %s" url))))

    ;; (5) Word ?
    (unless found
      (set found (my/copy-word)))
     
    (unless found
      (message "No word, link, inline code, verbatim text or block found at point."))))

;;; === COPY TO CLIPBOARD

(defun my/save-region-as-html (temp-file-name)
   "Save region, otherwise buffer to HTML file.
(v1, available in occisn/emacs-utils GitHub repository)"
   (let* ((regionp (region-active-p))
          (beg (and regionp (region-beginning)))
          (end (and regionp (region-end)))
	  (buf (current-buffer)))
     (when (file-exists-p temp-file-name) (delete-file temp-file-name))
     (with-temp-buffer
       (insert-buffer-substring buf beg end)
       (my-init--html-add-b-u-i-br-tags)
       (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>")
       (insert "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
       (insert "<html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">")
       (insert "<html>")
       (insert "<head>")
       (insert "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>")
       (insert "</head>")
       (insert "<body>")
       ;;(insert "<pre white-space=\"-moz-pre-wrap\">")
       (insert "<font face='Calibri'>") ; size=\"-1\" 
       (insert "<div style=\"white-space: pre-wrap;\">")
       (insert "<div style=\"font-size:14.5px\">") ; to have Calibri 11
       (goto-char (point-max))
       (insert "</div>")               ; font-size
       (insert "</div>")               ; white-space
       (insert "</font>")
       ;;(insert "</pre>")
       (insert "</body>")
       (insert "</html>")
       ;; (set-buffer-file-coding-system 'utf-8)

       ;; (2) Save buffer as temporary HTML file
       (write-file temp-file-name)
       (kill-buffer))))

;;; === PASTE CLIPBOARD

(defun my/paste-clipboard-as-raw-html ()
  "Insert raw HTML from the Windows clipboard (CF_HTML) into current buffer, with visible tags.
(v1 as of 2025-10-28, available in occisn/emacs-utils GitHub repository)"
  (interactive)
  (let* ((html-raw
          (with-temp-buffer
            (call-process
             "powershell.exe" nil t nil
             "-NoProfile" "-Command"
             "Add-Type -AssemblyName System.Windows.Forms; [System.Windows.Forms.Clipboard]::GetText([System.Windows.Forms.TextDataFormat]::Html)")
            (buffer-string)))
         ;; Extract fragment between <!--StartFragment--> and <!--EndFragment-->
         (fragment
          (if (string-match "<!--StartFragment-->(.*)<!--EndFragment-->" html-raw)
              (match-string 1 html-raw)
            html-raw)))
    (insert fragment)))
;; proposed by AI with personal supervision

(defun my/html-to-org (html)
  "Convert HTML string to org-mode format.
(v1 as of 2025-10-29, available in occisn/elisp-utils GitHub repository)"
  (with-temp-buffer
    (insert html)
    ;; Convert common HTML elements to org-mode
    (goto-char (point-min))
    
    ;; Headers (h1-h6)
    (dolist (level '(6 5 4 3 2 1))
      (goto-char (point-min))
      (let ((stars (make-string level ?*)))
        (while (re-search-forward (format "<h%d[^>]*>\\(.*?\\)</h%d>" level level) nil t)
          (replace-match (format "%s \\1" stars)))))
    
    ;; Bold
    (goto-char (point-min))
    (while (re-search-forward "<\\(b\\|strong\\)[^>]*>\\(.*?\\)</\\(b\\|strong\\)>" nil t)
      (replace-match "*\\2*"))
    
    ;; Italic
    (goto-char (point-min))
    (while (re-search-forward "<\\(i\\|em\\)[^>]*>\\(.*?\\)</\\(i\\|em\\)>" nil t)
      (replace-match "/\\2/"))
    
    ;; Code
    (goto-char (point-min))
    (while (re-search-forward "<code[^>]*>\\(.*?\\)</code>" nil t)
      (replace-match "~\\1~"))
    
    ;; Links
    (goto-char (point-min))
    (while (re-search-forward "<a[^>]*href=\"\\([^\"]+\\)\"[^>]*>\\(.*?\\)</a>" nil t)
      (replace-match "[[\\1][\\2]]"))
    
    ;; Unordered lists
    (goto-char (point-min))
    (while (re-search-forward "<li[^>]*>\\(.*?\\)</li>" nil t)
      (replace-match "- \\1"))
    
    ;; Paragraphs (add newlines)
    (goto-char (point-min))
    (while (re-search-forward "<p[^>]*>\\(.*?\\)</p>" nil t)
      (replace-match "\\1\n"))
    
    ;; Line breaks
    (goto-char (point-min))
    (while (re-search-forward "<br[^>]*>" nil t)
      (replace-match "\n"))
    
    ;; Remove remaining HTML tags
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match ""))
    
    ;; Decode HTML entities
    (goto-char (point-min))
    (while (re-search-forward "&nbsp;" nil t)
      (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward "&amp;" nil t)
      (replace-match "&"))
    (goto-char (point-min))
    (while (re-search-forward "&lt;" nil t)
      (replace-match "<"))
    (goto-char (point-min))
    (while (re-search-forward "&gt;" nil t)
      (replace-match ">"))
    (goto-char (point-min))
    (while (re-search-forward "&quot;" nil t)
      (replace-match "\""))
    
    ;; Clean up extra whitespace
    (goto-char (point-min))
    (while (re-search-forward "\n\n\n+" nil t)
      (replace-match "\n\n"))
    
    (string-trim (buffer-string))))
;; proposed by AI with personal supervision

(defun my/paste-from-Teams-Word-as-org ()
  "Paste from clipboard and convert to org-mode format.
Content of the clipboard may come from Microsoft Teams or Word.
Does not work as such from Thunderbird. Not tested from Gmail.
Requires my/html-to-org.
(v1 as of 2025-10-29, available in occisn/emacs-utils GitHub repository)"
  (interactive)
  (let* ((powershell-cmd "Add-Type -AssemblyName System.Windows.Forms; [System.Windows.Forms.Clipboard]::GetText([System.Windows.Forms.TextDataFormat]::Html)")
         (html-content (shell-command-to-string 
                        (format "powershell.exe -Command \"%s\"" powershell-cmd))))
    (if (string-empty-p (string-trim html-content))
        (message "No HTML content in clipboard")
      ;; Extract the actual HTML fragment (Windows clipboard includes metadata)
      (let* ((fragment-start (string-match "<!--StartFragment-->" html-content))
             (fragment-end (string-match "<!--EndFragment-->" html-content))
             (html (if (and fragment-start fragment-end)
                       (substring html-content 
                                  (+ fragment-start (length "<!--StartFragment-->"))
                                  fragment-end)
                     html-content))
             (org-content (my/html-to-org html)))
        (insert org-content)))))
;; proposed by AI with personal supervision

;;; end
