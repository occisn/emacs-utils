;;; -*- lexical-binding: t; -*-

(defun my/org-copy-link-or-inline-code-or-verbatim-or-block ()
   "Copy link, inline code between = or ~ signs in org-mode, or content of org block.
Fallback : current word.
(v1, available in occisn/emacs-utils GitHub repository)"
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
       (let ((element (org-element-context)))
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

;;; end
