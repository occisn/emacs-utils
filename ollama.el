;;; -*- lexical-binding: t; -*-

(require 'url)
(require 'json)
;; Uses built-in url.el and json.el — no external packages needed.

(defun my--ollama-post-json (url payload &optional read-timeout)
  "Send a JSON POST to URL with PAYLOAD (a Lisp object). Return parsed JSON.
READ-TIMEOUT is in seconds (sets `url-request-timeout')."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/json")))
        (url-request-data (encode-coding-string
                           (json-encode payload) 'utf-8))
        ;; url.el doesn't distinguish read/connect timeouts
        (url-timeout (or read-timeout 120)))
    (let ((buf (condition-case nil
                   (url-retrieve-synchronously url t)
                 (error nil))))
      (if (not buf)
          (format "Ollama is not available at %s" url)
        (with-current-buffer buf
          (goto-char (point-min))
          ;; Skip HTTP headers
          (re-search-forward "\n\n" nil t)
          (let ((body (buffer-substring-no-properties (point) (point-max))))
            (kill-buffer)
            (json-read-from-string body)))))))

(defun my/ollama-call1 (prompt &optional model stream)
  "Call Ollama's API with PROMPT and return the response text.
MODEL defaults to \"llama3\".  STREAM defaults to :json-false (off).

Use: (my/ollama-call \"define recursion in three sentences\")"
  (let* ((model  (or model "llama3"))
         (stream (or stream :json-false))
         (url    "http://localhost:11434/api/generate")
         (payload `((model  . ,model)
                    (prompt . ,prompt)
                    (stream . ,stream)))
         (parsed (my--ollama-post-json url payload 120)))
    (if (stringp parsed) parsed
      (alist-get 'response parsed))))

(defun my/ollama-call2 (prompt &optional model stream)
  "Call Ollama's API with PROMPT; return response text and print stats.
MODEL defaults to \"llama3\".  STREAM defaults to :json-false (off).
Prints a progress dot every second while waiting.

Use: (my/ollama-call2 \"define recursion in three sentences\")"
  (let* ((model  (or model "llama3"))
         (stream (or stream :json-false))
         (url    "http://localhost:11434/api/generate")
         (payload `((model  . ,model)
                    (prompt . ,prompt)
                    (stream . ,stream)))
         (url-request-method "POST")
         (url-request-extra-headers
          '(("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string
                            (json-encode payload) 'utf-8))
         (url-timeout 300)
         (done nil)
         (response-buf nil)
         (errored nil))
    (condition-case nil
        (url-retrieve url
                      (lambda (_status)
                        (setq response-buf (current-buffer)
                              done t)))
      (error (setq errored t done t)))
    (while (not done)
      (princ ".")
      (sit-for 1))
    (if errored
        (format "Ollama is not available at %s" url)
      (with-current-buffer response-buf
        (goto-char (point-min))
        (re-search-forward "\n\n" nil t)
        (let* ((body (buffer-substring-no-properties (point) (point-max)))
               (parsed (json-read-from-string body)))
          (kill-buffer)
          (let* ((answer          (alist-get 'response parsed))
                 (total-ns        (alist-get 'total_duration parsed))
                 (prompt-tokens   (alist-get 'prompt_eval_count parsed))
                 (response-tokens (alist-get 'eval_count parsed))
                 (eval-duration   (alist-get 'eval_duration parsed)))
            (princ (format "\n\n--- Stats ---\n"))
            (princ (format "Duration:         %.2fs\n"
                           (/ (float total-ns) 1e9)))
            (princ (format "Prompt tokens:    %d\n" prompt-tokens))
            (princ (format "Response tokens:  %d\n" response-tokens))
            (princ (format "Total tokens:     %d\n"
                           (+ prompt-tokens response-tokens)))
            (when (and eval-duration (> eval-duration 0))
              (princ (format "Speed:            %.1f tokens/s\n"
                             (/ (float response-tokens)
                                (/ (float eval-duration) 1e9)))))
            (princ "\n")
            answer))))))

;;; end
