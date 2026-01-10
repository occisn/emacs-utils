;;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defmacro aprogn (&rest body)
  "Anaphoric progn.
(v1, available in occisn/elisp-utils GitHub repository)"
  `(let*
       ,@(cl-loop for remaining-clauses on body
                  until (<= (length remaining-clauses) 1)
                  collect `(it ,(car remaining-clauses)) into bindings
                  finally (return (list bindings (car remaining-clauses))))))

(defmacro amapcar (form list)
  "Anaphoric mapcar.
(v1, available in occisn/elisp-utils GitHub repository)"
  `(mapcar (lambda (it) ,form) ,list))

(defmacro awhen (test &rest body)
  "Anaphoric when.
(v1, available in occisn/elisp-utils GitHub repository)"
  `(let ((it ,test))
     (when it ,@body)))

(defmacro aif (test clause1 clause2)
  "Anaphoric if.
(v1, available in occisn/elisp-utils GitHub repository)"
  `(let ((it ,test))
     (if it ,clause1 ,clause2))) ; end of init section

;;; end
