;;; -*- lexical-binding: t; -*-

(cl-defun my/parse-csv-file-to-list (input-file &key (separator ";") (verbose nil))
   "Parse CSV file INPUT-FILE, structured with separator SEPARATOR, and return data as list of lists.
(v1, available in occisn/emacs-utils GitHub repository)"
   (when (null input-file) (error "File to be parsed is nil."))
   (let ((result nil))
     (with-temp-buffer
       (insert-file-contents input-file)
       (while (not (eobp))
         (let* ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
           (push (split-string line separator) result))
         (forward-line 1))
       (when verbose (message "File %s parsed, %s lines found" input-file (length result)))
       (when (null result) (error "Content of file %s is null" input-file))
       (reverse result))))
;; inspired by: https://gist.github.com/syohex/5487731

;; (require 'subr-x) ?
(cl-defun my/write-CSV-to-file (csv-data output-file &key (separator ";") (verbose nil))
   "Write csv data CSV-DATA (presented as list of lists) to file OUTPUT-FILE with separator SEPARATOR.
(v1, available in occisn/emacs-utils GitHub repository)" 
   (with-temp-file output-file
     (cl-loop for line in csv-data
              do (insert (string-join line separator) "\n")))
   (when verbose (message "%s lines written to file %s" (length csv-data) output-file)))

;; (require 'subr-x) ?
(cl-defun my/reconciliate-two-csv-files (&key input-file-1 key-column-1 value-column-1 input-file-2 key-column-2 value-column-2 output-file (separator-1 ";") (separator-2 ";")  (output-separator ";") (account-column-2 nil) (voucher-column-2 nil) (verbose nil))
   "Reconciliate two csv files INPUT-FILE-1 and INPUT-FILE-2 by creating a third csv file OUTPUT-FILE which represents them side by side.
The reconciliation is made according to a key in column KEY-COLUMN-1 and KEY-COLUMN-2 of respective files.
If VALUE-COLUMN-1 and -2 are non nil, the sum of this column is calculated for each key, and the difference is computed. It is supposed to be 0 for perfect reconciliation. Results are presented by decreasing absolute value of the difference.
(v1, available in occisn/emacs-utils GitHub repository)"

   (cl-labels ((excel-column-to-index
                 (column-as-string)
                 "Convert Excel column COLUMN-AS-STRING to integer index, for instance: A --> 0. Does not work for AA and higher."
                 (let ((res (- (string-to-char column-as-string) 65)))
                   (if (<= 0 res 25) res (error "Unable to convert %s to index" column-as-string))))

               (remove-internal-space-in-string
                 (s)
                 "Remove spaces in a string."
                 (concat (seq-remove (lambda (c) (eql c 32)) s))) ;; 32 = space
               
               (string-remove-surrounding-quotes (s)
                 "Remove quotes at the beginning and at the end of a string.
(v1, available in occisn/elisp-utils GitHub repository)"
                 (aprogn
                  s
                  (string-remove-prefix "\"" it)
                  (string-remove-suffix "\"" it)))

               (field-to-float
                 (s)
                 "Convert an Excel field (string) to a float."
                 (aprogn
                  s
                  (string-remove-surrounding-quotes it)
                  (remove-internal-space-in-string it)
                  (subst-char-in-string ?, ?. it)
                  (string-to-number it)))
               
               (float-to-Excel-string2
                 (x)
                 "Convert number as 2.34 to string '2,34' with a comma."
                 (subst-char-in-string ?. ?, (format "%0.2f" x)))

               (string-finish-with-8
                 (s)
                 "Return t if and only if the string finishes with 8."
                 (string-match-p "8\\'" s))

               (parse-csv-file-to-list (input-file &key (separator ";") (verbose nil))
   "Parse CSV file INPUT-FILE, structured with separator SEPARATOR, and return data as list of lists.
(v1, available in occisn/emacs-utils GitHub repository)"
   (when (null input-file) (error "File to be parsed is nil."))
   (let ((result nil))
     (with-temp-buffer
       (insert-file-contents input-file)
       (while (not (eobp))
         (let* ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
           (push (split-string line separator) result))
         (forward-line 1))
       (when verbose (message "File %s parsed, %s lines found" input-file (length result)))
       (when (null result) (error "Content of file %s is null" input-file))
       (reverse result))))

               (write-CSV-to-file (csv-data output-file &key (separator ";") (verbose nil))
                 "Write csv data CSV-DATA (presented as list of lists) to file OUTPUT-FILE with separator SEPARATOR.
(v1, available in occisn/emacs-utils GitHub repository)" 
                 (with-temp-file output-file
                   (cl-loop for line in csv-data
                            do (insert (string-join line separator) "\n")))
                 (when verbose (message "%s lines written to file %s" (length csv-data) output-file)))) ; end of labels definitions
     
     (let* (
            
            ;; (1) parse both files:
            ;; ---------------------
            (filecontent1 (parse-csv-file-to-list input-file-1 :separator separator-1 :verbose verbose))
            (filecontent2 (parse-csv-file-to-list input-file-2 :separator separator-2 :verbose verbose))

            ;; (2) prepare data
            ;; ----------------
            ;; title of columns:
            (titles1 (car filecontent1))
            (titles2 (car filecontent2))
            (merged-titles (append (list "idx") titles1 (list "OK?" "key" "delta") titles2))
            ;; final number of columns:
            (final-nb-of-columns (length merged-titles))
            ;; widths (number of columns):
            (width1 (length titles1))
            (width2 (length titles2))
            ;; key indexes:
            (key-index-1 (excel-column-to-index key-column-1))
            (key-index-2 (excel-column-to-index key-column-2))
            ;; value indexes:
            (delta-p (not (or (null value-column-1) (null value-column-2))))
            (value-index-1 (when delta-p (excel-column-to-index value-column-1)))
            (value-index-2 (when delta-p (excel-column-to-index value-column-2)))
            ;; indexes for account and voucher (in case of pre-treatment:
            (account-index-2 (awhen account-column-2 (excel-column-to-index it)))
            (voucher-index-2 (awhen voucher-column-2 (excel-column-to-index it)))
            ;; data (no header)
            (data1 (cdr filecontent1))
            (data2 (cdr filecontent2))
            ;; check width
            (_useless (progn
                        (cl-loop for line in data1
                                 when (not (= width1 (length line)))
                                 do (error "Inconsistent line lengths in file 1 %s vs %s" (length line) width1))
                        (cl-loop for line in data2
                                 when (not (= width2 (length line)))
                                 do (error "Inconsistent line lengths in file 2 %s vs %s" (length line) width2))))

            ;; (3) Pre-treatment
            ;; -----------------
            ;; if account and voucher are provided, first try to put aside lines with account in -8, common voucher, and sum = 0
            (prov
             (when (not (or (null account-column-2) (null voucher-column-2)))
               (let ((prov% nil)
                     (prov-ht (make-hash-table :test 'equal))
                     (data2-bis nil))
                 (cl-loop for line2 in data2
                          do (let ((key-value2 (string-remove-surrounding-quotes (elt line2 key-index-2)))
                                   (voucher2 (string-remove-surrounding-quotes (elt line2 voucher-index-2)))
                                   (account2 (string-remove-surrounding-quotes (elt line2 account-index-2))))
                               (if (and (string-finish-with-8 account2) (not (string= "" voucher2))) ; (string= "0" key-value2)
                                   (aif (gethash voucher2 prov-ht)
                                        (puthash voucher2 (cons line2 it) prov-ht)
                                        (puthash voucher2 (list line2) prov-ht))
                                 (push line2 data2-bis))))
                 ;; at this stage, prov-ht contains lines with account in -8, common voucher (sum not checked yet)
                 ;;     and data2-bis contains the rest of data2
                 (cl-loop for voucher2 in (hash-table-keys prov-ht)
                          do (let* ((lines2 (gethash voucher2 prov-ht))
                                    (sum0 (apply #'+ (amapcar (field-to-float (elt it value-index-2)) lines2))))
                               (if (= 0 sum0)
                                   (setq prov% (append prov% lines2))
                                 (setq data2-bis (append data2-bis lines2)))))
                 (unless (= (length data2) (+ (length data2-bis) (length prov%)))
                   (error "Inconsistent lengths: data2 (%s) <> data2-bis (%s) + prov (%s)" (length data2) (length data2-bis) (length %prov)))
                 (setq data2 data2-bis)
                 prov%)))
            ;; at this stage, prov contains the line of reconciliated "account in -8, common voucher, and sum = 0"
            ;;    and data2 does not contain them any more
            
            ;; (4) build intermediate delta+lines
            ;; ----------------------
            (delta+lines
             (let ((rest1 (sort data1 (lambda (lstA lstB) (string< (string-remove-surrounding-quotes (elt lstA key-index-1)) (string-remove-surrounding-quotes (elt lstB key-index-1)))))) ; sorted!
                   (rest2 (sort data2 (lambda (lstA lstB) (string< (string-remove-surrounding-quotes (elt lstA key-index-2)) (string-remove-surrounding-quotes (elt lstB key-index-2)))))) ; sorted!
                   (nb-keys 0)
                   (%delta+lines nil))

               (while (or rest1 rest2)

                 (let ((occurences1 nil)
                       (occurences2 nil)
                       (key nil)
                       (sum1 0)
                       (sum2 0))

                   ;; within while 1/2

                   (let ((next-key-1 (string-remove-surrounding-quotes (elt (car rest1) key-index-1)))
                         (next-key-2 (string-remove-surrounding-quotes (elt (car rest2) key-index-2))))

                     (cond ( ; case: nothing on the right or left has no correspondence
                            (or (null rest2) (and (not (null rest1)) (string< next-key-1 next-key-2)))
                            (progn
                              (cl-incf nb-keys)
                              (setq key next-key-1)
                              (while (string= key (string-remove-surrounding-quotes (elt (car rest1) key-index-1)))
                                (when delta-p (cl-incf sum1 (field-to-float (elt (car rest1) value-index-1))))
                                (push (pop rest1) occurences1))))
                           
                           ( ; case: nothing on the left or right has no correspondence
                            (or (null rest1) (and (not (null rest2)) (string< next-key-2 next-key-1)))
                            (progn
                              (cl-incf nb-keys)
                              (setq key next-key-2)
                              (while (string= key (string-remove-surrounding-quotes (elt (car rest2) key-index-2)))
                                (when delta-p (cl-incf sum2 (field-to-float (elt (car rest2) value-index-2))))
                                (push (pop rest2) occurences2))))
                           
                           (        ; case: same key on left and right
                            (string= next-key-1 next-key-2)
                            (progn
                              (setq key next-key-1) ; or -2
                              (cl-incf nb-keys)
                              (while (string= key (string-remove-surrounding-quotes (elt (car rest1) key-index-1)))
                                (when delta-p (cl-incf sum1 (field-to-float (elt (car rest1) value-index-1))))
                                (push (pop rest1) occurences1))
                              (while (string= key (string-remove-surrounding-quotes (elt (car rest2) key-index-2)))
                                (when delta-p (cl-incf sum2 (field-to-float (elt (car rest2) value-index-2))))
                                (push (pop rest2) occurences2))))
                           
                           (t (error "This should not happen (a)"))))

                   ;; within while 2/2
                   
                   ;; now, we have occurences1 and occurences2 (and key, sum1, sum2)
                   
                   (let* ((heigth1 (length occurences1))
                          (heigth2 (length occurences2))
                          (sum1-as-string (when delta-p (float-to-Excel-string2 sum1)))
                          (sum2-as-string (when delta-p (float-to-Excel-string2 sum2)))
                          (delta (when delta-p (- sum2 sum1)))
                          (delta-as-string (when delta-p (float-to-Excel-string2 delta)))
                          (chunk
                           (cl-loop with %chunk = nil
                                    for i from 0 below (max heigth1 heigth2)
                                    for central = (if (= i 0)
                                                      (if delta-p
                                                          (if (string= sum1-as-string sum2-as-string)
                                                              `("OK" ,key ,delta-as-string)
                                                            `("NOK" ,key ,delta-as-string))
                                                        `("" ,key ""))
                                                    '("" "" ""))
                                    do (cond 
                                        ((>= i heigth1) ; nothing more to print on the left
                                         (push (append (make-list width1 "") central (elt occurences2 i)) %chunk))
                                        ((>= i heigth2) ; nothing more to print on the right
                                         (push (append (elt occurences1 i) central (make-list width2 "")) %chunk))
                                        (t
                                         (push (append (elt occurences1 i) central (elt occurences2 i)) %chunk)))
                                    finally (return (reverse %chunk)))))
                     
                     (if delta-p
                         (push (cons delta chunk) %delta+lines)
                       (push (cons 'useless chunk) %delta+lines))))) ; end of while

               (when verbose (message "%s keys found" nb-keys))
               
               %delta+lines))

            ;; (5) sort it
            ;; -----------
            (sorted-delta+lines
             (if delta-p
                 (sort delta+lines (lambda (x y) (> (abs (car x)) (abs (car y)))))
               delta+lines))

            ;; (6) drop delta in the data structure, add index on each line, and headers
            ;; we obtain the "result" ready to be witten in file
            ;; -------------------------------------------------------------------------
            (final-lines
             (let ((idx 0)
                   (%final-lines nil))

               (cl-loop for delta+lines in sorted-delta+lines
                        for lines = (cdr delta+lines)
                        do (cl-loop for line in lines
                                    do (progn
                                         (cl-incf idx)
                                         (push (cons (number-to-string idx) line) %final-lines))))

               ;; (7) add "provisions" in case of pre-treatment
               ;; ---------------------------------------------
               (unless (null prov)
                 (cl-loop for line2 in prov
                          do (progn
                               (cl-incf idx)
                               (push (append (list (number-to-string idx)) (make-list width1 "") (list "PROV" "0" "") line2) %final-lines))))

               ;; add titles
               (setq %final-lines (cons merged-titles (reverse %final-lines)))
               
               ;; check width:
               (cl-loop for line in %final-lines
                        when (not (= final-nb-of-columns (length line)))
                        do (error "Inconsistency in line width: %s vs %s ; line = %s" (length line) final-nb-of-columns line))

               ;; result:
               %final-lines)))


       ;; (8) write to CSV
       ;; ----------------
       (write-CSV-to-file final-lines output-file :separator output-separator :verbose verbose))))

;;; end
