;;;; http://www.gigamonkeys.com/book/practical-a-simple-database.html

(defvar *db* nil)

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))


(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))


(defun add-record (cd) (push cd *db*))


(defun prompt-read (prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))


(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]: ")))


(defun add-cds ()
  (loop (add-record (prompt-for-cd))
      (if (not (y-or-n-p "Another? [y/n]: ")) (return))))


(defun save-db (filename)
  (with-open-file (out filename
                   :direction :output
                   :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))


(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))


(defun select (selector-fn)
  (remove-if-not selector-fn *db*))


(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))


(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title    (setf (getf row :title) title))
               (if artist   (setf (getf row :artist) artist))
               (if rating   (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))


(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))


; here's the meaty stuff
(defun make-comparison-expr (field value)
  ; we're just making a string for dynamic evaluation.  We use the leading ` instead of '
  ; so we can use the , operator to sub in the field and value 
  `(equal (getf cd ,field) ,value))
  ; oh by the way, we could just use a formatted string like so - not clear why the above is better
  ; '"(equal (getf cd ~a) \"~a\")" 'field value)


(defun make-comparisons-list (field-value-pairs)
  (loop while field-value-pairs
     collecting (make-comparison-expr (pop field-value-pairs) (pop field-value-pairs))))


(defmacro where (&rest field-value-pairs)
  `#'(lambda (cd) (and ,@(make-comparisons-list field-value-pairs))))


; (add-cds)
(add-record (make-cd "Hoist" "Phish" 6 T))
(format t "~a~%" (select (where :title "Hoist")))
(update (where :artist "Phish") :rating 1)
(format t "~a~%" (select (where :title "Hoist")))
(dump-db)
