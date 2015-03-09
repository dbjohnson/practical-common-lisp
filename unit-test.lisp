;;;; http://www.gigamonkeys.com/book/practical-building-a-unit-test-framework.html

(defun println (str &key (leading-newline nil))
    (format t "~:[~;~%~]~a~%" leading-newline str))

;;; 
(println "Step 1) Totes verbose")
(defun test-+ ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

(test-+)


;;; 
(println "Step 2) Duplicated code removed" :leading-newline t)

;; Why do we need a macro here? Because we want to use "testform" as both 
;; a) code (evaluate to get true/false) and b) data (print the form as a string)
(defmacro report-result (testform)
    `(format t "~:[FAIL~;pass~] ... ~a~%" ,testform ',testform))

(defun test-+ ()
    (report-result (= (+ 1 2) 3)))

(test-+)


;;;
(println "Step 3) Include test name via plain ol' test name variable" :leading-newline t)
(defmacro report-result (testname testform)
    `(format t "~a~5t~:[FAIL~;pass~] ... ~a~%" ,testname ,testform ',testform))

(defun test-+ ()
    (report-result 'test-+ (= (+ 1 2) 3)))

(defun test-* ()
    (report-result 'test-* (= (* 1 2) 2)))

(test-+)
(test-*)


;;;
(println "Step 4) Include test name via dynamically scoped test name - not exactly pertinent exploration of dyn scoping" :leading-newline t)
(defvar *testname* nil)

(defmacro report-result (testform)
    ; interesting - don't want to sub in the value of *testname* with a comma,
    ; because this string is generated at compile time, and *testname* will be nil.
    ; We can simply leave in the "raw" symbol, which will evaluate properly at runtime.
    `(format t "~a~5t~:[FAIL~;pass~] ... ~a~%" *testname* ,testform ',testform))

(defun test-+ ()
    (let ((*testname* 'test-+))
        (report-result (= (+ 1 2) 3))))

(defun test-* ()
    (let ((*testname* 'test-*))
        (report-result (= (* 1 2) 3))))

(test-+)
(test-*)


; ;;;
; (println "Step 5) Aggregate results" :leading-newline t)
; ;; we want to pass a test suite only if all tests pass, but don't want to use the and operator directly, since
; ;; it will abort test execution as soon as one fails

; ;; goal: function that evaluates a list of test cases, prints per-test results and overall result
; (defun report-result (result form)
;     (format t "~a~5t~:[FAIL~;pass~] ... ~a~%" *testname* result form))

; (defmacro check (testform)
;     `(report-result ,testform ',testform))

; (defun run-tests (&rest testcases)
;     (loop for test in testcases collect (check test)))

; (defun test-+ ()
;     (let ((*testname* 'test-+))
;         (run-tests (= (+ 1 2) 3) (= (+ 1 2) 4))))

; (test-+)

;;;;;;;;;;;;;;;;;;;;;;;;

(println "hm, looks like for macro expansion to handle test names the way I want, the macro has
 to come directly after the test form definition - otherwise, the form is evaluated before being 
 passed as a function argument." :leading-newline t)
(defmacro report-result (form)
    `(format t "~:[FAIL~;pass~] ... ~a~%" ,form ',form))

(defun intermediate (test)
    (report-result test))

(report-result (= (+ 1 2) 3))
(intermediate (= (+ 1 2) 3))

;;;;;;;;;;;;;;;;;;;;;;;;

(defun report-result (result form)
    (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defmacro with-gensyms1 (names &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro check (&rest forms)
    (with-gensyms1 (result)
        `(let ((,result t))
            ,@(loop for f in forms collect `(progn (unless ,f (setf ,result nil)) (report-result ,f ',f)))
            ,result)))

(defun test-+ ()
    (check 
        (= (+ 1 2) 3)
        (= (+ 2 5) 7)))


(test-+)
