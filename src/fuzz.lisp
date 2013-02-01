(in-package :software-evolution)
(use-package :cl-ppcre)
(use-package :curry-compose-reader-macros)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar *test* "../../bin/test-indent.sh"
  "The indent test script with fuzzing.")

(defvar *fuzz-test* "../../test-fuzz.sh"
  "Script to run a variant on a fuzz file.")

(defvar *fuzz* "../../bin/break-indent.sh"
  "Script to break indent with fuzzing.")

(defvar *orig* (from-file (make-instance 'cil) "indent/indent_comb.c")
  "The original program.")

(defvar *work-dir* "sh-runner/work/"
  "Needed because SBCL chokes after too many shell outs.")

(setf *max-population-size* (expt 2 8))

(setf *tournament-size* 2)

(defmethod fuzz ((variant cil))
  (with-temp-file (file)
    (phenome variant :bin file)
    (multiple-value-bind (stdout stderr exit)
        (shell "~a ~a 2>&1" *fuzz* file)
      (declare (ignorable stderr))
      (bind (((fuzz err-str) (split-sequence #\Space stdout)))
        (values fuzz (parse-number err-str))))))

(defmethod positive-tests ((variant cil))
  (with-temp-file (file)
    (or (ignore-errors
          (phenome variant :bin file)
          (multiple-value-bind (stdout stderr exit)
              (shell "~a ~a 2>&1" *test* file)
            (declare (ignorable stderr))
            (when (zerop exit)
              (parse-number stdout))))
        0)))

(defmethod fuzz-tests ((variant cil) fuzz-file)
  (with-temp-file (file)
    (or (ignore-errors
          (phenome variant :bin file)
          (multiple-value-bind (stdout stderr exit)
              (shell "~a ~a ~a 2>&1" *fuzz-test* file fuzz-file)
            (declare (ignorable stderr))
            (if (zerop exit) 5)))
        0)))

(defun test (fuzz-file variant)
  (incf *fitness-evals*)
  (+ (positive-tests variant)
     (fuzz-tests variant fuzz-file)))

(defvar *best* nil
  "Will hold that which defaults the fuzz.")

(defmacro prepeatedly (n &body body)
  "Return the result of running BODY N times in parallel."
  (let ((loop-sym (gensym))
        (result-sym (gensym)))
    `(let ((,result-sym (loop :for ,loop-sym :upto ,n :collect (pexec ,@body))))
       (map-into ,result-sym #'yield ,result-sym))))

(defmethod harden ((variant cil))
  (format t "fuzzing ~S~%" variant)
  (multiple-value-bind (fuzz-file errno) (fuzz variant)
    (format t "found fuzz ~S(~d)~%" fuzz-file errno)
    (shell "cp ~a ../../" fuzz-file)
    (prepeatedly 46
      (setf *best* (evolve {test fuzz-file} :max-fit 10))
      (setf *running* nil))
    *best*))

;; TODO: rather than the below, implement a multi-threaded solution
;;  - 1 population
;;  - many threads of (new -> test -> incorporate -> evict)
;;  - fitness cached in functions *not* saved w/individual
;;  - the test functions themselves note passing fuzzes and trigger a re-fuzz
;;  - accumulate collected fuzz tests, best passing individuals and pop

;; Run -- this will just run forever
#+run
(progn
  (setf *best* *orig*)
  (loop :for i :upfrom 0 :do
     (format t "fuzzing ~S~%" *best*)
     (multiple-value-bind (fuzz-file errno) (fuzz variant)
       (format t "found fuzz ~S(~d)~%" fuzz-file errno)
       (shell "cp ~a ../../fuzz-~d" fuzz-file i)
       (setf (fitness *best*) (test fuzz-file *best*))
       (setf *population* (repeatedly *max-population-size* (copy *best*)))
       (prepeatedly 46
         (setf *best* (evolve {test fuzz-file} :max-fit 10))
         (setf *running* nil))
       (store *best* (format nil "best-~d.store" i))
       (store *population* (format nil "pop-~d.store" i)))))
