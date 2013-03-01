(require :software-evolution)
(in-package :software-evolution)

;; support for debugging output
(defvar *debug-path* "fuzz.debug")

(defun message (&rest args)
  (flet ((say (stream)
           (apply #'format (cons stream args))
           (format stream " at ~S~%" (print-time nil))))
    (with-open-file (out *debug-path* :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (say out))
    (say t)))

(defvar *test* "./bin/test-indent.sh"
  "The indent test script with fuzzing.")

(defvar *fuzz-test* "./bin/test-fuzz.sh"
  "Script to run a variant on a fuzz file.")

(defvar *fuzz* "./bin/break.sh"
  "Script to break indent with fuzzing.")

(defvar *fuzz-data* nil
  "List of alists holding fuzz files, associated errnos and passing variants.")

;; (from-file (make-instance 'cil) "indent/indent_comb.c")
(defvar *orig* (from-file (make-instance 'asm) "indent_comb.s")
  "The original program.")

(defvar *work-dir* nil
  "Needed because SBCL chokes after too many shell outs.")

(setf *max-population-size* (expt 2 10))

(setf *tournament-size* 2)

(defmethod fuzz ((variant software))
  (with-temp-file (file)
    (phenome variant :bin file)
    (multiple-value-bind (stdout stderr exit)
        (shell "~a ~a 2>&1" *fuzz* file)
      (declare (ignorable stderr))
      (let ((results (split-sequence #\Space stdout)))
        (if (and (zerop exit) (= 2 (length results)))
            (bind (((fuzz err-str) results))
              (values fuzz (parse-number err-str)))
            nil)))))

(defun positive-tests (variant)
  (with-temp-file (file)
    (or (ignore-errors
          (phenome variant :bin file)
          (multiple-value-bind (stdout stderr exit)
              (shell "~a ~a 2>&1" *test* file)
            (declare (ignorable stderr))
            (when (zerop exit)
              (parse-number stdout))))
        0)))
(memoize #'positive-tests :key (compose #'genome #'car))

(defun fuzz-test (variant fuzz-spec)
  "FUZZ-SPEC is an alist with :file and :errno elements."
  (with-temp-file (file)
    (or (ignore-errors
          (phenome variant :bin file)
          (multiple-value-bind (stdout stderr exit)
              (shell "~a ~a ~a 2>&1" *fuzz-test* file (aget :file fuzz-spec))
            (declare (ignorable stderr stdout))
            (if (< exit (aget :errno fuzz-spec)) 1)))
        0)))
(memoize #'fuzz-test :key (lambda-bind ((variant fuzz-spec)) (cons (genome variant) fuzz-spec)))

(defun test (variant)
  (incf *fitness-evals*)
  (let* ((total-fuzz (length *fuzz-data*))
         ;; score against regression tests
         (regression-score (positive-tests variant))
         ;; score against accumulated fuzz tests in `*fuzz-data*'
         (old-fuzz (reduce #'+ (mapcar (curry fuzz-test variant) (cdr *fuzz-data*))))
         (new-fuzz (fuzz-test variant (car *fuzz-data*)))
         ;; total score and maximum possible score
         (score (+ (* regression-score total-fuzz) old-fuzz new-fuzz))
         (max-score (* (+ 5 1) total-fuzz)))
    (prog1 score
      ;; when we find the best possible, generate more fuzz tests
      (when (= score max-score)
        ;; save this variant associated with the fuzz test
        (push (cons :solution variant) (car *fuzz-data*))
        ;; generate a new fuzz test defeating this variant
        (sb-thread:make-thread (lambda () (push (harden variant) *fuzz-data*)))))))

(defmethod harden ((variant software))
  (message "fuzzing ~S after ~S evals" (edits variant) *fitness-evals*)
  (multiple-value-bind (fuzz-file errno) (fuzz variant)
    (message "found fuzz ~S(~d)~%" fuzz-file errno)
    ;; store the solution to the previous fuzz
    (store variant (format nil "fuzz-data/~a.store" (length *fuzz-data*)))
    ;; save the new fuzz file and errorno
    (shell "cp ~a ../../fuzz-data/~a-fuzz.~a"
           fuzz-file (length *fuzz-data*) errno)
    ;; return the new fuzz file and errno
    `((:file . ,fuzz-file) (:errno . ,errno))))

(defun evolve ()
  "Run a thread of evolution; new -> test -> incorporate -> evict."
  (flet ((tourny ()
           (car (sort (loop :for i :below *tournament-size* :collect
                         (random-elt *population*))
                      *fitness-predicate*
                      :key #'test))))
    (loop :while *running* :do
       ;; generate a new individual, and incorporate
       (push (if (< (random 1.0) *cross-chance*)
                 (crossover (tourny) (tourny))
                 (mutate (copy (tourny))))
             *population*)
       ;; possibly reduce the population size below the max
       (loop :while (and *max-population-size*
                         (> (length *population*) *max-population-size*)) :do
          (setf *population* (remove (random-elt *population*) *population*))))))

;; Run -- this will just run forever
#+run
(progn
  (message "building the initial population")
  (setf *population* (repeatedly *max-population-size* (copy *orig*)))
  (push (harden *orig*) *fuzz-data*)
  (message "kicking off the 32 evolution threads")
  (loop :for i :below 32 :do
     (sb-thread:make-thread #'evolve :name (format nil "evolver-~d" i))))
