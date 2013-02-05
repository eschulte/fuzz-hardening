(require :software-evolution)
(in-package :software-evolution)
(mapc (lambda (pkg) (require pkg) (use-package pkg))
      '(:cl-ppcre :curry-compose-reader-macros))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar *work-dir* "sh-runner/work/"
  "Needed because SBCL chokes after too many shell outs.")

(defvar *fuzz-data* (restore "fuzz-results/fuzz.store"))

(defvar *fix* (aget :solution (lastcar *fuzz-data*)))

#+run-fuzz-data
(loop :for i :from 1 :to 11 :do
   (let ((fix (restore (format nil "results/fuzz-data/~d.store" i))))
     (with-temp-file (file)
       (phenome fix :bin file)
       (multiple-value-bind (stdout stderr exit)
           (shell "~a < /tmp/fuzz-~d >/dev/null 2>/dev/null" file i)
         (format t "~a ~a~%" i exit)))))
