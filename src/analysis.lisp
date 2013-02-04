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

(lastcar *fuzz-data*)

;; ((:SOLUTION . #<ASM {10039B9473}>)
;;  (:FILE . "/tmp/tmp.UrGErzkY9B")
;;  (:ERRNO . 137))

(phenome *fix*) ;; => "/tmp/filefwIOrq"
