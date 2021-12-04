(load "~/.quicklisp/setup.lisp")

(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)

(defpackage :aoc
  (:use :cl)
  (:import-from :cl-ppcre
   :all-matches-as-strings
   :split)
  (:export
   :partition-by
   :rotate
   :solve
   :str->digits
   :str->ints
   :str-split
   ))

(in-package :aoc)

;; String ops
(defun str->list (s)
  (split "\\s*" s))

(defun str->digits (s)
  (mapcar 'parse-integer (str->list s)))

(defun str->ints (s)
  (mapcar #'parse-integer (all-matches-as-strings "\\d+" s)))

(defun str-split (s &key (by " "))
  (let ((by (concatenate 'string "\\" by)))
    (split by (string-trim '(#\space #\newline) s))))

;; List ops
(defun rotate (lol) (apply #'mapcar #'list lol))

(defun partition-by (sep lst &key transform)
  (loop with sublst
        for row in lst
        if (equal sep row)
          collect (nreverse sublst) into coll
          and do (setf sublst nil)
        else do
          (push (funcall (or transform #'identity) row) sublst)
        finally (return (append coll (list (nreverse sublst))))))

;; Macros
(defmacro solve (s1 s2)
  (let ((sep "------------------"))
    `(format t ";~a~%; part 1: ~a~%;~a~%; part 2: ~a~%;~a~%"
               ,sep (time ,s1) ,sep (time ,s2) ,sep)))

;; Tests
(use-package :lisp-unit)

(define-test str->list
  (assert-equal '("s" "t" "r" "i" "n" "g") (str->list "string")))

(define-test str->digits
  (assert-equal '(0 1 1 0) (str->digits "0110")))

(define-test str->ints
  (assert-equal '(1 88 9) (str->ints " 1  88 09 "))
  (assert-equal '(1 88 9) (str->ints "1 some text 88,09.")))

(define-test partition-by
  (assert-equal '((a b) (c) (1 2 3))
                (partition-by "sep" '(a b "sep" c "sep" 1 2 3)))
  (assert-equal '(("1" "2") ("4" "5"))
                (partition-by '3 '(1 2 3 4 5) :transform #'write-to-string)))

(run-tests)
