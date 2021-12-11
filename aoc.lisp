(load "~/.quicklisp/setup.lisp")

(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)

(defpackage :aoc
  (:use :cl)
  (:import-from :cl-ppcre
   #:all-matches-as-strings
   #:split)
  (:export
   #:coordinates
   #:partition-by
   #:print-hashmap
   #:product
   #:rotate
   #:solve
   #:str->charlist
   #:str->digits
   #:str->ints
   #:str->list
   #:str-split
   ))

(in-package :aoc)

;; String ops
(defun str->list (s)
  (split "\\s*" s))

(defun str->digits (s)
  (mapcar 'parse-integer (str->list s)))

(defun str->ints (s)
  (mapcar #'parse-integer (all-matches-as-strings "\\d+" s)))


(defun str-split (str &key (by " "))
  (let ((by (concatenate 'string "\\" by)))
    (mapcar (lambda (s) (string-trim '(#\space) s))
            (split by (string-trim '(#\space) str)))))

(defun str->charlist (s)
  (mapcar (lambda (e) (coerce e 'list)) (aoc:str-split s)))

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

(defun product (lst1 &optional lst2)
  (loop for y in lst1 nconc (loop for x in (or lst2 lst2) collect (list x y))))

;; Hashmap ops
(defun print-hashmap (hm) (maphash (lambda (k v) (format t "~a: ~a~%" k v)) hm))

;; AoC specific
(defun coordinates (mx my)
  "Return list of pairs (x y) for grid from 0 to MX on X axis and from 0
too MY on Y axis.  It's carthesian product of range 0 MX and range 0 MY."
  (loop for y from 0 to mx nconc (loop for x from 0 to my collect (list x y))))

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

(define-test str-split
  (assert-equal '("part1" "part2") (str-split " part1 part2 "))
  (assert-equal '("part1" "part2") (str-split "part1  part3  part2" :by "part3")))

(define-test str->charlist
  (assert-equal '((#\a #\b #\c)) (str->charlist "abc"))
  (assert-equal '((#\a #\b #\c) (#\d #\e #\f)) (str->charlist "abc def")))

(define-test partition-by
  (assert-equal '((a b) (c) (1 2 3))
                (partition-by "sep" '(a b "sep" c "sep" 1 2 3)))
  (assert-equal '(("1" "2") ("4" "5"))
                (partition-by '3 '(1 2 3 4 5) :transform #'write-to-string)))

(run-tests)
