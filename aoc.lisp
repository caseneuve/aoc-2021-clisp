(load "~/.quicklisp/setup.lisp")

(ql:quickload :cl-ppcre)
(ql:quickload :lisp-unit)

(defpackage :aoc
  (:use :cl)
  (:import-from :cl-ppcre :split)
  (:export :str->ints :str-split :solve))

(in-package :aoc)

(defun str->li (s) (split "\\s*" s))
(defun str->ints (s) (mapcar 'parse-integer (str->li s)))

(defun str-split (s &key (by " ")) (let ((by (concatenate 'string "\\" by))) (split by s)))

(defmacro solve (s1 s2)
  (let ((sep "------------------"))
    `(format t "~a~% part 1: ~a~%~a~% part 2: ~a~%~a~%"
               ,sep (time ,s1) ,sep (time ,s2) ,sep)))
