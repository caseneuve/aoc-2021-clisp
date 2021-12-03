(require :cl-ppcre)

(defpackage :aoc
  (:use :cl)
  (:import-from :cl-ppcre :split)
  (:export :str->ints :str-split))
(in-package :aoc)

(defun str->li (s) (split "\\s*" s))
(defun str->ints (s) (mapcar 'parse-integer (str->li s)))

(defun str-split (s &key (by " ")) (let ((by (concatenate 'string "\\" by))) (split by s)))
