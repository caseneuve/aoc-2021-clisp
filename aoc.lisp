;; (load "/usr/lib/quicklisp/setup.lisp")
(require :cl-ppcre)
(defpackage :aoc
  (:use :cl)
  (:import-from :cl-ppcre :split)
  (:export :str->ints))

(in-package :aoc)

(defun str->li (s) (split "\\s*" s))
(defun str->ints (s) (mapcar 'parse-integer (str->li s)))
