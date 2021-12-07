;; --- Day 5: Hydrothermal Venture ---

(load "aoc")

(defparameter +day5+ (mapcar #'aoc:str->ints (uiop:read-file-lines "05.in")))

;; Helpers
(defun mark-vertical (x1 x2 y1 ht)
  (loop for x from (min x1 x2) to (max x1 x2) do (incf (gethash (cons x y1) ht 0))))

(defun mark-horizontal (y1 y2 x1 ht)
  (loop for y from (min y1 y2) to (max y1 y2) do (incf (gethash (cons x1 y) ht 0))))

(defun mark-diagonal (x1 x2 y1 y2 ht)
  (loop
    for x from (min x1 x2) to (max x1 x2)
    with y = (if (= x1 (min x1 x2)) y1 y2)
    with fn = (if (< y (max y1 y2)) '1+ '1-)
    do (incf (gethash (cons x y) ht 0))
       (setf y (funcall fn y))))

(defun count-overlapping (ht)
  (loop for v being the hash-values of ht
        with counter = 0
        if (> v 1) do (incf counter)
        finally (return counter)))

(defun update-diagram! (coordinates ht &key part2)
  (loop
    for (x1 y1 x2 y2) in coordinates
    if      (= y1 y2) do (mark-vertical   x1 x2 y1 ht)
    else if (= x1 x2) do (mark-horizontal y1 y2 x1 ht)
    else if part2     do (mark-diagonal   x1 x2 y1 y2 ht)
    finally (return ht)))


;; Solution
(let ((diagram1 (make-hash-table :test #'equal))
      (diagram2 (make-hash-table :test #'equal)))
  (aoc:solve
   (count-overlapping (update-diagram! +day5+ diagram1))
   (count-overlapping (update-diagram! +day5+ diagram2 :part2 t))))


;; Tests
(use-package :lisp-unit)

(define-test day5
  (let ((diagram1 (make-hash-table :test #'equal))
        (diagram2 (make-hash-table :test #'equal))
        (input (mapcar #'aoc:str->ints '("0,9 -> 5,9"
                                         "8,0 -> 0,8"
                                         "9,4 -> 3,4"
                                         "2,2 -> 2,1"
                                         "7,0 -> 7,4"
                                         "6,4 -> 2,0"
                                         "0,9 -> 2,9"
                                         "3,4 -> 1,4"
                                         "0,0 -> 8,8"
                                         "5,5 -> 8,2"))))
    (assert-equal  5 (count-overlapping (update-diagram! input diagram1 )))
    (assert-equal 12 (count-overlapping (update-diagram! input diagram2 :part2 t)))))

(run-tests '(day5))
