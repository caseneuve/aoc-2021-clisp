(load "aoc")

(defparameter +day5+ (mapcar #'aoc:str->ints (uiop:read-file-lines "05.in")))

;; Helpers
(defun parse-input (input)
  (loop with maxx = 0
        with maxy = 0
        for (x1 y1 x2 y2) in input
        do (setf maxx (max maxx x1 x2) maxy (max maxy y1 y2))
        collect (list x1 y1 x2 y2) into crd
        finally (return (values crd maxx maxy))))

(defun make-diagram (maxx maxy)
  (loop for y from 0 to maxy collect (make-list (1+ maxx) :initial-element 0)))

(defun mark-vertical (x1 x2 y1 diagram)
  (loop with row = (nth y1 diagram)
        for n from (min x1 x2) to (max x1 x2)
        do (incf (nth n row))))

(defun mark-horizontal (x1 y1 y2 diagram)
  (loop for n from (min y1 y2) to (max y1 y2)
        for row = (nth n diagram)
        do (incf (nth x1 row))))

(defun mark-diagonal (x1 x2 y1 y2 diagram)
  (loop
    with y = (if (= x1 (min x1 x2)) y1 y2)
    with fn = (if (< y (max y1 y2)) '1+ '1-)
    for x from (min x1 x2) to (max x1 x2)
    do (incf (nth x (nth y diagram)))
       (setf y (funcall fn y))))

(defun update-diagram (coordinates diagram &key part2)
  (loop
    for (x1 y1 x2 y2) in coordinates
    if      (= y1 y2) do (mark-vertical   x1 x2 y1    diagram)
    else if (= x1 x2) do (mark-horizontal x1 y1 y2    diagram)
    else if part2     do (mark-diagonal   x1 x2 y1 y2 diagram)
    finally           (return diagram)))

(defun count-overlapping-lines (diagram)
  (length (remove-if (lambda (e) (< e 2)) (apply #'append diagram))))

;; Solution
(multiple-value-bind (coordinates maxx maxy) (parse-input +day5+)
  (aoc:solve
   (count-overlapping-lines (update-diagram coordinates (make-diagram maxx maxy)))
   (count-overlapping-lines (update-diagram coordinates (make-diagram maxx maxy) :part2 t))))

;; Tests
(use-package :lisp-unit)

(define-test day5
  (let ((input (mapcar #'aoc:str->ints '("0,9 -> 5,9"
                                         "8,0 -> 0,8"
                                         "9,4 -> 3,4"
                                         "2,2 -> 2,1"
                                         "7,0 -> 7,4"
                                         "6,4 -> 2,0"
                                         "0,9 -> 2,9"
                                         "3,4 -> 1,4"
                                         "0,0 -> 8,8"
                                         "5,5 -> 8,2"))))
    (assert-equal 5
     (multiple-value-bind (coordinates maxx maxy) (parse-input input)
       (count-overlapping-lines (update-diagram coordinates (make-diagram maxx maxy)))))
    (assert-equal 12
     (multiple-value-bind (coordinates maxx maxy) (parse-input input)
       (count-overlapping-lines (update-diagram coordinates (make-diagram maxx maxy) :part2 t))))))

(run-tests '(day5))
