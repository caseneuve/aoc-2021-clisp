;; --- Day 6: Lanternfish ---

(load "aoc")

(defparameter +day6+ (aoc:str->ints (car (uiop:read-file-lines "06.in"))))

;; Solution
(defun fish->ages (ages)
  "Create list where index represents fish age and value represents
count of fish in that age, so for given input example '3,4,3,1,2',
that would be:
list (0 1 1 2 1 0 0 0 0)
index 0 1 2 3 4 5 6 7 8"
  (loop for n from 0 to 8 collect (count n ages)))

(defun count-fish (ages days)
  (loop for day from 1 to days
        for fst = (pop ages)
        do (nconc ages (list fst))
           (incf (nth 6 ages) fst)
        finally (return (apply #'+ ages))))

(aoc:solve
 (count-fish (fish->ages +day6+) 80)
 (count-fish (fish->ages +day6+) 256))

;; Tests
(use-package :lisp-unit)

(define-test day6
  (let ((input (aoc:str->ints "3,4,3,1,2")))
    (assert-equal '(0 1 1 2 1 0 0 0 0) (fish->ages input))
    (assert-equal 5934 (count-fish (fish->ages input) 80))
    (assert-equal 26984457539 (count-fish (fish->ages input) 256))))

(run-tests '(day6))
