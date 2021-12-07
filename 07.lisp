;; --- Day 7: The Treachery of Whales ---

(load "aoc")

(defparameter +day7+ (aoc:str->ints (car (uiop:read-file-lines "07.in"))))

(defun sum-range (n) (/ (* n (+ n 1)) 2))

(defun find-cheapest (positions &optional (calculation #'*))
  (apply #'min
   (loop for n from (apply #'min positions) to (apply #'max positions)
         collect (apply #'+ (mapcar (lambda (e) (funcall calculation (abs (- e n)))) positions)))))

(aoc:solve
 (find-cheapest +day7+)
 (find-cheapest +day7+ #'sum-range))

;; Tests
(use-package :lisp-unit)

(define-test day7
  (assert-equal 5050 (sum-range 100))
  (let ((input (aoc:str->ints "16,1,2,0,4,2,7,1,2,14")))
    (assert-equal 37 (find-cheapest input))
    (assert-equal 168 (find-cheapest input #'sum-range))))

(run-tests '(day7))
