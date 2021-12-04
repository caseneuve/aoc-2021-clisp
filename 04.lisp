(load "aoc")

(defparameter +day4+ (uiop:read-file-lines "04.in"))

(defun parse-input (lines)
  (values (aoc:str->ints (car lines))
          (aoc:partition-by "" (cddr lines) :transform #'aoc:str->ints)))

;; Common
(defun mark-row (n row)
  (mapcar (lambda (el) (if (eq el n) nil el)) row))

(defun complete? (board)
  (some 'identity (mapcar (lambda (line) (notany 'identity line)) board)))

(defun won? (board)
  (some 'identity (mapcar #'complete? (list board (aoc:rotate board)))))

(defun update-board (n board)
  (loop for row in board collect (mark-row n row)))

(defun calculate-score (n board)
  (* n (apply #'+ (remove-if-not #'numberp (apply #'append board)))))

;; Solutions
(defun game1 (nums boards &key score)
  (if score score
      (game1
       (cdr nums)
       (loop named round
             with n = (car nums)
             for board in boards
             for this = (update-board n board)
             if (won? this) do
               (setf score (calculate-score n this))
               (return-from round this)
             else collect this)
       :score score)))

(defun game2 (nums boards &key score)
  (if (not nums) score
      (game2
       (cdr nums)
       (loop named round
             with n = (car nums)
             for board in boards
             for this = (update-board n board)
             if (won? this) do
               (setf score (calculate-score n this))
             else collect this)
       :score score)))

;; Results
(multiple-value-bind (numbers boards) (parse-input +day4+)
  (aoc:solve
   (game1 numbers boards)
   (game2 numbers boards)))
;------------------
; part 1: 23177
;------------------
; part 2: 6804
;------------------

;; Tests
(use-package :lisp-unit)

(define-test day04
  (let ((input
         '("7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
           ""
           "22 13 17 11  0"
           "8  2 23  4 24"
           "21  9 14 16  7"
           "6 10  3 18  5"
           "1 12 20 15 19"
           ""
           "3 15  0  2 22"
           "9 18 13 17  5"
           "19  8  7 25 23"
           "20 11 10 24  4"
           "14 21 16 12  6"
           ""
           "14 21 17 24  4"
           "10 16 15  9 19"
           "18  8 23 26 20"
           "22 11 13  6  5"
           "2  0 12  3  7")))
    (multiple-value-bind (numbers boards) (parse-input input)
      (assert-equal 4512 (game1 numbers boards))
      (assert-equal 1924 (game2 numbers boards)))))

(run-tests '(day04))
