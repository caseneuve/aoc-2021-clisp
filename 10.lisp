;; --- Day 10: Syntax Scoring ---

(defpackage day10
  (:use :cl)
  (:export #:calculate-incomplete))

(in-package day10)
(load "aoc")

;; Helpers
(defun input-lines ()
  (mapcar #'aoc:str->list (uiop:read-file-lines "10.in")))

(defparameter opening '("(" "[" "{" "<"))
(defparameter closing '(")" "]" "}" ">"))
(defparameter corrupted  (pairlis closing '(3 57 1197 25137)))
(defparameter incomplete (pairlis opening '(1 2 3 4)))

(defun match-p (op clo)
  (string= (cdr (assoc op (pairlis opening closing) :test #'string=)) clo))

(defun bracket->points (bracket &optional type)
  (cdr (assoc bracket (if (eq type :corrupted) corrupted incomplete) :test #'string=)))

;; I
(defun calculate-corrupted (scores)
  (loop for score in (remove-duplicates scores)
        with sum = 0
        do (incf sum (* (count score scores) score))
        finally (return sum)))

;; II
(defun calculate-incomplete (lines)
  (let* ((scores (loop for line in lines
                       collect (reduce (lambda (sum points) (+ (* 5 sum) points)) line)))
         (sorted-scores (sort scores #'<)))
    (nth (floor (/ (length sorted-scores) 2)) sorted-scores)))

;; Solution
(defun get-score-for (lines &optional (type :corrupted))
  (loop for line in lines
        collect
        (loop for bracket in (cdr line)
              with seen = (list (car line))
              if (match-p (car seen) bracket) do (pop seen)
              else if (member bracket opening :test #'string=) do (push bracket seen)
              else collect (bracket->points bracket type) into corruption
              finally (return (cond ((eq type :corrupted) (first corruption))
                                    ((atom corruption) (mapcar #'bracket->points seen)))))
          into points
        finally (return
                  (funcall
                   (if (eq type :corrupted) #'calculate-corrupted #'calculate-incomplete)
                   (remove-if #'null points)))))

(aoc:solve
 (get-score-for (input-lines) :corrupted)
 (get-score-for (input-lines) :incomplete))

;; Tests
(use-package :lisp-unit)

(define-test day10
  (let ((lines (mapcar #'aoc:str->list
                       '("[({(<(())[]>[[{[]{<()<>>"
                         "[(()[<>])]({[<{<<[]>>("
                         "{([(<{}[<>[]}>{[]{[(<()>"
                         "(((({<>}<{<{<>}{[]{[]{}"
                         "[[<[([]))<([[{}[[()]]]"
                         "[{[{({}]{}}([{[{{{}}([]"
                         "{<[[]]>}<{[{[{[]{()[[[]"
                         "[<(<(<(<{}))><([]([]()"
                         "<{([([[(<>()){}]>(<<{{"
                         "<{([{{}}[<[[[<>{}]]]>[]]"))))
    (assert-equal  26397 (get-score-for lines :corrupted))
    (assert-equal 288957 (get-score-for lines :incomplete))))

(run-tests '(day10))
