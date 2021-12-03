(load "aoc")

(defparameter +in+ (mapcar #'aoc:str->ints (uiop:read-file-lines "03.in")))

;;; COMMONS
(defun rotate (xs)
  (apply #'mapcar #'list xs))

(defun most-least (xs ll)
  (let ((len (length ll)))
    (loop for n in '(1 0) for c = (count n xs)
          collect (if (= c (/ len 2)) n (round (/ c len))))))

(defun bin->dec (xs)
  (reduce (lambda (x y) (+ (* 2 x) y)) xs))

(defun multiply-bins (xs)
  (apply #'* (mapcar #'bin->dec xs)))

;; I
(defun solution1 (data)
  (multiply-bins (rotate (mapcar (lambda (l) (most-least l data)) (rotate data)))))

(solution1 +in+) ; => 4118544

;; II
(defun get-rating (ll &key (pos 0) subs)
  (if (cdr ll)
      (let ((bit (funcall subs (most-least (nth pos (rotate ll)) ll))))
        (get-rating (remove-if (lambda (l) (not (= (nth pos l) bit))) ll)
                    :pos (incf pos) :subs subs))
      (first ll)))

(setf (fdefinition 'oxygen) #'car
      (fdefinition 'co2)    #'cadr)

(defun solution2 (data)
  (multiply-bins (loop for s in '(oxygen co2) collect (get-rating data :subs s))))

(solution2 +in+) ; => 3832770

;;; TESTS
(ql:quickload :lisp-unit)
(use-package :lisp-unit)

(define-test day03
  (let ((input (mapcar #'aoc:str->ints
                       '("00100" "11110" "10110" "10111" "10101" "01111"
                         "00111" "11100" "10000" "11001" "00010" "01010"))))
    (assert-equal 198 (solution1 input))
    (assert-equal 230 (solution2 input))))
(run-tests '(day03))
