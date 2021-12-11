;; --- Day 11: Dumbo Octopus ---

(load "aoc")

;; Helpers
(defun input-lines () (mapcar #'aoc:str->digits (uiop:read-file-lines "11.in")))

(defun parse-input (input)
  (let* ((grid (make-hash-table :test #'equal))
         (mx (1- (length (car input))))
         (my (1- (length input)))
         (coords (aoc:coordinates mx my)))
    (loop for xy in coords
          do (setf (gethash xy grid) (cons (nth (car xy) (nth (cadr xy) input)) :loading)))
    (values grid mx my coords)))

;; Solution
(defun adjacent-pos (xy coords &optional (deltas '(-1 0 1)))
  (flet ((x> (z) (+ (car  xy) z))
         (y> (z) (+ (cadr xy) z))
         (valid-p (nxy) (and (member nxy coords :test #'equal) (not (equal xy nxy)))))
    (remove-if-not #'valid-p (aoc:product (mapcar #'y> deltas) (mapcar #'x> deltas)))))

(defun powerup! (gr coords)
  (loop for xy in coords do (incf (car (gethash xy gr)))))

(defun flash! (gr xy coords)
  (let ((flashes 0))
    (labels
        ((walk (xy &optional blast)
           (when blast (incf (car (gethash xy gr))))
           (let ((this (gethash xy gr)))
             (unless (eq (cdr this) :flashed)
               (when (> (car this) 9)
                 (incf flashes)
                 (setf (gethash xy gr) (cons (incf (car this)) :flashed))
                 (loop for nxy in (adjacent-pos xy coords) do (walk nxy 'boom)))))))
      (walk xy))
    flashes))

(defun afterstep! (gr coords)
  (loop for xy in coords
        for val = (car (gethash xy gr))
        do (setf (gethash xy gr) (cons (if (> val 9) 0 val) :loading))))

(defun count-flashes (gr coords)
  (loop for xy in coords
        collect (eq :flashed (cdr (gethash xy gr))) into flashes
        finally (return (count 't flashes))))

(defun solution (raw-input)
  (multiple-value-bind (gr mx my coords) (parse-input raw-input)
    (loop for step from 1
          with flashes = (list)
          with simoultaneous = (list)
          while (= (length simoultaneous) 0)
          do (powerup! gr coords)
             (push (apply #'+ (loop for xy in coords collect (flash! gr xy coords))) flashes)
             (when (= (* (1+ mx) (1+ my)) (count-flashes gr coords)) (push step simoultaneous))
             (afterstep! gr coords)
          finally (return (values
                           (apply #'+ (subseq (reverse flashes) 0 100))
                           (pop simoultaneous))))))

(multiple-value-bind (part1 part2) (time (solution (input-lines)))
  (format t "part 1: ~4d~%part 2: ~4d~2%" part1 part2))

;; Tests
(use-package :lisp-unit)

(define-test day10
  (let ((lines (mapcar #'aoc:str->digits '("5483143223"
                                           "2745854711"
                                           "5264556173"
                                           "6141336146"
                                           "6357385478"
                                           "4167524645"
                                           "2176841721"
                                           "6882881134"
                                           "4846848554"
                                           "5283751526"))))
    (multiple-value-bind (part1 part2) (solution lines)
      (assert-equal 1656 part1)
      (assert-equal  195 part2))))

(run-tests '(day10))
