;; --- Day 14: Extended Polymerization ---

(load "aoc")

;; Solution
(defun pairs (lst)
  (loop for pair in (mapcar #'list (reverse (cdr (reverse lst))) (cdr lst))
        collect (format nil "~{~A~}" pair)))

(defun prepare-data (input-lines)
  (let ((i (aoc:str->list (car input-lines)))
        (r (make-hash-table :test #'equal))
        (c (make-hash-table :test #'equal))
        (e (make-hash-table :test #'equal)))
    (loop for (k v) in (mapcar (lambda (e) (aoc:str-split e :by "->")) (cddr input-lines))
          do (setf (gethash k r) v))
    (loop with p = (pairs i)
          for k being the hash-keys in r
          do (setf (gethash k c) (count k p :test #'string=)))
    (loop for l in i do (incf (gethash l e 0)))
    (values r c e)))

(defun solution (input steps)
  (multiple-value-bind (rules count elements) (prepare-data input)
    (loop for new-count = (make-hash-table :test #'equal)
          for step from 1 to steps
          do (loop for k being the hash-keys in count using (hash-value v)
                   for new = (gethash k rules)
                   for k1 = (format nil "~a~a" (subseq k 0 1) new)
                   for k2 = (format nil "~a~a" new (subseq k 1))
                   do (incf (gethash k1 new-count 0) v)
                      (incf (gethash k2 new-count 0) v)
                      (incf (gethash new elements 0) (gethash k count)))
             (setf count new-count)
          finally (return
                    (let ((vals (loop for v being the hash-values in elements collect v)))
                      (- (apply #'max vals) (apply #'min vals)))))))

(let ((input (uiop:read-file-lines "14.in")))
  (aoc:solve
   (solution input 10)
   (solution input 40)))

;; Tests
(use-package :lisp-unit)

(define-test day14
  (let ((input '("NNCB"
                 ""
                 "CH -> B"
                 "HH -> N"
                 "CB -> H"
                 "NH -> C"
                 "HB -> C"
                 "HC -> B"
                 "HN -> C"
                 "NN -> C"
                 "BH -> H"
                 "NC -> B"
                 "NB -> B"
                 "BN -> B"
                 "BB -> N"
                 "BC -> B"
                 "CC -> N"
                 "CN -> C")))
    (assert-equal          1588 (solution input 10))
    (assert-equal 2188189693529 (solution input 40))))

(run-tests '(day14))
