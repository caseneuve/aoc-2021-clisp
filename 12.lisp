;; --- Day 12: Passage Pathing ---

(load "aoc")

;; Helpers
(defun input-lines ()
  (mapcar (lambda (s) (aoc:str-split s :by "-")) (uiop:read-file-lines "12.in")))

(defun parse-input (pairs)
  (let ((tr (make-hash-table :test #'equal)))
    (flet ((start-p (e) (string= "start" e)))
      (loop for (c1 c2) in pairs
            for (val1 val2) = (list (gethash c1 tr (list c2)) (gethash c2 tr (list c1)))
            do (setf (gethash c1 tr) (remove-if #'start-p (remove-duplicates (push c2 val1))))
               (setf (gethash c2 tr) (remove-if #'start-p (remove-duplicates (push c1 val2))))
            finally (return tr)))))

(defun get-small (tree)
  (flet ((small-p (s) (lower-case-p (char s 0)))
         (inter-p (c) (not (member c '("start" "end") :test #'string=))))
    (loop for k being the hash-keys in tree if (and (small-p k) (inter-p k)) collect k)))

(defun valid-p (path part small)
  (let ((freqs (loop for cave in small collect (count cave path :test #'string=))))
    (and (= 0 (count 't (loop for freq in freqs collect (> freq part))))
         (> 2 (count 2 freqs)))))

(defun solution (input part)
  (loop
    with found = 0
    with current = '(("start"))
    with small = (get-small input)
    while current
    for this = (pop current)
    do (loop for cave in (gethash (first this) input)
             for candidate = (append (list cave) this)
             if (string= "end" cave) do (incf found)
               else if (valid-p candidate part small) do (push candidate current))
    finally (return found)))

(aoc:solve
 (solution (parse-input (input-lines)) 1)
 (solution (parse-input (input-lines)) 2))

;; Tests
(use-package :lisp-unit)

(define-test day12
  (let ((input (parse-input
                (mapcar
                 (lambda (l) (aoc:str-split l :by "-"))
                 '("dc-end"
                   "HN-start"
                   "start-kj"
                   "dc-start"
                   "dc-HN"
                   "LN-dc"
                   "HN-end"
                   "kj-sa"
                   "kj-HN"
                   "kj-dc")))))
    (assert-equal  19 (solution input 1))
    (assert-equal 103 (solution input 2))))

(run-tests '(day12))
