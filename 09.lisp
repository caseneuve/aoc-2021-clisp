;; --- Day 9: Smoke Basin ---

(load "aoc")

;; Helpers
(defun parsed-input ()
  (mapcar #'aoc:str->digits (uiop:read-file-lines "09.in")))

;; Part I
(defun adjacent-pos (x y mx my)
  (flet ((→ (z fns) (mapcar (lambda (f) (funcall f z)) fns)))
    (flet ((⇒ (xfns yfns) (mapcar #'list (→ x xfns) (→ y yfns))))
      (cond ((and (= x 0)  (= y 0))           (⇒ '(1+ *)      '(* 1+)))      ; upper left corner
            ((and (= x mx) (= y 0))           (⇒ '(1- *)      '(* 1+)))      ; upper right corner
            ((and (= x 0)  (= y my))          (⇒ '(1+ *)      '(* 1-)))      ; lower left corner
            ((and (= x mx) (= y my))          (⇒ '(1- *)      '(* 1-)))      ; lower right corner
            ((and (> x 0)  (< x mx) (= y 0))  (⇒ '(1- * 1+)   '(* 1+ *)))    ; upper border
            ((and (> x 0)  (< x mx) (= y my)) (⇒ '(1- * 1+)   '(* 1- *)))    ; lower border
            ((= x 0)                          (⇒ '(1+ * *)    '(* 1+ 1-)))   ; left border
            ((= x mx)                         (⇒ '(1- * *)    '(* 1+ 1-)))   ; right border
            (t                                (⇒ '(1- * 1+ *) '(* 1- * 1+))) ; inner tiles
            ))))

(defun part1 (input)
  (let ((mx (1- (length (car input))))
        (my (1- (length input))))
    (flet ((adjacent-values (tx ty)
             (loop for (lx ly) in (adjacent-pos tx ty mx my)
                   collect (nth lx (nth ly input)))))
      (loop for y from 0 to my
            with lowest = 0
            do (loop for x from 0 to mx
                     for this = (nth x (nth y input))
                     if (< this (apply #'min (adjacent-values x y)))
                       do (incf lowest (1+ this)))
            finally (return lowest)))))

;; Part II
(defun flood-fill (cave xx yy mx my)
  (let ((basin (list)))
    (labels
        ((walk (x y)
           (unless (member (nth x (nth y cave)) '(9 :visited))
             (push (setf (nth x (nth y cave)) :visited) basin)
             (loop for (nx ny) in (adjacent-pos x y mx my) do (walk nx ny)))))
      (walk xx yy))
    (length basin)))

(defun part2 (input)
  (let* ((mx (1- (length (car input))))
         (my (1- (length input))))
    (loop for y from 0 to my
          collect (loop for x from 0 to mx collect (flood-fill input x y mx my)) into areas
          finally (return (apply #'* (subseq (sort (apply #'append areas) #'>) 0 3))))))

;; Solution
(aoc:solve
 (part1 (parsed-input))
 (part2 (parsed-input)))

;; Tests
(use-package :lisp-unit)

(define-test day9
  (let ((input (mapcar #'aoc:str->digits
                       '("2199943210"
                         "3987894921"
                         "9856789892"
                         "8767896789"
                         "9899965678"))))
    (assert-equal 15 (part1 input))
    (assert-equal 1134 (part2 input))))

(run-tests '(day9))
