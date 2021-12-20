;;  --- Day 20: Trench Map ---

(load "aoc")

(defun parse-input (lines)
  (flet ((str->zero-one (l) (map 'list (lambda (c) (case c (#\# 1) (#\. 0))) l)))
    (let* ((bins (mapcar #'str->zero-one lines))
           (image (cddr bins)))
      (values
       (first bins)
       (make-array (list (length image) (length (first image))) :initial-contents image)))))

(defun combine (x y mx my image turn)
  (loop
    with decimal = 0
    for yy from (1- y) to (1+ y)
    do (loop for xx from (1- x) to (1+ x)
             for pxl = (if (and (< -1 yy my) (< -1 xx mx))
                           (aref image yy xx)
                           (mod turn 2)) ; that was tricky! (the image is infinite and 'outside' pixels flip each turn
             do (setf decimal (+ pxl (* 2 decimal))))
             finally (return decimal)))

(defun enhance (image algorithm turn)
  (let ((new (make-array (mapcar (lambda (d) (+ 2 d)) (array-dimensions image)))))
    (destructuring-bind (my mx) (array-dimensions image)
      (loop for y from -1 to my
            do (loop for x from -1 to mx
                     do (setf (aref new (1+ y) (1+ x))
                              (elt algorithm (combine x y mx my image turn)))))
      new)))

(defun solution (input-lines &optional (enhancements 2))
  (multiple-value-bind (algorithm image) (parse-input input-lines)
    (loop for enhancement from 0 below enhancements
          do (setf image (enhance image algorithm enhancement))
          finally (return (apply #'+ (loop for ii from 0 below (array-total-size image)
                                           collect (row-major-aref image ii)))))))

(let ((lines (uiop:read-file-lines "20.in")))
  (aoc:solve
   (solution lines)
   (solution lines 50)))
