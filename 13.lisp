;; --- Day 13: Transparent Origami ---

(load "aoc")

(defun input (&optional (lines (uiop:read-file-lines "13.in")))
  (loop for line in lines
        with points = ()
        with folds = ()
        if (not (string= "" line))
          do (cond
               ((digit-char-p (char line 0)) (push (aoc:str->ints line) points))
               (t (push
                   (mapcar #'read-from-string (aoc:str-split (subseq line 11) :by "="))
                   folds)))
        finally (return (values (reverse points) (reverse folds)))))

(defun decode (co mx my)
  (loop for y from 0 to my
        do (loop for x from 0 to mx
                 collect (if (member (list x y) co :test 'equal) "#" " ") into line
                 finally (format t "~{~A~}~%" line))))

(defun fold ()
  (multiple-value-bind (coord folds) (input)
    (loop for (axis xy) in folds
          with visible = ()
          do (loop for co in coord
                   for (a b) = (if (eq axis 'x) co (reverse co))
                   for fn = (if (eq axis 'x) 'identity 'reverse)
                   if (> a xy) collect (funcall fn (list (- xy (- a xy)) b)) into new
                     else if (< a xy) collect (funcall fn (list a b)) into new
                            finally (setf coord (remove-duplicates new :test 'equal))
                                    (push (length coord) visible))
          finally (return (values coord (reverse visible))))))

(multiple-value-bind (coordinates visible-points) (fold)
  (aoc:solve
   (first visible-points)
   (decode coordinates (* 8 5) 5)))     ; 8 letters 4 chars wide, 5 chars high
