;;; --- Day 8: Seven Segment Search ---

(load "aoc")

;;; Helpers
(defun parse-input (lns)
  (mapcar (lambda (l) (mapcar #'aoc:str->charlist (aoc:str-split l :by "|"))) lns))

(defparameter +day8+ (parse-input (uiop:read-file-lines "08.in")))

;;; Part I
(defun count-unique (numbers)
  (count-if (lambda (n) (member (length n) '(2 3 4 7))) (apply #'append numbers)))

;;; Part II
(defun filter-by-len (lst len) (remove-if-not (lambda (e) (= (length e) len)) lst))

(defun — (s1 &rest sets)
  (reduce #'set-difference (funcall (if (atom (car s1)) #'list* #'append) s1 sets)))

(defun ∩ (s1 &rest sets)
    (reduce #'intersection (funcall (if (atom (car s1)) #'list* #'append) s1 sets)))

(defun decode-signals (sgn)
  "Letters correspond to the example:
  0:      1:      2:      3:      4:
 aaaa    ....    aaaa    aaaa    ....
b    c  .    c  .    c  .    c  b    c
b    c  .    c  .    c  .    c  b    c
 ....    ....    dddd    dddd    dddd
e    f  .    f  e    .  .    f  .    f
e    f  .    f  e    .  .    f  .    f
 gggg    ....    gggg    gggg    ....

  5:      6:      7:      8:      9:
 aaaa    aaaa    aaaa    aaaa    aaaa
b    .  b    .  .    c  b    c  b    c
b    .  b    .  .    c  b    c  b    c
 dddd    dddd    ....    dddd    dddd
.    f  e    f  .    f  e    f  .    f
.    f  e    f  .    f  e    f  .    f
 gggg    gggg    ....    gggg    gggg
"
  (let* ((one       (car (filter-by-len sgn 2)))
         (four      (car (filter-by-len sgn 4)))
         (seven     (car (filter-by-len sgn 3)))
         (eight     (car (filter-by-len sgn 7)))
         (two-three-five (filter-by-len sgn 5))
         (zero-six-nine  (filter-by-len sgn 6))
         (A  (— seven one))
         (EG (— eight four one A))
         (D  (— (∩ two-three-five) A EG))
         (B  (— eight seven EG D))
         (G  (∩ (— (∩ zero-six-nine) A B) EG))
         (E  (— EG G))
         (F  (— (∩ zero-six-nine) A B G))
         (C  (— one F)))
    (list
     (append A B C E F G)               ; 0
     one                                ; 1
     (append A C D E G)                 ; 2
     (append A C D F G)                 ; 3
     four                               ; 4
     (append A B D F G)                 ; 5
     (append A B D E F G)               ; 6
     seven                              ; 7
     eight                              ; 8
     (append A B C D F G))))            ; 9

(defun set-equal-p (s1 s2)
  (null (set-exclusive-or s1 s2)))

(defun line->number (line)
  (loop for num in (cadr line)
        with decoded-nums = (decode-signals (car line))
        collect (- 10 (length (member num decoded-nums :test #'set-equal-p))) into digits
        finally (return (parse-integer (format nil "~{~A~}" digits)))))

(defun sum-numbers (displays)
  (apply '+ (loop for line in displays collect (line->number line))))

;; Solution
(aoc:solve
 (count-unique (mapcar #'cadr +day8+))
 (sum-numbers +day8+))

;; Tests
(use-package :lisp-unit)

(define-test day8
  (let ((input (parse-input
                '("be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe"
                  "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc"
                  "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg"
                  "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb"
                  "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea"
                  "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb"
                  "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe"
                  "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef"
                  "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb"
                  "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"))))
    (assert-equal 26 (count-unique (mapcar #'cadr input)))
    (assert-equal 61229 (sum-numbers input))))

(run-tests '(day8))
