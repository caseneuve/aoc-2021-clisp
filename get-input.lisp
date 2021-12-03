#!/usr/bin/sbcl --script

(load "~/.quicklisp/setup.lisp")

(ql:quickload :drakma)
(ql:quickload :local-time)

(load "env.lisp")

(defparameter *input-url* "https://adventofcode.com/2021/day/~a/input")
(defparameter *cookie* (cons "cookie" (concatenate 'string "session=" *token*)))

(defun current-day ()
  (local-time:format-timestring nil (local-time:now) :format '(:day)))

(defun use-day ()
  (let ((arg (first (rest *posix-argv*))))
    (if (and arg (numberp (parse-integer arg)))
        arg (current-day))))

(defun interpolate-url (day)
  (format nil *input-url* day))

(defun save-input ()
  (let* ((day (use-day))
         (file (format nil "~2,'0d.in" (parse-integer day))))
    (multiple-value-bind
          (body status)
        (drakma:http-request (interpolate-url day) :additional-headers (list *cookie*))
      (if (= status 200)
          (with-open-file (stream file
                                  :direction :output
                                  :if-exists nil
                                  :if-does-not-exist :create)
            (if stream
                (progn
                  (print status)
                  (format stream body)
                  (format t "Saved ~A~%" file))
                (format t "~A already exists!~%" file)))
          (format t "Something went wrong (got ~a)...~%" status)))))

(save-input)
