(defpackage rpg
  (:use :cl))
(in-package :rpg)

(defparameter *area* '())

(defparameter *player* (make-hash-table))
(setf (gethash 'hp *player*) 100)
(setf (gethash 'money *player*) 50)
(setf (gethash 'items *player*) '())
(setf (gethash 'str *player*) 1)
(setf (gethash 'def *player*) 1)
(setf (gethash 'agi *player*) 1)

(defparameter *panel-list*
  '(shop battle status random hp money))

(defun panel-to-str-one (p)
  (cond ((eq p 'shop) "s")
	((eq p 'battle) "b")
	((eq p 'status) "t")
	((eq p 'random) "r")
	((eq p 'hp) "h")
	((eq p 'money) "m")
	(t "?")
	)
  )

(defun interleave-with (lst elem)
  (cdr (reverse
	(reduce #'(lambda (a b)
		    (cons b (cons elem a)))
		lst
		:initial-value '())))
  )

(defun range (max &key (min 0) (step 1))
  (loop for n from min below max by step
	collect n))

(defun print-player-status ()
  (maphash #'(lambda (k v)
	       (format t "~A: ~A~%" k v))
	   *player*))

(defun next-panel-list (n)
  (let ((panel-list '()))
    (loop for v = (nth (random (length *panel-list*)) *panel-list*)
	    then (nth (random (length *panel-list*)) *panel-list*)
	  while (< (length panel-list) n)
	  do (cond ((eq v 'random)
		    (push v panel-list))
		   ((not (find v panel-list))
		    (push v panel-list))
		   )
	  )
    panel-list
    )
  )
