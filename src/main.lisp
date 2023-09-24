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

;; アイテムごとの増減管理
(defparameter *approp-table* (make-hash-table))
(setf (gethash 'hp *approp-table*) '(0 . 10))
(setf (gethash 'money *approp-table*) '(-1000 . 1000))
(setf (gethash 'str *approp-table*) '(-10 . 10))
(setf (gethash 'def *approp-table*) '(-10 . 10))
(setf (gethash 'agi *approp-table*) '(-10 . 10))

(defparameter *item-table* (make-hash-table))
(setf (gethash 'red-posion *item-table*) '(amount . 100 description . "HP is healed 100 times."))
(setf (gethash 'blue-posion *item-table*) '(amount . 100 description . "HP is healed 200 times."))
(setf (gethash 'sword *item-table*) '(amount . 1000 description . "STR is healed 1 times."))
(setf (gethash 'shield *item-table*) '(amount . 1000 description . "DEF is healed 1 times."))
(setf (gethash 'shoes *item-table*) '(amount . 1000 description . "AGI is healed 1 times."))

(defparameter *panel-list*
  '(shop battle status random hp money))

(defparameter *panel-event-list*
  '(shop battle status random hp money))

(defun set-panel-event ()
  (mapcar #'(lambda (p) `(,p . ,(generate-event p))) *panel-list*))

(defun rand-panel ()
  (nth (random (length *panel-list*)) *panel-list*))

(defun rise-and-fall (panel)
  (let ((v-range (gethash panel *approp-table*)))
    (setf (gethash panel *player*)
	  (+ (random (- (cdr v-range) (car v-range) -1)) (car v-range)))
    )
  )

(defun battle ())

(defun shop ()
  (format t "welcome!~% ~{~A~}"
	  (mapcar #'(lambda (item) (format nil "" (car item) (cdr item)))))
  )

(defun generate-event (panel)
  (cond ((or (eq panel 'status) (eq panel 'hp) (eq panel 'money))
	 (setf (gethash panel *player*)
	       (+ (gethash panel *player*) (rise-and-fall p)))
	 )
	((eq panel 'shop)
	 (shop))
	((eq panel 'battle)
	 (battle))
	((eq panel 'random)
	 (generate-event (rand-panel)))
	))

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
    (loop for v = (rand-panel) then (rand-panel)
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
