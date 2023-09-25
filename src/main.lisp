(defpackage rpg
  (:use :cl))
(in-package :rpg)

(defparameter *player* (make-hash-table))
(setf (gethash 'hp *player*) 100)
(setf (gethash 'money *player*) 50)
(setf (gethash 'items *player*) '())
(setf (gethash 'str *player*) 1)
(setf (gethash 'def *player*) 1)
(setf (gethash 'agi *player*) 1)

;; アイテム,statusごとの増減管理
(defparameter *approp-table* (make-hash-table))
(setf (gethash 'hp *approp-table*) '(0 . 10))
(setf (gethash 'money *approp-table*) '(-1000 . 1000))
(setf (gethash 'str *approp-table*) '(-10 . 10))
(setf (gethash 'def *approp-table*) '(-10 . 10))
(setf (gethash 'agi *approp-table*) '(-10 . 10))

;; shopでえらべるアイテムリスト
(defparameter *item-table* (make-hash-table))
(setf (gethash 'red-posion *item-table*) '((amount . 100) (description . "HP is healed 100 times.")))
(setf (gethash 'blue-posion *item-table*) '((amount . 100) (description . "HP is healed 200 times.")))
(setf (gethash 'sword *item-table*) '((amount . 1000) (description . "STR is healed 1 times.")))
(setf (gethash 'shield *item-table*) '((amount . 1000) (description . "DEF is healed 1 times.")))
(setf (gethash 'shoes *item-table*) '((amount . 1000) (description . "AGI is healed 1 times.")))

;; パネルの種類リスト
(defparameter *panel-list*
  '(shop battle status random hp money))

;;panel eventを実行する
(defun panel-event (panel)
  (cond ((or  (eq panel 'hp) (eq panel 'money))
	 (setf (gethash panel *player*)
	       (+ (gethash panel *player*) (rise-and-fall panel)))
	 )
	((eq panel 'status)
	 ;; 3つ共に変動させる
	 (rise-and-fall 'str)
	 (rise-and-fall 'def)
	 (rise-and-fall 'agi)
	 )
	((eq panel 'shop)
	 (shop))
	((eq panel 'battle)
	 (battle))
	((eq panel 'random)
	 (panel-event (rand-panel)))
	)
  )

;; ランダムにパネルを生成する
(defun rand-panel ()
  (nth (random (length *panel-list*)) *panel-list*))

;; プロパティごとの増減をきめて、そのままグローバル変数に設定する
(defun rise-and-fall (panel)
  (let ((v-range (gethash panel *approp-table*)))
    (setf (gethash panel *player*)
	  (+ (random (- (cdr v-range) (car v-range) -1)) (car v-range)))
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


;; battle
(defun battle ()
  (print "battle!")
  (print "cresating...")
  )

;; shop
(defun shop ()
  (print "creating...")
  (format t "welcome!~% ~{~A~}"
	  (mapcar #'(lambda (item) (format nil "" (car item) (cdr item)))))
  )

(defun main-loop ()
  (let ((cont t)
	(user-input -1)
	(choose-panel nil)
	(next-panels '()))
    (loop while cont do
      (progn
	;; choose
	(setf next-panels (next-panel-list 3))
	(format t "==== Player Status ====~%~%")
	(print-player-status)
	(format t "==== Next Choices ====~%~%")
	(loop for p in next-panels for i from 0 do (format t "~A: ~A~%" i p))
	(format t "q: Quit~%~%input: ")
	;; event
	(setf user-input (read))
	;; (print "choose panel number:")
	(cond ((eq user-input 'q)
	       (print "quit")
	       (setf cont nil))
	      (t
	       (setf choose-panel (nth user-input next-panels))
	       (panel-event choose-panel)
	       ;; (print choose-panel))
	       )
	      )

	)
	  )
    )
  )
