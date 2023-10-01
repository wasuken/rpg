(defpackage rpg
  (:use :cl)
  (:export :main-loop))
(in-package :rpg)

(defparameter *player* (make-hash-table))
(defun init-player ()
  (setf *player* (make-hash-table))
  (setf (gethash 'hp *player*) 100)
  (setf (gethash 'money *player*) 50)
  (setf (gethash 'items *player*) '())
  (setf (gethash 'str *player*) 1)
  (setf (gethash 'def *player*) 1)
  (setf (gethash 'agi *player*) 1))

;; アイテム,statusごとの増減管理
(defparameter *approp-table* (make-hash-table))
(defun init-approp ()
  (setf *approp-table* (make-hash-table))
  (setf (gethash 'hp *approp-table*) '(0 . 10))
  (setf (gethash 'money *approp-table*) '(-1000 . 1000))
  (setf (gethash 'str *approp-table*) '(-10 . 10))
  (setf (gethash 'def *approp-table*) '(-10 . 10))
  (setf (gethash 'agi *approp-table*) '(-10 . 10)))

;; shopでえらべるアイテムリスト
(defparameter *item-table* (make-hash-table))
(defparameter *item-table-keys* '())
(defun init-item ()
  (setf *item-table* (make-hash-table))
  (setf *item-table-keys* '())
  (setf (gethash 'red-posion *item-table*) '((amount . 100) (description . "HP is healed 100 times.")))
  (setf (gethash 'blue-posion *item-table*) '((amount . 100) (description . "HP is healed 200 times.")))
  (setf (gethash 'sword *item-table*) '((amount . 1000) (description . "STR is healed 1 times.")))
  (setf (gethash 'shield *item-table*) '((amount . 1000) (description . "DEF is healed 1 times.")))
  (setf (gethash 'shoes *item-table*) '((amount . 1000) (description . "AGI is healed 1 times.")))
  (maphash #'(lambda (k v) (push k *item-table-keys*)) *item-table*)
  )

(defun all-init ()
  (init-player)
  (init-approp)
  (init-item)
  )


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
	 ;; (shop)
	 (simple-shop)
	 )
	((eq panel 'battle)
	 ;; (battle)
	 (simple-battle)
	 )
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

;; minus allways zero
(defmacro minus-init-zero (&rest args)
  (let ((rst `(- ,@args)))
    `(if (> ,rst -1)
	 ,rst
	 0)
    )
  )

;; random fluctuation in status(hp)
;; end game if hp is zero
(defun simple-battle ()
  (let* ((enemy-damage (+ (random 5) (random 5))) ; atk(tentative) + base(tentative)
	 (player-anti-damage (gethash 'def *player*))
	 (damage (- enemy-damage player-anti-damage))
	 (updated-hp (- (gethash 'hp *player*) damage)))
    (setf (gethash 'hp *player*) updated-hp)
    )
  )

;; random selection of items
;; determine if an item can be purchased
(defun simple-shop ()
  (let* ((item-names *item-table-keys*)
	 (item-name (nth (random (length item-names)) *item-table-keys*))
	 (item-alist (gethash item-name *item-table*))
	 (item-amount (cdr (assoc 'amount item-alist)))
	 (player-money (gethash 'money *player*)))
    (if (>= player-money item-amount)
	(progn
	  (setf (gethash 'money *player*) (- player-money item-amount))
	  (format t "buy: ~A..~A~%" item-name (assoc 'description item-alist)))
	(format t "few money, failed shopping...~%")
	)
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
	(next-panels (next-panel-list 3))
	(message ""))
    (all-init)
    (loop while cont do
      (progn
	;; choose
	(format t "==== Player Status ====~%~%")
	(print-player-status)
	(format t "~%==== Next Choices ====~%~%")
	(loop for p in next-panels for i from 0 do (format t "~A: ~A~%" i p))
	(format t "q: Quit~%")
	(when (> (length message) 0)
	  (format t "~%==== Messages ====~%~%")
	  (format t "~%~A~%" message)
	  (setf message "")
	  )
	(format t "~%input: ")
	;; event
	(setf user-input (read))
	;; (print "choose panel number:")
	;; (format t "~A: ~A: ~A ~%" (type-of user-input) user-input (string= (symbol-name user-input) "Q"))

	(cond ((numberp user-input)
	       (cond ((nth user-input next-panels)
		      (progn
			(setf choose-panel (nth user-input next-panels))
			(panel-event choose-panel)
			;; if success, must code.
			(setf next-panels (next-panel-list 3))
			)
		      )
		     (t
		      (setf message "no panel number."))
		     )
	       ;; (print choose-panel))
	       )
	      ((string= (symbol-name user-input) "Q")
	       (print "quit")
	       (setf cont nil)
	       (sb-ext:quit)
	       )

	      (t (setf message "ignore input."))
	      )
	(format t "~c[2J" #\Escape)
	)
	  )
    )
  )
