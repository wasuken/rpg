(defpackage rpg
  (:use :cl))
(in-package :rpg)

・空マス：特に何もないマス
・再生成マス：マップを再生性する。これは後ほど階層移動マスと変更するかも。1エリアにつき必ず一つだけ生成する。
・ショップマス：アイテムを購入できる
・戦闘マス：戦闘が始まる
・能力マス：能力が変わる
・ランダムマス：何が起こるかわからないマス。内容としてはランダムマス以外の種類のマスを生成時点でランダムに生成する。
・体力マス：体力が増減する
・お金マス：お金が増減する

(defconstant +panel-empty+ 0)
(defconstant +panel-regenerate+ 1)
(defconstant +panel-shop+ 2)
(defconstant +panel-battle+ 3)
(defconstant +panel-status+ 4)
(defconstant +panel-random+ 5)
(defconstant +panel-hp+ 6)
(defconstant +panel-money+ 7)


(defparameter *player-hp* 100)
(defparameter *player-money* 50)
(defparameter *player-items* '())
(defparameter *area* '())

(defun generate-panel ()
  )
