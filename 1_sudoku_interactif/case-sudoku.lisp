(in-package :sudoku)

;; Classe qui représente une case de la grille de sudoku, elle a un numéro et est modifiable ou non
(defclass case-sudoku ()
  ((contenu :initarg :contenu :accessor contenu)
   (modifiable :initarg :modifiable :accessor modifiable))
  (:documentation "Case de la grille de sudoku"))

(defmethod print-object ((c case-sudoku) stream)
  (format stream "~A " (slot-value c 'contenu)))
