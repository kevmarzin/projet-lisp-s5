(in-package :sudoku)

(defclass case ()
  ((ligne :initarg :ligne :accessor lig)
   (colonne :initarg :colonne :accessor col)
   (contenu :initarg :contenu :accessor cont))
  (:documentation "case"))
