(in-package :sudoku)

(defclass case-sudoku ()
  ((contenu :initarg :contenu :accessor contenu))
  (:documentation "case"))

(defmethod print-object ((c case-sudoku) stream)
  (format stream "~A " (slot-value c 'contenu)))
