(in-package :sudoku)

(defclass grille ()
  ((nb-lignes :initform 9 :reader nb-lignes)
   (nb-colonnes :initform 9 :reader nb-colonnes)
   (g :initarg :grid :reader g))
  (:documentation "grille de jeu"))

(defmethod initgrille ((gr grille) tab)
  (dotimes (i 9)
    (dotimes (j 9)
      (setf (aref (slot-value gr 'g) i j) (make-instance 'case :ligne i :colonne j :contenu (aref tab i j))))))
