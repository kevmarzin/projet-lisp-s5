(in-package :sudoku)

(defparameter *grille-test* #2A((1 0 0 0 0 4 0 0 5)
				(0 0 0 9 5 0 0 8 0)
				(0 0 0 0 0 3 0 9 0)
				(0 0 5 0 0 2 0 0 4)
				(0 0 1 0 6 0 7 0 0)
				(7 0 0 3 0 0 2 0 0)
				(0 6 0 5 0 0 0 0 0)
				(0 8 0 0 1 6 0 0 0)
				(5 0 0 2 0 0 0 0 7)))

;; Creation de la grille de jeu
(defparameter *grille* (make-instance 'grille-sudoku))
  
;;Fonction principale
(defun sudoku (grid)
  (init-grille *grille* grid) ; Initialisation de la grille
  *grille*)
