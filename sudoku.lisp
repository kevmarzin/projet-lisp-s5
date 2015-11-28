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

;; Définition de la boucle while
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

(defmacro conversion-char-to-int (caractere)
  `(- (char-int ,caractere) (char-int #\A)))
    

;;Fonction principale
(defun sudoku (grid)
  (init-grille *grille* grid) ; Initialisation de la grille

  (let ((jouer t))
    (while jouer ; Boucle de jeu
      (print *grille*)
      (let ((saisie-incorrect t))
	(while saisie-incorrect
	  (format t "C L? ")
	  (let ((colonne (read-char))
		(ligne (read)))
	    (format t "Value? ")
	    (if (setf saisie-incorrect (not (modifier-case *grille*
							   (1- ligne)
							   (conversion-char-to-int colonne)
							   (read))))
		(format t "Informations saisies invalides reessayer ~%"))))))))
