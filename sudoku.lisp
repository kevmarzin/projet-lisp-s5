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
    

;; Fonction principale
(defun sudoku (grid)
  (init-grille *grille* grid) ; Initialisation de la grille

  (let ((jouer t)
	(saisie-incorrect nil)
	(colonne-saisie nil)
	(ligne-saisie nil))
    (while jouer ; Boucle de jeu
      (print *grille*) ; Affichage de la grille
      (setf saisie-incorrect t)
      (while saisie-incorrect ; Boucle qui redemande la saisie si elle est incorrecte
	(format t " C L? ") ; Demande des coordonnées de la case à modifer
	(setf colonne-saisie (read-char)) ; Récupération de la colonne
	(setf ligne-saisie (read)) ; Récupération de la ligne
	(format t "Value? ") ; Demande de la valeur

	; Vérification de la validité de la saisie, on continu la demande si elle est incorrect
	(if (setf saisie-incorrect (not (modifier-case *grille*
						       (1- ligne-saisie)
						       (conversion-char-to-int colonne-saisie)
						       (read))))
	    (format t "Informations saisies invalides reessayer ~%"))))))
