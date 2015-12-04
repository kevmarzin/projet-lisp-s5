(in-package :sudoku)

; Grille de départ de test
(defparameter *grille-test* #2A((1 0 0 0 0 4 0 0 5)
				(0 0 0 9 5 0 0 8 0)
				(0 0 0 0 0 3 0 9 0)
				(0 0 5 0 0 2 0 0 4)
				(0 0 1 0 6 0 7 0 0)
				(7 0 0 3 0 0 2 0 0)
				(0 6 0 5 0 0 0 0 0)
				(0 8 0 0 1 6 0 0 0)
				(5 0 0 2 0 0 0 0 7)))

;; Création de la grille de jeu
(defparameter *grille* (make-instance 'grille-sudoku))

;; Définition de la boucle while
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

;; Conversion du caractère passé en paramètre en un entier de 0 (A) à 25 (Z)
(defmacro conversion-char-to-int (caractere)
  `(- (char-int ,caractere) (char-int #\A)))
    

;; Fonction qui renvoie T si les information saisie par l'utilisiateur sont valide
(defun saisie-valide (ligne colonne valeur)
  (and (<= 0 ligne (1- (nb-lignes *grille*))) ; 0 <= ligne <= nb-lignes - 1
       (<= 0 colonne (1- (nb-colonnes *grille*))) ; 0 <= colonne <= nb-colonnes - 1
       (<= 0 valeur 9) ; 0 <= valeur <= 9
       (modifiable (aref (tab *grille*) ligne colonne)))) ; case modifiable ?


;; Fonction principale
(defun sudoku (grid)
  (init-grille *grille* grid) ; Initialisation de la grille

  (let ((jouer t)
	(coup-impossible nil)
	(colonne-saisie nil)
	(ligne-saisie nil)
	(valeur-saisie nil)
	(partie-finie nil))
    
    (while jouer ; Boucle de jeu
      (if (not partie-finie)
	  (progn (print *grille*) ; Affichage de la grille
		 (setf coup-impossible t)
		 (while coup-impossible ; Boucle qui redemande la saisie tant qu'elle est incorrecte
		   (format t "~A coups restants~%" (nb-coups-restants *grille*))
		   (format t " C L? ") ; Demande des coordonnées de la case à modifer
		   (setf colonne-saisie (conversion-char-to-int (read-char))) ; Récupération de la colonne
		   (setf ligne-saisie (1- (read))) ; Récupération de la ligne
		   (format t "Value? ") ; Demande de la valeur
		   (setf valeur-saisie (read)) ; Récupération de la valeur

		   ; Vérification de la validité de la saisie, on continu la demande si elle est incorrect
		   (if (setf coup-impossible (not (saisie-valide ligne-saisie
								 colonne-saisie
								 valeur-saisie)))
		       (format t "Informations saisies invalides reessayer~%")
		       ; vérification que le coup demandé soit valide : pas la même valeur dans le carré
		       ; ou sur la même ligne ou colonne
		       (if (setf coup-impossible (not (coup-valide *grille*
								   ligne-saisie
								   colonne-saisie
								   valeur-saisie)))
			   (format t "Coup impossible reessayer~%")
			   (progn (jouer-coup *grille* ligne-saisie colonne-saisie valeur-saisie) ; jouer le coup
				  (setf partie-finie (grille-finie *grille*))))))) ; vérifier que la grille n'est pas finie
	  (progn (format t "Bien joue !! (quitter avec q) ") ; Message de fin
		 (if (equal #\q (read-char))
		     (setf jouer nil)))))))
		
