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

;; Constante permettant de distinguer le caractère permettant de quitter
(defparameter *caractere-abandon* #\Q)

;; Définition de la boucle while
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

;; Vérifie que la ligne saisie par l'utilisateur est un entier
(defun convertir-ligne (ligne-saisie)
  (if (integerp ligne-saisie)
      (1- ligne-saisie)
      nil))

;; Si colonne n'est composé que du caractère d'abandon, elle est retourner
;; sinon si c'est la chaine est une lettre de A à Z elle est convertie en entier
;; sinon nil est renvoyé
(defun convertir-colonne (colonne-saisie)
  (let* ((colonne (string colonne-saisie))
	 (colonne-char (char colonne 0)))
    (if (= (length colonne) 1)
	(if (eq colonne-char *caractere-abandon*)
	    colonne-char
	    (- (char-int colonne-char) (char-int #\A)))
	nil)))

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
	(colonne-du-coup nil)
	(ligne-du-coup nil)
	(valeur-du-coup nil)
	(partie-finie nil))
    
    (while jouer ; Boucle de jeu
      (if (not partie-finie)
	  (progn (print *grille*) ; Affichage de la grille
		 (setf coup-impossible t)
		 (while coup-impossible ; Boucle qui redemande la saisie tant qu'elle est incorrecte
		   (format t "~A coups restants~%" (nb-coups-restants *grille*))
		   (format t " C L? ") ; Demande des coordonnées de la case à modifer
		   (setf colonne-du-coup (convertir-colonne (read))) ; Récupération de la colonne
		   (if (eq colonne-du-coup *caractere-abandon*)
		       (progn (setf jouer nil)
			      (setf coup-impossible nil))
		       (progn (setf ligne-du-coup (convertir-ligne (read))) ; Récupération de la ligne
			      (format t "Value? ") ; Demande de la valeur
			      (setf valeur-du-coup (read)) ; Récupération de la valeur
			      ; Vérification de la validité de la saisie, on continu la demande si elle est incorrect
			      (if (and colonne-du-coup
				       ligne-du-coup
				       (integerp valeur-du-coup)
				       (setf coup-impossible (not (saisie-valide ligne-du-coup
										 colonne-du-coup
										 valeur-du-coup))))
				  (format t "Informations saisies invalides reessayer~%")

				  ; vérification que le coup demandé soit valide : pas la même valeur dans le carré
				  ; ou sur la même ligne ou colonne
				  (if (setf coup-impossible (not (coup-valide *grille*
									      ligne-du-coup
									      colonne-du-coup
									      valeur-du-coup)))
				      (format t "Coup impossible reessayer~%")
				      (progn (jouer-coup *grille* ligne-du-coup colonne-du-coup valeur-du-coup) ; jouer le coup
					     (setf partie-finie (grille-finie *grille*))))))))) ; vérifier que la grille n'est pas finie
	  (progn (format t "Bien joue !! (quitter avec Q) ") ; Message de fin
		 (if (equal *caractere-abandon* (read-char))
		     (setf jouer nil)))))))
		
