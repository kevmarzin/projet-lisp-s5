(in-package :sudoku)

(defclass grille-sudoku ()
  ((nb-lignes :initform 9 :reader nb-lignes)
   (nb-colonnes :initform 9 :reader nb-colonnes)
   (taille-carre :initform 3 :reader taille-carre)
   (nb-coups-restants :initform 0 :accessor nb-coups-restants)
   (tab :accessor tab :initform nil))
  (:documentation "grille de jeu"))

(defgeneric init-grille (grille new-tab)
  (:documentation "Initialise les cases de la grille avec le tableau new-tab ainsi que le nombre de coups restants"))

(defgeneric jouer-coup (grille ligne colonne valeur)
  (:documentation "Modifie la case demandee avec la valeur donnee, si les arguments donnes sont valides"))

(defgeneric coup-valide (grille ligne colonne valeur)
  (:documentation "Détermine si le coup est valide"))

;; Initialisation des cases de la grille
;;   - avec le tableau d'entiers new-tab
;;   - si l'entier est égal à 0, la case n'est pas modifiable
;; Et initialisation du nombre de coups restants
(defmethod init-grille ((gr grille-sudoku) new-tab)
  (setf (tab gr) (make-array (list (nb-lignes gr) (nb-colonnes gr)) :initial-element nil))
  (setf (nb-coups-restants gr) (* (nb-lignes gr) (nb-colonnes gr))) 
  (dotimes (i (nb-lignes gr))
    (dotimes (j (nb-colonnes gr))
      (let ((contenu-case (aref new-tab i j)))
	(setf (aref (tab gr) i j)
	      (make-instance 'case-sudoku
			     :contenu contenu-case
			     :modifiable (= contenu-case 0)))
	(if (not (zerop contenu-case))
	    (decf (nb-coups-restants gr)))))))

;; Joue le coup demandé et décrémente le nombre de coups restant, sauf si la valeur est égal à 0 (incrémente)
(defmethod jouer-coup ((gr grille-sudoku) ligne colonne valeur)
  (setf (contenu (aref (tab gr) ligne colonne))
	valeur)
  (if (not (zerop valeur))
      (decf (nb-coups-restants gr))
      (incf (nb-coups-restants gr))))

;; Détermine si la valeur n'est pas présente sur la ligne demandée, sur la colonne demandée ou dans le carré
(defmethod coup-valide ((gr grille-sudoku) ligne colonne valeur)
  (let ((valide t))
    (if (not (zerop valeur))
	(progn (do ((c 0 (1+ c)))
		   ((or (not valide) (>= c (nb-colonnes gr))))
		 (setf valide (not (= valeur (contenu (aref (tab gr) ligne c))))))
	       (if valide
		   (do ((l 0 (1+ l)))
		       ((or (not valide) (>= l (nb-lignes gr))) valide)
		     (setf valide (not (= valeur (contenu (aref (tab gr) l colonne)))))))
	       (if valide
		   (setf valide (coup-valide-carre gr ligne colonne valeur)))))
    valide))

(defmethod coordonnees-carre ((gr grille-sudoku) ligne colonne)
  (cons (* (do ((coef-ligne 0 (1+ coef-ligne)))
	       ((< (- ligne (* coef-ligne (taille-carre gr)))
		   (taille-carre gr))
		coef-ligne))
	   (taille-carre gr))
	(cons (* (do ((coef-colonne 0 (1+ coef-colonne)))
		     ((< (- colonne (* coef-colonne (taille-carre gr)))
			 (taille-carre gr)) coef-colonne))
		 (taille-carre gr))
	      '())))

(defmethod coup-valide-carre ((gr grille-sudoku) ligne colonne valeur)
  (let* ((coor-carre (coordonnees-carre gr ligne colonne))
	 (ligne-carre (car coor-carre))
	 (colonne-carre (car (cdr coor-carre)))
	 (coup-ok t))
    (dotimes (l (taille-carre gr))
      (dotimes (c (taille-carre gr))
	(let* ((ligne-a-verif (+ ligne-carre l))
	      (colonne-a-verif (+ colonne-carre c))
	      (contenu-a-verif (contenu (aref (tab gr) ligne-a-verif colonne-a-verif))))
	  (if (and (not (= ligne-a-verif ligne))
		   (not (= colonne-a-verif colonne))
		   (= contenu-a-verif valeur))
	      (setf coup-ok nil)))))
    coup-ok))

(defmethod grille-finie ((gr grille-sudoku))
  (zerop (nb-coups-restants gr)))
  

(defmethod print-object ((gr grille-sudoku) stream)
  (format stream "   | A B C | D E F | G H I |~%")
  (format stream "****************************~%")
  (dotimes (i (nb-lignes gr))
    (if (and (> i 0) (= (mod i 3) 0))
	(format stream "****************************~%"))
    (format stream " ~A | " (1+ i))
    (dotimes (j (nb-colonnes gr))
      (if (and (> j 0) (= (mod j 3) 0))
	  (format stream "| "))
      (if (zerop (contenu (aref (tab gr) i j)))
	  (format stream "  ")
	  (format stream "~A " (contenu (aref (tab gr) i j))))
      (if (= j 8)
	  (format stream "|~%"))))
  (format stream "****************************~%"))
