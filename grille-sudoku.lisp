(in-package :sudoku)

(defclass grille-sudoku ()
  ((nb-lignes :initform 9 :reader nb-lignes)
   (nb-colonnes :initform 9 :reader nb-colonnes)
   (tab :accessor tab :initform nil))
  (:documentation "grille de jeu"))

(defgeneric init-grille (grille new-tab)
  (:documentation "Initialise les cases de la grille avec le tableau new-tab"))

(defgeneric modifier-case (grille ligne colonne valeur)
  (:documentation "Modifie la case demandee avec la valeur donnee, si les arguments donnes sont valides"))

;; Initialisation des cases de la grille
;;   - avec le tableau d'entiers new-tab
;;   - si l'entier est égal à 0, la case n'est pas modifiable
(defmethod init-grille ((gr grille-sudoku) new-tab)
  (setf (tab gr) (make-array (list (nb-lignes gr) (nb-colonnes gr)) :initial-element nil))
  (dotimes (i (nb-lignes gr))
    (dotimes (j (nb-colonnes gr))
      (let ((contenu-case (aref new-tab i j)))
	(setf (aref (tab gr) i j)
	      (make-instance 'case-sudoku
			     :contenu contenu-case
			     :modifiable (= contenu-case 0)))))))

(defmethod modifier-case ((gr grille-sudoku) ligne colonne valeur)
  (let ((case-a-modifier (aref (tab gr) ligne colonne)))
    (if (and (<= 0 ligne (1- (nb-lignes gr)))
	     (<= 0 colonne (1- (nb-colonnes gr)))
	     (<= 1 valeur 9)
	     (modifiable case-a-modifier))
	(setf (contenu case-a-modifier) valeur)
	nil)))

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
