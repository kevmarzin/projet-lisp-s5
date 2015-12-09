;; Classe qui représente une case de la grille de sudoku, elle a un numéro et est modifiable ou non
(defclass case-sudoku ()
  ((contenu :initarg :contenu :accessor contenu)
   (modifiable :initarg :modifiable :accessor modifiable)
   (valeurs-possibles :initform '() :initarg :vxaleurs-possibles :accessor coups-possibles))
  (:documentation "Case de la grille de sudoku"))

;; Grille du sudoku
(defclass grille-sudoku ()
  ((nb-lignes :initform 9 :reader nb-lignes)
   (nb-colonnes :initform 9 :reader nb-colonnes)
   (taille-carre :initform 3 :reader taille-carre)
   (nb-coups-restants :initform 0 :accessor nb-coups-restants)
   (tab :accessor tab :initform nil))
  (:documentation "Grille de jeu"))

(defgeneric init-grille (grille new-tab)
  (:documentation "Initialise les cases de la grille avec le tableau new-tab ainsi que le nombre de coups restants"))

(defgeneric jouer-coup (grille ligne colonne valeur)
  (:documentation "Modifie la case demandee avec la valeur donnee, si les arguments donnes sont valides"))

(defgeneric coup-valide (grille ligne colonne valeur)
  (:documentation "Détermine si le coup est valide"))

(defgeneric coup-valide-carre (grille ligne colonne valeur)
  (:documentation "Renvoie vrai si la valeur se trouve dans le carré de la case (ligne, colonne), hormis cette case"))

(defgeneric coordonnees-carre (grille ligne colonne)
  (:documentation "Renvoie une liste de de nombre qui sont les coordonnees du carre ou se trouve la case (ligne, colonne)"))

;; Initialisation des cases de la grille
;;   - avec le tableau d'entiers new-tab
;;   - si l'entier est égal à 0, la case n'est pas modifiable
;; Et initialisation du nombre de coups restants
(defmethod init-grille ((gr grille-sudoku) new-tab)
  (setf (tab gr) (make-array (list (nb-lignes gr) (nb-colonnes gr)) :initial-element nil))
  (setf (nb-coups-restants gr) (* (nb-lignes gr) (nb-colonnes gr)))

  ;; Parcours de la grille et initialisation des cases
  ;; Le nombre de coups restants est mis à jour quand une case contient un numéro
  (dotimes (l (nb-lignes gr))
    (dotimes (c (nb-colonnes gr))
      (let ((contenu-case (aref new-tab l c)))
	(setf (aref (tab gr) l c)
	      (make-instance 'case-sudoku
			     :contenu contenu-case
			     :modifiable (= contenu-case 0)))
	(if (not (zerop contenu-case))
	    (decf (nb-coups-restants gr))))))

  ;; Initialise la liste de coups possibles pour chaque case modifiable
  (dotimes (l (nb-lignes gr))
    (dotimes (c (nb-colonnes gr))
      (if (modifiable (aref (tab gr) l c))
	  (do ((val 1 (1+ val)))
	      ((> val (nb-lignes gr)))
	    (if (coup-valide gr l c val)
		(setf (coups-possibles (aref (tab gr) l c))
		      (cons val (coups-possibles (aref (tab gr) l c))))))))))

;; Joue le coup demandé
(defmethod jouer-coup ((gr grille-sudoku) ligne colonne valeur)
  (setf (coups-possibles (aref (tab gr) ligne colonne)) nil) ; coups possibles de la case mis à 0
  (setf (contenu (aref (tab gr) ligne colonne)) valeur) ; on joue le coup
  (decf (nb-coups-restants gr))

  ;; On parcours la ligne pour enlever valeur des coups possibles de chaque case
  (dotimes (l (nb-lignes gr))
    (setf (coups-possibles (aref (tab gr) l colonne))
	  (remove valeur (coups-possibles (aref (tab gr) l colonne)))))

  ;; Idem pour chaque colonne
  (dotimes (c (nb-colonnes gr))
    (setf (coups-possibles (aref (tab gr) ligne c))
	  (remove valeur (coups-possibles (aref (tab gr) ligne c)))))

  ;; Idem pour le carré
  (let* ((coor-carre (coordonnees-carre gr ligne colonne))
	 (ligne-carre (car coor-carre))
	 (colonne-carre (car (cdr coor-carre))))
    (dotimes (l (taille-carre gr))
      (dotimes (c (taille-carre gr))
	(let* ((ligne-a-verif (+ ligne-carre l))
	       (colonne-a-verif (+ colonne-carre c))
	       (case-a-verif (aref (tab gr) ligne-a-verif colonne-a-verif)))
	  (if (and (not (= ligne-a-verif ligne))
		   (not (= colonne-a-verif colonne)))
	      (setf (coups-possibles case-a-verif)
		    (remove valeur (coups-possibles case-a-verif)))))))))

;; Détermine si la valeur n'est pas présente sur la ligne demandée, sur la colonne demandée ou dans le carré
(defmethod coup-valide ((gr grille-sudoku) ligne colonne valeur)
  (let ((valide t))
    (if (not (zerop valeur))
	(progn (do ((c 0 (1+ c))) ; parcours de la colonne
		   ((or (not valide) (>= c (nb-colonnes gr))))
		 (setf valide (not (= valeur (contenu (aref (tab gr) ligne c))))))
	       (if valide ; si la valeur entrée n'est pas présente sur la colonne
		   (do ((l 0 (1+ l))) ; on vérifie sur la ligne
		       ((or (not valide) (>= l (nb-lignes gr))) valide)
		     (setf valide (not (= valeur (contenu (aref (tab gr) l colonne)))))))
	       (if valide ; si la valeur entrée n'est pas présente sur la ligne et sur la colonne on vérifie dans le carré
		   (setf valide (coup-valide-carre gr ligne colonne valeur)))))
    valide))

;; Renvoie les coordonnées de la première case en haut à gauche du carré où se trouve la case spécifiée
(defmethod coordonnees-carre ((gr grille-sudoku) ligne colonne)
  (cons (* (do ((coef-ligne 0 (1+ coef-ligne)))
	       ((< (- ligne (* coef-ligne (taille-carre gr)))
		   (taille-carre gr))
		coef-ligne))
	   (taille-carre gr))
	(list (* (do ((coef-colonne 0 (1+ coef-colonne)))
		     ((< (- colonne (* coef-colonne (taille-carre gr)))
			 (taille-carre gr)) coef-colonne))
		 (taille-carre gr)))))

;; Vérifie que la valeur qui veut être entrée dans la case (ligne, colonne) n'est pas présente dans le carré
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
	  ;; Si on est pas sur la case à modifier, et que les valeurs sont les mêmes le coups n'est pas bon
	  (if (and (not (= ligne-a-verif ligne))
		   (not (= colonne-a-verif colonne))
		   (= contenu-a-verif valeur))
	      (setf coup-ok nil)))))
    coup-ok))

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

;; Création de la grille de jeu
(defparameter *grille* (make-instance 'grille-sudoku))

;; Définition de la boucle while
(defmacro while (test &rest body)
  `(do ()
       ((not ,test))
     ,@body))

;; Parcours de la grille, lorsqu'une case n'a qu'un seul coups possibles on le joue et on arrête le parcours
(defun strat-inclusion ()
  (let ((ligne-case nil)
	(colonne-case nil)
	(valeur-case nil)
	(l 0)
	(c 0)
	(case-trouvee nil))
    (while (and (not case-trouvee)
		(< l (nb-lignes *grille*)))
      (setf c 0)
      (while (and (not case-trouvee)
		  (< c (nb-colonnes *grille*)))
	(let ((case-a-verifiee (aref (tab *grille*) l c)))
	  (if (and (modifiable case-a-verifiee)
		   (= (length (coups-possibles case-a-verifiee)) 1))
	      (progn (setf case-trouvee t)
		     (setf ligne-case l)
		     (setf colonne-case c)
		     (setf valeur-case (car (coups-possibles case-a-verifiee)))))
	  (incf c)))
      (incf l))
    (format t "strat-inclusion : ~A~%" case-trouvee)
    (if case-trouvee
	(progn (jouer-coup *grille* ligne-case colonne-case valeur-case) ; on joue le coup
	       (values ligne-case colonne-case valeur-case)) ; et on retourne les valeurs
	nil)))

;; Parcours de toutes les lignes, si une valeur ne peut être jouée qu'une fois sur la ligne, elle est jouée et on arrête le parcours
(defun strat-exclusion-ligne ()
  (let ((ligne-a-verif 0)
	(occurrence-valeurs nil)
	(case-trouvee nil)
	(retour-colonne nil))

    ;; Parcours de toutes les lignes jusqu'à trouver une case réunissant les conditions
    (while (and (< ligne-a-verif (nb-lignes *grille*))
		(not case-trouvee))
      (setf occurrence-valeurs (make-list 9 :initial-element 0))

      ;; Parcours de toutes les cases de la ligne
      (dotimes (id-case-dans-ligne (nb-colonnes *grille*))
	;; Incrémente le compteur de la valeur à chaque coup possible
	(mapcar (lambda (x) (incf (nth (1- x) occurrence-valeurs)))
		(coups-possibles (aref (tab *grille*) ligne-a-verif id-case-dans-ligne)))) 

      ;; Si une ou plusieurs valeurs ne sont jouables qu'une fois (<=> à un seul endroit)
      (if (>= (length (remove-if-not (lambda (x) (= x 1)) occurrence-valeurs)) 1)
	  (let ((c 0))
	    ;; Recherche de la case où ce coup possible est présent
	    (while (and (< c (nb-colonnes *grille*)) (not case-trouvee))
	      (if (setf case-trouvee (position (1+ (position 1 occurrence-valeurs))
					       (coups-possibles (aref (tab *grille*) ligne-a-verif c))))
		  (setf retour-colonne c) ; sauvegarde de la colonne de la case
		  (incf c)))) ; on change de case sinon
	  (incf ligne-a-verif))) ; on change de ligne si toutes les valeurs sont trouvées plusieurs fois sur la ligne
    (format t "strat-exclusion-ligne : ~A~%" case-trouvee)
    (if case-trouvee
	(progn (jouer-coup *grille* ligne-a-verif retour-colonne (1+ (position 1 occurrence-valeurs)))
	       (values ligne-a-verif retour-colonne (1+ (position 1 occurrence-valeurs))))
	nil)))

;; Parcours de toutes les colonnes, si une valeur ne peut être jouée qu'une fois sur la colonne, elle est jouée et on arrête le parcours
(defun strat-exclusion-colonne ()
  (let ((colonne-a-verif 0)
	(occurrence-valeurs nil)
	(case-trouvee nil)
	(retour-ligne nil))

    ;; Parcours de toutes les colonnes jusqu'à trouver une case réunissant les conditions
    (while (and (< colonne-a-verif (nb-colonnes *grille*))
		(not case-trouvee))
      (setf occurrence-valeurs (make-list 9 :initial-element 0))

      ;; Parcours de toutes les cases de la colonnes
      (dotimes (id-case-dans-colonne (nb-lignes *grille*))
	;; Incrémente le compteur de la valeur à chaque coup possible
	(mapcar (lambda (x) (incf (nth (1- x) occurrence-valeurs)))
		(coups-possibles (aref (tab *grille*) id-case-dans-colonne colonne-a-verif))))

      ;; Si une ou plusieurs valeurs ne sont jouables qu'une fois
      (if (>= (length (remove-if-not (lambda (x) (= x 1)) occurrence-valeurs)) 1)
	  (let ((l 0))
	    ;; Recherche de la case ou ce coup possible est présent
	    (while (and (< l (nb-lignes *grille*)) (not case-trouvee))
	      
	      (if (setf case-trouvee (position (1+ (position 1 occurrence-valeurs))
					       (coups-possibles (aref (tab *grille*) l colonne-a-verif))))
		  (setf retour-ligne l) ; sauvegarde de la ligne de la case
		  (incf l)))) ; on change de case sinon 
	  (incf colonne-a-verif))) ; onchange de colonne si toutes les valeurs sont trouvées plusieurs fois sur la ligne
    (format t "strat-exclusion-colonne : ~A~%" case-trouvee)
    (if case-trouvee
	(progn (jouer-coup *grille* retour-ligne colonne-a-verif (1+ (position 1 occurrence-valeurs)))
	       (values retour-ligne colonne-a-verif (1+ (position 1 occurrence-valeurs))))
	nil)))

;; Parcours de toutes les cases d'un carre, si une valeur ne peut être jouer qu'une fois dans le carré,
;; elle est jouée et on arrête le parcours
(defun strat-exclusion-carre ()
  (let ((nb-carre-par-ligne (/ (nb-lignes *grille*) (taille-carre *grille*)))
	(nb-carre-par-colonne (/ (nb-colonnes *grille*) (taille-carre *grille*)))
	(numcarre-ligne 0)
	(numcarre-colonne 0)
	(case-trouvee nil)
	(occurence-valeurs nil)
	(valeur-retour nil)
	(ligne-case-a-changer nil)
	(colonne-case-a-changer nil))

    ;; Parcours de tous les carrés jusqu'à trouver une case réunissant les conditions
    (while (and (< numcarre-ligne nb-carre-par-ligne) (not case-trouvee))
      (setf numcarre-colonne 0)
      (while (and (< numcarre-colonne nb-carre-par-colonne) (not case-trouvee))
	(setf occurence-valeurs (make-list 9 :initial-element 0))

	;; Parcours de toutes les cases du carré
	(dotimes (l (taille-carre *grille*))
	  (dotimes (c (taille-carre *grille*))
	    (let* ((ligne-a-verif (+ (* numcarre-ligne (taille-carre *grille*)) l))
		   (colonne-a-verif (+ (* numcarre-colonne (taille-carre *grille*)) c))
		   (case-a-verifier (aref (tab *grille*) ligne-a-verif colonne-a-verif)))
	      ;; Incrémente le compteur de la valeur à chaque coup possible
	      (mapcar (lambda (x) (incf (nth (1- x) occurence-valeurs))) (coups-possibles case-a-verifier)))))

	;; Si une ou plusieurs valeurs ne sont jouables qu'une fois
	(if (>= (length (remove-if-not (lambda (x) (= x 1)) occurence-valeurs))
		1)
	    (let ((l 0)
		  (c 0))

	      ;; Recherche de la case où ce coup possible est présent
	      (while (and (< l (taille-carre *grille*)) (not case-trouvee))
		(setf c 0)
		(while (and (< c (taille-carre *grille*)) (not case-trouvee))
		  (setf ligne-case-a-changer (+ (* numcarre-ligne (taille-carre *grille*)) l))
		  (setf colonne-case-a-changer (+ (* numcarre-colonne (taille-carre *grille*)) c))
		  
		  (if (setf case-trouvee (position (1+ (position 1 occurence-valeurs))
						   (coups-possibles (aref (tab *grille*) ligne-case-a-changer colonne-case-a-changer))))
		      (setf valeur-retour (1+ (position 1 occurence-valeurs))) ; sauvegarde de la valeur à jouer
		      (incf c)))
		(incf l)))
	    (incf numcarre-colonne)))
      (incf numcarre-ligne))
    (format t "strat-exclusion-carre : ~A~%" case-trouvee)
    (if case-trouvee
	(progn (jouer-coup *grille* ligne-case-a-changer colonne-case-a-changer valeur-retour)
	       (values ligne-case-a-changer colonne-case-a-changer valeur-retour))
	nil)))

;; Si deux cases contiennent la même paire de coups possibles sur une ligne
;; alors ces coups peuvent être enlevés des autres cases de cette ligne
(defun strat-paire-exclusive-ligne ()
  (format t "strat-paire-exclusive-ligne~%")
  (let ((ligne-a-verif 0)
	(cases-trouvees nil)
	(liste-cases-trouvees nil)
	(colonne-des-cases nil)
	(case-a-jouer-par-ligne 0))

    ;; Parcours de toutes les lignes 
    (while (and (< ligne-a-verif (nb-lignes *grille*))
		(not cases-trouvees))
      (setf liste-cases-trouvees (make-list 9 :initial-element nil))
      (setf case-a-jouer-par-ligne 0)
      ;; Sauvegarde des paires trouvées sur la ligne
      (dotimes (id-case-dans-ligne (nb-colonnes *grille*))
	(let ((coups-possibles-case (coups-possibles (aref (tab *grille*) ligne-a-verif id-case-dans-ligne))))
	  (if coups-possibles-case
	      (progn (incf case-a-jouer-par-ligne)
		     (if (= (length (coups-possibles (aref (tab *grille*) ligne-a-verif id-case-dans-ligne))) 2)
			 (setf (nth id-case-dans-ligne liste-cases-trouvees)
			       (coups-possibles (aref (tab *grille*) ligne-a-verif id-case-dans-ligne))))))))

      ;; Si on a au moins deux paires dans la liste et s'il y a plus de deux cases vides dans la ligne
      (if (and (> case-a-jouer-par-ligne 2)
	       (>= (length (remove-if (lambda (x) (not x)) liste-cases-trouvees)) 2))
	  (let ((i 0)
		(j 0))

	    ;; Parcours de cette liste de paires
	    (while (and (< i (1- (length liste-cases-trouvees)))
			(not cases-trouvees))
	      (setf j (1+ i))
	      (while (and (< j (length liste-cases-trouvees))
			  (not cases-trouvees))

		;; Si deux paires sont égales on sauvegarde la colonne des cases où elle est présente
		(if (= (length (intersection (nth i liste-cases-trouvees)
					     (nth j liste-cases-trouvees)))
		       2)
		    (progn (setf colonne-des-cases (list i j))
			   (setf cases-trouvees t))
		    (incf j)))
	      (incf i))))
      (if (not cases-trouvees)
	  (incf ligne-a-verif)))
    (if cases-trouvees
	;; Parcours de la ligne et suppression de la paire dans les coups possibles des autres cases
	(dotimes (i (nb-colonnes *grille*))
	  (if (not (position i colonne-des-cases))
	      (setf (coups-possibles (aref (tab *grille*) ligne-a-verif i))
		    (set-difference (coups-possibles (aref (tab *grille*) ligne-a-verif i))
				    (coups-possibles (aref (tab *grille*) ligne-a-verif (car colonne-des-cases))))))))
    cases-trouvees))

;; Si deux cases contiennent la même paire de coups possibles sur une colonne
;; alors ces coups peuvent être enlevés des autres cases de cette colonne
(defun strat-paire-exclusive-colonne ()
  (format t "strat-paire-exclusive-colonne~%")
  (let ((colonne-a-verif 0)
	(cases-trouvees nil)
	(liste-cases-trouvees nil)
	(ligne-des-cases nil)
	(case-a-jouer-par-colonne 0))

    ;; Parcours de toutes les colonnes
    (while (and (< colonne-a-verif (nb-colonnes *grille*))
		(not cases-trouvees))
      (setf liste-cases-trouvees (make-list 9 :initial-element nil))
      (setf case-a-jouer-par-colonne 0)
      ;; Sauvegarde des paires trouvées sur la colonne
      (dotimes (id-case-dans-colonne (nb-lignes *grille*))
	(let ((coups-possibles-case (coups-possibles (aref (tab *grille*) id-case-dans-colonne colonne-a-verif))))
	  (if coups-possibles-case
	      (progn (incf case-a-jouer-par-colonne)
		     (if (= (length coups-possibles-case) 2)
			 (setf (nth id-case-dans-colonne liste-cases-trouvees)
			       coups-possibles-case))))))

      ;; Si on a au moins deux paires dans la liste et s'il y a plus de deux cases vides dans la colonne
      (if (and (> case-a-jouer-par-colonne 2)
	       (>= (length (remove-if (lambda (x) (not x)) liste-cases-trouvees)) 2))
	  (let ((i 0)
		(j 0))
	    ;; Parcours de cette liste de paires
	    (while (and (< i (1- (length liste-cases-trouvees)))
			(not cases-trouvees))
	      (setf j (1+ i))
	      (while (and (< j (length liste-cases-trouvees))
			  (not cases-trouvees))

		;; Si deux paires sont égales on sauvegarde la ligne des cases où elle est présente
		(if (= (length (intersection (nth i liste-cases-trouvees)
					     (nth j liste-cases-trouvees)))
		       2)
		    (progn (setf ligne-des-cases (list i j))
			   (setf cases-trouvees t))
		    (incf j)))
	      (incf i))))
      (if (not cases-trouvees)
	  (incf colonne-a-verif)))
    (if cases-trouvees
	;; Parcours de la colonne et suppression de la paire dans les coups possibles des autres cases
	(dotimes (i (nb-lignes *grille*))
	  (position i ligne-des-cases)
	  (if (not (position i ligne-des-cases))
	      (setf (coups-possibles (aref (tab *grille*) i colonne-a-verif))
		    (set-difference (coups-possibles (aref (tab *grille*) i colonne-a-verif))
				    (coups-possibles (aref (tab *grille*) (car ligne-des-cases) colonne-a-verif)))))))
    cases-trouvees))

;; Si dans les cases verifiees,
;; on trouve deux cases qui ont la meme paire de coups possible, on renvoie cette paire sinon nil
(defun paire-trouvee-dans-carre (tab)
  (let ((ligne-i 0)
	(ligne-j 0)
	(colonne-i 0)
	(colonne-j 0)
	(paire nil))

    ;; Parcours du carre tab
    (while (and (< ligne-i (taille-carre *grille*))
		(not paire))
      (setf colonne-j 0)
      (while (and (< colonne-i (taille-carre *grille*))
		  (or (<= colonne-i (- (taille-carre *grille*) 1))
		      (<= ligne-i (taille-carre *grille*)))
		  (not paire))
	(setf ligne-j ligne-i)
	(setf colonne-j (1+ colonne-i))
	(if (>= colonne-j (taille-carre *grille*))
	    (progn (setf colonne-j 0)
		   (incf ligne-j)))

	;; Pour une case i on recherche une paire identique dans les autres cases, si elle est trouvée on la sauvegarde
	;; sinon on change de case i
	(while (and (< ligne-j (taille-carre *grille*))
		    (not paire))
	  (if (and (/= ligne-j ligne-i)
		   (/= colonne-j colonne-i))
	      (setf colonne-j 0))
	  (while (and (< colonne-j (taille-carre *grille*))
		      (not paire))
	    (let ((coups-possibles-i (aref tab ligne-i colonne-i))
		  (coups-possibles-j (aref tab ligne-j colonne-j)))
	      (if (and coups-possibles-i coups-possibles-j
		       (not (set-difference coups-possibles-i coups-possibles-j)))
		  (setf paire coups-possibles-i)
		  (incf colonne-j))))
	  (incf ligne-j))
	(incf colonne-i))
      (incf ligne-i))
    paire))

;; Si deux cases contiennent la même paire de coups possibles alors ces coups peuvent être enlevés des autres cases du carré
(defun strat-paire-exclusive-carre ()
  (let ((nb-carre-par-ligne (/ (nb-lignes *grille*) (taille-carre *grille*)))
	(nb-carre-par-colonne (/ (nb-colonnes *grille*) (taille-carre *grille*)))
	(numcarre-ligne 0)
	(numcarre-colonne 0)
	(paire-coups-possibles nil)
	(tab-cases-trouvees nil)
	(case-a-jouer-par-carre 0))

    ;; Parcours de tous les carrés
    (while (and (< numcarre-ligne nb-carre-par-ligne) (not paire-coups-possibles))
      (setf numcarre-colonne 0)
      (while (and (< numcarre-colonne nb-carre-par-colonne) (not paire-coups-possibles))
	(setf tab-cases-trouvees (make-array (list (taille-carre *grille*) (taille-carre *grille*)) :initial-element nil))
	(setf case-a-jouer-par-carre 0)
	;; Parcours de toutes les cases d'un carré et sauvegarde des paires trouvées dans "tab-cases-trouvées"
	(dotimes (l (taille-carre *grille*))
	  (dotimes (c (taille-carre *grille*))
	    (let* ((ligne-a-verif (+ (* numcarre-ligne (taille-carre *grille*)) l))
		   (colonne-a-verif (+ (* numcarre-colonne (taille-carre *grille*)) c))
		   (case-a-verifier (aref (tab *grille*) ligne-a-verif colonne-a-verif)))
	      (if (coups-possibles case-a-verifier)
		  (progn (incf case-a-jouer-par-carre)
			 (if (= (length (coups-possibles case-a-verifier)) 2)
			     (setf (aref tab-cases-trouvees l c)
				   (coups-possibles (aref (tab *grille*) ligne-a-verif colonne-a-verif)))))))))

	;; S'il y a plus de deux case vides dans le carré
	;; et si deux paires trouvées sont identiques,
	;; on enlève cette paire de toutes les autres cases (!= celles où on a trouvé les paires)
	(if (and (> case-a-jouer-par-carre 2)
		 (setf paire-coups-possibles (paire-trouvee-dans-carre tab-cases-trouvees)))
	    (dotimes (l (taille-carre *grille*))
	      (dotimes (c (taille-carre *grille*))
		(let* ((ligne-a-verif (+ (* numcarre-ligne (taille-carre *grille*)) l))
		       (colonne-a-verif (+ (* numcarre-colonne (taille-carre *grille*)) c))
		       (case-a-verifier (aref (tab *grille*) ligne-a-verif colonne-a-verif)))
		  ;; S'il y a une différence entre la paire et les coups possibles de la case vérifiée
		  ;; càd que la paire relevée ne l'a pas été dans cette case
		  (if (set-difference (coups-possibles case-a-verifier) paire-coups-possibles)
		      (setf (coups-possibles case-a-verifier)
			    (set-difference (coups-possibles case-a-verifier) paire-coups-possibles)))))))
	(incf numcarre-colonne))
      (incf numcarre-ligne))
    (format t "strat-paire-exclusive-carre~%")
    paire-coups-possibles))

;; Initialisation de la grille
(defun init-standalone (grid)
  (init-grille *grille* grid))

;; Liste des stratégies qui renvoient des coups
(defparameter *liste-strat* (list '(strat-inclusion)
				  '(strat-exclusion-ligne)
				  '(strat-exclusion-colonne)
				  '(strat-exclusion-carre)))

;; Appel des stratégies
(defun main-standalone ()
  (let ((num-strat 0)
	(liste-coup nil))
    (while (and (< num-strat (length *liste-strat*))
		(not (nth 0 liste-coup))
		(/= (nb-coups-restants *grille*) 0))
      (if (not (setf liste-coup (multiple-value-bind (l c val) (eval (nth num-strat *liste-strat*))
				  (if (not l)
				      nil
				      (list *grille* l c val)))))
	  (progn (incf num-strat)
		 (if (and (= num-strat (length *liste-strat*))
			  (or (strat-paire-exclusive-colonne) (strat-paire-exclusive-ligne) (strat-paire-exclusive-carre)))
		     (setf num-strat 0)))))
    (if (not liste-coup)
	nil
	(values-list liste-coup))))
