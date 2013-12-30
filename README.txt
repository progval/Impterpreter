Utilisation
===========

Exécution
---------

À partir d’un fichier existant : cat tests/test_042.imp | make run
En entrant directement le programme : 
    * Exécuter : make run
    * Entrer le programme
    * Terminer avec Ctrl-D

Remarque : « make run » est équivalent à « make && ./impterpreter ».

Tests
-----

Des fichiers de test sont présents dans le répertoire tests/.

Le script tests/run.sh exécute tous les tests dont le nom commence par
« test_ » et compare leur sortie à celle attendue (indiquée dans
tests/expected_output.txt). S’il y a une différence, elle est
indiquée dans le fichier _build/tests.diff.
On peut aussi exécuter les tests avec la commande « make tests ».

Le fichier tests/infinite_recursion.imp ne s’arrête jamais et permet de
vérifier (à l’aide d’un outil externe, tel que « ps -v ») qu’une récursion
infinie mais terminale ne fait pas augmenter la taille de la pile.

Notes d’implémentation
======================

Ambiguïtés
----------

Priorité classique des opérateurs :  */ puis +- puis andor puis not puis ><=

Dangling else : un else est associé au if le plus proche possible

while b do c1;c2 : interprété comme (while b do c1); c2

Structure du code
-----------------

Les définitions des expressions arithmétiques, booléennes, et des commandes
sont respectivement dans expr.ml, assertion.ml, et com.ml.
La gestion de la mémoire est dans memory.ml. La liste des fonctions est aussi
stockée dans la Memory.mem (pour éviter de devoir passer un paramètre de plus
à toutes les fonctions de réduction).
À cause des problèmes de dépendance circulaire :
    * Les réductions des trois types d’expressions sont dans step.ml
    * Les types « mem », « expr », et « assertion » sont paramétrés par
      un type « 'com » (qui, en pratique, est toujours Com.com).

Toute la sémantique (expressions arithmétiques, expressions booléennes,
commandes) est à petits pas.

Réduction des appels de fonction
--------------------------------

Lorsque l’on rencontre une expression de la forme « f(e1, e2) »
(codée par « RawCall(f, e1, e2) »), les deux expressions arithmétiques
e1 et e2 sont d’abord réduites en deux entiers, on obtient alors une
expression de la forme « f(i1, i2) » (codée par
« RawCall(f, Const i1, Const i2) »).
On réduit alors celle-ci en un doublet « {mem, c} » (codé par
« Call(mem, c) ») où mem est l’état mémoire « {A=i1, B=i2} »
(si les arguments de f sont A et B), et on réduit ce doublet jusqu’à ce
qu’il soit d’une des formes suivantes :
    * {mem, return i} avec i entier
    * {mem, return i; c} avec i entier
    * {mem, skip}
L’étape suivante de la réduction est de remplacer ce doublet par
« Const i » (ou « Const 0 » dans le troisième cas).

Optimisation de la récursion terminale et des « tail calls »
------------------------------------------------------------

Elle est désactivable en supprimant les lignes 93 à 96 de step.ml.

Elle se fait en détectant les structures de la forme « return f(i, j) »
où i et j sont des entiers, qui est obtenue après réduction de toute
structure de la forme « return f(e1, f2) » où e1 et e2 sont des
expressions arithmétiques.

En exécutant l’interpréteur sur le fichier tests/infinite_recursion.imp,
avec et sans cette optimisation, on constate que sans celle-ci, le programme
va moins vite et consomme plus de mémoire.

Autres
------

Possibilité d’avoir des instructions vides (ie. terminer un programme par
un point-virgule, comme dans la plupart des langages en utilisant).

Pas de variable globale. À l’intérieur d’une fonction on ne peut pas
accéder aux variables définies dans le corps du programme.
Pas de notion de fermeture puisque les fonctions sont définies hors du
corps du programme.
