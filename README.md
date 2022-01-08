# functional-programming-project


## Partie 1 - Utilisation de Ford-Fulkerson

```
>>> make clean
>>> make build
>>> ./ftest.native <input graph file path> <source node> <sink node> <output file name>
```
Exemple :
```
>>> ./ftest.native graphs/graph1.txt 0 5 output
```

Durant l'exécution du programme, le terminal affiche les différentes étapes de réalisation ainsi que le résultat obtenu. Les différentes itérations du programme sont visibles dans le dossier iterations sous la forme de fichiers .dot.
A l'issue de celle-ci, un nouveau fichier graphe est généré ainsi qu'un fichier .dot permettant de le visualiser avec un programme tel que KGraphViewer ou bien en générant un SVG à partir de celui-ci avec la commande.

```
>>> dot -Tsvg output.dot > output.svg
```
Le fichier SVG peut-être visualisé dans un navigateur (Firefox par exemple).


## Partie 2 - Application de l'algorithme à un graphe bipartis
### Objectif : Organiser un secret santa (chaque personne au sein d'un groupe doit acheter un cadeau à une autre)

```
>>> make clean
>>> make santa
>>> matchmaking.native <liste de noms> <output>
```
Exemple : 
```
>>> matchmaking.native names.txt output
```


### Piste d'amélioration : 
- Introduire de l'aléatoire dans find_path et plus précisément "mélanger" les différents arcs donnés par out_arcs.
- Introduire des contraintes pour chaque participant (par exemple budget).
