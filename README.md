# Chaînes de Markov appliquées à la file d'attente d'une banque

**Auteure : Yeline Fawa** · Projet de Licence 3 SDIA · Recherche opérationnelle

> Toutes les données chiffrées sont **fictives** : elles illustrent la méthode et ne décrivent aucune banque réelle.

## Le problème
Combien de guichets ouvrir dans une banque (de 1 à 5), avec des clients prioritaires et ordinaires ?

Le modèle est une **chaîne de Markov** dont l'état est le nombre de clients prioritaires et ordinaires présents (capacité 8, soit 45 états, un pas = 1 minute). On construit sa **matrice de transition P**, on calcule la distribution stationnaire (πP = π), puis l'attente moyenne et la probabilité de salle pleine. Avec un seuil de 1 minute d'attente (valeur fictive), il faut **2 guichets en heure creuse et 4 à l'heure de pointe**. Une simulation Monte Carlo confirme le calcul.

## Démarrer (RStudio)
1. *File > Open Project…* puis `projet-markov.Rproj`.
2. Installer les paquets une fois : ouvrir `installer_paquets.R`, cliquer sur **Source**.
3. Lancer `run_all.R` (calculs, figures, tests) ou `demo_simulation.R` (une journée à l'agence simulée client par client, cas par cas de 1 à 5 guichets, avec animation).
4. `projet_markov.Rmd` : bouton **Knit** pour produire le notebook complet.

En ligne de commande : `Rscript run_all.R`

Paquets : `jsonlite`, `testthat`, `rmarkdown`, `knitr` (et `officer`, facultatif).

## Contenu
| Élément | Rôle |
|---|---|
| `src/modele.R` | Matrice de transition `construire_P`, distribution stationnaire, indicateurs |
| `src/simulation.R` | Simulation Monte Carlo de la chaîne |
| `src/scenarios.R` | Comparaison de 1 à 5 guichets, creuse et pointe |
| `src/journee.R` | Journée simulée client par client, animation |
| `src/figures.R` | Graphiques |
| `data/parametres.json` | Paramètres fictifs (modifiables) |
| `tests/` | Tests automatiques (testthat) |
| `markovguichet/` | Le même modèle sous forme de paquet R local |
| `run_all.R`, `demo_simulation.R` | Scripts à lancer |

Les dossiers `figures/` et `outputs/` sont créés automatiquement au lancement.

## Paquet `markovguichet`
```r
install.packages("markovguichet", repos = NULL, type = "source")
library(markovguichet)
```

## Licence
MIT.
