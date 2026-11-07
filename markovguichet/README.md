# markovguichet

Package R : **chaînes de Markov appliquées à la file d'attente d'une banque** (plusieurs guichets, clients prioritaires). Projet de Licence 3.

> Les paramètres fournis avec le package (`inst/extdata/parametres.json`) sont **fictifs**.

## Installation locale

Depuis R, en remplaçant le chemin par celui du dossier `markovguichet` :

```r
install.packages("markovguichet", repos = NULL, type = "source")   # depuis le dossier du projet
```

Avec `devtools` (comme dans le cours) :

```r
devtools::install_local("markovguichet")
```

## Exemple

```r
library(markovguichet)

# Exemple à 3 états (une classe de clients, 1 guichet, capacité 2)
P3 <- construire_P_simple(K = 2, lambda = 0.3, s = 0.4)
distribution_stationnaire(P3)          # (56, 60, 27) / 143

# Cas complet : heure de pointe, 2 guichets
params <- charger_parametres()
P <- construire_P(params$K, c = 2, params$s, lp = 0.10, lo = 0.45)
pv <- distribution_stationnaire(P)
indicateurs(P, pv, params$K, c = 2, params$s, lp = 0.10, lo = 0.45)$Wq_o   # attente des ordinaires (min)

# Comparer 1 à 5 guichets, en heure creuse et en pointe
lancer_scenarios(params)

# Validation par simulation Monte Carlo
sim <- simuler_banque(params$K, 2, params$s, 0.10, 0.45, pas = 100000)
```

## Fonctions

| Fonction | Rôle |
|---|---|
| `construire_etats()` | Liste des états (a, b) |
| `construire_P()` | Matrice de transition du modèle complet |
| `construire_P_simple()` | Matrice de l'exemple à une classe et un guichet |
| `distribution_stationnaire()` | Résout πP = π |
| `stationnaire_vecteur_propre()` | Même résultat par vecteur propre (vérification) |
| `distribution_apres_n()` | π(n) = π(0)·Pⁿ |
| `indicateurs()` | Probabilité de salle pleine, nombre moyen de clients, attentes (formule de Little), occupation |
| `lancer_scenarios()` | 1 à 5 guichets × creuse / pointe |
| `guichets_necessaires()` | Plus petit nombre de guichets sous un seuil d'attente |
| `trajectoire_banque()` | Nombre de clients minute par minute (pour un graphique) |
| `simuler_banque()` | Simulation Monte Carlo indépendante de la matrice |
| `charger_parametres()` | Lit les paramètres JSON |

## Développement

À lancer depuis RStudio, dans le projet `markovguichet.Rproj` :

```r
devtools::document()   # régénère la documentation et NAMESPACE
devtools::test()       # lance les tests testthat
devtools::check()      # contrôle complet (R CMD check)
```

## À compléter avant le rendu

- Remplacer l'auteur `Prenom Nom` et l'e-mail dans `DESCRIPTION`, `LICENSE` et `LICENSE.md`.
- Choisir le nom du dépôt GitHub si l'enseignant demande une publication.

## Licence

MIT, voir `LICENSE.md`.
