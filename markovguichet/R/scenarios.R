#' Charger les parametres (fictifs) du projet
#'
#' Lit un fichier JSON. Par defaut, lit le fichier fourni avec le package,
#' dont les valeurs sont **fictives**.
#'
#' @param chemin Chemin d'un fichier JSON de parametres.
#' @return Une liste (K, s, guichets, periodes, exemple_main, simulation).
#' @examples
#' params <- charger_parametres()
#' params$K
#' @export
charger_parametres <- function(chemin = system.file("extdata", "parametres.json",
                                                    package = "markovguichet")) {
  if (!nzchar(chemin) || !file.exists(chemin)) stop("Fichier de parametres introuvable.")
  jsonlite::fromJSON(chemin)
}

#' Scenarios : nombre de guichets x periode (creuse / pointe)
#'
#' Pour chaque combinaison, construit la matrice, calcule la distribution
#' stationnaire et les indicateurs de performance.
#'
#' @param params Liste de parametres (voir [charger_parametres()]).
#' @return Un data.frame : `periode`, `guichets`, `charge`, `L`, `Wq_prio`,
#'   `Wq_ord`, `W_prio`, `W_ord`, `p_pleine`, `occupation`. Les attentes sont
#'   en minutes.
#' @examples
#' lancer_scenarios(charger_parametres())
#' @export
lancer_scenarios <- function(params) {
  lignes <- list()
  for (per in names(params$periodes)) {
    lp <- params$periodes[[per]]$lambda_prio
    lo <- params$periodes[[per]]$lambda_ord
    for (c in params$guichets) {
      P <- construire_P(params$K, c, params$s, lp, lo)
      pv <- distribution_stationnaire(P)
      ind <- indicateurs(P, pv, params$K, c, params$s, lp, lo)
      lignes[[length(lignes) + 1]] <- data.frame(
        periode = per, guichets = c,
        charge = (lp + lo) / (c * params$s),
        L = ind$L, Wq_prio = ind$Wq_p, Wq_ord = ind$Wq_o,
        W_prio = ind$W_p, W_ord = ind$W_o,
        p_pleine = ind$p_pleine, occupation = ind$occupation)
    }
  }
  do.call(rbind, lignes)
}

#' Nombre de guichets necessaires pour une attente acceptable
#'
#' Plus petit nombre de guichets pour que l'attente moyenne des clients
#' ordinaires ne depasse pas `seuil` minutes, pour chaque periode.
#'
#' @param sc Resultat de [lancer_scenarios()].
#' @param seuil Attente maximale acceptee, en minutes.
#' @return Un vecteur nomme (`creuse`, `pointe`) ; `NA` si aucun nombre de
#'   guichets etudie ne suffit.
#' @examples
#' guichets_necessaires(lancer_scenarios(charger_parametres()), seuil = 1)
#' @export
guichets_necessaires <- function(sc, seuil) {
  sapply(split(sc, sc$periode), function(d) {
    ok <- d$guichets[d$Wq_ord <= seuil]
    if (length(ok)) min(ok) else NA_integer_
  })
}
