#' Un pas de simulation (interne)
#'
#' Chaque guichet occupe termine son service avec la probabilite `s`.
#'
#' @noRd
pas_simulation <- function(a, b, K, c, s, lp, lo, u_arrivee) {
  plein <- (a + b) >= K
  sp <- min(a, c)
  so <- min(b, c - sp)
  xp <- if (sp > 0) sum(stats::runif(sp) < s) else 0L
  yo <- if (so > 0) sum(stats::runif(so) < s) else 0L
  arr <- if (u_arrivee < lp) 1L else if (u_arrivee < lp + lo) 2L else 0L
  list(a = as.integer(a + (arr == 1L && !plein) - xp),
       b = as.integer(b + (arr == 2L && !plein) - yo),
       arr = arr, plein = plein)
}

#' Simulation Monte Carlo de la banque
#'
#' Simule la banque minute par minute en tirant au hasard arrivees et
#' departs (guichet par guichet), **sans utiliser la matrice de
#' transition**. Si les frequences observees retombent sur la distribution
#' stationnaire, la matrice est juste.
#'
#' @inheritParams construire_P
#' @param pas Nombre de minutes simulees.
#' @param graine Graine du generateur aleatoire (resultats reproductibles).
#' @param rechauffement Nombre de minutes initiales ignorees.
#' @return Une liste : `freq` (matrice des frequences par etat (a, b)),
#'   `nb_pas`, `debit_p`, `debit_o` (clients admis par minute) et `refus_p`,
#'   `refus_o` (clients refuses par minute).
#' @examples
#' sim <- simuler_banque(3, 1, 0.4, 0, 0.3, pas = 5000)
#' sim$nb_pas
#' @export
simuler_banque <- function(K, c, s, lp, lo, pas, graine = 123, rechauffement = 1000) {
  set.seed(graine)
  ua <- stats::runif(pas)
  a <- 0L
  b <- 0L
  tab <- matrix(0, K + 1, K + 1)
  adm_p <- adm_o <- ref_p <- ref_o <- 0
  nb <- 0
  for (t in seq_len(pas)) {
    en_regime <- t > rechauffement
    if (en_regime) {
      tab[a + 1, b + 1] <- tab[a + 1, b + 1] + 1
      nb <- nb + 1
    }
    r <- pas_simulation(a, b, K, c, s, lp, lo, ua[t])
    if (r$arr > 0L && en_regime) {
      if (r$plein) {
        if (r$arr == 1L) ref_p <- ref_p + 1 else ref_o <- ref_o + 1
      } else {
        if (r$arr == 1L) adm_p <- adm_p + 1 else adm_o <- adm_o + 1
      }
    }
    a <- r$a
    b <- r$b
  }
  list(freq = tab / nb, nb_pas = nb,
       debit_p = adm_p / nb, debit_o = adm_o / nb,
       refus_p = ref_p / nb, refus_o = ref_o / nb)
}

#' Frequences simulees dans l'ordre des etats de la matrice
#'
#' @param sim Resultat de [simuler_banque()].
#' @param etats Data.frame des etats, par exemple `attr(P, "etats")`.
#' @return Un vecteur de frequences, dans le meme ordre que les lignes de `P`.
#' @examples
#' P <- construire_P(3, 1, 0.4, 0, 0.3)
#' sim <- simuler_banque(3, 1, 0.4, 0, 0.3, pas = 5000)
#' frequences_dans_ordre(sim, attr(P, "etats"))
#' @export
frequences_dans_ordre <- function(sim, etats) {
  mapply(function(a, b) sim$freq[a + 1, b + 1], etats$a, etats$b)
}

#' Trajectoire minute par minute
#'
#' Pour visualiser la banque qui se remplit et se vide.
#'
#' @inheritParams construire_P
#' @param pas Nombre de minutes.
#' @param graine Graine du generateur aleatoire.
#' @return Un data.frame : `t`, `prio`, `ord` et `total` (clients presents).
#' @examples
#' head(trajectoire_banque(8, 2, 0.25, 0.1, 0.45, pas = 30))
#' @export
trajectoire_banque <- function(K, c, s, lp, lo, pas, graine = 42) {
  set.seed(graine)
  ua <- stats::runif(pas)
  a <- 0L
  b <- 0L
  res <- data.frame(t = 0:(pas - 1), prio = integer(pas), ord = integer(pas))
  for (t in seq_len(pas)) {
    res$prio[t] <- a
    res$ord[t] <- b
    r <- pas_simulation(a, b, K, c, s, lp, lo, ua[t])
    a <- r$a
    b <- r$b
  }
  res$total <- res$prio + res$ord
  res
}
