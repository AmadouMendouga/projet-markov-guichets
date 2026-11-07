# =====================================================================
# scenarios.R : 1 à 5 guichets x heure creuse / heure de pointe
# =====================================================================

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
        charge = (lp + lo) / (c * params$s),     # arrivées / capacité de service
        L = ind$L, Wq_prio = ind$Wq_p, Wq_ord = ind$Wq_o,
        W_prio = ind$W_p, W_ord = ind$W_o,
        p_pleine = ind$p_pleine, occupation = ind$occupation)
    }
  }
  do.call(rbind, lignes)
}

# Plus petit nombre de guichets pour que l'attente moyenne des ordinaires
# ne dépasse pas `seuil` minutes, pour chaque période (creuse / pointe).
guichets_necessaires <- function(sc, seuil) {
  sapply(split(sc, sc$periode), function(d) {
    ok <- d$guichets[d$Wq_ord <= seuil]
    if (length(ok)) min(ok) else NA_integer_
  })
}
