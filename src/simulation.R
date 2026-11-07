# =====================================================================
# simulation.R : simulation Monte Carlo "événement par événement".
# Elle NE UTILISE PAS la matrice P : elle tire directement arrivées et
# départs, guichet par guichet. Si elle retombe sur les résultats de P,
# c'est que P est juste.
# =====================================================================

# Un pas de simulation : renvoie le nouvel état (a, b) et ce qui s'est passé.
# Chaque guichet occupé termine son service avec la probabilité s.
pas_simulation <- function(a, b, K, c, s, lp, lo, u_arrivee) {
  plein <- (a + b) >= K
  sp <- min(a, c)                          # guichets occupés par des prioritaires
  so <- min(b, c - sp)                     # guichets occupés par des ordinaires
  xp <- if (sp > 0) sum(runif(sp) < s) else 0L   # services terminés (prioritaires)
  yo <- if (so > 0) sum(runif(so) < s) else 0L   # services terminés (ordinaires)
  arr <- if (u_arrivee < lp) 1L else if (u_arrivee < lp + lo) 2L else 0L
  list(a = as.integer(a + (arr == 1L && !plein) - xp),
       b = as.integer(b + (arr == 2L && !plein) - yo),
       arr = arr, plein = plein)
}

simuler_banque <- function(K, c, s, lp, lo, pas, graine = 123, rechauffement = 1000) {
  set.seed(graine)
  ua <- runif(pas)
  a <- 0L; b <- 0L
  tab <- matrix(0, K + 1, K + 1)      # nombre de pas passés dans chaque état
  adm_p <- adm_o <- ref_p <- ref_o <- 0
  nb <- 0
  for (t in seq_len(pas)) {
    en_regime <- t > rechauffement
    if (en_regime) { tab[a + 1, b + 1] <- tab[a + 1, b + 1] + 1; nb <- nb + 1 }
    r <- pas_simulation(a, b, K, c, s, lp, lo, ua[t])
    if (r$arr > 0L && en_regime) {
      if (r$plein) { if (r$arr == 1L) ref_p <- ref_p + 1 else ref_o <- ref_o + 1 }
      else         { if (r$arr == 1L) adm_p <- adm_p + 1 else adm_o <- adm_o + 1 }
    }
    a <- r$a; b <- r$b
  }
  list(freq = tab / nb, nb_pas = nb,
       debit_p = adm_p / nb, debit_o = adm_o / nb,
       refus_p = ref_p / nb, refus_o = ref_o / nb)
}

# Met la fréquence simulée dans le même ordre d'états que la matrice P
frequences_dans_ordre <- function(sim, etats) {
  mapply(function(a, b) sim$freq[a + 1, b + 1], etats$a, etats$b)
}

# Trajectoire minute par minute (pour visualiser la banque qui se remplit et se vide)
trajectoire_banque <- function(K, c, s, lp, lo, pas, graine = 42) {
  set.seed(graine)
  ua <- runif(pas)
  a <- 0L; b <- 0L
  res <- data.frame(t = 0:(pas - 1), prio = integer(pas), ord = integer(pas))
  for (t in seq_len(pas)) {
    res$prio[t] <- a; res$ord[t] <- b
    r <- pas_simulation(a, b, K, c, s, lp, lo, ua[t])
    a <- r$a; b <- r$b
  }
  res$total <- res$prio + res$ord
  res
}
