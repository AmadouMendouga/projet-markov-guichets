# =====================================================================
# modele.R : construction de la matrice de transition P et calculs
# Modèle : banque avec c guichets, capacité K, deux classes de clients
# (prioritaires et ordinaires). Pas de temps = 1 minute.
# Etat = (a, b) : a prioritaires et b ordinaires présents (a + b <= K).
# =====================================================================

charger_parametres <- function(chemin = "data/parametres.json") {
  jsonlite::fromJSON(chemin)
}

# Liste des états (a, b). Si lp = 0, il n'y a pas de prioritaires : a = 0.
construire_etats <- function(K, avec_prio = TRUE) {
  amax <- if (avec_prio) K else 0
  etats <- do.call(rbind, lapply(0:amax, function(a) data.frame(a = a, b = 0:(K - a))))
  rownames(etats) <- NULL
  etats
}

# Matrice de transition en un pas (1 minute).
# Règles (voir la documentation du modèle) :
#  - au plus 1 arrivée : prioritaire (proba lp), ordinaire (lo), sinon aucune ;
#  - arrivée refusée si a + b = K au début du pas ;
#  - chaque guichet occupé termine son service avec la probabilité s,
#    indépendamment des autres (il y a min(a + b, c) guichets occupés) ;
#  - les prioritaires sont servis d'abord ; arrivées et départs sont indépendants.
construire_P <- function(K, c, s, lp, lo) {
  stopifnot(lp >= 0, lo >= 0, lp + lo <= 1, s > 0, s <= 1, c >= 1)
  etats <- construire_etats(K, lp > 0)
  n <- nrow(etats)
  idx <- matrix(NA_integer_, K + 1, K + 1)
  for (i in seq_len(n)) idx[etats$a[i] + 1, etats$b[i] + 1] <- i
  P <- matrix(0, n, n, dimnames = list(seq_len(n), seq_len(n)))

  for (i in seq_len(n)) {
    a <- etats$a[i]; b <- etats$b[i]
    sp <- min(a, c)                 # guichets occupés par des prioritaires
    so <- min(b, c - sp)            # guichets occupés par des ordinaires
    # loi du nombre de services terminés : chaque guichet occupé termine
    # avec la probabilité s, indépendamment des autres (loi binomiale)
    dp <- stats::dbinom(0:sp, sp, s)
    do <- stats::dbinom(0:so, so, s)
    # loi de l'arrivée : (aucune, prioritaire, ordinaire)
    arr <- if (a + b == K) c(1, 0, 0) else c(1 - lp - lo, lp, lo)
    for (x in 1:3) for (xp in 0:sp) for (yo in 0:so) {
      pr <- arr[x] * dp[xp + 1] * do[yo + 1]
      if (pr == 0) next
      a2 <- a + (x == 2) - xp
      b2 <- b + (x == 3) - yo
      j <- idx[a2 + 1, b2 + 1]
      P[i, j] <- P[i, j] + pr
    }
  }
  attr(P, "etats") <- etats
  P
}

# Petite chaîne à une classe et un guichet (exemple à 3 états fait à la main)
construire_P_simple <- function(K, lambda, s) {
  P <- matrix(0, K + 1, K + 1, dimnames = list(0:K, 0:K))
  for (i in 0:K) {
    up <- if (i < K) lambda * (1 - (if (i > 0) s else 0)) else 0
    dn <- if (i > 0) s * (1 - (if (i < K) lambda else 0)) else 0
    if (i < K) P[i + 1, i + 2] <- up
    if (i > 0) P[i + 1, i] <- dn
    P[i + 1, i + 1] <- 1 - up - dn
  }
  P
}

# Distribution stationnaire : on résout pi (P - I) = 0 avec somme(pi) = 1.
distribution_stationnaire <- function(P) {
  n <- nrow(P)
  A <- t(P) - diag(n)
  A[n, ] <- 1             # remplace la dernière équation par somme = 1
  b <- c(rep(0, n - 1), 1)
  pi_vec <- solve(A, b)
  names(pi_vec) <- rownames(P)
  pi_vec
}

# Même résultat par vecteur propre à gauche (valeur propre 1), pour vérifier.
stationnaire_vecteur_propre <- function(P) {
  e <- eigen(t(P))
  v <- Re(e$vectors[, which.min(abs(e$values - 1))])
  v / sum(v)
}

# Indicateurs de performance (temps en minutes car 1 pas = 1 minute)
indicateurs <- function(P, pi_vec, K, c, s, lp, lo) {
  et <- attr(P, "etats")
  a <- et$a; b <- et$b
  serv_p <- pmin(a, c)
  serv_o <- pmin(b, c - serv_p)
  p_pleine <- sum(pi_vec[a + b == K])
  lam_p <- lp * (1 - p_pleine)       # débit effectif des prioritaires
  lam_o <- lo * (1 - p_pleine)
  Lq_p <- sum(pi_vec * (a - serv_p))
  Lq_o <- sum(pi_vec * (b - serv_o))
  L_p <- sum(pi_vec * a)
  L_o <- sum(pi_vec * b)
  list(
    p_pleine = p_pleine,
    L = L_p + L_o, L_p = L_p, L_o = L_o,
    Lq_p = Lq_p, Lq_o = Lq_o,
    lambda_eff_p = lam_p, lambda_eff_o = lam_o,
    W_p = if (lam_p > 0) L_p / lam_p else NA_real_,           # Little
    W_o = if (lam_o > 0) L_o / lam_o else NA_real_,
    Wq_p = if (lam_p > 0) Lq_p / lam_p else NA_real_,
    Wq_o = if (lam_o > 0) Lq_o / lam_o else NA_real_,
    occupation = sum(pi_vec * pmin(a + b, c)) / c
  )
}

# Distribution après n pas : pi(n) = pi(0) P^n
distribution_apres_n <- function(P, pi0, n) {
  v <- pi0
  for (k in seq_len(n)) v <- v %*% P
  as.numeric(v)
}
