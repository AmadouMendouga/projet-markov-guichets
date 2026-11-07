#' Liste des etats (a, b) du modele
#'
#' Un etat est un couple (a, b) : `a` clients prioritaires et `b` clients
#' ordinaires presents dans la banque, avec `a + b <= K`.
#'
#' @param K Capacite maximale de la banque (nombre de clients).
#' @param avec_prio Si `FALSE`, il n'y a pas de clients prioritaires (a = 0).
#' @return Un data.frame avec les colonnes `a` et `b`, une ligne par etat.
#' @examples
#' construire_etats(2)
#' @export
construire_etats <- function(K, avec_prio = TRUE) {
  amax <- if (avec_prio) K else 0
  etats <- do.call(rbind, lapply(0:amax, function(a) data.frame(a = a, b = 0:(K - a))))
  rownames(etats) <- NULL
  etats
}

#' Matrice de transition de la banque (un pas = 1 minute)
#'
#' Regles du modele : au plus une arrivee par minute (prioritaire avec la
#' probabilite `lp`, ordinaire avec `lo`), refusee si `a + b = K` ; chaque
#' guichet occupe (il y en a `min(a + b, c)`) termine son service avec la
#' probabilite `s`, independamment des autres ; les prioritaires sont servis
#' d'abord ; arrivees et departs sont independants.
#'
#' Chaque ligne de la matrice somme a 1, car elle est le produit d'une loi
#' d'arrivee et de lois binomiales de departs, de somme 1 chacune.
#'
#' @param K Capacite maximale (entier positif).
#' @param c Nombre de guichets.
#' @param s Probabilite de fin de service par guichet et par minute.
#' @param lp Probabilite d'arrivee d'un client prioritaire par minute.
#' @param lo Probabilite d'arrivee d'un client ordinaire par minute.
#' @return Une matrice stochastique. Son attribut `"etats"` contient le
#'   data.frame des etats (voir [construire_etats()]).
#' @examples
#' P <- construire_P(K = 3, c = 1, s = 0.4, lp = 0, lo = 0.3)
#' rowSums(P)
#' @export
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

#' Matrice de transition simple : une classe de clients, un guichet
#'
#' Sert d'exemple a la main (par exemple 3 etats avec `K = 2`).
#'
#' @param K Capacite maximale.
#' @param lambda Probabilite d'arrivee par minute.
#' @param s Probabilite de fin de service par minute.
#' @return Une matrice stochastique de taille `(K + 1) x (K + 1)`.
#' @examples
#' construire_P_simple(2, 0.3, 0.4)
#' @export
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

#' Distribution stationnaire d'une chaine de Markov
#'
#' Resout `pi (P - I) = 0` avec `sum(pi) = 1`. Elle est unique si la chaine
#' est finie et irreductible.
#'
#' @param P Matrice de transition (stochastique, carree).
#' @return Un vecteur nomme de probabilites de somme 1.
#' @examples
#' P <- construire_P_simple(2, 0.3, 0.4)
#' distribution_stationnaire(P)
#' @export
distribution_stationnaire <- function(P) {
  n <- nrow(P)
  A <- t(P) - diag(n)
  A[n, ] <- 1
  b <- c(rep(0, n - 1), 1)
  pi_vec <- solve(A, b)
  names(pi_vec) <- rownames(P)
  pi_vec
}

#' Distribution stationnaire par vecteur propre (verification)
#'
#' Meme resultat que [distribution_stationnaire()], obtenu avec le vecteur
#' propre a gauche associe a la valeur propre 1.
#'
#' @inheritParams distribution_stationnaire
#' @return Un vecteur de probabilites de somme 1.
#' @examples
#' stationnaire_vecteur_propre(construire_P_simple(2, 0.3, 0.4))
#' @export
stationnaire_vecteur_propre <- function(P) {
  e <- eigen(t(P))
  v <- Re(e$vectors[, which.min(abs(e$values - 1))])
  v / sum(v)
}

#' Indicateurs de performance de la banque
#'
#' Les temps sont en minutes car un pas vaut une minute. Les temps moyens
#' utilisent la formule de Little, `L = lambda_eff * W`.
#'
#' @param P Matrice issue de [construire_P()].
#' @param pi_vec Distribution stationnaire (voir [distribution_stationnaire()]).
#' @param K,c,s,lp,lo Parametres utilises pour construire `P`.
#' @return Une liste : `p_pleine`, `L`, `L_p`, `L_o`, `Lq_p`, `Lq_o`,
#'   `lambda_eff_p`, `lambda_eff_o`, `W_p`, `W_o`, `Wq_p`, `Wq_o`
#'   (attentes en file) et `occupation`.
#' @examples
#' P <- construire_P(8, 2, 0.25, 0.10, 0.45)
#' ind <- indicateurs(P, distribution_stationnaire(P), 8, 2, 0.25, 0.10, 0.45)
#' ind$Wq_o
#' @export
indicateurs <- function(P, pi_vec, K, c, s, lp, lo) {
  et <- attr(P, "etats")
  a <- et$a
  b <- et$b
  serv_p <- pmin(a, c)
  serv_o <- pmin(b, c - serv_p)
  p_pleine <- sum(pi_vec[a + b == K])
  lam_p <- lp * (1 - p_pleine)
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
    W_p = if (lam_p > 0) L_p / lam_p else NA_real_,
    W_o = if (lam_o > 0) L_o / lam_o else NA_real_,
    Wq_p = if (lam_p > 0) Lq_p / lam_p else NA_real_,
    Wq_o = if (lam_o > 0) Lq_o / lam_o else NA_real_,
    occupation = sum(pi_vec * pmin(a + b, c)) / c
  )
}

#' Distribution apres n pas : pi(n) = pi(0) P^n
#'
#' @param P Matrice de transition.
#' @param pi0 Distribution initiale (vecteur ligne de somme 1).
#' @param n Nombre de pas (minutes).
#' @return Le vecteur de probabilites apres `n` pas.
#' @examples
#' P <- construire_P_simple(2, 0.3, 0.4)
#' distribution_apres_n(P, c(1, 0, 0), 10)
#' @export
distribution_apres_n <- function(P, pi0, n) {
  v <- pi0
  for (k in seq_len(n)) v <- v %*% P
  as.numeric(v)
}
