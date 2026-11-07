racine <- normalizePath(file.path("..", ".."))
for (f in c("modele", "simulation", "scenarios", "journee")) source(file.path(racine, "src", paste0(f, ".R")), encoding = "UTF-8")
params <- charger_parametres(file.path(racine, "data", "parametres.json"))

test_that("chaque ligne de P somme à 1 et P est >= 0 (tous scénarios)", {
  for (per in names(params$periodes)) for (c in params$guichets) {
    P <- construire_P(params$K, c, params$s,
                      params$periodes[[per]]$lambda_prio, params$periodes[[per]]$lambda_ord)
    expect_equal(unname(rowSums(P)), rep(1, nrow(P)), tolerance = 1e-12)
    expect_true(all(P >= 0))
  }
})

test_that("pi P = pi, pi >= 0, somme 1, et accord avec le vecteur propre", {
  P <- construire_P(params$K, 2, params$s, 0.10, 0.45)
  pv <- distribution_stationnaire(P)
  expect_equal(sum(pv), 1, tolerance = 1e-12)
  expect_true(all(pv >= -1e-12))
  expect_equal(as.numeric(pv %*% P), unname(pv), tolerance = 1e-10)
  expect_equal(unname(pv), stationnaire_vecteur_propre(P), tolerance = 1e-8)
})

test_that("convergence : pi(n) -> pi (chaîne irréductible et apériodique)", {
  P <- construire_P(params$K, 2, params$s, 0.05, 0.20)
  pv <- distribution_stationnaire(P)
  v <- distribution_apres_n(P, c(1, rep(0, nrow(P) - 1)), 2000)
  expect_equal(v, unname(pv), tolerance = 1e-8)
})

test_that("cas à une classe : le modèle général égale la chaîne simple", {
  Pg <- construire_P(2, 1, 0.4, 0, 0.3)
  Ps <- construire_P_simple(2, 0.3, 0.4)
  expect_equal(as.vector(Pg), as.vector(Ps), tolerance = 1e-12)
})

test_that("simulation Monte Carlo proche du calcul théorique", {
  K <- params$K; c <- 2; lp <- 0.10; lo <- 0.45
  P <- construire_P(K, c, params$s, lp, lo)
  pv <- distribution_stationnaire(P)
  sim <- simuler_banque(K, c, params$s, lp, lo, pas = 300000, graine = 1, rechauffement = 2000)
  pis <- frequences_dans_ordre(sim, attr(P, "etats"))
  expect_lt(max(abs(pis - pv)), 0.01)
  ind <- indicateurs(P, pv, K, c, params$s, lp, lo)
  expect_equal(sim$debit_o, ind$lambda_eff_o, tolerance = 0.03)
  expect_equal(sim$debit_p, ind$lambda_eff_p, tolerance = 0.08)
})

test_that("Little : L simulé = débit effectif * temps moyen W calculé", {
  K <- params$K; c <- 2; lp <- 0.10; lo <- 0.45
  P <- construire_P(K, c, params$s, lp, lo)
  ind <- indicateurs(P, distribution_stationnaire(P), K, c, params$s, lp, lo)
  sim <- simuler_banque(K, c, params$s, lp, lo, pas = 300000, graine = 2, rechauffement = 2000)
  et <- attr(P, "etats")
  L_sim <- sum(sim$freq[cbind(et$a + 1, et$b + 1)] * (et$a + et$b))
  lam_sim <- sim$debit_p + sim$debit_o              # débit effectif mesuré
  W_theo <- (ind$L_p + ind$L_o) / (ind$lambda_eff_p + ind$lambda_eff_o)
  expect_equal(L_sim, lam_sim * W_theo, tolerance = 0.03)
})

test_that("les prioritaires attendent moins que les ordinaires", {
  P <- construire_P(params$K, 1, params$s, 0.10, 0.45)
  ind <- indicateurs(P, distribution_stationnaire(P), params$K, 1, params$s, 0.10, 0.45)
  expect_lt(ind$Wq_p, ind$Wq_o)
})


test_that("simulation client par client : attentes proches du calcul (avec interruption)", {
  K <- params$K; c <- 2; lp <- 0.10; lo <- 0.45
  T <- 60000
  sim <- simuler_clients(K, c, params$s, rep(lp, T), rep(lo, T), graine = 3, interruption = TRUE)
  cl <- sim$clients[sim$clients$arrivee > 2000 & !is.na(sim$clients$debut), ]
  P <- construire_P(K, c, params$s, lp, lo)
  ind <- indicateurs(P, distribution_stationnaire(P), K, c, params$s, lp, lo)
  expect_equal(mean(cl$attente[cl$type == "ordinaire"]), ind$Wq_o, tolerance = 0.12)
  expect_equal(mean(!sim$clients$admis[sim$clients$arrivee > 2000]), ind$p_pleine, tolerance = 0.12)
})

test_that("journee : les regles sont respectees", {
  profil <- profil_journee(params)
  sim <- simuler_clients(params$K, 1, params$s, profil$lp, profil$lo, graine = 1)
  cl <- sim$clients
  expect_equal(length(profil$lp), 480)
  ok <- !is.na(cl$debut)
  expect_true(all(cl$debut[ok] >= cl$arrivee[ok]))     # on ne commence pas avant d'arriver
  expect_true(all(sim$presents <= params$K))           # jamais plus de K clients
  expect_true(all(is.na(cl$debut[!cl$admis])))         # un client refuse n'est jamais servi
})
