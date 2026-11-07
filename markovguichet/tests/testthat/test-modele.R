params <- charger_parametres()

test_that("chaque ligne de P somme a 1 et P est >= 0 (tous scenarios)", {
  for (per in names(params$periodes)) for (c in params$guichets) {
    P <- construire_P(params$K, c, params$s,
                      params$periodes[[per]]$lambda_prio, params$periodes[[per]]$lambda_ord)
    expect_equal(unname(rowSums(P)), rep(1, nrow(P)), tolerance = 1e-12)
    expect_true(all(P >= 0))
  }
})

test_that("le nombre d'etats vaut (K+1)(K+2)/2 avec prioritaires", {
  P <- construire_P(params$K, 2, params$s, 0.10, 0.45)
  expect_equal(nrow(P), (params$K + 1) * (params$K + 2) / 2)
})

test_that("pi P = pi, pi >= 0, somme 1, et accord avec le vecteur propre", {
  P <- construire_P(params$K, 2, params$s, 0.10, 0.45)
  pv <- distribution_stationnaire(P)
  expect_equal(sum(pv), 1, tolerance = 1e-12)
  expect_true(all(pv >= -1e-12))
  expect_equal(as.numeric(pv %*% P), unname(pv), tolerance = 1e-10)
  expect_equal(unname(pv), stationnaire_vecteur_propre(P), tolerance = 1e-8)
})

test_that("exemple a 3 etats : pi = (56, 60, 27) / 143", {
  P <- construire_P_simple(2, 0.3, 0.4)
  expect_equal(unname(distribution_stationnaire(P)), c(56, 60, 27) / 143, tolerance = 1e-10)
})

test_that("convergence : pi(n) tend vers pi", {
  P <- construire_P(params$K, 2, params$s, 0.05, 0.20)
  pv <- distribution_stationnaire(P)
  v <- distribution_apres_n(P, c(1, rep(0, nrow(P) - 1)), 2000)
  expect_equal(v, unname(pv), tolerance = 1e-8)
})

test_that("cas a une classe : le modele general egale la chaine simple", {
  Pg <- construire_P(2, 1, 0.4, 0, 0.3)
  Ps <- construire_P_simple(2, 0.3, 0.4)
  expect_equal(as.vector(Pg), as.vector(Ps), tolerance = 1e-12)
})

test_that("les prioritaires attendent moins que les ordinaires", {
  P <- construire_P(params$K, 1, params$s, 0.10, 0.45)
  ind <- indicateurs(P, distribution_stationnaire(P), params$K, 1, params$s, 0.10, 0.45)
  expect_lt(ind$Wq_p, ind$Wq_o)
})

test_that("lancer_scenarios renvoie une ligne par periode et par nombre de guichets, et plus de guichets = moins d'attente", {
  sc <- lancer_scenarios(params)
  expect_equal(nrow(sc), 2 * length(params$guichets))
  pointe <- sc[sc$periode == "pointe", ]
  expect_true(all(diff(pointe$Wq_ord) < 0))
})

test_that("les parametres invalides sont refuses", {
  expect_error(construire_P(8, 5, 1.5, 0.1, 0.45))
  expect_error(construire_P(8, 1, 0.25, 0.7, 0.45))
})
