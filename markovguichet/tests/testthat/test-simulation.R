test_that("simulation Monte Carlo proche du calcul theorique", {
  params <- charger_parametres()
  K <- params$K; c <- 2; lp <- 0.10; lo <- 0.45
  P <- construire_P(K, c, params$s, lp, lo)
  pv <- distribution_stationnaire(P)
  sim <- simuler_banque(K, c, params$s, lp, lo, pas = 120000, graine = 1, rechauffement = 2000)
  pis <- frequences_dans_ordre(sim, attr(P, "etats"))
  expect_lt(max(abs(pis - pv)), 0.015)
  ind <- indicateurs(P, pv, K, c, params$s, lp, lo)
  expect_equal(sim$debit_o, ind$lambda_eff_o, tolerance = 0.05)
})

test_that("Little : L simule = debit effectif mesure * W calcule", {
  params <- charger_parametres()
  K <- params$K; c <- 2; lp <- 0.10; lo <- 0.45
  P <- construire_P(K, c, params$s, lp, lo)
  ind <- indicateurs(P, distribution_stationnaire(P), K, c, params$s, lp, lo)
  sim <- simuler_banque(K, c, params$s, lp, lo, pas = 120000, graine = 2, rechauffement = 2000)
  et <- attr(P, "etats")
  L_sim <- sum(sim$freq[cbind(et$a + 1, et$b + 1)] * (et$a + et$b))
  W_theo <- (ind$L_p + ind$L_o) / (ind$lambda_eff_p + ind$lambda_eff_o)
  expect_equal(L_sim, (sim$debit_p + sim$debit_o) * W_theo, tolerance = 0.05)
})

test_that("la simulation est reproductible avec la meme graine", {
  a <- simuler_banque(3, 1, 0.4, 0, 0.3, pas = 3000, graine = 7)
  b <- simuler_banque(3, 1, 0.4, 0, 0.3, pas = 3000, graine = 7)
  expect_identical(a, b)
})
