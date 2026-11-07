# Lance tout : calculs, simulation, figures, tests.  Usage : Rscript run_all.R
args <- commandArgs(trailingOnly = FALSE)
f <- sub("--file=", "", args[grep("--file=", args)])
if (length(f)) {
  setwd(dirname(normalizePath(f)))                      # lancé avec Rscript
} else {
  of <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(of)) setwd(dirname(normalizePath(of)))   # RStudio : bouton Source
}
if (!file.exists("data/parametres.json"))
  stop("Dossier de travail incorrect : ouvre le projet 'projet-markov.Rproj' ou fais setwd() vers le dossier Fawa.")
for (s in c("modele", "simulation", "scenarios", "figures")) source(file.path("src", paste0(s, ".R")), encoding = "UTF-8")
dir.create("figures", showWarnings = FALSE); dir.create("outputs", showWarnings = FALSE)
params <- charger_parametres()

# --- 1. Exemple à 3 états (calcul à la main du rapport) ---
ex <- params$exemple_main
P3 <- construire_P_simple(ex$K, ex$lambda, ex$s)
pi3 <- distribution_stationnaire(P3)
P3_2 <- P3 %*% P3
fig_diagramme_3etats(P3, "figures/fig1_diagramme_3etats.png")

# --- 2. Scénarios ---
sc <- lancer_scenarios(params)
write.csv(sc, "outputs/scenarios.csv", row.names = FALSE)
fig_scenarios(sc, "figures/fig4_scenarios.png")

# --- 3. Scénario de référence : pointe, 2 guichets ---
lp <- params$periodes$pointe$lambda_prio; lo <- params$periodes$pointe$lambda_ord; c_ref <- 2
P <- construire_P(params$K, c_ref, params$s, lp, lo)
pv <- distribution_stationnaire(P)
fig_heatmap_P(P, "figures/fig2_heatmap_P.png",
              sprintf("Matrice P (pointe, %d guichets, %d états)", c_ref, nrow(P)))
fig_convergence(P, pv, "figures/fig3_convergence.png", "Convergence vers la distribution stationnaire")

# --- 4. Validation Monte Carlo ---
sm <- params$simulation
sim <- simuler_banque(params$K, c_ref, params$s, lp, lo, sm$pas, sm$graine, sm$rechauffement)
pis <- frequences_dans_ordre(sim, attr(P, "etats"))
fig_validation(pv, pis, "figures/fig5_validation.png", "Théorie vs simulation (pointe, 2 guichets)")
ind <- indicateurs(P, pv, params$K, c_ref, params$s, lp, lo)

besoin <- guichets_necessaires(sc, params$seuil_attente_min)
resultats <- list(
  seuil_attente_min = params$seuil_attente_min,
  guichets_necessaires = as.list(besoin),
  exemple3 = list(P = P3, P2 = P3_2, pi = pi3),
  reference = list(periode = "pointe", guichets = c_ref, nb_etats = nrow(P),
                   indicateurs = ind,
                   ecart_max_pi = max(abs(pis - pv)),
                   ecart_moyen_pi = mean(abs(pis - pv)),
                   simulation = list(pas = sim$nb_pas, debit_p = sim$debit_p, debit_o = sim$debit_o,
                                     refus_p = sim$refus_p, refus_o = sim$refus_o)),
  scenarios = sc)
jsonlite::write_json(resultats, "outputs/resultats.json", pretty = TRUE, digits = 8, auto_unbox = TRUE)

cat("== Exemple 3 états ==\n"); print(round(P3, 4)); print(round(pi3, 4))
cat("\n== Scénarios ==\n"); print(format(sc, digits = 3), row.names = FALSE)
cat(sprintf("\nValidation (pointe, 2 guichets) : écart max |pi_sim - pi| = %.4f\n", max(abs(pis - pv))))

# --- 5. Affichage des graphiques dans l'onglet Plots (RStudio) ---
# Les mêmes graphiques sont déjà enregistrés en PNG dans le dossier 'figures'.
if (interactive()) {
  cat("\nAffichage des 5 graphiques dans l'onglet Plots (flèches ◀ ▶ pour passer de l'un à l'autre)...\n")
  montrer(function() dessiner_diagramme_3etats(P3))
  montrer(function() dessiner_heatmap_P(P, sprintf("Matrice P (pointe, %d guichets, %d états)", c_ref, nrow(P))))
  montrer(function() dessiner_convergence(P, pv, "Convergence vers la distribution stationnaire"))
  montrer(function() dessiner_scenarios(sc))
  montrer(function() dessiner_validation(pv, pis, "Théorie vs simulation (pointe, 2 guichets)"))
}

# --- 6. Tests ---
if (requireNamespace("testthat", quietly = TRUE)) {
  testthat::test_dir("tests/testthat", reporter = "summary", stop_on_failure = FALSE)
}

cat("\nLes 5 images sont aussi enregistrées dans le dossier 'figures' (onglet Files).\n")
if (interactive()) {
  cat("Pour voir la banque minute par minute : ouvre demo_simulation.R puis clique sur Source.\n")
} else {
  cat("(Les graphiques s'affichent dans l'onglet Plots quand on lance ce script depuis RStudio.)\n")
}
