# =====================================================================
# demo_simulation.R : UNE JOURNÉE À L'AGENCE, simulée client par client.
#
# Lundi, de 8 h à 16 h. Les clients arrivent au hasard (plus nombreux de
# 11 h à 14 h), les guichets les servent, et on note pour CHAQUE client
# son heure d'arrivée, son attente et son départ. On compare 1 à 5
# guichets avec exactement la même suite d'arrivées.
#
# RStudio : ouvrir ce fichier puis cliquer sur "Source".
# Les graphiques apparaissent dans l'onglet Plots (bouton Zoom pour les agrandir).
# =====================================================================
args <- commandArgs(trailingOnly = FALSE)
f <- sub("--file=", "", args[grep("--file=", args)])
if (length(f)) {
  setwd(dirname(normalizePath(f)))
} else {
  of <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!is.null(of)) setwd(dirname(normalizePath(of)))
}
if (!file.exists("data/parametres.json"))
  stop("Ouvre le projet 'projet-markov.Rproj' (dossier du projet) avant de lancer ce script.")
for (s in c("modele", "simulation", "scenarios", "figures", "journee")) source(file.path("src", paste0(s, ".R")), encoding = "UTF-8")
dir.create("outputs", showWarnings = FALSE); dir.create("figures", showWarnings = FALSE)

# ---- RÉGLAGES (tu peux les modifier) ----
animation <- TRUE        # TRUE : on regarde la banque se remplir en direct. FALSE : pas d'animation.
vitesse   <- 0.04        # pause entre deux images de l'animation, en secondes (plus petit = plus rapide)
pause     <- 2           # pause (secondes) après chaque cas, pour lire le résultat
pause_entre_cas <- FALSE # TRUE : on attend que tu appuies sur Entrée avant de passer au cas suivant
# ------------------------------------------

params <- charger_parametres()
K <- params$K
guichets <- params$guichets

# ---------------------------------------------------------------------
# 1. Une journée complète, client par client
# ---------------------------------------------------------------------
cat("\n=== 1. Lundi, de 8 h à 16 h : 1 à 5 guichets, mêmes clients ===\n")
profil <- profil_journee(params)               # 8 h - 16 h, pointe de 11 h à 14 h
journees <- lapply(guichets, function(c) simuler_clients(K, c, params$s, profil$lp, profil$lo, graine = 2026))
names(journees) <- guichets
resume <- do.call(rbind, lapply(seq_along(guichets), function(k) resumer_journee(journees[[k]], guichets[k])))
print(resume, row.names = FALSE)
write.csv(resume, "outputs/journee_resume.csv", row.names = FALSE)
write.csv(journees[["4"]]$clients, "outputs/journee_clients_4guichets.csv", row.names = FALSE)
cat("\nLes clients de la journée avec 4 guichets sont dans outputs/journee_clients_4guichets.csv\n",
    "(une ligne par client : arrivée, début de service, fin, attente en minutes).\n", sep = "")
cat("Colonnes : arrives = clients arrivés ; refuses = salle pleine ; attente_* = minutes avant le guichet.\n",
    "Lecture : avec 1 guichet, des clients sont refusés (salle pleine) et l'attente explose à l'heure de pointe ;\n",
    "avec 4 guichets, presque tout le monde est servi sans attendre.\n", sep = "")

# Graphiques
dessiner_journees_comparees <- function() {
  op <- par(mfrow = c(2, 1)); on.exit(par(op))
  dessiner_journee(journees[["1"]], 1, profil, K)
  dessiner_journee(journees[["4"]], 4, profil, K)
}
dessiner_attentes_comparees <- function() dessiner_attentes(journees[["1"]], 1, journees[["4"]], 4)
dessiner_cinq_journees <- function() dessiner_journees_toutes(journees, profil, K, resume)

if (interactive()) {
  # CAS PAR CAS : on commence avec 1 guichet, puis 2, puis 3, 4 et 5.
  # Pour chaque cas : l'animation de la banque en direct, puis le graphique de la journée.
  for (c in guichets) {
    r <- resume[resume$guichets == c, ]
    cat(sprintf("
--- CAS %d sur %d : %d guichet%s ---
", which(guichets == c), length(guichets), c, ifelse(c > 1, "s", "")))
    if (animation) {
      cat("Animation : la banque se remplit en direct (de 8 h à 16 h)...
")
      montrer(function() animer_journee(journees[[as.character(c)]], c, profil, K, pas = 8, delai = vitesse))
    }
    montrer(function() dessiner_journee(journees[[as.character(c)]], c, profil, K))
    cat(sprintf("Résultat : %d clients refusés sur %d, attente moyenne %s min (maximum %d min).
",
                r$refuses, r$arrives, format(r$attente_moy, decimal.mark = ","), r$attente_max))
    if (pause_entre_cas && c != tail(guichets, 1)) invisible(readline("Appuie sur Entrée pour passer au cas suivant... "))
    else Sys.sleep(pause)
  }
  cat("
--- Pour finir : l'attente de chaque client, puis les 5 journées réunies ---
",
      "Flèche ◀ au-dessus du graphique pour revenir en arrière, Zoom pour agrandir.
", sep = "")
  montrer(dessiner_attentes_comparees)
  montrer(dessiner_cinq_journees)            # dernier graphique : les 5 journées d'un coup
}
sortie_png("figures/fig7_journee.png", 1600, 1200, 180, dessiner_journees_comparees)
sortie_png("figures/fig8_attentes.png", 1800, 800, 200, dessiner_attentes_comparees)
sortie_png("figures/fig9_cinq_journees.png", 1800, 1500, 200, dessiner_cinq_journees)

# ---------------------------------------------------------------------
# 2. Vérification : la simulation client par client retombe-t-elle sur le calcul ?
# ---------------------------------------------------------------------
cat("\n=== 2. Vérification à l'heure de pointe (arrivées constantes, 100 000 minutes) ===\n")
lp <- params$periodes$pointe$lambda_prio; lo <- params$periodes$pointe$lambda_ord
T_val <- 100000
verif <- do.call(rbind, lapply(guichets, function(c) {
  sim <- simuler_clients(K, c, params$s, rep(lp, T_val), rep(lo, T_val), graine = 5, interruption = TRUE)
  cl <- sim$clients[sim$clients$arrivee > 2000, ]
  servis <- cl[!is.na(cl$debut), ]
  P <- construire_P(K, c, params$s, lp, lo)
  ind <- indicateurs(P, distribution_stationnaire(P), K, c, params$s, lp, lo)
  data.frame(guichets = c,
             ord_mesure = round(mean(servis$attente[servis$type == "ordinaire"]), 2),
             ord_calcul = round(ind$Wq_o, 2),
             prio_mesure = round(mean(servis$attente[servis$type == "prioritaire"]), 2),
             prio_calcul = round(ind$Wq_p, 2),
             refus_mesure = round(mean(!cl$admis), 4),
             pleine_calcul = round(ind$p_pleine, 4))
}))
print(verif, row.names = FALSE)
write.csv(verif, "outputs/journee_verification.csv", row.names = FALSE)
jsonlite::write_json(list(resume = resume, verification = verif, pas_verification = T_val, graine_journee = 2026),
                     "outputs/journee.json", pretty = TRUE, digits = 6, auto_unbox = TRUE)
cat("\nLes colonnes 'mesure' et 'calcul' doivent être proches : la simulation client par client\n",
    "confirme le calcul par matrice de Markov.\n", sep = "")
