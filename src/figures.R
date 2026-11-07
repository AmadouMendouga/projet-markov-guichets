# =====================================================================
# figures.R : graphiques du projet.
#  - dessiner_*()  : dessinent le graphique à l'écran (onglet Plots de RStudio)
#  - fig_*()       : enregistrent le même graphique en PNG dans /figures
#  - montrer()     : affiche un graphique à l'écran sans jamais planter,
#                    même si le panneau Plots est trop petit
# =====================================================================

COL_ACCENT <- "#1F4E79"; COL_2 <- "#C55A11"; COL_GRIS <- "#7F7F7F"

# Affiche un graphique dans l'onglet Plots. Si le panneau est trop petit,
# R refuse de dessiner : on affiche alors un message clair au lieu d'une erreur.
montrer <- function(dessin) {
  tryCatch({ dessin(); invisible(TRUE) },
           error = function(e) {
             message("Le graphique n'a pas pu s'afficher (le panneau Plots est sans doute trop petit). ",
                     "Agrandis-le (double-clic sur l'onglet Plots) ou ouvre l'image dans le dossier 'figures'.")
             invisible(FALSE)
           })
}

# Enregistre un graphique en PNG
sortie_png <- function(chemin, largeur, hauteur, res, dessin) {
  png(chemin, width = largeur, height = hauteur, res = res)
  on.exit(invisible(dev.off()))
  dessin()
  invisible(chemin)
}

# ---------- 1. Diagramme à 3 états ----------
dessiner_diagramme_3etats <- function(P) {
  op <- par(mar = c(1, 1, 3, 1)); on.exit(par(op))
  plot(NULL, xlim = c(0, 10), ylim = c(0, 5), axes = FALSE, xlab = "", ylab = "",
       main = "Diagramme de transitions (exemple à 3 états)")
  x <- c(1.5, 5, 8.5); y <- 2.5
  for (i in 1:2) {
    arrows(x[i] + 0.6, y + 0.35, x[i + 1] - 0.6, y + 0.35, length = 0.12, lwd = 2, col = COL_ACCENT)
    text((x[i] + x[i + 1]) / 2, y + 0.7, sprintf("%.3f", P[i, i + 1]), cex = 0.9)
    arrows(x[i + 1] - 0.6, y - 0.35, x[i] + 0.6, y - 0.35, length = 0.12, lwd = 2, col = COL_ACCENT)
    text((x[i] + x[i + 1]) / 2, y - 0.7, sprintf("%.3f", P[i + 1, i]), cex = 0.9)
  }
  symbols(x, rep(y, 3), circles = rep(0.6, 3), inches = FALSE, add = TRUE,
          bg = "white", fg = COL_ACCENT, lwd = 3)
  text(x, y, c("0", "1", "2"), cex = 1.6, font = 2)
  for (i in 1:3) text(x[i], y - 1.3, sprintf("reste : %.3f", P[i, i]), cex = 0.85, col = COL_GRIS)
}
fig_diagramme_3etats <- function(P, chemin) sortie_png(chemin, 1600, 800, 200, function() dessiner_diagramme_3etats(P))

# ---------- 2. Carte de chaleur de P ----------
dessiner_heatmap_P <- function(P, titre) {
  op <- par(mar = c(4, 4, 3, 2)); on.exit(par(op))
  n <- nrow(P)
  image(1:n, 1:n, t(P[n:1, ]), col = colorRampPalette(c("white", COL_ACCENT))(100),
        xlab = "indice de l'état d'arrivée j", ylab = "indice de l'état de départ i (en haut : i = 1)",
        main = titre, axes = FALSE)
  pos <- seq(1, n, by = 4)
  axis(1, at = pos, labels = pos)
  axis(2, at = n + 1 - pos, labels = pos)
  box()
}
fig_heatmap_P <- function(P, chemin, titre) sortie_png(chemin, 1400, 1300, 180, function() dessiner_heatmap_P(P, titre))

# ---------- 3. Convergence vers la distribution stationnaire ----------
distances_convergence <- function(P, pi_vec, nmax = 150) {
  v <- c(1, rep(0, nrow(P) - 1))               # départ : banque vide
  d <- numeric(nmax + 1); d[1] <- 0.5 * sum(abs(v - pi_vec))
  for (k in 1:nmax) { v <- as.numeric(v %*% P); d[k + 1] <- 0.5 * sum(abs(v - pi_vec)) }
  d
}
dessiner_convergence <- function(P, pi_vec, titre, nmax = 150) {
  d <- distances_convergence(P, pi_vec, nmax)
  plot(0:nmax, d, type = "l", lwd = 3, col = COL_ACCENT, log = "y",
       xlab = "Nombre de minutes n", ylab = "Distance à π (variation totale, échelle log)",
       main = titre)
  grid()
}
fig_convergence <- function(P, pi_vec, chemin, titre, nmax = 150) {
  sortie_png(chemin, 1400, 900, 200, function() dessiner_convergence(P, pi_vec, titre, nmax))
  invisible(distances_convergence(P, pi_vec, nmax))
}

# ---------- 4. Comparaison des scénarios ----------
dessiner_scenarios <- function(df) {
  op <- par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1)); on.exit(par(op))
  ymax <- max(df$Wq_ord, df$Wq_prio, na.rm = TRUE) * 1.15
  for (per in unique(df$periode)) {
    d <- df[df$periode == per, ]
    m <- rbind(d$Wq_prio, d$Wq_ord); colnames(m) <- d$guichets
    barplot(m, beside = TRUE, col = c(COL_2, COL_ACCENT), ylab = "Attente moyenne en file (min)",
            xlab = "Nombre de guichets", main = paste("Heure", per), ylim = c(0, ymax))
    if (per == unique(df$periode)[1])
      legend("topright", legend = c("Prioritaires", "Ordinaires"), fill = c(COL_2, COL_ACCENT), bty = "n")
  }
}
fig_scenarios <- function(df, chemin) sortie_png(chemin, 1800, 900, 200, function() dessiner_scenarios(df))

# ---------- 5. Validation : théorie contre simulation ----------
dessiner_validation <- function(pi_theo, pi_sim, titre) {
  op <- par(mar = c(4.5, 4.5, 3, 1)); on.exit(par(op))
  barplot(rbind(pi_theo, pi_sim), beside = TRUE, col = c(COL_ACCENT, COL_2),
          xlab = "Indice de l'état (a, b)", ylab = "Probabilité", main = titre,
          names.arg = seq_along(pi_theo), cex.names = 0.6)
  legend("topright", legend = c("Calcul (π)", "Simulation Monte Carlo"),
         fill = c(COL_ACCENT, COL_2), bty = "n")
}
fig_validation <- function(pi_theo, pi_sim, chemin, titre) sortie_png(chemin, 1800, 900, 200, function() dessiner_validation(pi_theo, pi_sim, titre))
