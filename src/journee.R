# =====================================================================
# journee.R : une VRAIE simulation client par client.
#
# On fait vivre une journée à l'agence minute par minute. Chaque client
# est suivi individuellement : heure d'arrivée, début du service, fin du
# service, attente. Les règles sont celles du modèle (voir la documentation du modèle) :
#  - au plus une arrivée par minute (prioritaire avec la proba lp[t],
#    ordinaire avec lo[t]) ; refusée si la banque contient déjà K clients ;
#  - chaque guichet occupé termine son service avec la probabilité s à chaque
#    minute ;
#  - les prioritaires passent avant les ordinaires, dans l'ordre d'arrivée.
#  - interruption = FALSE (réaliste) : un service commencé n'est jamais interrompu.
#    interruption = TRUE : un prioritaire peut prendre la place d'un ordinaire en
#    cours de service (c'est l'hypothèse de la matrice de transition P ; on s'en
#    sert pour comparer la simulation au calcul).
# =====================================================================

# lp et lo sont des vecteurs : une probabilité d'arrivée pour CHAQUE minute
# (par exemple plus élevée à l'heure de pointe).
simuler_clients <- function(K, c, s, lp, lo, graine = 1, interruption = FALSE) {
  set.seed(graine)
  T <- length(lp)
  u <- runif(T)                                   # tirage des arrivées (même suite pour tous les c)
  type <- integer(T); arrivee <- integer(T); debut <- rep(NA_integer_, T)
  fin <- rep(NA_integer_, T); admis <- logical(T)
  n <- 0L
  fp <- integer(0); fo <- integer(0)              # files d'attente : prioritaires, ordinaires
  serveur <- integer(c)                           # 0 = guichet libre, sinon numéro du client servi
  presents <- integer(T)                          # clients dans la banque au début de chaque minute
  for (t in seq_len(T)) {
    occ <- which(serveur > 0L)
    presents[t] <- length(fp) + length(fo) + length(occ)
    deja <- presents[t]
    if (length(occ)) {                            # fins de service
      finis <- occ[runif(length(occ)) < s]
      if (length(finis)) { fin[serveur[finis]] <- t; serveur[finis] <- 0L }
    }
    a <- if (u[t] < lp[t]) 1L else if (u[t] < lp[t] + lo[t]) 2L else 0L   # arrivée
    if (a > 0L) {
      n <- n + 1L; type[n] <- a; arrivee[n] <- t
      if (deja < K) {
        admis[n] <- TRUE
        if (a == 1L) fp <- c(fp, n) else fo <- c(fo, n)
      }
    }
    for (j in which(serveur == 0L)) {             # affectation des guichets libres
      if (length(fp)) { id <- fp[1]; fp <- fp[-1] }
      else if (length(fo)) { id <- fo[1]; fo <- fo[-1] }
      else break
      serveur[j] <- id; if (is.na(debut[id])) debut[id] <- t
    }
    if (interruption && length(fp)) {             # un prioritaire peut interrompre un ordinaire en service
      for (j in which(serveur > 0L & type[pmax(serveur, 1L)] == 2L)) {
        if (!length(fp)) break
        fo <- c(serveur[j], fo)                   # l'ordinaire retourne en tête de sa file
        id <- fp[1]; fp <- fp[-1]
        serveur[j] <- id; if (is.na(debut[id])) debut[id] <- t
      }
    }
  }
  clients <- data.frame(
    id = seq_len(n), type = ifelse(type[seq_len(n)] == 1L, "prioritaire", "ordinaire"),
    arrivee = arrivee[seq_len(n)], admis = admis[seq_len(n)],
    debut = debut[seq_len(n)], fin = fin[seq_len(n)])
  clients$attente <- clients$debut - clients$arrivee     # minutes passées en file avant le guichet
  list(clients = clients, presents = presents)
}

# Probabilités d'arrivée minute par minute pour une journée à plusieurs périodes.
# periodes : liste de (debut_min, fin_min, nom) ; params : lu dans data/parametres.json
profil_journee <- function(params, ouverture = 8, fermeture = 16, pointe = c(11, 14)) {
  T <- (fermeture - ouverture) * 60
  heure <- ouverture + (seq_len(T) - 1) / 60
  en_pointe <- heure >= pointe[1] & heure < pointe[2]
  lp <- ifelse(en_pointe, params$periodes$pointe$lambda_prio, params$periodes$creuse$lambda_prio)
  lo <- ifelse(en_pointe, params$periodes$pointe$lambda_ord, params$periodes$creuse$lambda_ord)
  list(lp = lp, lo = lo, heure = heure, pointe = pointe, ouverture = ouverture)
}

# Résumé d'une journée : combien de clients, d'attente, de refus
resumer_journee <- function(sim, c) {
  cl <- sim$clients
  servis <- cl[!is.na(cl$debut), ]
  data.frame(
    guichets = c,
    arrives = nrow(cl),
    refuses = sum(!cl$admis),
    servis = nrow(servis),
    attente_moy = if (nrow(servis)) round(mean(servis$attente), 2) else NA,
    attente_ord = round(mean(servis$attente[servis$type == "ordinaire"]), 2),
    attente_prio = round(mean(servis$attente[servis$type == "prioritaire"]), 2),
    attente_max = max(servis$attente))
}

# ---------- Graphiques ----------
dessiner_journee <- function(sim, c, profil, K, court = FALSE, ylab = TRUE) {
  p <- profil
  cl <- sim$clients
  n_ref <- sum(!cl$admis)
  att <- round(mean(cl$attente, na.rm = TRUE), 1)
  op <- par(mar = c(3, if (ylab) 3.8 else 2.4, 3.6, 0.8), cex.axis = 0.95, cex.lab = 1)
  on.exit(par(op))
  x <- p$heure; y <- sim$presents
  plot(x, y, type = "n", ylim = c(0, K + 0.6), xlim = range(x), xaxt = "n", las = 1,
       xlab = "", ylab = if (!ylab) "" else if (court) "clients" else "clients dans la banque")
  rect(p$pointe[1], -1, p$pointe[2], K + 1, col = "#0000000F", border = NA)       # zone grise : pointe
  abline(h = K, lty = 2, col = "#C55A11", lwd = 1.3)                              # capacité
  xn <- c(x[-1], x[length(x)] + 1 / 60)                                           # courbe en escalier, avec aire
  xs <- as.vector(rbind(x, xn)); ys <- as.vector(rbind(y, y))
  polygon(c(xs[1], xs, xs[length(xs)]), c(0, ys, 0), col = "#1F4E7924", border = NA)
  lines(xs, ys, col = "#1F4E79", lwd = 2)
  h0 <- p$ouverture; h1 <- p$ouverture + length(x) / 60
  axis(1, at = seq(h0, h1, by = 2), labels = paste0(seq(h0, h1, by = 2), " h"))
  title(main = sprintf("%d guichet%s", c, ifelse(c > 1, "s", "")), line = 1.7, cex.main = 1.15)
  mtext(sprintf("%d refusés · attente moyenne %s min", n_ref, format(att, decimal.mark = ",")),
        side = 3, line = 0.35, cex = 0.85, font = 2, col = if (n_ref > 0) "#C55A11" else "#2E7D6B")
}

dessiner_attentes <- function(sim_a, c_a, sim_b, c_b) {
  op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1), cex.main = 1.15, cex.axis = 0.95); on.exit(par(op))
  m <- max(c(sim_a$clients$attente, sim_b$clients$attente), na.rm = TRUE)
  cas <- list(list(sim_a, c_a), list(sim_b, c_b))
  for (z in cas) {
    w <- z[[1]]$clients$attente[z[[1]]$clients$admis & !is.na(z[[1]]$clients$attente)]
    hist(w, breaks = seq(0, ceiling(m / 5) * 5 + 5, by = 5), col = "#1F4E79", border = "white",
         main = sprintf("%d guichet%s", z[[2]], ifelse(z[[2]] > 1, "s", "")),
         xlab = "attente avant le guichet (min)", ylab = "nombre de clients")
  }
}

# ---------------------------------------------------------------------
# Animation : on regarde la banque se remplir EN DIRECT, minute après minute
# ---------------------------------------------------------------------
heure_txt <- function(t, ouverture = 8) {
  m <- ouverture * 60 + (t - 1)
  sprintf("%d h %02d", m %/% 60, m %% 60)
}

# Qui est en service, qui attend, combien de refusés, à la minute t
etat_a_la_minute <- function(cl, t) {
  adm <- cl$admis
  serv <- adm & !is.na(cl$debut) & cl$debut <= t & (is.na(cl$fin) | cl$fin > t)
  att  <- adm & cl$arrivee <= t & (is.na(cl$debut) | cl$debut > t)
  list(service = cl[serv, ], attente = cl[att, ],
       refuses = sum(!adm & cl$arrivee <= t),
       servis = sum(adm & !is.na(cl$fin) & cl$fin <= t))
}

# Une image de la banque à la minute t : guichets, file d'attente, compteurs, courbe
dessiner_agence <- function(sim, c, profil, K, t) {
  e <- etat_a_la_minute(sim$clients, t)
  op <- par(no.readonly = TRUE); on.exit({ layout(1); par(op) })
  layout(matrix(1:2, 2, 1), heights = c(1.35, 1))
  par(mar = c(0.3, 1, 2.8, 1))
  plot(NULL, xlim = c(0, 10), ylim = c(0, 4.2), axes = FALSE, xlab = "", ylab = "",
       main = sprintf("%d guichet%s · il est %s", c, ifelse(c > 1, "s", ""), heure_txt(t, profil$ouverture)), cex.main = 1.25)
  larg <- min(1.35, 6.4 / c)
  text(0.3, 4.0, "GUICHETS", adj = 0, cex = 0.85, font = 2, col = "#555555")
  for (j in seq_len(c)) {                                    # les guichets
    x0 <- 0.3 + (j - 1) * (larg + 0.12)
    occupe <- j <= nrow(e$service)
    rect(x0, 2.75, x0 + larg, 3.8, col = if (occupe) "#1F4E79" else "#F0F0F0", border = "#1F4E79", lwd = 1.5)
    text(x0 + larg / 2, 3.5, if (occupe) "occupé" else "libre", cex = 0.85, font = 2, col = if (occupe) "white" else "#888888")
    if (occupe) points(x0 + larg / 2, 3.05, pch = 19, cex = 1.5,
                       col = if (e$service$type[j] == "prioritaire") "#F4A259" else "white")
  }
  att <- e$attente[order(e$attente$type != "prioritaire", e$attente$arrivee), ]
  text(0.3, 2.45, sprintf("FILE D'ATTENTE (%d)", nrow(att)), adj = 0, cex = 0.85, font = 2, col = "#555555")
  for (k in seq_len(nrow(att)))                              # la file d'attente
    rect(0.3 + (k - 1) * 0.6, 1.35, 0.3 + (k - 1) * 0.6 + 0.5, 2.25, border = NA,
         col = if (att$type[k] == "prioritaire") "#F4A259" else "#7FA6CF")
  dans <- nrow(e$service) + nrow(att)
  text(9.8, 3.75, sprintf("dans la banque : %d / %d", dans, K), adj = 1, cex = 1.15, font = 2)
  text(9.8, 3.15, sprintf("servis : %d", e$servis), adj = 1, cex = 1.0, col = "#444444")
  text(9.8, 2.55, sprintf("refusés : %d", e$refuses), adj = 1, cex = 1.05,
       col = if (e$refuses > 0) "#C55A11" else "#444444", font = if (e$refuses > 0) 2 else 1)
  rect(0.3, 0.3, 0.6, 0.6, col = "#F4A259", border = NA); text(0.75, 0.45, "prioritaire", adj = 0, cex = 0.8, col = "#555555")
  rect(2.6, 0.3, 2.9, 0.6, col = "#7FA6CF", border = NA); text(3.05, 0.45, "ordinaire", adj = 0, cex = 0.8, col = "#555555")
  par(mar = c(3, 4, 0.8, 1), cex.axis = 0.95)                # la courbe qui se construit
  plot(profil$heure[seq_len(t)], sim$presents[seq_len(t)], type = "s", lwd = 2, col = "#1F4E79", las = 1,
       xlim = range(profil$heure), ylim = c(0, K + 0.6), xaxt = "n", xlab = "", ylab = "clients")
  rect(profil$pointe[1], -1, profil$pointe[2], K + 1, col = "#0000000F", border = NA)
  abline(h = K, lty = 2, col = "#C55A11")
  axis(1, at = seq(profil$ouverture, profil$ouverture + length(profil$heure) / 60, by = 2),
       labels = paste0(seq(profil$ouverture, profil$ouverture + length(profil$heure) / 60, by = 2), " h"))
}

# Joue la journée en accéléré : une image toutes les `pas` minutes
animer_journee <- function(sim, c, profil, K, pas = 6, delai = 0.04) {
  T <- length(profil$lp)
  for (t in unique(c(seq(1, T, by = pas), T))) {
    dev.hold(); dessiner_agence(sim, c, profil, K, t); dev.flush()
    Sys.sleep(delai)
  }
  invisible(TRUE)
}

# Les 5 journées côte à côte : un graphique par nombre de guichets + un résumé lisible
dessiner_journees_toutes <- function(journees, profil, K, resume) {
  op <- par(mfrow = c(3, 2)); on.exit(par(op))
  par(cex = 1)                                   # texte plus grand que le réglage automatique de R
  for (k in seq_along(journees))
    dessiner_journee(journees[[k]], as.integer(names(journees)[k]), profil, K, court = TRUE, ylab = (k %% 2 == 1))
  op2 <- par(mar = c(1, 1, 1, 1)); on.exit(par(op2), add = TRUE)
  plot(NULL, xlim = c(0, 1), ylim = c(0, 1), axes = FALSE, xlab = "", ylab = "")
  text(0.02, 0.99, "Résumé de la journée", adj = c(0, 1), font = 2, cex = 1.0)
  text(c(0.02, 0.40, 0.66), 0.80, c("Guichets", "Refusés", "Attente"), adj = 0, font = 2, cex = 0.8, col = "#555555")
  segments(0.02, 0.745, 0.98, 0.745, col = "#BBBBBB")
  for (i in seq_len(nrow(resume))) {
    y <- 0.67 - (i - 1) * 0.105
    refus <- resume$refuses[i] > 0
    text(0.02, y, resume$guichets[i], adj = 0, cex = 0.8)
    text(0.40, y, resume$refuses[i], adj = 0, cex = 0.8, font = if (refus) 2 else 1, col = if (refus) "#C55A11" else "#2E7D6B")
    text(0.66, y, sprintf("%s min", format(resume$attente_moy[i], decimal.mark = ",")), adj = 0, cex = 0.8)
  }
  rect(0.02, 0.07, 0.07, 0.17, col = "#0000000F", border = "#999999")
  text(0.10, 0.12, "zone grise : heure de pointe", adj = 0, cex = 0.7, col = "#555555")
  segments(0.02, 0.01, 0.07, 0.01, lty = 2, col = "#C55A11", lwd = 1.5)
  text(0.10, 0.01, "ligne orange : capacité (8 clients)", adj = 0, cex = 0.7, col = "#555555")
}
