# =====================================================================
# installer_paquets.R : installe les paquets R nécessaires au projet.
# À faire UNE SEULE FOIS par ordinateur (nécessite internet).
# RStudio : ouvrir ce fichier puis cliquer sur "Source".
# =====================================================================
options(timeout = 600)                       # internet lent : on attend plus longtemps
cat("Version de R :", as.character(getRversion()), "\n")
if (getRversion() < "4.0.0")
  stop("Ta version de R est trop ancienne. Installe R 4.2 ou plus récent (cran.r-project.org).")

besoin <- c("jsonlite",   # lit le fichier de chiffres (data/parametres.json)
            "testthat",   # vérifications automatiques
            "rmarkdown",  # bouton Knit : fabrique le document Word / HTML
            "knitr",      # exécute le code R dans le document
            "officer")    # fabrique les fichiers Word des livrables (facultatif)

manquants <- besoin[!vapply(besoin, requireNamespace, logical(1), quietly = TRUE)]
if (length(manquants) == 0) {
  cat("\nTout est déjà installé. Tu peux passer à run_all.R.\n")
} else {
  cat("\nInstallation de :", paste(manquants, collapse = ", "), "\n")
  install.packages(manquants, repos = "https://cloud.r-project.org")
  encore <- manquants[!vapply(manquants, requireNamespace, logical(1), quietly = TRUE)]
  if (length(encore) == 0) {
    cat("\nTerminé : tous les paquets sont installés.\n")
  } else {
    cat("\nAttention, ces paquets n'ont pas pu être installés :", paste(encore, collapse = ", "),
        "\nVérifie ta connexion internet puis relance ce script.\n")
  }
}
