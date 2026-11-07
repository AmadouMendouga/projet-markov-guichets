#' markovguichet : chaines de Markov pour une file d'attente de banque
#'
#' Le package modelise une banque avec plusieurs guichets et des clients
#' prioritaires par une chaine de Markov a temps discret (un pas = 1 minute).
#' Il construit la matrice de transition ([construire_P()]), calcule la
#' distribution stationnaire ([distribution_stationnaire()]) et des
#' indicateurs d'attente ([indicateurs()]), compare des scenarios
#' ([lancer_scenarios()]) et valide le tout par simulation Monte Carlo
#' ([simuler_banque()]). Les parametres fournis sont **fictifs**.
#'
#' @keywords internal
"_PACKAGE"
