#' @title Generate oviposition transitions
#' @description Generates oviposition transitions for a single node
#' @param cube an inheritance cube (see [MGDrivE::cubeHomingDrive] for an example)
#' @param u character vector of places
#' @export
generate_oviposition_transitions <- function(cube, u) {

  stopifnot(length(u) > 0L)
  stopifnot(is.character(u))
  stopifnot(inherits(cube, "list"))

  # possible oviposition events
  cube_nonzero <- which(cube$ih * cube$tau > 0, arr.ind = TRUE)
  cube_nonzero <- unname(as.list(data.frame(t(cube_nonzero))))

  # to index into set of transitions
  transition_index_ovi <- 1:length(cube_nonzero)

  # generate transitions
  transitions <- mapply(FUN = function(g, trans_idx) {
    make_transition_ovi(T_index = trans_idx, u = u, f_gen = cube$genotypesID[g[1]], m_gen = cube$genotypesID[g[2]], o_gen = cube$genotypesID[g[3]])
  }, g = cube_nonzero, trans_idx = transition_index_ovi, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  return(transitions)
}


#' @title Generate egg advancement transitions
#' @description Generates egg advancement transitions for a single node
#' @param cube an inheritance cube (see [MGDrivE::cubeHomingDrive] for an example)
#' @param u character vector of places
#' @param nE number of egg stages (shape parameter)
#' @export
generate_egg_advancement_transitions <- function(cube, u, nE) {

  stopifnot(length(u) > 0L)
  stopifnot(is.character(u))
  stopifnot(inherits(cube, "list"))

  g <- cube$genotypesID
  nG <- cube$genotypesN

  # states to loop over
  egg_stage <- rep(x = 1:nE, each = nG)
  genotype <- rep(x = g, times = nE)

  # generate transitions
  transitions <- mapply(FUN = function(g, e_stage) {
    if (e_stage == nE) {
      stage2 <- NULL
    } else {
      stage2 <- e_stage + 1L
    }
    make_transition_egg_adv(T_index = NA, u = u, e_gen = g, stage1 = e_stage, stage2 = stage2)
  }, g = genotype, e_stage = egg_stage, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  return(transitions)
}


#' @title Generate egg mortality transitions
#' @description Generates egg mortality transitions for a single node
#' @param cube an inheritance cube (see [MGDrivE::cubeHomingDrive] for an example)
#' @param u character vector of places
#' @param nE number of egg stages (shape parameter)
#' @export
generate_egg_mortality_transitions <- function(cube, u, nE) {

  stopifnot(length(u) > 0L)
  stopifnot(is.character(u))
  stopifnot(inherits(cube, "list"))

  g <- cube$genotypesID
  nG <- cube$genotypesN

  # states to loop over
  egg_stage <- rep(x = 1:nE, each = nG)
  genotype <- rep(x = g, times = nE)

  # generate transitions
  transitions <- mapply(FUN = function(g, e_stage) {
    make_transition_egg_mort(T_index = NA, u = u, e_gen = g, stage = e_stage)
  }, g = genotype, e_stage = egg_stage, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  return(transitions)
}
