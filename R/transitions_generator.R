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
  oviposit_transitions <- mapply(FUN = function(g, trans_idx) {
    make_transition_ovi(T_index = trans_idx, u = u, f_gen = cube$genotypesID[g[1]], m_gen = cube$genotypesID[g[2]], o_gen = cube$genotypesID[g[3]])
  }, g = cube_nonzero, trans_idx = transition_index_ovi, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  return(oviposit_transitions)
}


generate_egg_advancement_transitions <- function(cube, u, nE) {

}
