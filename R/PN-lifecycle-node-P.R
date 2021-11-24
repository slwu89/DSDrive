################################################################################
#
#   DSDrivE: SPN structure for a single node (lifecycle only)
#   Marshall Lab
#   Agastya Mondal (agastya_mondal@berkeley.edu)
#   November 2021
#
################################################################################

################################################################################
# make the places (P) of the SPN
################################################################################

#' Make Places (P) For a Node (D. Suzukii only)
#'
#' This function makes the set of places (P) for a SPN. It is used alone if our model
#' is a single-node metapopulation for d. suzukii dynamics only.
#'
#' The \code{params} argument supplies all of the ecological parameters necessary
#' to calculate equilibrium values. This function requires the \code{nE},
#' \code{nL}, \code{nA}, and \code{nP} parameters to be specified.
#'
#'
#' @param params a named list of parameters (see details)
#' @param cube an inheritance cube from the \code{MGDrivE} package (e.g. \code{\link[MGDrivE]{cubeMendelian}})
#'
#' @return a list with two elements: \code{ix} contains labeled indices of the places
#' by life stage, \code{u} is the character vector of places (P)
#'
#' @importFrom stats setNames
#'
#' @export
spn_P_lifecycle_node <- function(params, cube) {
  # checks
  nE <- params$nE
  nL <- params$nL
  nP <- params$nP
  nA <- params$nA
  if (nE < 2 || nL < 2 || nP < 2 || nA < 2) {
    warning(
      paste0(
        "A shape parameter ('nE', 'nL', 'nA', or 'nP') of 1 implies ",
        "exponentially distributed dwell times in that compartment."
      )
    )
  }
  
  # genetic information
  nG <- cube$genotypesN
  g <- cube$genotypesID
  
  # setup place names
  eggs <- file.path("E", 1:nE, "_", rep(g, each = nE), fsep = "")
  
  larvae <- file.path("L", 1:nL, "_", rep(g, each = nL), fsep = "")
  
  pupae <- file.path("P", 1:nP, "_", rep(g, each = nP), fsep = "")
  
  females_unmated <-
    file.path("U", 1:nA, "_", rep(g, each = nA), fsep = "")
  
  females <-
    c(sapply(1:nA, function(i) { file.path("F", i, "_", rep(g, each = nG), "_", g, fsep = "") }))
  
  males <- file.path("M", 1:nA, "_", rep(g, each = nA), fsep = "")
  
  # indices of states
  ix <- list()
  ix$egg <-
    matrix(
      seq_along(eggs),
      nrow = nE,
      byrow = FALSE,
      dimnames = list(1:nE, g)
    )
  ix$larvae <-
    matrix(
      data = seq_along(larvae) + nG * nE,
      nrow = nL,
      byrow = FALSE,
      dimnames = list(1:nL, g)
    )
  ix$pupae <-
    matrix(
      data = seq_along(pupae) + nG * (nE + nL),
      nrow = nP,
      byrow = FALSE,
      dimnames = list(1:nP, g)
    )
  ix$females_unmated <-
    matrix(
      data = seq_along(females_unmated) + nG * (nE + nL + nP),
      nrow = nA,
      byrow = F,
      dimnames = list(1:nA, g)
    )
  ix$females <- lapply(1:nA, function(i) {
    aperm(
      a = array(
        data = seq_along(females) + nG * (nE + nL + nP + nA) + ((i - 1) * nG * nG),
        dim = c(nG, nG, 1),
        dimnames = list(g, g, i)
      ),
      perm = c(2, 1, 3),
      resize = TRUE
    )
  })
  ix$males <-
    matrix(
      data = seq_along(males) + nG * (nE + nL + nP + nA) + (nA * nG * nG),
      nrow = nA,
      byrow = F,
      dimnames = list(1:nA, g)
    )
  
  # places (u)
  u <- c(eggs, larvae, pupae, females_unmated, females, males)
  
  # return list of places
  #  make ix a list to match network version
  return(list("ix" = list(ix),
              "u" = u))
  
}