################################################################################
#
#   SPN: set up the SPN for a single node
#   Marshall Lab
#   Sean L. Wu (slwu89@berkeley.edu)
#   June 2019
#
################################################################################


################################################################################
# make the places (P) of the SPN
################################################################################

#' @title Build the set of places of the SPN
#' @param parameters lifecycle parameters (named [list])
#' @param cube an inheritance cube (named [list])
#' @export
spn_P <- function(parameters, cube){

  nE <- parameters$k_E
  nL <- parameters$k_L
  nP <- parameters$k_P

  # genetic information
  nG <- cube$genotypesN
  g <- cube$genotypesID

  # setup places
  eggs <- paste0("E",as.vector(outer(1:nE,g,function(x,y){
    paste0(x,"_",y)
  })))

  larvae <- paste0("L",as.vector(outer(1:nL,g,function(x,y){
    paste0(x,"_",y)
  })))

  pupae <- paste0("P",as.vector(outer(1:nP,g,function(x,y){
    paste0(x,"_",y)
  })))

  females_unmated <- paste0("U_",g)

  females <- paste0("F_",as.vector(t(outer(g,g,function(x,y){
    paste0(x,"_",y)
  }))))

  males <- paste0("M_",g)

  # indices of states
  ix <- list()
  ix$egg <- matrix(data = seq_along(eggs), nrow = nE, ncol = nG, byrow = FALSE, dimnames = list(1:nE, g))
  ix$larvae <- matrix(seq(from = max(ix$egg) + 1L, length.out = length(larvae)), nrow = nL, ncol = nG, byrow = FALSE, dimnames = list(1:nL, g))
  ix$pupae <- matrix(data = seq(from = max(ix$larvae) + 1L, length.out = length(pupae)), nrow = nP, ncol = nG, byrow = FALSE, dimnames = list(1:nP, g))
  ix$females_unmated <- setNames(object = seq(from = max(ix$pupae) + 1L, length.out  = length(females_unmated)), nm = g)
  ix$females <- matrix(seq(from = max(ix$females_unmated) + 1L,length.out = length(females)), nrow = nG, ncol= nG, byrow = TRUE,dimnames = list(g, g))
  ix$males <- setNames(object = seq(from = max(ix$females) + 1L,length.out = length(males)), nm = g)

  # places (u)
  u <- c(eggs,larvae,pupae,females_unmated,females,males)

  return(list(ix = ix,u = u))
}


