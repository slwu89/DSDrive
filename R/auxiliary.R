# something that will throw errors if you pass it anything except a well defined float
#' @noRd
check_double <- function(dbl){
  return(is.null(dbl) || is.na(dbl) || is.nan(dbl) || !length(dbl))
}

#' @noRd
fequal <- function(x,y,tol=sqrt(.Machine$double.eps)){
  abs(x-y) <= tol
}

# functional forms for temp-dependence from reference

#' @title functional form f
#' @description Return a function that takes a single argument `w` (current temperature).
#' @param c maximum value of function
#' @param mu optimal temperature
#' @param sigma temperature tolerance
ff_f <- function(c, mu, sigma) {
  return(
    function(w) {
      c * exp(-((mu - w) / sigma)^2)
    }
  )
}

#' @title functional form g
#' @description Return a function that takes a single argument `w` (current temperature).
#' @param c minimum value of function
#' @param mu optimal temperature
#' @param sigma temperature tolerance
ff_g <- function(c, mu, sigma) {
  return(
    function(w) {
      c * exp(((mu - w) / sigma)^2)
    }
  )
}

#' @title female fecundity rate
#' @description This uses functional form `f` but also the genotype specific multiplier `s`
#' @param c maximum value of function
#' @param mu optimal temperature
#' @param sigma temperature tolerance
#' @param s genotype specific multiplier of fecundity
make_beta <- function(c, mu, sigma, s) {
  return(
    function(w) {
      s * c * exp(-((mu - w) / sigma)^2)
    }
  )
}

#' @title Return females from simulation
#' @description Collapse (sum) over male mate genotyp
#' @param out an [array]
#' @param spn_P set of places
#' @export
suzukii_summarize_females <- function(out,spn_P){

  # get constants for later
  gNames <- dimnames(spn_P$ix$females)[[1]]
  numGeno <- dim(spn_P$ix$females)[1]
  numRep <- dim(out)[3]

  # setup data holder
  fArray <- array(data = 0, dim = c(nrow(out),numGeno,numRep))

  # loop over reps, nodes/genotypes, collapse females by mate
  for(r in 1:numRep){
    for(gen in 1:numGeno){
      fArray[ ,gen,r] <- rowSums(out[,spn_P$ix$females[gen, ]+1,r])
    }
  }

  # setup return df
  retDF <- expand.grid("time" = out[ ,"time",1], "genotype" = gNames, "rep" = 1:numRep)

  # fill count data
  retDF$value <- as.vector(fArray)

  # if 1 rep, remove repetition designation
  if(numRep == 1){retDF$rep <- NULL}

  # return data frame
  return(retDF)
}

