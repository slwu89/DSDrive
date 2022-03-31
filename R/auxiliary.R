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
