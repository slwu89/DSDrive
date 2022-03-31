#' @title Make d. suzukii life history parameters
#' @param c_beta max daily fecundity
#' @param mu_beta optimal temperature for fecundity
#' @param sigma_beta temperature tolerance for fecundity
#' @param c_E min duration of egg stage
#' @param c_L min duration of larvae stage
#' @param c_P min duration of pupae stage
#' @param mu_E optimal temperature for egg stage
#' @param mu_L optimal temperature for larvae stage
#' @param mu_P optimal temperature for pupae stage
#' @param sigma_E temperature tolerance for egg stage
#' @param sigma_L temperature tolerance for larvae stage
#' @param sigma_P temperature tolerance for pupae stage
#' @param k_E number of egg substages
#' @param k_L number of larvae substages
#' @param k_P number of pupae substages
#' @param c_A max duration of adult stage
#' @param mu_A optimal temperature of adult stage
#' @param sigma_A temperature tolerance of adult stage
#' @param k_A number of adult substages (not used)
#' @param c_delta min juvenile mortality
#' @param mu_delta optimal temperature for juvenile mortality
#' @param sigma_delta temperature tolerance for juvenile mortality
#' @export
make_lifecycle_parameters <- function(
  c_beta = 1.6,
  mu_beta = 21.9,
  sigma_beta = 6.1,
  c_E = 1.3,
  c_L = 5.1,
  c_P = 4.6,
  mu_E = 25.3,
  mu_L = 25.3,
  mu_P = 25.3,
  sigma_E = 11.1,
  sigma_L = 11.1,
  sigma_P = 11.1,
  k_E = 15,
  k_L = 25,
  k_P = 25,
  c_A = 22.0,
  mu_A = 5.0,
  sigma_A = 25.9,
  k_A = 4,
  c_delta = 0.014,
  mu_delta = 17.7,
  sigma_delta = 7.8
) {
  list(
    c_beta = c_beta,
    mu_beta = mu_beta,
    sigma_beta = sigma_beta,
    c_E = c_E,
    c_L = c_L,
    c_P = c_P,
    mu_E = mu_E,
    mu_L = mu_L,
    mu_P = mu_P,
    sigma_E = sigma_E,
    sigma_L = sigma_L,
    sigma_P = sigma_P,
    k_E = k_E,
    k_L = k_L,
    k_P = k_P,
    c_A = c_A,
    mu_A = mu_A,
    sigma_A = sigma_A,
    k_A = k_A,
    c_delta = c_delta,
    mu_delta = mu_delta,
    sigma_delta = sigma_delta
  )
}
