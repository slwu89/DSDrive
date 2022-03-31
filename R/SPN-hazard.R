################################################################################
#
#   SPN: make the hazard functions for a single node
#   Marshall Lab
#   Sean L. Wu (slwu89@berkeley.edu)
#   June 2019
#
################################################################################

# even these are "only" for single node SPN we can use them for metapop SPN; once v and u are setup, these are generic!
# we can use them because the patch ID is just appened to the end of the place names
# so it wont mess up the subsetting we rely upon here; these functions don't need
# to know about the metapopulation.


################################################################################
# OVIPOSITION
################################################################################

# make an oviposition hazard function
# the functions these make need to be stored in the same order as v
# so we put the transition at the place in v[t], where t comes from oviposit[i,j,k]
# i: row index into oviposit transition array
# j: col index into oviposit transition array
# k: slice index into oviposit transition array
# exact: check enabling (if not, always calc hazard and truncate to zero at small values)

#' @noRd
make_oviposit_haz <- function(t,u,cube,par,exact = TRUE,tol = 1e-8){

  # which places have input arcs to this transition
  s <- t$s

  # weights of those arcs
  w <- t$s_w

  # get the genotype-specific bits for this transition
  input <- strsplit(u[s],"_")[[1]]
  output <- strsplit(u[t$o[2]],"_")[[1]]

  f_gen <- input[2]
  m_gen <- input[3]
  o_gen <- output[2]

  # offspring genotype probability
  o_prob <- cube$ih[f_gen,m_gen,o_gen]

  # egg laying rate
  beta <- make_beta(c = par$c_beta, mu = par$mu_beta, sigma = par$sigma_beta, s = cube$s[f_gen])

  # return the hazard function
  if(exact){

    # EXACT hazards (check enabling degree: for discrete simulation only)
    return(
      function(t,M){
        # check the transition is enabled to fire
        if(w <= M[s]){
          temp <- par$temp(t)
          return(o_prob * beta(temp) * M[s])
        } else {
          return(0)
        }
      }
    )

  } else {

    # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
    return(
      function(t,M){
        temp <- par$temp(t)
        haz <- o_prob * beta(temp) * M[s]
        if(haz < tol){
          return(0)
        } else {
          return(haz)
        }
      }
    )

  }
  # end of function
}


################################################################################
# EGG TRANSITIONS
################################################################################

#' @noRd
make_egg_mort_haz <- function(t,u,cube,par,exact = TRUE,tol = 1e-8){

  # which places have input arcs to this transition
  s <- t$s

  # weights of those arcs
  w <- t$s_w

  # mortality rate
  delta <- ff_g(c = par$c_delta, mu = par$mu_delta, sigma = par$sigma_delta)

  # return the hazard function
  if(exact){

    # EXACT hazards (check enabling degree: for discrete simulation only)
    return(
      function(t,M){
        if(w <= M[s]){
          temp <- par$temp(t)
          return(delta(temp) * M[s])
        } else {
          return(0)
        }
      }
    )

  } else {

    # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
    return(
      function(t,M){
        temp <- par$temp(t)
        haz <- delta(temp) * M[s]
        if(haz < tol){
          return(0)
        } else {
          return(haz)
        }
      }
    )

  }
  # end of function
}

# make egg advancement function
make_egg_adv_haz <- function(t,u,cube,par,exact = TRUE,tol = 1e-8){

  # if these are time-varying, chuck them into the returned function
  # nE is not allowed to vary with time
  omega_E <- ff_g(c = par$c_E, mu = par$mu_E, sigma = par$sigma_E)
  nE <- par$nE

  # which places have input arcs to this transition
  s <- t$s

  # weights of those arcs
  w <- t$s_w

  # return the hazard function
  if(exact){

    # EXACT hazards (check enabling degree: for discrete simulation only)
    return(
      function(t,M){
        if(w <= M[s]){
          temp <- par$temp(t)
          return((1/omega_E(temp))*nE*M[s])
        } else {
          return(0)
        }
      }
    )

  } else {

    # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
    return(
      function(t,M){
        temp <- par$temp(t)
        haz <- (1/omega_E(temp))*nE*M[s]
        if(haz < tol){
          return(0)
        } else {
          return(haz)
        }
      }
    )

  }
  # end of function
}


################################################################################
# LARVAE TRANSITIONS
################################################################################

# this one is ~special~ it needs the places of the larvae it uses to compute the hazard.
make_larvae_mort_haz <- function(t,u,l_ix,cube,par,exact = TRUE,tol = 1e-8){

  # rate constants
  K <- par$K

  # mortality rate
  delta <- ff_g(c = par$c_delta, mu = par$mu_delta, sigma = par$sigma_delta)

  # which places have input arcs to this transition
  s <- t$s

  # weights of those arcs
  w <- t$s_w

  # assign here so that each newly generated closure has the right indices
  l_ix <- l_ix

  # return the hazard function
  if(exact){

    # EXACT hazards (check enabling degree: for discrete simulation only)
    return(
      function(t,M){
        if(w <= M[s]){
          temp <- par$temp(t)
          L <- sum(M[l_ix])
          return(delta(temp)*(1 + (L/K))*M[s])
        } else {
          return(0)
        }
      }
    )

  } else {

    # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
    return(
      function(t,M){
        temp <- par$temp(t)
        L <- sum(M[l_ix])
        haz <- delta(temp)*(1 + (L/K))*M[s]
        if(haz < tol){
          return(0)
        } else {
          return(haz)
        }
      }
    )

  }
  # end of function
}

# larval advancement
make_larvae_adv_haz <- function(t,u,cube,par,exact = TRUE,tol = 1e-8){

  # if these are time-varying, chuck them into the returned function
  # nE is not allowed to vary with time
  omega_L <- ff_g(c = par$c_L, mu = par$mu_L, sigma = par$sigma_L)
  nL <- par$nL

  # which places have input arcs to this transition
  s <- t$s

  # weights of those arcs
  w <- t$s_w

  # return the hazard function
  if(exact){

    # EXACT hazards (check enabling degree: for discrete simulation only)
    return(
      function(t,M){
        if(w <= M[s]){
          temp <- par$temp(t)
          return((1/omega_L(temp))*nL*M[s])
        } else {
          return(0)
        }
      }
    )

  } else {

    # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
    return(
      function(t,M){
        temp <- par$temp(t)
        haz <- (1/omega_L(temp))*nL*M[s]
        if(haz < tol){
          return(0)
        } else {
          return(haz)
        }
      }
    )

  }
  # end of function
}


################################################################################
# PUPAE TRANSITIONS
################################################################################

make_pupae_mort_haz <- function(t,u,cube,par,exact = TRUE,tol = 1e-8){

  # which places have input arcs to this transition
  s <- t$s

  # weights of those arcs
  w <- t$s_w

  # mortality rate
  delta <- ff_g(c = par$c_delta, mu = par$mu_delta, sigma = par$sigma_delta)

  # return the hazard function
  if(exact){

    # EXACT hazards (check enabling degree: for discrete simulation only)
    return(
      function(t,M){
        if(w <= M[s]){
          temp <- par$temp(t)
          return(delta(temp)*M[s])
        } else {
          return(0)
        }
      }
    )

  } else {

    # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
    return(
      function(t,M){
        temp <- par$temp(t)
        haz <- delta(temp)*M[s]
        if(haz < tol){
          return(0)
        } else {
          return(haz)
        }
      }
    )

  }
  # end of function
}

# pupae advancement
make_pupae_adv_haz <- function(t,u,cube,par,exact = TRUE,tol = 1e-8){

  # if these are time-varying, chuck them into the returned function
  # nE is not allowed to vary with time
  omega_P <- ff_g(c = par$c_P, mu = par$mu_P, sigma = par$sigma_P)
  nP <- par$nP

  # which places have input arcs to this transition
  s <- t$s

  # weights of those arcs
  w <- t$s_w

  # return the hazard function
  if(exact){

    # EXACT hazards (check enabling degree: for discrete simulation only)
    return(
      function(t,M){
        if(w <= M[s]){
          temp <- par$temp(t)
          return((1/omega_P(temp))*nP*M[s])
        } else {
          return(0)
        }
      }
    )

  } else {

    # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
    return(
      function(t,M){
        temp <- par$temp(t)
        haz <- (1/omega_P(temp))*nP*M[s]
        if(haz < tol){
          return(0)
        } else {
          return(haz)
        }
      }
    )

  }
  # end of function
}


################################################################################
# EMERGENCE TRANSITIONS
################################################################################

# pupae emerge to male
make_pupae_2male_haz <- function(t,u,cube,par,exact = TRUE,tol = 1e-8){

  # if these are time-varying, chuck them into the returned function
  # nE is not allowed to vary with time
  omega_P <- ff_g(c = par$c_P, mu = par$mu_P, sigma = par$sigma_P)
  nP <- par$nP

  # which places have input arcs to this transition
  s <- t$s

  # weights of those arcs
  w <- t$s_w

  # phi is dependent on genotype
  p_gen <- strsplit(u[s],"_")[[1]][2]
  phi <- cube$phi[p_gen]

  # xiM is also dependent on genotype
  xi <- cube$xiM[p_gen]

  # return the hazard function
  if(exact){

    # EXACT hazards (check enabling degree: for discrete simulation only)
    return(
      function(t,M){
        if(w <= M[s]){
          temp <- par$temp(t)
          return((1/omega_P(temp))*nP*(1 - phi)*xi*M[s])
        } else {
          return(0)
        }
      }
    )

  } else {

    # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
    return(
      function(t,M){
        temp <- par$temp(t)
        haz <- (1/omega_P(temp))*nP*(1 - phi)*xi*M[s]
        if(haz < tol){
          return(0)
        } else {
          return(haz)
        }
      }
    )

  }
  # end of function
}

# pupae emerge to female
make_pupae_2female_haz <- function(t,u,m_ix,cube,par,exact = TRUE,tol = 1e-8){

  # assign here so that each newly generated closure has the right indices
  m_ix <- m_ix

  # if these are time-varying, chuck them into the returned function
  # nE is not allowed to vary with time
  omega_P <- ff_g(c = par$c_P, mu = par$mu_P, sigma = par$sigma_P)
  nP <- par$nP

  # which places have input arcs to this transition
  s <- t$s

  # weights of those arcs
  w <- t$s_w

  # mating "weights"
  eta <- cube$eta

  # xiF is also dependent on genotype
  xi <- cube$xiF[p_gen]

  # phi is dependent on genotype
  p_gen <- strsplit(u[s[1]],"_")[[1]][2]
  phi <- cube$phi[p_gen]

  # need to know the index of the male genotype
  m_gen <- strsplit(u[s[2]],"_")[[1]][2]
  j <- which(cube$genotypesID == m_gen)

  # safety check
  if(check_double(phi) | check_double(eta)){
    stop("phi or eta missing from cube list; called from 'make_pupae_2female_haz'")
  }

  # return the hazard function
  if(exact){

    # EXACT hazards (check enabling degree: for discrete simulation only)
    return(
      function(t,M){
        if(all(w <= M[s])){
          temp <- par$temp(t)
          # mating propensity
          mate_p <- M[m_ix] * eta
          mate_p <- mate_p / sum(mate_p)
          return((1/omega_P(temp))*nP*phi*xi*mate_p[j]*M[s[1]])
        } else {
          return(0)
        }
      }
    )

  } else {

    # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
    return(
      function(t,M){
        temp <- par$temp(t)
        # mating propensity
        mate_p <- M[m_ix] * eta
        mate_p <- mate_p / sum(mate_p)
        haz <- (1/omega_P(temp))*nP*phi*xi*mate_p[j]*M[s[1]]
        if(haz < tol){
          return(0)
        } else {
          return(haz)
        }
      }
    )

  }
  # end of function
}


# pupae emerge to unmated female
make_pupae_2unmated_haz <- function(t,u,m_ix,cube,par,exact = TRUE,tol = 1e-8){

  # assign here so that each newly generated closure has the right indices
  m_ix <- m_ix

  # if these are time-varying, chuck them into the returned function
  # nE is not allowed to vary with time
  omega_P <- ff_g(c = par$c_P, mu = par$mu_P, sigma = par$sigma_P)
  nP <- par$nP

  # which places have input arcs to this transition
  s <- t$s

  # weights of those arcs
  w <- t$s_w

  # phi is dependent on genotype
  p_gen <- strsplit(x = u[s], split = "_", fixed = TRUE)[[1]][2]
  phi <- cube$phi[p_gen]

  # xiF is also dependent on genotype
  xi <- cube$xiF[p_gen]

  # return the hazard function
  if(exact){

    # EXACT hazards (check enabling degree: for discrete simulation only)
    return(
      function(t,M){
        if((sum(M[m_ix]) == 0) & (w <= M[s])){
          temp <- par$temp(t)
          return((1/omega_P(temp))*nP*phi*xi*M[s])
        } else {
          return(0)
        }
      }
    )

  } else {

    # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
    return(
      function(t,M){
        if(sum(M[m_ix]) > tol){
          return(0)
        }
        temp <- par$temp(t)
        haz <- (1/omega_P(temp))*nP*phi*xi*M[s]
        if(haz < tol){
          return(0)
        } else {
          return(haz)
        }

      }
    )

  }
  # end of function
}


################################################################################
# MALE TRANSITIONS
################################################################################

make_male_mort_haz <- function(t,u,cube,par,exact = TRUE,tol = 1e-8){

  # rate constants
  omega_A <- ff_f(c = par$c_A, mu = par$mu_A, sigma = par$sigma_A)

  # which places have input arcs to this transition
  s <- t$s

  # weights of those arcs
  w <- t$s_w

  # omega is dependent on genotype
  m_gen <- strsplit(u[s],"_")[[1]][2]
  omega <- cube$omega[m_gen]

  # return the hazard function
  if(exact){

    # EXACT hazards (check enabling degree: for discrete simulation only)
    return(
      function(t,M){
        if(w <= M[s]){
          temp <- par$temp(t)
          return((1/omega_A(temp)) * omega * M[s])
        } else {
          return(0)
        }
      }
    )

  } else {

    # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
    return(
      function(t,M){
        temp <- par$temp(t)
        haz <- (1/omega_A(temp)) * omega * M[s]
        if(haz < tol){
          return(0)
        } else {
          return(haz)
        }
      }
    )

  }
  # end of function
}


################################################################################
# FEMALE TRANSITIONS
################################################################################

make_female_mort_haz <- function(t,u,cube,par,exact = TRUE,tol = 1e-8){

  # rate constants
  omega_A <- ff_f(c = par$c_A, mu = par$mu_A, sigma = par$sigma_A)

  # which places have input arcs to this transition
  s <- t$s

  # weights of those arcs
  w <- t$s_w

  # omega is dependent on genotype
  f_gen <- strsplit(u[s],"_")[[1]][2]
  omega <- cube$omega[f_gen]

  # return the hazard function
  if(exact){

    # EXACT hazards (check enabling degree: for discrete simulation only)
    return(
      function(t,M){
        if(w <= M[s]){
          temp <- par$temp(t)
          return((1/omega_A(temp)) * omega * M[s])
        } else {
          return(0)
        }
      }
    )

  } else {

    # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
    return(
      function(t,M){
        temp <- par$temp(t)
        haz <- (1/omega_A(temp)) * omega * M[s]
        if(haz < tol){
          return(0)
        } else {
          return(haz)
        }
      }
    )

  }
  # end of function
}


################################################################################
# UNMATED FEMALE TRANSITIONS
################################################################################

make_unmated_2female_haz <- function(t,u,m_ix,cube,par,exact = TRUE,tol = 1e-8){

  # assign here so that each newly generated closure has the right indices
  m_ix <- m_ix

  nu <- par$nu

  # which places have input arcs to this transition
  s <- t$s

  # weights of those arcs
  w <- t$s_w

  # mating "weights"
  f_gen <- strsplit(x = u[s[1]], split = "_", fixed = TRUE)[[1]][2]
  eta <- cube$eta[f_gen,]

  # need to know the index of the male genotype
  m_gen <- strsplit(x = u[s[2]], split = "_", fixed = TRUE)[[1]][2]
  j <- match(x = m_gen, table = cube$genotypesID)

  # return the hazard function
  if(exact){

    # EXACT hazards (check enabling degree: for discrete simulation only)
    return(
      function(t,M){
        if(all(w <= M[s])){
          # mating propensity
          mate_p <- M[m_ix] * eta
          mate_p <- mate_p / sum(mate_p)
          return(nu*mate_p[j]*M[s[1]])
        } else {
          return(0)
        }
      }
    )

  } else {

    # APPROXIMATE hazards (tolerance around zero; for continuous approximation only)
    return(
      function(t,M){
        if(sum(M[m_ix]) < tol){
          return(0)
        }
        # mating propensity
        mate_p <- M[m_ix] * eta
        mate_p <- mate_p / sum(mate_p)
        haz <- nu*mate_p[j]*M[s[1]]
        if(haz < tol){
          return(0)
        } else {
          return(haz)
        }
      }
    )

  }
  # end of function
}



################################################################################
# MAKE THE SPN HAZARDS (LAMBDA)
################################################################################

#' @importFrom utils setTxtProgressBar txtProgressBar
spn_hazards <- function(spn_P,spn_T,cube,par,exact=TRUE,tol=1e-12,pbar=TRUE){

  if(tol > 1e-6 & !exact){
    cat("warning: hazard function tolerance ",tol," is large; consider tolerance < 1e-6 for sufficient accuracy\n")
  }

  # transitions and places
  v <- spn_T$v
  u <- spn_P$u

  n <- length(v)
  if(pbar){
    pb <- txtProgressBar(min = 1,max = n,style = 3)
    pp <- 1
  }

  # the hazard functions
  h <- vector("list",n)
  h <- setNames(h,v)

  # get male and larvae indices
  l_ix <- as.vector(spn_P$ix$larvae)
  m_ix <- spn_P$ix$males

  cat(" --- generating hazard functions for SPN --- \n")

  # make the hazards
  for(t in 1:n){

    type <- spn_T$T[[t]]$class

    # make the correct type of hazard
    if(type == "oviposit"){
      h[[t]] <- make_oviposit_haz(t = spn_T$T[[t]],u = u,cube = cube,par = par,exact = exact,tol = tol)
    } else if(type == "egg_adv"){
      h[[t]] <- make_egg_adv_haz(t = spn_T$T[[t]],u = u,cube = cube,par = par,exact = exact,tol = tol)
    } else if(type == "egg_mort"){
      h[[t]] <- make_egg_mort_haz(t = spn_T$T[[t]],u = u,cube = cube,par = par,exact = exact,tol = tol)
    } else if(type == "larvae_adv"){
      h[[t]] <- make_larvae_adv_haz(t = spn_T$T[[t]],u = u,cube = cube,par = par,exact = exact,tol = tol)
    } else if(type == "larvae_mort"){
      h[[t]] <- make_larvae_mort_haz(t = spn_T$T[[t]],u = u,l_ix = l_ix,cube = cube,par = par,exact = exact,tol = tol)
    } else if(type == "pupae_adv"){
      h[[t]] <- make_pupae_adv_haz(t = spn_T$T[[t]],u = u,cube = cube,par = par,exact = exact,tol = tol)
    } else if(type == "pupae_mort"){
      h[[t]] <- make_pupae_mort_haz(t = spn_T$T[[t]],u = u,cube = cube,par = par,exact = exact,tol = tol)
    } else if(type == "pupae_2male"){
      h[[t]] <- make_pupae_2male_haz(t = spn_T$T[[t]],u = u,cube = cube,par = par,exact = exact,tol = tol)
    } else if(type == "pupae_2female"){
      h[[t]] <- make_pupae_2female_haz(t = spn_T$T[[t]],u = u,m_ix = m_ix,cube = cube,par = par,exact = exact,tol = tol)
    } else if(type == "pupae_2unmated"){
      h[[t]] <- make_pupae_2unmated_haz(t = spn_T$T[[t]],u = u,m_ix = m_ix,cube = cube,par = par,exact = exact,tol = tol)
    } else if(type == "male_mort"){
      h[[t]] <- make_male_mort_haz(t = spn_T$T[[t]],u = u,cube = cube,par = par,exact = exact,tol = tol)
    } else if(type == "female_mort" | type == "unmated_mort"){
      h[[t]] <- make_female_mort_haz(t = spn_T$T[[t]],u = u,cube = cube,par = par,exact = exact,tol = tol)
    } else if(type == "unmated_mate"){
      h[[t]] <- make_unmated_2female_haz(t = spn_T$T[[t]],u = u,m_ix = m_ix,cube = cube,par = par,exact = exact,tol = tol)
    } else {
      stop(paste0("error in making hazard function for unknown class type: ",type))
    }

    if(pbar){setTxtProgressBar(pb,t)}
  }

  if(pbar){close(pb)}

  cat(" --- done generating hazard functions for SPN --- \n")

  return(h)
}
