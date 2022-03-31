################################################################################
# helper functions that make the t in T (each transition in the full set)
# NOTE:
# these don't make the hazards yet; we have to make {P,T,Pre,Post}
# and then construct the hazards last before we are ready to simulate
# these encode the full structural information, {Pre,Post} are for easy
# analysis and computation.
#
#
#
################################################################################


################################################################################
# OVIPOSITION
################################################################################

#' @noRd
make_transition_ovi <- function(T_index,u,f_gen,m_gen,o_gen){

  # tokens required
  ftoken <- paste0("F_",f_gen,"_",m_gen)

  # tokens produced
  etoken <- paste0("E1_",o_gen)

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(ftoken,"->",etoken) # name of this t (corresponds to v)

  # requires a female token
  t$s <- which(u == ftoken)
  t$s_w <- 1

  # outputs a female token and an egg token
  t$o <- c(t$s,which(u == etoken))
  t$o_w <- c(1,1)

  # class of the transition
  t$class <- "oviposit"

  # return the transition
  return(t)
}

################################################################################
# EGGS
################################################################################

# transition event for egg advancement
# a NULL stage2 means they advance to larvae

#' @noRd
make_transition_egg_adv <- function(T_index,u,e_gen,stage1,stage2=NULL){

  # tokens required
  etoken <- paste0("E",stage1,"_",e_gen)

  # tokens produced (otoken = output token, maybe nomenclature could be better)
  if(is.null(stage2)){
    otoken <- paste0("L1_",e_gen)
  } else {
    otoken <- paste0("E",stage2,"_",e_gen)
  }

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(etoken,"->",otoken) # name of this t (corresponds to v)

  # requires a egg token
  t$s <- which(u == etoken)
  t$s_w <- 1

  # outputs a new egg token of the next stage; or a larvae of stage 1
  t$o <- which(u == otoken)
  t$o_w <- 1

  # class of the transition
  t$class <- "egg_adv"

  # return the transition
  return(t)
}

# death event for eggs

#' @noRd
make_transition_egg_mort <- function(T_index,u,e_gen,stage){

  # tokens required
  etoken <- paste0("E",stage,"_",e_gen)

  # no tokens are produced

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(etoken,"->D") # name of this t (corresponds to v)

  # requires a egg token
  t$s <- which(u == etoken)
  t$s_w <- 1

  # no tokens produced
  t$o <- NaN
  t$o_w <- NaN

  # class of the transition
  t$class <- "egg_mort"

  # return the transition
  return(t)
}

################################################################################
# LARVAE
################################################################################

# larvae advancement
# NULL stage2 indicates advancement to pupae 1

#' @noRd
make_transition_larvae_adv <- function(T_index,u,l_gen,stage1,stage2=NULL){

  # tokens required
  ltoken <- paste0("L",stage1,"_",l_gen)

  # tokens produced (otoken = output token, maybe nomenclature could be better)
  if(is.null(stage2)){
    otoken <- paste0("P1_",l_gen)
  } else {
    otoken <- paste0("L",stage2,"_",l_gen)
  }

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(ltoken,"->",otoken) # name of this t (corresponds to v)

  # requires a egg token
  t$s <- which(u == ltoken)
  t$s_w <- 1

  # outputs a new egg token of the next stage; or a larvae of stage 1
  t$o <- which(u == otoken)
  t$o_w <- 1

  # class of the transition
  t$class <- "larvae_adv"

  # return the transition
  return(t)
}

# death event for larvae

#' @noRd
make_transition_larvae_mort <- function(T_index,u,l_gen,stage){

  # tokens required
  ltoken <- paste0("L",stage,"_",l_gen)

  # no tokens are produced

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(ltoken,"->D") # name of this t (corresponds to v)

  # requires a larvae token
  t$s <- which(u == ltoken)
  t$s_w <- 1

  # no tokens produced
  t$o <- NaN
  t$o_w <- NaN

  # class of the transition
  t$class <- "larvae_mort"

  # return the transition
  return(t)
}


################################################################################
# PUPAE
################################################################################

# inter-pupal stages transitions (handle emergence "delicately" ... )
# so only from 1,...,nP-1

#' @noRd
make_transition_pupae_adv <- function(T_index,u,p_gen,stage1,stage2){

  # tokens required
  ptoken <- paste0("P",stage1,"_",p_gen)

  # tokens produced (otoken = output token, maybe nomenclature could be better)
  otoken <- paste0("P",stage2,"_",p_gen)

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(ptoken,"->",otoken) # name of this t (corresponds to v)

  # requires a egg token
  t$s <- which(u == ptoken)
  t$s_w <- 1

  # outputs a new egg token of the next stage; or a pupae of stage 1
  t$o <- which(u == otoken)
  t$o_w <- 1

  # class of the transition
  t$class <- "pupae_adv"

  # return the transition
  return(t)
}

# death event for pupae

#' @noRd
make_transition_pupae_mort <- function(T_index,u,p_gen,stage){

  # tokens required
  ptoken <- paste0("P",stage,"_",p_gen)

  # no tokens are produced

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(ptoken,"->D") # name of this t (corresponds to v)

  # requires a pupae token
  t$s <- which(u == ptoken)
  t$s_w <- 1

  # no tokens produced
  t$o <- NaN
  t$o_w <- NaN

  # class of the transition
  t$class <- "pupae_mort"

  # return the transition
  return(t)
}

# emergence to males event for pupae

#' @noRd
make_transition_pupae_emerge_m <- function(T_index,u,p_gen,nP){

  # tokens required
  ptoken <- paste0("P",nP,"_",p_gen)

  # produces a male token
  mtoken <- paste0("M_",p_gen)

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(ptoken,"->",mtoken) # name of this t (corresponds to v)

  # requires a pupae token
  t$s <- which(u == ptoken)
  t$s_w <- 1

  # male token produced
  t$o <- which(u == mtoken)
  t$o_w <- 1

  # class of the transition
  t$class <- "pupae_2m"

  # return the transition
  return(t)
}

# emergence to females event for pupae
# p_gen: genotype of the pupae
# m_gen: genotype of the male mate
# nP: number of pupae compartments

#' @noRd
make_transition_pupae_emerge_f <- function(T_index,u,p_gen,m_gen,nP){

  # tokens required
  ptoken <- paste0("P",nP,"_",p_gen)
  mtoken <- paste0("M_",m_gen)

  # produces a female token (with her genotype + the genotype of her mate)
  # also return the male token.
  ftoken <- paste0("F_",p_gen,"_",m_gen)

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(ptoken,"->",ftoken) # name of this t (corresponds to v)

  # requires a pupae token and a male token
  t$s <- c(which(u == ptoken),which(u == mtoken))
  t$s_w <- c(1,1)

  # produces a female and a male token
  t$o <- c(which(u == ftoken),which(u == mtoken))
  t$o_w <- c(1,1)

  # class of the transition
  t$class <- "pupae_2f"

  # return the transition
  return(t)
}

#' @noRd
make_transition_pupae_emerge_unmated <- function(T_index,u,p_gen,nP){

  # tokens required
  input_token <- paste0("P",nP,"_",p_gen)

  # produces a female token (with her genotype + the genotype of her mate)
  # also return the male token.
  output_token <- paste0("U_", p_gen)

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(input_token,"->",output_token) # name of this t (corresponds to v)

  # requires a pupae token and a male token
  t$s <- match(x = input_token,table = u)
  t$s_w <- 1L

  # produces a female and a male token
  t$o <- match(x = output_token,table = u)
  t$o_w <- 1L

  # class of the transition
  t$class <- "pupae_2unmated"

  # return the transition
  return(t)
}


################################################################################
# MALES
################################################################################

# kill a male token

#' @noRd
make_transition_male_mort <- function(T_index,u,m_gen){

  # tokens required
  mtoken <- paste0("M_",m_gen)

  # no tokens are produced

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(mtoken,"->D") # name of this t (corresponds to v)

  # requires a male token
  t$s <- which(u == mtoken)
  t$s_w <- 1

  # no tokens produced
  t$o <- NaN
  t$o_w <- NaN

  # class of the transition
  t$class <- "male_mort"

  # return the transition
  return(t)
}


################################################################################
# FEMALES
################################################################################

# kill a female token

#' @noRd
make_transition_female_mort <- function(T_index,u,f_gen,m_gen){

  # tokens required
  ftoken <- paste0("F_",f_gen,"_",m_gen)

  # no tokens are produced

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(ftoken,"->D") # name of this t (corresponds to v)

  # requires a female token
  t$s <- which(u == ftoken)
  t$s_w <- 1

  # no tokens produced
  t$o <- NaN
  t$o_w <- NaN

  # class of the transition
  t$class <- "female_mort"

  # return the transition
  return(t)
}

# kill an unmated female token

#' @noRd
make_transition_female_unmated_mort <- function(T_index,u,f_gen){

  # tokens required
  ftoken <- paste0("U_",f_gen)

  # no tokens are produced

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(ftoken,"->D") # name of this t (corresponds to v)

  # requires a female token
  t$s <- match(x = ftoken,table = u)
  t$s_w <- 1

  # no tokens produced
  t$o <- NaN
  t$o_w <- NaN

  # class of the transition
  t$class <- "female_unmated_mort"

  # return the transition
  return(t)
}

# mate an unmated female token

#' @noRd
make_transition_female_unmated_mate <- function(T_index,u,f_gen,m_gen){

  # tokens required
  ftoken_u <- paste0("U_",f_gen)
  mtoken <- paste0("M_",m_gen)

  # produces a female token (with her genotype + the genotype of her mate)
  # also return the male token.
  ftoken_m <- paste0("F_",f_gen,"_",m_gen)

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(ftoken_u,"->",ftoken_m) # name of this t (corresponds to v)

  # requires a pupae token and a male token
  t$s <- match(x = c(ftoken_u,mtoken),table = u)
  t$s_w <- c(1,1)

  # produces a female and a male token
  t$o <- match(x = c(ftoken_m,mtoken),table = u)
  t$o_w <- c(1,1)

  # class of the transition
  t$class <- "female_unmated_mate"

  # return the transition
  return(t)
}



################################################################################
# make the transitions (T) of the SPN
################################################################################

#' @title Build the set of transitions of the SPN
#' @param u character vector of places
#' @param parameters lifecycle parameters (named [list])
#' @param cube an inheritance cube (named [list])
#' @export
spn_T <- function(u,parameters,cube){

  nE <- parameters$k_E
  nL <- parameters$k_L
  nP <- parameters$k_P

  # genetic states
  g <- cube$genotypesID
  nG <- cube$genotypesN

  T_index <- 1 # in c++ this is a static variable so everyone in this TU can see it; in r it doesnt matter, we just walk up the env until we find it

  # make oviposition transitions (events)

  # empty list to put the transitions in (X_tt is the set of transitions in this subset of the total T)
  ovi_tt <- vector("list",sum(cube$tau * cube$ih > 0))
  vv <- 1

  # OVIPOSITION

  # make the transitions
  ovi_dims <- dim(cube$ih)
  for(i in 1:ovi_dims[1]){
    for(j in 1:ovi_dims[2]){
      for(k in 1:ovi_dims[3]){
        # only make valid events (based on tau and ih)
        if(!fequal(cube$tau[i,j,k],0) & !fequal(cube$ih[i,j,k],0)){
          ovi_tt[[vv]] <- make_transition_ovi(T_index,u=u,f_gen=g[i],m_gen=g[j],o_gen=g[k])
          T_index <- T_index + 1
          vv <- vv + 1
        }
      }
    }
  }

  ovi_tt <- ovi_tt[sapply(ovi_tt,Negate(is.null))]

  # EGG TRANSITIONS

  # make egg mortality transitions
  egg_mort_tt <- vector("list",nE*nG)
  vv <- 1

  for(i in 1:nE){
    for(j in 1:nG){
      egg_mort_tt[[vv]] <- make_transition_egg_mort(T_index,u=u,e_gen=g[j],stage=i)
      T_index <- T_index + 1
      vv <- vv + 1
    }
  }

  # make egg advancement transitions
  egg_adv_tt <- vector("list",nE*nG)
  vv <- 1

  for(i in 1:nE){
    for(j in 1:nG){
      if(i == nE){
        egg_adv_tt[[vv]] <- make_transition_egg_adv(T_index,u=u,e_gen=g[j],stage1=i,stage2=NULL)
        T_index <- T_index + 1
        vv <- vv + 1
      } else {
        egg_adv_tt[[vv]] <- make_transition_egg_adv(T_index,u=u,e_gen=g[j],stage1=i,stage2=i+1)
        T_index <- T_index + 1
        vv <- vv + 1
      }
    }
  }

  # LARVAE TRANSITIONS

  # make larvae mortality transitions
  larvae_mort_tt <- vector("list",nL*nG)
  vv <- 1

  for(i in 1:nL){
    for(j in 1:nG){
      larvae_mort_tt[[vv]] <- make_transition_larvae_mort(T_index,u=u,l_gen=g[j],stage=i)
      T_index <- T_index + 1
      vv <- vv + 1
    }
  }

  # make larvae advancement transitions
  larvae_adv_tt <- vector("list",nL*nG)
  vv <- 1

  for(i in 1:nL){
    for(j in 1:nG){
      if(i == nL){
        larvae_adv_tt[[vv]] <- make_transition_larvae_adv(T_index,u=u,l_gen=g[j],stage1=i,stage2=NULL)
        T_index <- T_index + 1
        vv <- vv + 1
      } else {
        larvae_adv_tt[[vv]] <- make_transition_larvae_adv(T_index,u=u,l_gen=g[j],stage1=i,stage2=i+1)
        T_index <- T_index + 1
        vv <- vv + 1
      }
    }
  }

  # PUPAE TRANSITIONS

  # make pupae mortality transitions
  pupae_mort_tt <- vector("list",nP*nG)
  vv <- 1

  for(i in 1:nP){
    for(j in 1:nG){
      pupae_mort_tt[[vv]] <- make_transition_pupae_mort(T_index,u=u,p_gen=g[j],stage=i)
      T_index <- T_index + 1
      vv <- vv + 1
    }
  }

  # make pupae advancement transitions
  pupae_adv_tt <- vector("list",0)

  if (nP > 1) {
    pupae_adv_tt <- vector("list",(nP-1)*nG)
    vv <- 1

    for(i in 1:(nP-1)){
      for(j in 1:nG){
        pupae_adv_tt[[vv]] <- make_transition_pupae_adv(T_index,u=u,p_gen=g[j],stage1=i,stage2=i+1)
        T_index <- T_index + 1
        vv <- vv + 1
      }
    }
  }

  # make pupae -> male emergence
  pupae_2male_tt <- vector("list",nG)
  vv <- 1

  for(j in 1:nG){
    pupae_2male_tt[[vv]] <- make_transition_pupae_emerge_m(T_index,u=u,p_gen=g[j],nP=nP)
    T_index <- T_index + 1
    vv <- vv + 1
  }

  # make pupae -> female emergence
  pupae_2female_tt <- vector("list",nG^2)
  vv <- 1

  for(j_f in 1:nG){
    for(j_m in 1:nG){
      pupae_2female_tt[[vv]] <- make_transition_pupae_emerge_f(T_index,u=u,p_gen=g[j_f],m_gen=g[j_m],nP=nP)
      T_index <- T_index + 1
      vv <- vv + 1
    }
  }

  # make pupae -> unmated female emergence
  pupae_2unmated_tt <- vector("list",nG)
  vv <- 1

  for(j in 1:nG){
    pupae_2unmated_tt[[vv]] <- make_transition_pupae_emerge_unmated(T_index,u=u,
                                                                    p_gen=g[j],
                                                                    nP=nP)
    T_index <- T_index + 1
    vv <- vv + 1
  }

  # FEMALE TRANSITIONS

  # make female mortality
  female_mort_tt <- vector("list",nG^2)
  vv <- 1

  for(j_f in 1:nG){
    for(j_m in 1:nG){
      female_mort_tt[[vv]] <- make_transition_female_mort(T_index,u=u,f_gen=g[j_f],m_gen=g[j_m])
      T_index <- T_index + 1
      vv <- vv + 1
    }
  }

  # make unmated female mortality
  unmated_mort_tt <- vector("list",nG)
  vv <- 1

  for(j in 1:nG){
    unmated_mort_tt[[vv]] <- make_transition_female_unmated_mort(T_index,u=u,f_gen=g[j])
    T_index <- T_index + 1
    vv <- vv + 1
  }

  # make unmated female mating
  unmated_mate_tt <- vector("list",nG)
  vv <- 1

  for(j_f in 1:nG){
    for(j_m in 1:nG){
      unmated_mate_tt[[vv]] <- make_transition_female_unmated_mate(T_index,u=u,
                                                                   f_gen=g[j_f],
                                                                   m_gen=g[j_m])
      T_index <- T_index + 1
      vv <- vv + 1
    }
  }

  # MALE TRANSITIONS

  # make male mortality
  male_mort_tt <- vector("list",nG)
  vv <- 1

  for(j in 1:nG){
    male_mort_tt[[vv]] <- make_transition_male_mort(T_index,u=u,m_gen=g[j])
    T_index <- T_index + 1
    vv <- vv + 1
  }

  # the set of transitions
  t <- list()
  t$oviposit <- ovi_tt
  t$egg_mort <- egg_mort_tt
  t$egg_adv <- egg_adv_tt
  t$larvae_mort <- larvae_mort_tt
  t$larvae_adv <- larvae_adv_tt
  t$pupae_mort <- pupae_mort_tt
  t$pupae_adv <- pupae_adv_tt
  t$pupae_2male <- pupae_2male_tt
  t$pupae_2female <- pupae_2female_tt
  t$pupae_2unmated <- pupae_2unmated_tt
  t$female_mort <- female_mort_tt
  t$unmated_mort <- unmated_mort_tt
  t$unmated_mate <- unmated_mate_tt
  t$male_mort <- male_mort_tt

  # transitions (v)
  v <- unlist(x = lapply(X = t, FUN = lapply, '[[', 'label'), use.names = FALSE)

  # check the set for errors
  invisible(sapply(t,function(t_set){
    sapply(t_set,function(tt){
      label <- tt$label
      vix <- tt$vix
      if(label != v[vix]){
        stop(paste0("error in set of transitions T and transition vector v at transition ",label))
      }
    })
  }))

  # return the set of transitions and the vector (v)
  list("T"=unlist(x = t, recursive = FALSE, use.names = FALSE),"v"=v)
}
