# oviposition ------------------------------------------------------------

#' @title transition for oviposition
#' @noRd
make_transition_ovi <- function(T_index, u, f_gen, m_gen, o_gen){

  # tokens required
  ftoken <- paste0("F_", f_gen, "_", m_gen)

  # tokens produced
  etoken <- paste0("E1_", o_gen)

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(ftoken, "->", etoken) # name of this t (corresponds to v)

  # requires a female token
  t$s <- which(u == ftoken)
  t$s_w <- 1

  # outputs a female token and an egg token
  t$o <- c(t$s, which(u == etoken))
  t$o_w <- c(1, 1)

  # class of the transition
  t$class <- "oviposit"

  # return the transition
  return(t)
}


# eggs ------------------------------------------------------------

#' @title transition for egg advancement
#' @noRd
make_transition_egg_adv <- function(T_index, u, e_gen, stage1, stage2 = NULL){

  # tokens required
  input_token <- paste0("E", stage1, "_", e_gen)

  # tokens produced
  if (is.null(stage2)) {
    output_token <- paste0("L1_", e_gen)
  } else {
    output_token <- paste0("E", stage2, "_", e_gen)
  }

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(input_token, "->", output_token) # name of this t (corresponds to v)

  # requires a egg token
  t$s <- which(u == input_token)
  t$s_w <- 1

  # outputs a new egg token of the next stage; or a larvae of stage 1
  t$o <- which(u == output_token)
  t$o_w <- 1

  # class of the transition
  t$class <- "egg_adv"

  # return the transition
  return(t)
}


#' @title transition for egg mortality
#' @noRd
make_transition_egg_mort <- function(T_index, u, e_gen, stage){

  # tokens required
  input_token <- paste0("E", stage, "_", e_gen)

  # no tokens are produced

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(input_token,"->D") # name of this t (corresponds to v)

  # requires a egg token
  t$s <- which(u == input_token)
  t$s_w <- 1

  # no tokens produced
  t$o <- NaN
  t$o_w <- NaN

  # class of the transition
  t$class <- "egg_mort"

  # return the transition
  return(t)
}


# larvae ------------------------------------------------------------

#' @title transition for larvae advancement
#' @noRd
make_transition_larvae_adv <- function(T_index, u, l_gen, stage1, stage2 = NULL){

  # tokens required
  input_token <- paste0("L",stage1,"_",l_gen)

  # tokens produced
  if(is.null(stage2)){
    output_token <- paste0("P1_",l_gen)
  } else {
    output_token <- paste0("L",stage2,"_",l_gen)
  }

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(input_token,"->",output_token) # name of this t (corresponds to v)

  # requires a egg token
  t$s <- which(u == input_token)
  t$s_w <- 1

  # outputs a new egg token of the next stage; or a larvae of stage 1
  t$o <- which(u == output_token)
  t$o_w <- 1

  # class of the transition
  t$class <- "larvae_adv"

  # return the transition
  return(t)
}

#' @title transition for larvae mortality
#' @noRd
make_transition_larvae_mort <- function(T_index,u,l_gen,stage){

  # tokens required
  input_token <- paste0("L",stage,"_",l_gen)

  # no tokens are produced

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(input_token,"->D") # name of this t (corresponds to v)

  # requires a larvae token
  t$s <- which(u == input_token)
  t$s_w <- 1

  # no tokens produced
  t$o <- NaN
  t$o_w <- NaN

  # class of the transition
  t$class <- "larvae_mort"

  # return the transition
  return(t)
}


# pupae ------------------------------------------------------------

#' @title transition for intra-substage pupae advancement
#' @noRd
make_transition_pupae_adv <- function(T_index,u,p_gen,stage1,stage2){

  # tokens required
  input_token <- paste0("P",stage1,"_",p_gen)

  # tokens produced (otoken = output token, maybe nomenclature could be better)
  output_token <- paste0("P",stage2,"_",p_gen)

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(input_token,"->",output_token) # name of this t (corresponds to v)

  # requires a egg token
  t$s <- match(x = input_token,table = u)
  t$s_w <- 1

  # outputs a new egg token of the next stage; or a pupae of stage 1
  t$o <- match(x = output_token,table = u)
  t$o_w <- 1

  # class of the transition
  t$class <- "pupae_adv"

  # return the transition
  return(t)
}

#' @title transition for pupae mortality
#' @noRd
make_transition_pupae_mort <- function(T_index,u,p_gen,stage){

  # tokens required
  input_token <- paste0("P",stage,"_",p_gen)

  # no tokens are produced

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(input_token,"->D") # name of this t (corresponds to v)

  # requires a pupae token
  t$s <- match(x = input_token,table = u)
  t$s_w <- 1

  # no tokens produced
  t$o <- NaN
  t$o_w <- NaN

  # class of the transition
  t$class <- "pupae_mort"

  # return the transition
  return(t)
}

#' @title transition for pupae emergence to male adults
#' @noRd
make_transition_pupae_emerge_m <- function(T_index,u,p_gen,nP){

  # tokens required
  input_token <- paste0("P",nP,"_",p_gen)

  # produces a male token
  output_token <- paste0("M_",p_gen)

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(input_token,"->",output_token) # name of this t (corresponds to v)

  # requires a pupae token
  t$s <- match(x = input_token,table = u)
  t$s_w <- 1

  # male token produced
  t$o <- match(x = output_token,table = u)
  t$o_w <- 1

  # class of the transition
  t$class <- "pupae_2m"

  # return the transition
  return(t)
}

#' @title transition for pupae emergence to mated female adults
#' @param p_gen genotype of the pupae
#' @param m_gen genotype of the male mate
#' @param nP number of pupae compartments
#' @noRd
make_transition_pupae_emerge_f <- function(T_index,u,p_gen,m_gen,nP){

  # tokens required
  input_token_1 <- paste0("P",nP,"_",p_gen)
  input_token_2 <- paste0("M_",m_gen)

  # produces a female token (with her genotype + the genotype of her mate)
  # also return the male token.
  output_token <- paste0("F1_",p_gen,"_",m_gen)

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(input_token_1,"->",output_token) # name of this t (corresponds to v)

  # requires a pupae token and a male token
  t$s <- match(x = c(input_token_1,input_token_2),table = u)
  t$s_w <- c(1,1)

  # produces a female and a male token
  t$o <- match(x = c(output_token,input_token_2),table = u)
  t$o_w <- c(1,1)

  # class of the transition
  t$class <- "pupae_2f"

  # return the transition
  return(t)
}

#' @title transition for pupae emergence to unmated female adults
#' @param p_gen genotype of the pupae
#' @param m_gen genotype of the male mate
#' @param nP number of pupae compartments
#' @noRd
make_transition_pupae_emerge_unmated <- function(T_index,u,p_gen,nP,node=NULL){

  # tokens required
  input_token <- paste0("P",nP,"_",p_gen)

  # produces a female token (with her genotype + the genotype of her mate)
  # also return the male token.
  output_token <- paste0(c("U1",p_gen,node),collapse = "_")

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


# males ------------------------------------------------------------

# kill a male token
make_transition_male_mort <- function(T_index,u,m_gen,node=NULL){

  # tokens required
  input_token <- paste0(c("M",m_gen,node),collapse = "_")

  # no tokens are produced

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(input_token,"->D") # name of this t (corresponds to v)

  # requires a male token
  t$s <- match(x = input_token,table = u)
  t$s_w <- 1

  # no tokens produced
  t$o <- NaN
  t$o_w <- NaN

  # class of the transition
  t$class <- "male_mort"

  # return the transition
  return(t)
}


# females ------------------------------------------------------------

make_transition_female_mort <- function(T_index, u, f_gen, m_gen, stage){

  # tokens required
  ftoken <- paste0("F",stage,"_",f_gen,"_",m_gen)

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
make_transition_female_unmated_mort <- function(T_index,u,f_gen,stage){

  # tokens required
  input_token <- paste0("U",stage,"_",f_gen)

  # no tokens are produced

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(input_token,"->D") # name of this t (corresponds to v)

  # requires a female token
  t$s <- match(x = input_token,table = u)
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
make_transition_female_unmated_mate <- function(T_index,u,f_gen,m_gen,stage){

  # tokens required
  input_token_1 <- paste0("U",stage,"_",f_gen)
  # paste0()
  input_token_2 <- paste0(c("M",m_gen,node),collapse = "_")

  # produces a female token (with her genotype + the genotype of her mate)
  # also return the male token.
  output_token <- paste0(c("F",f_gen,m_gen,node),collapse = "_")

  # t: {index into v, label, input arcs/weights, output arcs/weights}
  t <- list()
  t$vix <- T_index # where we can find this t in v
  t$label <- paste0(input_token_1,"->",output_token) # name of this t (corresponds to v)

  # requires a pupae token and a male token
  t$s <- match(x = c(input_token_1, input_token_2),table = u)
  t$s_w <- c(1,1)

  # produces a female and a male token
  t$o <- match(x = c(output_token, input_token_2),table = u)
  t$o_w <- c(1,1)

  # class of the transition
  t$class <- "female_unmated_mate"

  # return the transition
  return(t)
}

