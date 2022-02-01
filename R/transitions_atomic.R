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

