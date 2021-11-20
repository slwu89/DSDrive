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
