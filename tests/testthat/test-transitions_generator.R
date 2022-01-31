test_that("oviposition transitions work", {

  cube <- MGDrivE::cubeHoming1RA()

  # set of places
  u <- c(
    "E1_HH", "E1_HW", "E1_HR", "E1_WW", "E1_WR", "E1_RR",
    "F_HH_HH", "F_HH_HW", "F_HH_HR", "F_HH_WW", "F_HH_WR", "F_HH_RR", "F_HW_HH", "F_HW_HW", "F_HW_HR", "F_HW_WW", "F_HW_WR", "F_HW_RR",
    "F_HR_HH", "F_HR_HW", "F_HR_HR", "F_HR_WW", "F_HR_WR", "F_HR_RR", "F_WW_HH", "F_WW_HW", "F_WW_HR", "F_WW_WW", "F_WW_WR", "F_WW_RR",
    "F_WR_HH", "F_WR_HW", "F_WR_HR", "F_WR_WW", "F_WR_WR", "F_WR_RR", "F_RR_HH", "F_RR_HW", "F_RR_HR", "F_RR_WW", "F_RR_WR", "F_RR_RR"
  )

  # arbitrarily make some crosses not work
  n_bad <- 3
  bad_crosses <- which(cube$ih > 0)
  bad_crosses <- bad_crosses[sort(sample.int(n = length(bad_crosses), size = n_bad, replace = FALSE))]
  cube$tau[bad_crosses] <- 0

  cube_nonzero <- which(cube$ih * cube$tau > 0, arr.ind = TRUE)

  ovi_transitions <- generate_oviposition_transitions(cube = cube, u = u)

  expect_equal(length(ovi_transitions), nrow(cube_nonzero))

  for (i in 1:length(ovi_transitions)) {
    label_split <- strsplit(ovi_transitions[[i]]$label, split = "->")[[1]]
    expect_equal(which(label_split[1] == u), ovi_transitions[[i]]$s)
    expect_equal(length(ovi_transitions[[i]]$s), length(ovi_transitions[[i]]$s_w))

    expect_equal(which(label_split[1] == u), ovi_transitions[[i]]$o[1])
    expect_equal(which(label_split[2] == u), ovi_transitions[[i]]$o[2])
    expect_equal(length(ovi_transitions[[i]]$o), length(ovi_transitions[[i]]$o_w))
  }

})


# labels <- c("E1_HH->D", "E1_HW->D", "E1_HR->D", "E1_WW->D", "E1_WR->D",
#             "E1_RR->D", "E2_HH->D", "E2_HW->D", "E2_HR->D", "E2_WW->D", "E2_WR->D", "E2_RR->D")

test_that("egg advancement transitions work", {

  cube <- MGDrivE::cubeHoming1RA()

  nE <- 2
  nL <- 1
  nP <- 2
  nA <- 1
  params <- list(nE=nE,nL=nL,nP=nP,nA=nA)

  # genetic states
  g <- cube$genotypesID
  nG <- cube$genotypesN

  spn_p <- spn_P_lifecycle_node(params = params,cube = cube)
  u = spn_p$u

  egg_adv_trans <- generate_egg_advancement_transitions(cube = cube, u = u, nE = nE)

  labels <- c("E1_HH->E2_HH", "E1_HW->E2_HW", "E1_HR->E2_HR", "E1_WW->E2_WW",
              "E1_WR->E2_WR", "E1_RR->E2_RR", "E2_HH->L1_HH", "E2_HW->L1_HW",
              "E2_HR->L1_HR", "E2_WW->L1_WW", "E2_WR->L1_WR", "E2_RR->L1_RR")

  input_tokens <- c(1, 3, 5, 7, 9, 11, 2, 4, 6, 8, 10, 12)
  input_weight <- rep(1, 12L)
  output_tokens <- c(2, 4, 6, 8, 10, 12, 13, 14, 15, 16, 17, 18)
  output_weight <- rep(1, 12L)
  class_exp <- rep("egg_adv", 12L)

  expect_equal(length(egg_adv_trans), 12L)
  expect_equal(unlist(lapply(X = egg_adv_trans, function(x) {x$label})), labels)
  expect_equal(unlist(lapply(X = egg_adv_trans, function(x) {x$s})), input_tokens)
  expect_equal(unlist(lapply(X = egg_adv_trans, function(x) {x$s_w})), input_weight)
  expect_equal(unlist(lapply(X = egg_adv_trans, function(x) {x$o})), output_tokens)
  expect_equal(unlist(lapply(X = egg_adv_trans, function(x) {x$o_w})), output_weight)
  expect_equal(unlist(lapply(X = egg_adv_trans, function(x) {x$class})), class_exp)

})
