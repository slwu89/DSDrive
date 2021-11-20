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
