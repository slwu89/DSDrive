test_that("SPN places generation works, shape params > 1", {
    cube <- MGDrivE::cubeMendelian()

    # parameters
    theta <- make_lifecycle_parameters(
        k_E = 3,
        k_L = 3,
        k_P = 3
    )

    # set of places
    u <- c(
        "E1_AA", "E1_Aa", "E1_aa",
        "E2_AA", "E2_Aa", "E2_aa",
        "E3_AA", "E3_Aa", "E3_aa",
        "L1_AA", "L1_Aa", "L1_aa",
        "L2_AA", "L2_Aa", "L2_aa",
        "L3_AA", "L3_Aa", "L3_aa",
        "P1_AA", "P1_Aa", "P1_aa",
        "P2_AA", "P2_Aa", "P2_aa",
        "P3_AA", "P3_Aa", "P3_aa",
        "U_AA", "U_Aa", "U_aa",
        "F_AA_AA", "F_AA_Aa", "F_AA_aa",
        "F_Aa_AA", "F_Aa_Aa", "F_Aa_aa",
        "F_aa_AA", "F_aa_Aa", "F_aa_aa",
        "M_AA", "M_Aa", "M_aa"
    )

    # generate SPN_P
    SPN_P <- spn_P(parameters = theta, cube = cube)

    # check places are generated correctly
    expect_equal(sort(u), sort(SPN_P$u))

    # check indices
    expect_equal(length(u), length(unlist(SPN_P$ix)))

    # check indices generated properly
    expect_equal(theta$k_P * cube$genotypesN, length(SPN_P$ix$pupae))

    SPN_T <- spn_T(u = SPN_P$u, parameters = theta, cube = cube)

    S <- spn_S(spn_P = SPN_P, spn_T = SPN_T)

    expect_equal(nrow(S), length(u))
    expect_equal(ncol(S), length(SPN_T$T))

})


test_that("SPN places generation works, shape params = 1", {
    cube <- MGDrivE::cubeMendelian()

    # parameters
    theta <- make_lifecycle_parameters(
      k_E = 1,
      k_L = 1,
      k_P = 1
    )

    # set of places
    u <- c(
        "E1_AA", "E1_Aa", "E1_aa",
        "L1_AA", "L1_Aa", "L1_aa",
        "P1_AA", "P1_Aa", "P1_aa",
        "U_AA", "U_Aa", "U_aa",
        "F_AA_AA", "F_AA_Aa", "F_AA_aa",
        "F_Aa_AA", "F_Aa_Aa", "F_Aa_aa",
        "F_aa_AA", "F_aa_Aa", "F_aa_aa",
        "M_AA", "M_Aa", "M_aa"
    )

    # generate SPN_P
    SPN_P <- spn_P(parameters = theta, cube = cube)

    # check places are generated correctly
    expect_equal(sort(u), sort(SPN_P$u))

    # check indices
    expect_equal(length(u), length(unlist(SPN_P$ix)))

    # check indices generated properly
    expect_equal(theta$k_P * cube$genotypesN, length(SPN_P$ix$pupae))

})

