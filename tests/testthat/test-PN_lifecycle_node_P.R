test_that("SPN places generation works", {
    cube <- MGDrivE::cubeMendelian()

    # parameters
    theta <- list(
        nE = 3,
        nL = 3,
        nP = 3,
        nA = 3
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
        "U1_AA", "U1_Aa", "U1_aa",
        "U2_AA", "U2_Aa", "U2_aa",
        "U3_AA", "U3_Aa", "U3_aa",
        "F1_AA_AA", "F1_AA_Aa", "F1_AA_aa", 
        "F1_Aa_AA", "F1_Aa_Aa", "F1_Aa_aa", 
        "F1_aa_AA", "F1_aa_Aa", "F1_aa_aa",
        "F2_AA_AA", "F2_AA_Aa", "F2_AA_aa", 
        "F2_Aa_AA", "F2_Aa_Aa", "F2_Aa_aa", 
        "F2_aa_AA", "F2_aa_Aa", "F2_aa_aa",
        "F3_AA_AA", "F3_AA_Aa", "F3_AA_aa", 
        "F3_Aa_AA", "F3_Aa_Aa", "F3_Aa_aa", 
        "F3_aa_AA", "F3_aa_Aa", "F3_aa_aa",
        "M1_AA", "M1_Aa", "M1_aa",
        "M2_AA", "M2_Aa", "M2_aa",
        "M3_AA", "M3_Aa", "M3_aa"
    )

    # generate SPN_P
    SPN_P <- spn_P_lifecycle_node(theta, cube)

    # check places are generated correctly
    expect_equal(sort(u), sort(SPN_P$u))

    # check indices
    expect_equal(length(u), length(unlist(SPN_P$ix[[1]])))

    # check mated female indices generated properly
    expect_equal(theta$nA, length(SPN_P$ix[[1]]$females))

})