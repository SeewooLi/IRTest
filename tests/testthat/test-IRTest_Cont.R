test_that(
  "testing basic operations for IRTest_Cont",
  {
    Alldata <- DataGeneration(N=500,
                              nitem_C = 3,
                              latent_dist = "2NM",
                              d = 1.664,
                              sd_ratio = 2,
                              prob = 0.3)
    data <- Alldata$data_C
    item <- Alldata$item_C
    initialitem <- Alldata$initialitem_C
    theta <- Alldata$theta

    # Normal distribution
    # EAP
    Mod1 <- IRTest_Cont(initialitem = initialitem,
                        data = data,
                        latent_dist = "N",
                        max_iter = 1,
                        threshold = .0001)
    expect_equal(dim(Mod1$par_est), dim(item))
    expect_equal(length(Mod1$theta), length(theta))

    # MLE
    Mod1 <- IRTest_Cont(initialitem = initialitem,
                        data = data,
                        latent_dist = "N",
                        max_iter = 1,
                        threshold = .0001,
                        ability_method = "MLE")
    expect_equal(dim(Mod1$par_est), dim(item))
    expect_equal(length(Mod1$theta), length(theta))

    # EHM
    Mod1 <- IRTest_Cont(initialitem = initialitem,
                        data = data,
                        latent_dist = "EHM",
                        max_iter = 1,
                        threshold = .0001)
    expect_equal(dim(Mod1$par_est), dim(item))
    expect_equal(length(Mod1$theta), length(theta))

    # Two-component normal distribution
    Mod1 <- IRTest_Cont(initialitem = initialitem,
                        data = data,
                        latent_dist = "2NM",
                        max_iter = 1,
                        threshold = .0001)
    expect_equal(dim(Mod1$par_est), dim(item))
    expect_equal(length(Mod1$theta), length(theta))

    # KDE
    Mod1 <- IRTest_Cont(initialitem = initialitem,
                        data = data,
                        latent_dist = "KDE",
                        max_iter = 1,
                        threshold = .0001)
    expect_equal(dim(Mod1$par_est), dim(item))
    expect_equal(length(Mod1$theta), length(theta))

    # Davidian curve
    Mod1 <- IRTest_Cont(initialitem = initialitem,
                        data = data,
                        latent_dist = "DC",
                        max_iter = 1,
                        threshold = .0001,
                        h=2)
    expect_equal(dim(Mod1$par_est), dim(item))
    expect_equal(length(Mod1$theta), length(theta))

    # LLS
    Mod1 <- IRTest_Cont(initialitem = initialitem,
                        data = data,
                        latent_dist = "LLS",
                        max_iter = 1,
                        threshold = .0001,
                        h=2)
    expect_equal(dim(Mod1$par_est), dim(item))
    expect_equal(length(Mod1$theta), length(theta))


  }
)
