test_that(
  "testing basic operations for summary.irtest, print.irtest, and print.irtest_summary", {
    data <- DataGeneration(N=1000,
                           nitem_D = 10,
                           latent_dist = "2NM",
                           d = 1.664,
                           sd_ratio = 2,
                           prob = 0.3)$data_D


    KDM <- IRTest_Dich(data, latent_dist = "KDE")
    DC1 <- IRTest_Dich(data, latent_dist = "DC", h = 1)
    DC6 <- IRTest_Dich(data, latent_dist = "DC", h = 3)

    expect_no_error(anova(DC1, DC6))
    expect_no_error(best_model(DC6, KDM))

    logLik(KDM)
    coef(DC6)
  }
  )
