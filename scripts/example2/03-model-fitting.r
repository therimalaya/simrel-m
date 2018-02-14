## ----ModelFitting----------------------------------------
fit <- sim_obj %>%
  group_by(design, replicate, id) %>%
  transmute(
    pcr   = map(Train, ~pcr(y ~ x, data = ., ncomp = 10)),
    pls1  = map(Train, function(trn){
      map(1:ncol(trn$y), function(yi){
        dta <- data.frame(x = I(trn$x), y = I(trn$y[, yi]))
        plsr(y ~ x, data = dta, ncomp = 10)
      })
    }),
    pls2   = map(Train, ~plsr(y ~ x, data = ., ncomp = 10))
  )
names(fit)[-c(1:3)] <- mdls
save(fit, file = "scripts/example2/output/fit.Rdata")