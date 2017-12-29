## ----ModelFitting----------------------------------------
source("03-matlab.r")
fit <- sim_obj %>%
  group_by(design, replicate, id) %>%
  transmute(
    ols   = map(Train, ~lm(y ~ x, data = .)),
    pcr   = map(Train, ~pcr(y ~ x, data = ., ncomp = 10)),
    pls   = map(Train, ~plsr(y ~ x, data = ., ncomp = 10)),
    cpls = map(Train, ~cppls(y ~ x, data = ., ncomp = 10)),
    cppls = map(Train, ~cppls(y ~ x, data = ., ncomp = 10, lower = 0, upper = 1)),
    xenv  = map(Train, ~map(1:10, function(nc) with(., xenv(x, y, u = nc)))),
    yenv  = map(Train, ~map(1:ncol(.$y), function(nc) with(., env(x, y, u = nc)))),
    pls1  = map(Train, function(trn){
      map(1:ncol(trn$y), function(yi){
        plsr(trn$y[, yi] ~ trn$x)
      })
    })
  )
fit <- fit %>% left_join(senv)
names(fit)[-c(1:3)] <- mdls