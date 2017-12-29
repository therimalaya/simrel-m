## ---- TrueValues ----------------------------------------
trueValue <- sim_obj %>%
  group_by(design, replicate, id) %>%
  transmute(
    p         = map_dbl(obj, "p"),
    m         = map_dbl(obj, "m"),
    n         = map_dbl(obj, "n"),
    minerror  = map(obj, "minerror"),
    trueBeta  = map(obj, "beta"),
    testData  = map(obj, ~data.frame(x = I(.x$testX), y = I(.x$testY))),
    sigmaTest = map(obj, ~cov(.x[["testX"]])),
    sigma     = map(obj, ~with(.x, Sigma[-c(1:m), -c(1:m)]))
  )