## ----Simulation, results='hide'------------------------------------------
source("01-setup.r")
set.seed(123)
sim_obj <- map_df(1:4, function(d){
  map_df(1:20, function(r){
    tibble(
      id = paste('D', d, r, sep = "-"),
      obj = list(do.call(simulatr, design[d,] %>% t() %>% .[, 1])),
      Train = obj %>% map(~data.frame(x = I(.x$X), y = I(.x$Y))),
      Test = obj %>% map(~data.frame(x = I(.x$testX), y = I(.x$testY)))
    )
  }, .id = "replicate")
}, .id = "design")

