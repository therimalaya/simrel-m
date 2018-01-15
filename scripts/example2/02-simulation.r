## ----Simulation, results='hide'------------------------------------------
library(parallel)
sim_obj_list <- mclapply(1:NROW(design), function(d){
  out <- mclapply(1:6, function(r){
    set.seed(as.numeric(paste0(d, r)))
    out <- tibble(
      id = paste('D', d, r, sep = "-"),
      obj = list(do.call(simrel, design[d,] %>% t() %>% .[, 1])),
      Train = obj %>% map(~data.frame(x = I(.x$X), y = I(.x$Y))),
      Test = obj %>% map(~data.frame(x = I(.x$testX), y = I(.x$testY)))
    )
    return(out)
  }, mc.cores = 3)
  out <- bind_rows(out, .id = "replicate")
  return(out)
}, mc.cores = 4)
sim_obj <- bind_rows(sim_obj_list, .id = "design")

save(sim_obj, file = "scripts/example2/output/sim-obj.Rdata")
