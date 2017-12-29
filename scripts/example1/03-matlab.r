## ---- Writing-Matlab-file ----------------------------------------
source("02-simulation.r")
sim_obj %>%
  select(fn = id, Train) %>%
  mutate(fn = paste0('output/matfiles/', fn, '.mat')) %>%
  with(., walk2(fn, Train, ~R.matlab::writeMat(.x, data = .y)))

## ---- fitSimulEnv--------------------------------------------------------
## matlabr::run_matlab_script('fit-sim-env.m') #! Important Don't delete it
senv_raw <- drop(R.matlab::readMat("output/fitted-sim-env.mat")[[1]])
senv <- map_df(1:4, function(d){
  map_df(1:20, function(r){
    map_df(1:10, function(nc){
      senv_fit = senv_raw[[d]][[1]][[r]][[1]][[nc]][[1]][, , 1]
      tibble(estimate = names(senv_fit), senv_fit)
    }, .id = "ncomp")
  }, .id = "replicate")
}, .id = "design") %>%
  nest(4:5, .key = "senv") %>%
  nest(3:4, .key = "senv")