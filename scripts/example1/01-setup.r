## ----LoadingPackages-----------------------------------------------------
pkgs <- c("pls", "envlp", "simrel", "pander", "tidyverse")
for (pkg in pkgs) require(pkg, quietly = T, warn.conflicts = F, character.only = T)


## ----MakingDesign----------------------------------------
opts <- list(
  n      = rep(100, 4),
  p      = rep(16, 4),
  q      = rep("5, 5, 5", 4),
  m      = rep(5, 4),
  relpos = rep("1, 6; 2, 5; 3, 4", 4),
  gamma  = rep(c(0.2, 0.8), 2),
  R2     = rep(c("0.8, 0.8, 0.4", "0.4, 0.4, 0.4"), each = 2),
  ypos   = rep("1, 4; 2, 5; 3", 4),
  ntest  = rep(100000, 4)
)
design <- opts %>% 
  prepare_design %>% 
  transpose %>%
  as_data_frame %>%
  mutate(type = "multivariate")
mdls   <- c("OLS", "PCR", "PLS", "CPLS", "CPPLS", "Xenv", "Yenv", "PLS1", "Senv")
