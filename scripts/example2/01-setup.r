## ----LoadingPackages-----------------------------------------------------
pkgs <- c("pls", "envlp", "simrel", "pander", "tidyverse")
for (pkg in pkgs) require(pkg, quietly = T, warn.conflicts = F, character.only = T)

## ----MakingDesign----------------------------------------
opts <- list(
  eta = c(0.1, 0.8),
  relpos = list(list(c(2, 3, 5, 7)), list(c(2), c(3)))
)
design <- expand.grid(tibble::as_tibble(opts)) %>% 
  as_tibble() %>% 
  mutate(
    n = 100,
    p = 1000,
    ypos = list(list(1:5)),
    m = 5,
    gamma = 0.5,
    ntest = 1000,
    type = "multivariate",
    q = rep(list(1000, c(500, 500)), each = 2),
    R2 = rep(list(0.8, c(0.6, 0.6)), each = 2)
  )
mdls   <- c("PCR", "PLS1", "PLS2")
save(opts, design, mdls, file = "scripts/example2/output/design.Rdata")
