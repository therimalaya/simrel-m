## ---- Coefficients ----------------------------------------
coef <- fit %>%
  group_by(design, replicate, id) %>%
  transmute(
    pcr    = map(PCR,   ~get_beta("pcr")(.x)),
    pls1   = map(PLS1,  ~get_beta("pls1")(.x)),
    pls2    = map(PLS2,   ~get_beta("pls")(.x))
  )
names(coef)[-c(1:3)] <- mdls
save(coef, file = "scripts/example2/output/coef.Rdata")
