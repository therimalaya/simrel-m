## ---- Coefficients ----------------------------------------
source("04-model-fitting.r")
coef <- fit %>%
  group_by(design, replicate, id) %>%
  transmute(
    ols    = map(OLS,   ~get_beta("ols")(.x)),
    pcr    = map(PCR,   ~get_beta("pcr")(.x)),
    pls    = map(PLS,   ~get_beta("pls")(.x)),
    cpls = map(CPLS, ~get_beta("cpls")(.x)),
    cppls  = map(CPPLS, ~get_beta("cppls")(.x)),
    xenv   = map(Xenv,  ~get_beta("xenv")(.x)),
    yenv   = map(Yenv,  ~get_beta("yenv")(.x)),
    pls1   = map(PLS1,  ~get_beta("pls1")(.x)),
    senv   = map(Senv,  ~get_beta("senv")(.x))
  )
names(coef)[-c(1:3)] <- mdls