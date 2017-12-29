## ---- Prediction-Error ----------------------------------------
source("05-get-coefficients.r")
source("06-true-values.r")
predErr <- map_df(`names<-`(mdls, mdls), function(mdl){
  coef %>%
    group_by(design, replicate, id) %>%
    select_(mdl = mdl) %>%
    left_join(trueValue) %>%
    do(with(., pmap_df(list(mdl, minerror, trueBeta, sigma), getPredErr)))
}, .id = "Model")