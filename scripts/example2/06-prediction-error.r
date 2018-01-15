## ---- Prediction-Error ----------------------------------------
predErr <- map_df(`names<-`(mdls, mdls), function(mdl){
  coef %>%
    group_by(design, replicate, id) %>%
    select_(mdl = mdl) %>%
    left_join(trueValue) %>%
    do(with(., pmap_df(list(mdl, minerror, trueBeta, sigma), getIndivPredErr)))
}, .id = "Model")

rmsep_df <- sim_obj %>%
  select(-obj, -Train) %>%
  left_join(fit) %>%
  gather(Method, fit, PCR:PLS2) %>%
  group_by(design, replicate, id, Method) %>%
  transmute(
    RMSEP = map2(fit, Test, get_rmsep_df)
  ) %>%
  unnest(RMSEP) %>%
  mutate(model = as.integer(str_replace_all(model, "[A-Za-z()]", "")),
         model = ifelse(is.na(model), 0, model))
names(rmsep_df) <- c("design", "replicate", "id", "Model", "ErrorType", "Response", "comp", "RMSEP")
save(predErr, rmsep_df, file = "scripts/example2/output/pred-error.Rdata")