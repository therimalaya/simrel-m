## ---- Saving Final Dataset ----------------------------------------
source("07-prediction-error.r")
myData <- do.call(tibble, opts) %>% 
  mutate(design = as.character(1:n())) %>% 
  right_join(predErr, by = "design") %>% 
  mutate_if(is.character, as.factor) %>% 
  mutate_at("gamma", as.factor)
save(myData, file = "output/pred-err.Rdata")