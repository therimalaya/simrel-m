## ---- Saving Final Dataset ----------------------------------------
## Using RMSEP instead of predErr ----
myData <- rmsep_df %>%
  ungroup() %>%
  nest(-design) %>%
  left_join(
    design %>%
      mutate(design = as.character(1:n())),
    by = "design"
  ) %>%
  select(-n:-q) %>%
  mutate(relpos = map_chr(relpos, list2chr),
         R2 = map_chr(R2, list2chr)) %>%
  unnest(data)

save(myData, file = "scripts/example2/output/final-data.Rdata")
