## ---- functions ----
get_rmsep_df <- function(mdl, new_data){
  if (class(mdl) == "list") {
    rmsep <- map_df(seq_along(mdl), function(response){
      new_data <- within(new_data, y <- y[, response])
      out <- get_rmsep_df(mdl[[response]], new_data) %>% select(-response)
      return(out)
    }, .id = "response")
    rmsep_df <- rmsep %>%
      mutate(response = paste0("Y", response))
    return(rmsep_df[, c(2, 1, 3:ncol(rmsep_df))])
  }
  rmsep <- RMSEP(mdl, newdata = new_data, estimate = "all")
  rmsep_df <- reshape2::melt(unclass(rmsep)[["val"]], 
                             value.name = "RMSEP", as.is = TRUE)
  return(rmsep_df)
}
list2chr <- function(list) {
  paste(str_replace_all(as.character(list),
                        "[a-zA-Z()]", ""),
        collapse = "; ")
}

## ---- rmsep ----
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

rmsep_df_nested <- rmsep_df %>%
  ungroup() %>% nest(-design)

design_selected <- design %>%
    select(eta, relpos, R2) %>%
    rowwise() %>%
    mutate_at(vars(relpos, R2), list2chr) %>%
    ungroup() %>%
    mutate(design = as.character(1:n()))

## ---- fulldata ----
fullData <- rmsep_df_nested %>% left_join(design_selected) %>% unnest(data)

## ---- rmsep_plot ----
rmsep_plt <- ggplot(fullData %>% filter(estimate == "test"),
              aes(model, RMSEP, color = Method, group = Method, fill = Method)) +
  stat_summary(fun.y = mean, geom = "point", size = 1, shape = 21, color = "black") +
  stat_summary(fun.y = mean, geom = "line") +
  facet_grid(eta + relpos + R2 ~ response, scales = 'free_y', labeller = label_both) +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "Components", y = "RMSEP") +
  theme(legend.position = "top")
plot(rmsep_plt)

## ---- fulldata ----
dta <- fullData %>% filter(estimate == "test") %>% spread(response, RMSEP) %>%
  mutate_at(vars(eta, relpos, Method, model), as.factor)
mdl <- lm(cbind(Y1, Y2, Y3, Y4, Y5) ~ eta * relpos * model + Method, data = dta)
mnv <- manova(mdl)
summary(mnv)

## ---- effect ----
eff <- effects::allEffects(mdl)
method_eff <- map_df(eff$Method, as_tibble, .id = "Response")
interaction_eff <- map_df(eff[["eta:relpos:model"]], as_tibble, .id = "Response")

## ---- effect-plot ----
plt1 <- ggplot(method_eff, aes(Method, fit, ymin = lower, ymax = upper)) +
  geom_point() +
  geom_line(group = 1) +
  geom_errorbar(color = "red", width = 0.1) +
  facet_grid(Response ~ .) +
  ggtitle("Effect of Method") +
  labs(x = NULL, y = "RMSEP")
plot(plt1)

plt2 <- ggplot(interaction_eff, aes(as.numeric(model), fit, ymin = lower, ymax = upper, group = eta, color = eta)) +
  geom_errorbar(width = 0.1) +
  geom_point() +
  geom_line() +
  facet_grid(Response ~ relpos, labeller = label_both) +
  scale_color_brewer(palette = "Set1") +
  scale_x_continuous(breaks = 0:10) +
  labs(x = "Components", y = "RMSEP") +
  theme(legend.position = "bottom") +
  ggtitle("Interaction effect of eta * relpos * component")
plot(plt2)

eff_plt <- gridExtra::grid.arrange(gridExtra::arrangeGrob(plt1, plt2, ncol = 2, widths = c(2/5, 3/5)))

## ---- save-effect-plot ----
ggsave(filename = "scripts/example2/rmsep-plot.pdf", rmsep_plt, width = 9, height = 7, units = "in")
ggsave(filename = "scripts/example2/eff-plt.pdf", eff_plt, width = 7, height = 9, units = "in")

## Residual vs Fitted Plot -----------
augment_mlm <- function(mdl, type = c("Residuals", "Fitted", "CooksDistance")) {
  fn <- switch(
    type,
    Residuals = residuals,
    Fitted = fitted,
    CooksDistance = cooks.distance
  )
  out <- reshape2::melt(fn(mdl),
                        varnames = c("Predictors", "Response"),
                        value.name = type)
  return(out)
}
rvf_df <- augment_mlm(mdl, "Residuals") %>%
  left_join(augment_mlm(mdl, "Fitted")) %>%
  left_join(augment_mlm(mdl, "CooksDistance")) %>%
  as_tibble()

ggplot(rvf_df, aes(Fitted, Residuals)) +
  geom_point(size = 1) +
  facet_grid(. ~ Response) +
  geom_hline(yintercept = 0, color = "blue", linetype = 2)
