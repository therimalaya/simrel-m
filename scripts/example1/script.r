## ## ---- Plotting ----------------------------------------
## plt_labels <- opts %>% as_data_frame %>%
##   transmute(
##     design = 1:n(),
##     Model  = NA,
##     label  = paste0("R2: ", R2, "\ngamma: ", gamma, "\nypos: ", ypos, "\nrelpos: ", relpos)
##   )
## PE_OLS <- predErr %>% filter(pred_err <= 10, Model == "OLS", comp != 0)
## PE <- predErr %>% filter(pred_err <= 10, Model != "OLS")
## plt1 <- ggplot(PE, aes(comp, pred_err, color = Model)) +
##   geom_line(aes(group = Model), size = 0.7) +
##   geom_point(size = 1) +
##   facet_grid(. ~ design, labeller = label_both) +
##   scale_color_brewer(palette = "Set1") +
##   scale_x_continuous(breaks = 0:10) +
##   labs(x = '', y = expression(paste('||', alpha, '||'[F]))) +
##   theme(legend.position = "bottom") +
##   geom_text(aes(label = label), data = plt_labels,
##             inherit.aes = FALSE, family = "mono",
##             x = 10, y = c(2.18, 2.18, 1.7, 1.7), hjust = 1,
##             size = rel(3)) +
##   ggtitle("Prediction Error", sub = "Population Covariance Matrix") +
##   geom_hline(aes(yintercept = pred_err, color = Model), data = PE_OLS) +
##   guides(color = guide_legend(nrow = 1, direction = "horizontal"))

## ----PredictionErrorPropOLS----------------------------------------------
pe_wide <- predErr %>%
  spread(Model, pred_err) %>%
  group_by(design, replicate) %>%
  mutate(OLS = zoo::na.locf(OLS))
predProp <- pe_wide %>%
  mutate_at(-c(1:4, 6), funs((. - OLS)/OLS)) %>%
  mutate(OLS = 0) %>%
  gather(Model, pred_err, -c(1:4), na.rm = TRUE)

## ## ----prediction-error-plot, fig.cap='Model comparison based on test prediction error', fig.asp=0.5, out.width="100%", fig.pos='H'----
## plt2 <- ggplot(predProp, aes(comp, pred_err, color = Model)) +
##   geom_line(aes(group = Model), size = 0.7) +
##   geom_point(size = 1) +
##   facet_grid(.~design, labeller = label_both) +
##   scale_x_continuous(breaks = 0:10) +
##   labs(x = 'Number of Components',
##        y = expression(((paste('||', alpha, '||'[F])) - (paste('||', alpha, '||'[F]))[OLS]) /
##                       (paste('||', alpha, '||'[F])[OLS]))) +
##   theme(legend.position = "top") +
##   scale_color_brewer(palette = "Set1") +
##   ggtitle("Proportionate Prediction Error", sub = "With respect to OLS")
