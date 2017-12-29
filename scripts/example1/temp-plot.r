get_pred_plot <- function(pred_err, type_of_error = "average") {
  predErr_smry <- pred_err %>% 
    group_by(Model, design, comp) %>% 
    summarise(minimum = min(pred_err), average = mean(pred_err), maximum = max(pred_err)) %>% 
    gather(`Type of Error`, pred_err, -Model, -design, -comp)
  
  PE <- predErr_smry %>% 
    filter(`Type of Error` == type_of_error) %>% 
    group_by(Model, design) %>% 
    summarise(comp = comp[which.min(pred_err)],
              pred_err = min(pred_err))
  
    winner <- PE %>% 
    group_by(design) %>% 
    summarise(
      Model = Model[which.min(pred_err)],
      comp = comp[which.min(pred_err)],
      pred_err = min(pred_err)
    ) %>% mutate(label = "Winner")
  
  runnerup <- PE %>% 
    group_by(design) %>% 
    summarize(
      order = order(pred_err, decreasing = F)[2],
      pred_err = pred_err[order],
      comp = comp[order],
      Model = Model[order],
      label = "RunnerUp"
    )
  
  design_details <- pred_err %>% 
    filter(Model == "OLS") %>% 
    group_by(Model, design) %>% 
    transmute(
      label = paste0("R2: ", R2, "\ngamma: ", gamma)
    ) %>% unique()
  
  plt <- qplot(comp, pred_err, group = id, geom = c("line", "point"), 
               data = pred_err, facets = Model ~ design, color = I("grey")) +
    geom_line(data = predErr_smry, aes(color = `Type of Error`, x = comp, y = pred_err), inherit.aes = FALSE) +
    geom_point(data = predErr_smry, aes(color = `Type of Error`, x = comp, y = pred_err), inherit.aes = FALSE) +
    scale_x_continuous(breaks = seq(0, 10, 2)) +
    geom_point(data = PE, inherit.aes = FALSE, size = 2,
               aes(x = comp, y = pred_err), shape = 21, color = "black") +
    geom_text(data = PE, inherit.aes = FALSE, nudge_y = -0.1, family = "mono",
              aes(x = comp, y = pred_err, 
                  label = paste0("(", comp, ", ", round(pred_err, 3), ")"))) +
    geom_text(data = winner, aes(label = label, x = 10, y = Inf), family = "mono",
              inherit.aes = FALSE, color = "red", size = 3, vjust = 1, hjust = 1) +
    geom_text(data = runnerup, aes(label = label, x = 10, y = Inf), family = "mono",
              inherit.aes = FALSE, color = "blue", size = 3, vjust = 1, hjust = 1) +
    geom_text(data = design_details, inherit.aes = FALSE,
              aes(x = 10, y = -Inf, label = label), vjust = 0, hjust = 1,
              family = "mono") +
    labs(x = "Number of Components", y = "Prediction Error") +
    theme(legend.position = "bottom") +
    ggtitle("Minimum/Average/Maximum Prediction Error",
            subtitle = "The mimum error over all component is shown in brackets")
  
  plot(plt)  
}


