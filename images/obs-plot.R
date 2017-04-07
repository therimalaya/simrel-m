source("images/cov-plot.R")

get_obsplot <- function(sim.obj, before.rot = TRUE) {
  
  ## Set X and Y names
  xvar.name <- if (before.rot) "Z" else "X"
  yvar.name <- if (before.rot) "W" else "Y"
  
  ## Function to flatten list into vector
  flatten_list <- function(lst, yvar.name = NULL, xvar.name = NULL) {
    out <- unname(t(melt(lst)))
    if (all(is.null(c(yvar.name, xvar.name))))
      out <- `names<-`(as.numeric(out[2, ]), out[1, ])
    else
      out <- `names<-`(paste0(yvar.name, out[2, ]), paste0(xvar.name, out[1, ]))
    
    return(out)      
  }
  
  ## Create DataFrame out of data matrix
  dff <- melt(`colnames<-`(sim.obj[[xvar.name]], 
                           paste0(xvar.name, 1:ncol(sim.obj[[xvar.name]]))))
  
  ## Identify the position of relevant predictors (components)
  ypos <- sim.obj$ypos
  relpos <- sim.obj[["relpos"]] ## Needed for coloring
  predpos <- unname(sim.obj[["relpred"]]) ## Needed for ordering
  
  ## Vector for coloring
  relpos.vec <- flatten_list(relpos, yvar.name, xvar.name)
  dff$relpos <- relpos.vec[as.character(dff$Var2)]
  dff$relpos[is.na(dff$relpos)] <- "None"
  
  ## Ordering
  predpos.vec <- flatten_list(predpos, yvar.name, xvar.name)
  predpos.idx <- c(names(predpos.vec), 
                   setdiff(paste0(xvar.name, 1:sim.obj$p), names(predpos.vec)))
  dff$Var2 <- factor(as.character(dff$Var2), levels = predpos.idx)
  
  ## Adding Relevant Predictors vectors
  dff$predpos <- predpos.vec[as.character(dff$Var2)]
  dff$predpos[is.na(dff$predpos)] <- "None"
  
  ## Give sensible names
  names(dff) <- c("Obs.Idx", "Variables", "Obs.Value", 
                  "RelevantBefore", "RelevantAfter")
  
  ## Tweaking some details
  fill_colour_aes <- if (before.rot) "RelevantBefore" else "RelevantAfter"
  dff$RelevantAfter <- factor(dff$RelevantAfter, levels = unique(sort(dff$RelevantAfter, decreasing = T)))
  dff$RelevantBefore <- factor(dff$RelevantBefore, levels = unique(sort(dff$RelevantBefore, decreasing = T)))
  
  ## Plottings
  ThePlot <- ggplot(dff, aes(Variables, Obs.Value)) + 
    geom_boxplot(aes_string(fill = fill_colour_aes), 
                 alpha = 0.5) +
    geom_point(size = rel(4), shape = 21,
               aes_string(color = fill_colour_aes)) +
    labs(x = "", y = "") + 
    theme(panel.background = element_rect(fill = NA), 
          strip.text.x = element_text(angle = 90, hjust = 0.5, vjust = 1), 
          legend.position = "bottom", 
          strip.background = element_rect(fill = "#eeeeee")) +
    scale_fill_brewer(palette = "Set2") +
    scale_color_brewer(palette = "Set2")
  return(ThePlot)
}

obs.plot <- share_legend(
  get_obsplot(sim.obj, TRUE),
  get_obsplot(sim.obj, FALSE),
  ncol = 2
)

ggsave(obs.plot, width = 15, height = 5, dev = "svg", filename = "images/obs-plot.svg")

combined_plot <- gridExtra::grid.arrange(cov.plot, obs.plot, nrow = 2)
ggsave("images/combined-plot.pdf", combined_plot, width = 15, height = 10, dev = "pdf")
