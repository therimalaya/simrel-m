pkgs <- c("ggplot2", "simulatr", "reshape2")
for (pkg in pkgs) require(pkg, character.only = T)

pars <- list(
  n = 25,
  p = 15,
  m = 5, 
  gamma = 0.8,
  q = c(3, 3, 4),
  R2 = c(0.8, 0.7, 0.7),
  relpos = list(1:2, 3:4, 5:6),
  ypos = list(c(1, 4), 2, c(3, 5)),
  type = "multivariate"
)

set.seed(7)
sim.obj <- do.call(simulatr, pars)

get.cov.df <- function(cov.mat, n.xvar, n.yvar, relpos, xvar.name = "X",  yvar.name = "Y", relpred = NULL, ordering = TRUE, ypos) {
  mat.names <- c(paste0(yvar.name, 1:n.yvar), 
                 paste0(xvar.name, 1:n.xvar))
  rownames(cov.mat) <- colnames(cov.mat) <- mat.names
  abs.cov <- abs(cov.mat)
  lgl.cov <- abs.cov != 0
  abs.cov.df <- reshape2::melt(abs.cov)
  abs.cov.df$mat <- gsub("[[:digit:]]", "", paste(abs.cov.df$Var1, abs.cov.df$Var2, sep = ":"))
  abs.cov.df <- tidyr::separate(abs.cov.df, mat, c("facet1", "facet2"), sep = ":", remove = FALSE)
  
  flatten_list <- function(list, yvar.name, xvar.name) {
    out <- apply(melt(unname(list)), 1, function(l) paste0(c(xvar.name, yvar.name), l))
    `names<-`(out[2, ], out[1, ])
  }
  relpos.vec <- flatten_list(relpos, yvar.name, xvar.name)
  relpred.vec <- flatten_list(relpred, yvar.name, xvar.name)
  ypos.vec <- flatten_list(ypos, yvar.name, yvar.name)
  get_relvec <- function(df, relvec) {
    rpos1 <- relvec[df["Var1"]]
    rpos2 <- relvec[df["Var2"]]
    if (all(!is.na(c(rpos1, rpos2)))) {
      out <- if (as.numeric(df["value"]) == 0) "None" else rpos1
      return(out)
    } else {
      if (as.numeric(df["value"]) == 0) return("None")
      if (all(c(is.na(c(rpos1, rpos2)), as.numeric(df["value"]) != 0))) return("None")
      if (is.na(rpos1)) return(rpos2)
      if (is.na(rpos2)) return(rpos1)
    }
  }
  
  irrelpred.idx <- setdiff(paste0(xvar.name, 1:n.xvar), names(relpred.vec))
  irrelpred.vec <- `names<-`(rep(NA, length(irrelpred.idx)), irrelpred.idx)
  abs.cov.df$rotation <- apply(abs.cov.df, 1, get_relvec, relvec = c(relpred.vec, ypos.vec, irrelpred.vec))
  abs.cov.df$relpred <- apply(abs.cov.df, 1, get_relvec, relvec = c(relpred.vec, ypos.vec))
  abs.cov.df$relpos <- apply(abs.cov.df, 1, get_relvec, c(relpos.vec, ypos.vec[paste0(yvar.name, seq_along(ypos))]))
  
  abs.cov.df$facet1 <- factor(abs.cov.df$facet1, levels = c(yvar.name, xvar.name))
  abs.cov.df$facet2 <- factor(abs.cov.df$facet2, levels = c(yvar.name, xvar.name))
  
  # # Ordering of predpos ------------------------------
  relpred.numvec <- paste0(xvar.name, unname(unlist(relpred)))
  irrelpred.numvec <- setdiff(paste0(xvar.name, seq.int(n.xvar)), relpred.numvec)
  ypos.numvec <- paste0(yvar.name, unname(unlist(ypos)))
  if (ordering) {
    xy.idx <- c(ypos.numvec, c(relpred.numvec, irrelpred.numvec))
  } else {
    xy.idx <- c(paste0(yvar.name, seq.int(n.yvar)), paste0(xvar.name, seq.int(n.xvar)))
  }
  abs.cov.df$Var1 <- factor(abs.cov.df$Var1, levels = xy.idx)
  abs.cov.df$Var2 <- factor(abs.cov.df$Var2, levels = rev(xy.idx))
  
  abs.cov.df$relpos <- factor(abs.cov.df$relpos, levels = unique(sort(abs.cov.df$relpos, decreasing = T)))
  abs.cov.df$rotation <- factor(abs.cov.df$rotation, levels = unique(sort(abs.cov.df$rotation, decreasing = T)))
  abs.cov.df$relpred <- factor(abs.cov.df$relpred, levels = unique(sort(abs.cov.df$relpred, decreasing = T)))
  
  class(abs.cov.df) <- append("covdf", class(abs.cov.df))
  
  return(abs.cov.df)
}

plot.covdf <- function(covdf.obj, type) {
  plt <- ggplot(covdf.obj, aes_string("Var1", "Var2", fill = type)) +
    geom_tile(color = "lightgray", aes(alpha = value ^ (1/4))) +
    facet_grid(facet2 ~ facet1, scales = "free", space = "free") +
    scale_alpha_continuous(range = c(0, 1), guide = "none") +
    theme_minimal() +
    theme(legend.position = "bottom", strip.text = element_blank(),
          panel.background = element_rect(color = "darkgray", fill = NA),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.text = element_text(family = "mono", size = rel(0.7)),
          panel.grid.major = element_blank()) +
    labs(x = "", y = "") +
    scale_fill_brewer("Relevant for:", palette = "Set2", na.value = "#ffeeee")
  return(plt)
}

## ---- ggplot common legend -----------------
share_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  require(gridExtra)
  require(grid)
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)

  combined <- switch(
    position,
    "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                           legend,
                           ncol = 1,
                           heights = unit.c(unit(1, "npc") - lheight, lheight)),
    "right" = arrangeGrob(do.call(arrangeGrob, gl),
                          legend,
                          ncol = 2,
                          widths = unit.c(unit(1, "npc") - lwidth, lwidth))
  )
  grid.newpage()
  grid.draw(combined)
  return(combined)
}

cov.df <- function(sim.obj, type = "relpos", ordering = NULL) {
  pars <- list(
    cov.mat = switch(
      type,
      relpos = sim.obj$SigmaWZ,
      relpred = sim.obj$Sigma,
      rotation = rbind(cbind(sim.obj$Yrotation, matrix(0, sim.obj$m, sim.obj$p)), 
                       cbind(t(matrix(0, sim.obj$m, sim.obj$p)), sim.obj$Xrotation))
    ),
    n.xvar = sim.obj$p,
    n.yvar = sim.obj$m,
    relpos = sim.obj$relpos,
    relpred = sim.obj$relpred,
    ypos = sim.obj$ypos,
    ordering = ifelse(is.null(ordering), ifelse(type == "relpos", FALSE, TRUE), ordering),
    yvar.name = if (type == "relpos") "W" else "Y",
    xvar.name = if (type == "relpos") "Z" else "X"
  )
  do.call(get.cov.df, pars)
}

plts <- list(
  relpos = plot(cov.df(sim.obj, type = "relpos", ordering = TRUE), "relpos") + ggtitle("Principal Covariance Matrix"),
  rotation = plot(cov.df(sim.obj, "rotation"), "relpred") + ggtitle("Rotation Matrix"),
  relpred = plot(cov.df(sim.obj, type = "relpred"), type = "relpred") + ggtitle("Covariance Matrix of simulated Data"),
  ncol = 3
)

cov.plot <- do.call(share_legend, plts)

ggsave(cov.plot, width = 15, height = 5, dev = "svg", filename = "images/cov-plot.svg")
