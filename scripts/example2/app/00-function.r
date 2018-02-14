get_beta <- 
  function(model = c('pcr', 'pls', 'cppls', 'mvr', 'xenv', 'bayes', 'linear', 'cpls',
                     'try-error', "list", "lm", "lmm", "yenv", "ols", "senv", "senvelope", "pls1")){
    model <- match.arg(model)
    if (model %in% c('pcr', 'pls', 'cppls', 'cpls')) model <- 'mvr'
    if (model %in% c("lm", "lmm", "ols")) model <- "ols"
    if (model %in% c("senv", "senvelope")) model <- "senv"
    if (model %in% c("try-error")) return(model)
    switch(model,
           mvr = {
             coefs <- function(mdl, ncomp = 10)
             {
               coef <- drop(coef(mdl, intercept = TRUE, ncomp = 1:ncomp))
               return(coef)
             }
           },
           pls1 = {
             coefs <- function(mdl, ncomp = 10)
             {
               out.dim <- c(length(mdl[[1]]$Xmeans) + 1, length(mdl), ncomp)
               out <- array(0, out.dim)
               for (y in seq_along(mdl)) {
                 cf <- drop(coef(mdl[[y]], intercept = TRUE, ncomp = 1:ncomp)) #16x10
                 out[, y, ] <- cf
               }
               return(out)
             }
           },
           xenv = {
             coefs <- function(mdl)
             {
               coef <- lapply(mdl, function(obj) {
                 out <- rbind(t(obj$mu), obj$beta)
                 rownames(out) <- c("Intercept", 1:(nrow(out) - 1))
                 return(out)
               })
               out <- array(unlist(coef), dim = c(nrow(coef[[1]]), ncol(coef[[1]]), length(coef)))
               `dimnames<-`(out, append(dimnames(coef[[1]]), list(paste0("Comp", 1:length(coef)))))
             }
           },
           yenv = {
             coefs <- function(mdl)
             {
               coef <- lapply(mdl, function(obj) {
                 out <- t(cbind(obj$alpha, obj$beta))
                 rownames(out) <- c("Intercept", 1:(nrow(out) - 1))
                 colnames(out) <- paste0("Y", 1:ncol(out))
                 return(out)
               })
               out <- array(unlist(coef), dim = c(nrow(coef[[1]]), ncol(coef[[1]]), length(coef)))
               `dimnames<-`(out, append(dimnames(coef[[1]]), list(paste0("Comp", 1:length(coef)))))
             }
           },
           ols = {
             coefs <- function(mdl)
             {
               coef <- coef(mdl)
               out <- array(coef, dim = c(nrow(coef), ncol(coef), 1))
               return(out)
             }
           },
           senv = {
             coefs <- function(mdl)
             {
               coef <- lapply(mdl[[2]], function(mat){
                 out <- t(mat[3, ]$senv_fit[[1]])
                 rownames(out) <- c(1:(nrow(out)))
                 colnames(out) <- paste0("Y", 1:ncol(out))
                 return(out)
               })
               out <- array(unlist(coef), dim = c(nrow(coef[[1]]), ncol(coef[[1]]), length(coef)))
               `dimnames<-`(out, append(dimnames(coef[[1]]), list(paste0("Comp", 1:length(coef)))))
             }
           })
    return(coefs)
  }
getPredErr <- 
  function(coef, minerr, trueBeta, sigma, use.formula = TRUE, test = if (!use.formula) NULL){
    out <- map_df(0:dim(coef)[3], function(cmp){
      bdf <- if (cmp == 0) matrix(0, nrow = nrow(coef[, , cmp + 1]), ncol = ncol(coef[, , cmp + 1])) else coef[, , cmp]
      if (!use.formula) {
        test.x <- if (dim(sigma)[1] + 1 == nrow(bdf)) cbind(1, test$x) else test$x
        test.y <- test$y
        yhat <- test.x %*% bdf
        errsq <- (test.y - yhat) ^ 2
        msep <- apply(errsq, 2, mean)
        return(sum(msep))
      }
      beta    <- if (dim(sigma)[1] + 1 == nrow(bdf)) bdf[-1, ] else bdf
      err_out <- t(beta - trueBeta) %*% sigma %*% (beta - trueBeta)
      out <- tibble(
        without_norm = sum(diag(err_out + minerr)),
        with_norm = norm(err_out + minerr, type = "F")
      )
      return(out)
    })
    mutate(out, comp = 0:(n() - 1))
  }

shared_legend <- 
  function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
    require(grid)
    require(gridExtra)
    plots <- list(...)
    position <- match.arg(position)
    g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x) x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    combined <-
      switch(position,
             "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                    legend,
                                    ncol = 1,
                                    heights = unit.c(unit(1, "npc") - lheight, lheight)),
             "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                   legend,
                                   ncol = 2,
                                   widths = unit.c(unit(1, "npc") - lwidth, lwidth))
      )
    return(combined)
  }

getIndivPredErr <-
  function(coef, minerr, trueBeta, sigma){
    out <- map_df(0:dim(coef)[3], function(cmp){
      bdf <- if (cmp == 0) matrix(0, nrow = nrow(coef[, , cmp + 1]), ncol = ncol(coef[, , cmp + 1])) else coef[, , cmp]
      beta    <- if (dim(sigma)[1] + 1 == nrow(bdf)) bdf[-1, ] else bdf
      err_out <- t(beta - trueBeta) %*% sigma %*% (beta - trueBeta)
      out <- as_tibble(
        as.list(
          `names<-`(
            diag(err_out + minerr), 
            paste0("Y", 1:ncol(minerr))
          )
        )
      )
      out <- gather(out, Response, PredErr)
      # out <- tibble(
      #   without_norm = sum(diag(err_out + minerr)),
      #   with_norm = norm(err_out + minerr, type = "F")
      # )
      return(out)
    })
    mutate(out, comp = 0:(n() - 1))
  }

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
  paste(
    str_replace_all(
      as.character(list), "[a-zA-Z()]", ""), 
    collapse = "; "
  )
}

source_if_not <- function(obj_file, source_file) {
  if(file.exists(obj_file)) {
    load(obj_file, envir = .GlobalEnv)
  } else {
    source(source_file, local = TRUE)
  }
}