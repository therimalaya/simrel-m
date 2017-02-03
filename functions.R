## ----FitModelFn ---------------------------------------------------------
fit_me<- function(train_df, model = c("pcr", "pls", "envelope", 'ols', 'cppls'),
                  ncomp = 10, trace = FALSE) {

  ## Housekeeping Functions
  try_catch_error <- function(expression, fitError = NULL){
    if (is.null(fitError)) fitError <- list()
    tobj <- try(eval(expression), silent = TRUE)
    # if (!"try-error" %in% class(tobj)) return(tobj)
    # fitError <<- append(fitError, tobj)
    return(tobj)
  }

  ## ---- EnvlopeModelFitFn
  env_fit <- function(X, Y, ncomp){
    lapply(1:ncomp, function(nc){
      xenv(X, Y, u = nc)
      ## if(ncol(Y) < 2) {
      ##   xenv(X, Y, u = nc)
      ## } else {
      ##   env(X, Y, u = ncol(Y))
      ## }
    })
  }

  ## Get things in place
  train <- train_df

  ## Start Making some Expression
  switch(model,
         cppls = {
           fit_expr <- expression(cppls(y ~ x, data = train, scale = FALSE, ncomp = ncomp))
         },
         pcr = {
           fit_expr <- expression(pcr(y ~ x, data = train, scale = FALSE, ncomp = ncomp))
         },
         pls = {
           fit_expr <- expression(plsr(y ~ x, data = train, scale = FALSE, ncomp = ncomp))
         },
         envelope = {
           fit_expr <- expression(with(train, env_fit(x, y, ncomp = ncomp)))
         },
         ols = {
           fit_expr <- expression(with(train, lm(y ~ x, data = train)))
         }
  )
  # ---- Estimation Error
  fit_obj <- try_catch_error(fit_expr)
  # if (any(is.na(fit__obj))) return(NA)

  return(fit_obj) 
}


## ----GetBeta------------------------------------------------------------------
get_beta <- function(model = c('pcr', 'pls', 'mvr', 'envelope', 'ols',
                              'try-error', "list", "lm", "mlm")){
  model <- match.arg(model)
  ## Start Making some Expression
  if (model %in% c('pcr', 'pls')) model <- 'mvr'
  if (model %in% c("lm", "mlm")) model <- "ols"
  if (model %in% c("list")) model <- "envelope"
  if (model %in% c("try-error")) return(model)
  switch(model,
         mvr = {
           beta_fun <- function(mdl, ncomp = 10){
             coef <- mdl$coefficients
             coef <- abind(`0 comps` = array(0, dim = dim(coef)[-3]), coef, along = 3)
             coef <- melt(coef)
             coef <- within(coef, Var3 <- as.numeric(gsub(' comps', '', Var3)))
             names(coef) <- c('predictor', 'response', 'ncomp', 'beta')
             return(coef)
           }
         },
         envelope = {
           beta_fun <- function(mdl, ncomp = ncomp){
             if (length(mdl[[1]]) == 13) {
               mdl <- lapply(mdl, function(obj){
                 within(obj, beta <- t(beta))
               })
               ncomp <- ncol(mdl[[1]]$beta)
             }
             coef <- lapply(mdl, function(obj) obj$beta)
             coef <- array(unlist(coef), dim = c(dim(coef[[1]]), length(coef)))
             coef <- abind(array(0, dim = dim(coef)[-3]), coef, along = 3)
             dm <- dim(coef)
             dimnames(coef) <- list(paste0("X", 1:dm[1]), paste0("Y", 1:dm[2]), paste(0:(dm[3] - 1), "comps"))
             coef <- melt(coef)
             coef <- within(coef, Var3 <- as.numeric(gsub(' comps', '', Var3)))
             names(coef) <- c('predictor', 'response', 'ncomp', 'beta')
             return(coef)
           }
         },
         ols = {
           beta_fun <- function(mdl){
             coef <- coef(mdl)[-1, ]
             rownames(coef) <- gsub("x", "", rownames(coef))
             coef <- abind(array(0, dim = dim(coef)), coef, along = 3)
             coef <- melt(coef)
             names(coef) <- c('predictor', 'response', 'ncomp', 'beta')
             coef$ncomp <- coef$ncomp - 1
             return(coef)
           }
         }
  )
  return(beta_fun)
}

## ----getPredError---------------------------------------------------------------
get_pred_error <- function(train, test, fit_obj){
  mdl <- fit_obj
  model <- class(mdl)
  coef <- try(data.table(get_beta(model[1])(mdl)), silent = TRUE)
  coef <- coef[, dcast(.SD, predictor ~ response, value.var = 'beta'), by = ncomp]
  if ("try-error" %in% class(coef)) return(coef)
  mse <- function(x) sum(x^2)/length(x)
  err <- coef[, .(
    train = apply((train$x %*% as.matrix(.SD) - train$y), 2, mse),
    test = apply((test$x %*% as.matrix(.SD) - test$y), 2, mse)
  ), by = c("ncomp"), .SDcols = paste0("Y", 1:ncol(train$y))]
  err[, response := paste0("Y", 1:.N), by = ncomp]
  return(err)
}

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
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
  combined <- switch(position,
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

}

## Some internal functions
.do_parse <- function(character_string){
  x = unlist(strsplit(character_string, ";"))
  x = gsub("[[:space:]]", "", x)
  y = lapply(x, function(y) unlist(strsplit(y, ",")))
  ret = lapply(y, as.numeric)
  if (!grepl(";", character_string)) ret <- ret[[1]]
  return(ret)
}

.prepare_design <- function(option_list){
  lapply(option_list, function(x){
    lapply(x, function(y){
      if(is.character(y)) y <- .do_parse(y)
      return(y)
    })
  })
}
