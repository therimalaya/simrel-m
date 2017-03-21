## ---- Load Object if exist -----------------------------------------------
load_if_not <- function(obj_name, obj_path = ".", expression)
{
  full_path <- paste(file.path(obj_path, obj_name), "rds", sep = ".")
  if (file.exists(full_path)) {
    assign(obj_name, readRDS(full_path), envir = .GlobalEnv)
  } else {
    assign(obj_name, eval(expression), envir = .GlobalEnv)
    saveRDS(get(obj_name), file = full_path)
  }
}

## ---- Oridinary List Square Fit -----------------------------------------------
fit_ols <- function(sim_obj, ...)
{
  if (class(sim_obj) != "simrel") stop("wrong sim_obj")
  train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
  fit <- lm(y ~ x, data = train, ...)
  return(fit)
}

## ---- PCR or PLS model Fit -----------------------------------------------
fit_mvr <- function(sim_obj, mvr_fun = "pcr", ncomp = 10, ...)
{
  if (class(sim_obj) != "simrel") stop("wrong sim_obj")
  train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
  fit <- match.fun(mvr_fun)(y ~ x, data = train, ncomp = ncomp, ...)
  return(fit)
}

## ---- Envelope Model Fit -----------------------------------------------
fit_env <- function(sim_obj, ncomp = 10, type = "x", ...)
{
  if (class(sim_obj) != "simrel") stop("wrong sim_obj")
  train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
  fit <- lapply(1:ncomp, function(nc){
    if (type == "x") {
      out <- with(train, xenv(x, y, u = nc, ...))
    }
    if (type == "y") {
      out <- with(train, env(x, y, u = nc, ...))
    }
    return(out)
  })
  return(fit)
}

##---- Simultanious Envelope Fit ------------------------------
fit_senv <- function(sim_obj, nc.x = 10, nc.nc.y = 3, Fc = 1){
  require(R.matlab)
  
  ##----Start Matlab Server------------------------------
  options(matlab = "/Applications/MATLAB_R2015b.app/bin/matlab")
  Matlab$startServer()
  matlab <- Matlab()
  
  ##----Open Matlab Connection------------------------------
  open(matlab)
  
  evaluate(matlab, 
           "addpath(genpath('~/Documents/MATLAB/EnvelopeAlgorithms/'))", 
           "addpath(genpath('~/Documents/MATLAB/EnvelopeMLM/'))", 
           "addpath(genpath('~/Documents/MATLAB/SDR-Test/'))")
  
  ##----Setting up data----------------
  train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
  with(train, {
    setVariable(matlab, X = x)
    setVariable(matlab, Y = y)
  })
  setVariable(matlab, dx = nc.x)
  setVariable(matlab, dy = nc.Y)
  setVariable(matlab, Fc = Fc)
  
  evaluate(matlab, "simenv = SimulEnv(X, Y, dx, dy, Fc)")
  simenv <- getVariable(matlab, "simenv")
  simenv <- simenv$simenv[,,1]
  close(matlab)
  
  return(simenv)
}


##---- Arranging ggplot in gird ------------------------------
grid_arrange_shared_legend <- 
  function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) 
  {
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
    grid.newpage()
    grid.draw(combined)
    
  }

##---- Some internal functions ------------------------------
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
      if (is.character(y)) y <- .do_parse(y)
      return(y)
    })
  })
}

## ---- Get Beta Coefficient ----------------------------------------
## ---- Beta Function Generator for different Models -----------------
get_beta <- function(model = c('pcr', 'pls', 'cppls', 'mvr', 'xenv', 'bayes', 'linear',
                               'try-error', "list", "lm", "lmm", "yenv", "ols", "senv", "senvelope"))
{
  model <- match.arg(model)
  if (model %in% c('pcr', 'pls', 'cppls')) model <- 'mvr'
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
           coefs <- function(mdl, ncompX = 1:10, ncompY = 3)
           {
             coef <- mdl$beta[ncompX, ncompY]
             coef <- lapply(coef, function(mat) {
               out <- t(mat)
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

## ---- Mean Sum of square of Prediction ----------------------------
msep <- function(true, predicted) colMeans((true - predicted) ^ 2)

## ---- Prediction Error -----------------------------------------------
get_pred_error <- function(sim_obj, fit_obj, model_name, train = TRUE)
{
  train <- data.frame(x = I(sim_obj$X), y = I(sim_obj$Y))
  test <- data.frame(x = I(sim_obj$testX), y = I(sim_obj$testY))
  beta_fun <- get_beta(model_name)
  beta_df <- beta_fun(fit_obj)
  # beta_df[1, 1] <- mean(train$y)
  truebeta <- sim_obj$beta
  minerr <- c(sim_obj$minerror)
  # sigma <- sim_obj$Rotation %*% sim_obj$Sigma[-c(1:sim_obj$m), -c(1:sim_obj$m)] %*% t(sim_obj$Rotation)
  sigma <- sim_obj$Sigma[-c(1:sim_obj$m), -c(1:sim_obj$m)]
  n <- sim_obj$n
  pred_err <- apply(beta_df, 3, function(bdf){
    # bdf <- beta_df[, , bdf.i]
    if (any(model_name %in% c("senvelope", "senv"))) {
      yhat_train <- train$x %*% bdf
      yhat_test <- test$x %*% bdf
    } else {
      yhat_train <- cbind(1, train$x) %*% bdf
      yhat_test <- cbind(1, test$x) %*% bdf
      bdf <- bdf[-1, ]
    }
    mat <- t(bdf - truebeta) %*% cov(test$x) %*% (bdf - truebeta)
    # train <- msep(c(yhat_train), train$y)
    # test <- msep(c(yhat_test), test$y)
    # test <- diag((t(bdf - truebeta) %*% sigma %*% (bdf - truebeta) + sim_obj$minerror) * (n + 1)/n)
    # out <- rbind(train = train, test = test)
    return(norm(mat, type = "F"))
    # test = (minerr + t(beta - truebeta) %*% sigma %*% (beta - truebeta)) * (n + 1)/n
  })
  names(pred_err) <- paste0("Comp", seq_along(pred_err))
  pred_err <- as.data.frame(pred_err)
  pred_err$comp <- rownames(pred_err)
  rownames(pred_err) <- NULL
  # pred_err <- melt(pred_err)
  # names(pred_err) <- c("Estimate", "Response", "Pred_Error", "Comp")
  return(pred_err)
}