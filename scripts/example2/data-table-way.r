## Packages ----
library(data.table)
library(simrel)
library(pls)

## Preparation ----
source("scripts/00-function.r")
load("scripts/example2/app/design.Rdata")

## Simrel Object ----
design <- data.table(design)[, Design := .I]
sim_obj <- design[, .(sobj = list(do.call(simrel, lapply(.SD, function(x) x[[1]])))), by = Design]
sim_obj[, Train := lapply(sobj, function(obj) with(obj, data.frame(x = I(X), y = I(Y))))]
sim_obj[, Test := lapply(sobj, function(obj) with(obj, data.frame(x = I(testX), y = I(testY))))]
setkeyv(sim_obj, "Design")

## Fitted Object ----
fit_obj <- sim_obj[, .(
  pcr = .(pcr(y ~ x, data = Train[[1]], ncomp = 10)),
  pls2 = .(plsr(y ~ x, data = Train[[1]], ncomp = 10)),
  pls1 = .(lapply(1:ncol(Train[[1]]$y), function(yi) {
    plsr(y ~ x, data = within(Train[[1]], y <- y[, yi]), ncomp = 10)
  }))
  ), by = Design]
setkeyv(fit_obj, "Design")

## Error Object ----
