library(tidyverse)
library(data.table)

library(simulatr)

## ---- Parameters ----------------------------------------
n <- 25
p <- 11
m <- 4
q <- c(3, 3, 3)
relpos <- list(c(1, 2), c(3, 4), c(5, 6))
R2 <- c(0.8, 0.9, 0.7)
gamma <- 0.8
ypos <- list(c(1, 4), 2, 3)
type <- "multivariate"

## ---- Simrel Object ----------------------------------------
simObj <-
  simulatr(n = n, p = p, m = m, q = q, relpos = relpos,
           R2 = R2, gamma = gamma, ypos = ypos, type = type)

## ---- Covariance Matrix ----------------------------------------
sigma.wz <- simObj[["SigmaWZ"]]
sigma.yx <- simObj[["Sigma"]]
dimnames(sigma.wz) <- list(c(paste0("W", 1:4), paste0("Z", 1:11)), c(paste0("W", 1:4), paste0("Z", 1:11)))
dimnames(sigma.yx) <- list(c(paste0("Y", 1:4), paste0("X", 1:11)), c(paste0("Y", 1:4), paste0("X", 1:11)))

fill.wz <- sigma.wz != 0
fill.yx <- sigma.yx != 0

## ---- DataFrames ----------------------------------------
df.wz <- data.table(melt(abs(sigma.wz)))
df.wz[, c("x", "y") := map(.SD, ~as.numeric(gsub("[[:alpha:]]", "", .x))),
      .SDcols = 1:2]
df.wz[, matrix := paste0(substr(Var1, 0, 1), ":", substr(Var2, 0, 1))]
df.wz[, paste0("mat", 1:2) := strsplit(matrix, ":") %>% transpose]


ggplot(df.wz, aes(x, y, fill = matrix, alpha = value)) + 
  geom_tile() +
  scale_y_reverse() +
  coord_equal() +
  facet_grid(mat2 ~ mat1, space = "free", scales = 'free') +
  theme_void()


names(df) <- c("x", "y", "value")

ggplot(df, aes(x, y, fill = value)) +
  geom_tile(alpha = ifelse(c(S.WZ) != 0, 1, 0),
            color = "lightgray",
            size = 0.5) +
  coord_equal() +
  scale_y_reverse(breaks = 1:15) +
  scale_x_continuous(breaks = 1:15) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(x = "", y = "")

relpred <- simObj$relpred
irrelpred <- setdiff(1:p, relpred)

S.YX <- abs(simObj$Sigma)
relpred <- unname(unlist(lapply(simObj$relpred, `+`, m)))
irrelpred <- setdiff((m + 1):(p + m), relpred)
idx <- c(1:m, relpred, irrelpred)

df <- reshape2::melt(abs(S.YX))
names(df) <- c("x", "y", "value")

ggplot(df, aes(x, y, fill = value)) +
  geom_tile(alpha = ifelse(c(S.YX) != 0, 1, 0),
            color = "lightgray",
            size = 0.5) +
  coord_equal() +
  scale_y_reverse(breaks = idx) +
  scale_x_continuous(breaks = idx) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(x = "", y = "")
