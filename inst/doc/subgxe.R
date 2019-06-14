## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup---------------------------------------------------------------
# library(devtools)
# install_github("umich-cphds/subgxe", build_opts = c())
library(subgxe)

## ----pvalues, message = FALSE--------------------------------------------

library(lmtest)

K <- 5 # number of studies
study.pvals.marg <- NULL
study.pvals.joint <- NULL

for(i in 1:K){
  joint.model <- glm(D ~ G + E + I(G*E), data=studies[[i]], family="binomial")
  null.model <- glm(D ~ E, data=studies[[i]], family="binomial")
  marg.model <- glm(D ~ G, data=studies[[i]], family="binomial")
  study.pvals.marg[i] <- summary(marg.model)$coef[2,4]
  study.pvals.joint[i] <- lmtest::lrtest(null.model, joint.model)[2,5]
}

## ----pasta---------------------------------------------------------------

study.sizes <- c(nrow(studies[[1]]), nrow(studies[[2]]), nrow(studies[[3]]),
                 nrow(studies[[4]]), nrow(studies[[5]]))

cor.matrix <- diag(1, K)
pasta.joint <- pasta(p.values=study.pvals.joint, study.sizes=study.sizes, cor=cor.matrix)
pasta.marg <- pasta(p.values=study.pvals.marg, study.sizes=study.sizes, cor=cor.matrix)

pasta.joint$p.pasta # delete 'joint'
pasta.joint$test.statistic$selected.subset

pasta.marg$p.pasta # delete 'joint'
pasta.marg$test.statistic$selected.subset

