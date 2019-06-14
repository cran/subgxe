#' pasta for multi-phenotype analysis
#'
#' Search for the subset that yields the strongest evidence of
#' association and calculate the meta-analytic p-value, possibly in the
#' presence of gene-environmental interaction.
#'
#' @param p.values The p.value of each study.
#' @param study.sizes The sample size of each study.
#' @param cor The correlation matrix of the studies. For example, if each study
#'   is independent, \code{cor} would be the identity matrix.
#' @return A list containing the joint p value and the test statistic, which
#'   contains the optimal subset.
#' @examples
#' # grab synthetic study for example
#' data("studies")
#' n.studies <- 5
#' study.sizes <- c(nrow(studies[[1]]), nrow(studies[[2]]), nrow(studies[[3]]),
#'                    nrow(studies[[4]]), nrow(studies[[5]]))
#' study.pvals <- rep(0, n.studies)
#' # Correlations of p-values among the studies.
#' # In this case the studies were generated independently so its just I
#' cor.matrix <- diag(1, n.studies)
#'# load the lrtest() function to conduct the likelihood ratio test
#'# Used just to generate the input p-values, not required in pasta itself.
#'
#'library(lmtest)
#'
#'for(i in 1:n.studies) {
#'  # model with gene(G) by environment(E) interaction
#'  model <- glm(D ~ G + E + GbyE, data = studies[[i]], family = binomial)
#'  # model without G and GE interaction
#'  null.model <- glm(D ~ E, data = studies[[i]], family = binomial)
#'  # likelihood ratio test from the package lmtest
#'  study.pvals[i] = lmtest::lrtest(null.model, model)[2, 5]
#'}
#'
#'pasta <- pasta(study.pvals, study.sizes, cor.matrix)
#'
#'pasta$p.pasta
#'pasta$test.statistic$selected.subset
#' @references Yu Y, Xia L, Lee S, Zhou X, Stringham H, M, Boehnke M, Mukherjee
#'   B: Subset-Based Analysis Using Gene-Environment Interactions for Discovery
#'   of Genetic Associations across Multiple Studies or Phenotypes. Hum Hered
#'   2019. doi: 10.1159/000496867
#' @export
#' @export
pasta <- function(p.values, study.sizes, cor)
{
  statistic <- test.statistic(p.values, study.sizes)
  p.pasta <- p.dlm(statistic$test.stat, study.sizes, cor)
  list(p.pasta = p.pasta, test.statistic = statistic)
}


p.dlm <- function(test.stat, study.sizes, cor)
{
  all.combn <- expand.grid(rep(list(0:1), length(study.sizes)))
  all.combn <- all.combn[-1,]
  sum(apply(all.combn, 1, function(v)
    stats::integrate(cond.prob.z, test.stat, Inf, v, study.sizes, cor)$value)
  )
}

# p.values is a vector of study/trait-specific p-values
# study.size is a vector of sample sizes of the traits/studies
test.statistic <- function(p.values, study.size)
{
  # In multiple-phenotype analysis, sample sizes are usually the same for all traits
  # total number of studies
  n <- length(p.values)
  Z.stats <- -stats::qnorm(p.values)
  all.combn <- expand.grid(rep(list(0:1), n))
  all.combn <- all.combn[-1,]
  # data frame with the last column being the Z.meta of subset S
  Z.df <- cbind(all.combn, NA)
  colnames(Z.df) <- c(paste0("Study", 1:n), "Z.S")
  rownames(Z.df) <- 1:nrow(all.combn)

  # calculate Z(S) over all possible non-empty subsets
  for (r in 1:nrow(all.combn)) {
    # sample size of studies in a subset
    current.size <- study.size * all.combn[r,]
    current.wt   <- sqrt(current.size / sum(current.size))
    Z.meta <- sum(current.wt * Z.stats)
    Z.df[r, n + 1] <- Z.meta
  }
  list(test.stat = max(Z.df$Z.S), Z.df = Z.df,
       selected.subset = all.combn[which.max(Z.df$Z.S),])
}

cond.prob.z <- function(z, subset, study.sizes, cor)
{
  current.size <- subset * study.sizes
  subset.size <- sum(current.size)
  # weights of studies in current subset
  current.wt <- sqrt(current.size / sum(current.size))
  # weights of studies in current subset
  n <- length(study.sizes)
  p <- 0
  # integrate over all studies not included in subset
  for (k in 1:n) {
    e <- rep(0, n)
    e[k] <- 1
    A <- rbind(e, current.wt)
    sigma <- A %*% cor %*% t(A)
    cond.mean <- sigma[1, 2] / sigma[2, 2] * z
    cond.sigma  <- sqrt(sigma[1, 1] - sigma[1, 2] / sigma[2, 2] * sigma[2, 1])
    a <- subset.size / (subset.size + study.sizes[k])
    if (k %in% which(subset == 0)) {
      # conditional probability given Z_gamma = z
      a <- subset.size / (subset.size + study.sizes[k])
      p <- p + log(stats::pnorm(z * (1 - sqrt(a)) / sqrt(1 - a), cond.mean,
                                  cond.sigma))
    }
    # k-th study included in subset
    else {
      b <- subset.size / (subset.size - study.sizes[k])
      if (sum(subset) > 1)
        p <- p + log(stats::pnorm((z * (sqrt(b) - 1)) / sqrt(b - 1), cond.mean,
                                    cond.sigma, lower.tail = F))
    }
  }

  exp(p) * stats::dnorm(z, sd = sqrt(t(current.wt) %*% cor %*% current.wt))
}
