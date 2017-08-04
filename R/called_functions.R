## =============================================================================
## Random effects ERGM, Functions for Monte Carlo Simulations
## =============================================================================

require(lattice)
require(network)
require(ergm)
require(sna)
require(lme4)
require(latentnet)
require(snow)
require(rlecuyer)

## -----------------------------------------------------------------------------
## Functions for preparing the data.
## -----------------------------------------------------------------------------

## This function converts a matrix of indicators (such as a matrix indicating
## the sender or receiver) to vector(s) of indicators appropriate for use in a
## random effects model. NOTE: The resultant vector is converted to factor
## form. If directed=FALSE, then two sociality vectors are created, *each with
## the same factor levels*, which is the proper form for use as multiple
## membership models in MCMCglmm.
to_indicator <- function(X, directed=FALSE)
{
  if (isTRUE(directed)) {
    stop("Directed networks not yet supported.")
  } else {
    X[,1] <- paste0("Sociality", X[,1])
    X[,2] <- paste0("Sociality", X[,2])
    factor.levels <- unique(unlist(X))
    X[,1] <- factor(X[,1], levels=factor.levels)
    X[,2] <- factor(X[,2], levels=factor.levels)
  }

  X
}

## Function for preparing the data for use in a random effects model.
prepare_fergm_data <- function(net, form, verbose=FALSE)
{
  ## Temporary until directed code is added.
  if (is.directed(net)) stop("Directed networks not yet supported.")

  if (isTRUE(verbose)) cat("\n## Preparing FERGM dataset...")
  nodes <- nrow(as.matrix(net))
  ndyads <- network.dyadcount(net)
  form <- as.formula(paste("net ~", form))

  if (isTRUE(verbose)) cat("\n##   building array...")
  dta.array <- ergmMPLE(form, output="array", maxMPLEsamplesize=+Inf,
                        control=control.ergm(MPLE.max.dyad.types=ndyads*10))

  if (isTRUE(verbose)) cat("\n##   building data.frame...")
  ncoef <- length(dta.array$predictor[1,2,])
  dta <- matrix(0, nrow=ndyads, ncol=3+ncoef)

  idx <- 1
  for (tail in 1:(nodes-1)) {
    for (head in (tail+1):nodes) {
      dta[idx,] <- c(dta.array$response[tail, head],
                     dta.array$predictor[tail, head, ],
                     tail,
                     head)
      idx <- idx+1
    }
  }

  dta <- data.frame(dta)
  nm <- c("Y", names(dta.array$predictor[tail, head, ]),
          "Sociality1", "Sociality2")
  names(dta) <- nm

  if (isTRUE(verbose)) cat("\n##   setting random effects indicators...\n")
  if (is.directed(net)) {
    stop("Directed networks not yet supported.")
  } else {
    Soc <- to_indicator(dta[,c("Sociality1", "Sociality2")])
    dta[, "Sociality1"] <- Soc[,1]
    dta[, "Sociality2"] <- Soc[,2]
  }

  dta
}

prepare_stan_data <- function(net, dta)
{
  x <- dta[,2:(ncol(dta)-2)]
  y <- dta[,1]
  idx1 <- as.numeric(dta$Sociality1)
  idx2 <- as.numeric(dta$Sociality2)

  list(K = ncol(x),
       N = ncol(net[,]),
       D = nrow(x),
       x = x,
       y = y,
       node1_idx = idx1,
       node2_idx = idx2)
}

## Return the density of the network or list of networks. This is just a
## list-compatible wrapper around network.density(), which is faster than
## gden().
get_density <- function(lst)
{
  if (is.network(lst))
    network.density(lst)
  else
    sapply(lst, network.density)
}

## Simulate a set of networks with a given specification. `form' needs to be a
## one-sided formula in text form without the `~'. The length of `coef' needs to
## agree with `form'.
sim_networks <- function(n, form, coef, density=0.15, directed=FALSE,
                         X.mu=0, X.var=1.0, X.dist="gaussian",
                         nsim=100, seed=NULL, verbose=TRUE,
                         control=control.simulate(
                           MCMC.burnin=50000,
                           MCMC.interval=100))
{
  if (!is.null(seed))
    set.seed(seed)

  if (isTRUE(verbose)) cat("\n## Generating base network...")
  base <- network(n, density=density, directed=directed)
  base %v% "X" <- sample(LETTERS[1:2], n, replace=TRUE)


  if (X.dist == "gaussian") {
    base %v% "sigma" <- rnorm(n, mean=X.mu, sd=X.var)
  } else {
    base %v% "sigma" <- rnorm(n, mean=X.mu, sd=X.var)
  }

  if (isTRUE(verbose)) cat("\n## Simulating network(s)...\n")
  sims <- simulate(as.formula(paste0("~", form)), nsim=nsim,
                   coef=coef, basis=base, control=control)
  sims
}

## A simple wrapper around sim_networks to consistently take care of saving
## data, etc. 48 networks matches the number of cores that will be utilized on
## the OSC cluster. 21 batches of each network size/gamma value are simulated,
## resulting in 1008 networks for each.
sim_wrapper <- function(nodes, batch_size=48, verbose=TRUE)
{
  ## Note: the DGP changed from the results in the NSF and Polnet papers:
  ## nodecov captures the *sum* of two nodal covariates. This isn't how we
  ## explain the DGP in the paper. We want the absdiff (absolute difference)
  ## in the nodal covariates so that those nodes farther apart on the
  ## covariates are less likely to have a tie.
  ## OLD FORM: form <- "edges + gwesp(0.75, fixed=TRUE) + nodematch('X') + nodecov('sigma')"
  form <- "edges + gwesp(0.75, fixed=TRUE) + nodematch('X') + absdiff('sigma')"

  ## Needs to be set to the base directory where the tree of simulated
  ## networks begins.
  ## base_dir <- paste0("~/uni/research/papers/re-ergm/monte-carlo/osc/", nodes)

  gamma <- c(0, -0.25, -0.50, -0.75, -1.00)
  batches <- 1:21
  seeds <- sample(10000:99999, length(batches)*length(gamma))
  seed.idx <- 1

  if (isTRUE(verbose)) cat("\n## Simulating", nodes, "node networks ...\n")
  for (g in gamma) {
    if (isTRUE(verbose)) cat("   gamma:", g, "\n     batch: ")

    for (b in batches) {
      if (isTRUE(verbose)) cat(b, " ")

      coef <- c(-3.25, 0.75, 0.25, g)
      setwd(paste0(base_dir, "/", -(g*100), "/", b))
      Nets <- sim_networks(nodes, form, coef, nsim=batch_size,
                           seed=seeds[seed.idx], verbose=FALSE)
      Batch <- list(Nets=Nets, seed=seeds[seed.idx])

      save(Batch, file="ergm_nets.RData", compress="bzip2")
      seed.idx <- seed.idx + 1
    }

    if (isTRUE(verbose)) cat("\n")
  }
  setwd(base_dir)
}


## -----------------------------------------------------------------------------
## Estimation functions
## -----------------------------------------------------------------------------

est_ergm <- function(net, form, verbose=FALSE)
{
  message("## -----------------------------------------------------------------------------")
  message("## NETWORK ", N)
  message("## -----------------------------------------------------------------------------")

  form <- as.formula(paste("net ~", form))
  est <- NA

  ## ROUND 1: This round is meant to quickly get some reasonable estimates.
  if (isTRUE(verbose)) message("## Estimating model with ERGM, Round 1")
  try(est <- ergm(form, control=control.ergm(
    MCMLE.maxit=4,
    MCMLE.steplength=1,
    MCMLE.steplength.margin=0.05,
    parallel=1),
    verbose=verbose))

  ## ROUND 2: Refine the estimate.
  if (isTRUE(verbose)) message("## Estimating model with ERGM, Round 2")
  if (class(est) == "ergm") {
    CURRENT <- coef(est)
    try(est <- ergm(form, control=control.ergm(init=CURRENT,
                                               MCMLE.maxit=20,
                                               parallel=1),
                    verbose=verbose))
  } else {
    message("## Estimate failed, attempting restart")
    try(est <- ergm(form, control=control.ergm(parallel=1,
                                               MCMLE.maxit=20),
                    verbose=verbose))
  }

  ## Loop until convergence (or a total of 10 interations)
  if (isTRUE(verbose)) message("## Estimating model with ERGM, Round 3")

  iter <- 1
  while ((class(est) != "ergm" && iter <= 10) ||
         (class(est) == "ergm" && est$iterations == 20 && iter <= 10)) {
    if (isTRUE(verbose))
      message("## Failed convergence, re-estimating, iteration: ", iter)

    if (class(est) == "ergm") {
      CURRENT <- coef(est)
      try(est <- ergm(form, control=control.ergm(init=CURRENT,
                                                 MCMLE.maxit=20,
                                                 parallel=2),
                      verbose=verbose))
    } else {
      try(est <- ergm(form,
                      control=control.ergm(parallel=2, MCMLE.maxit=20),
                      verbose=verbose))
    }

    iter <- iter + 1
  }

  N <<- N + 1

  est
}

est_stan <- function(net)
{
  f <- "edges + gwesp(0.75, fixed=TRUE) + nodematch('X')"
  dta <- prepare_fergm_data(net, f, verbose=TRUE)
  dta <- prepare_stan_data(net, dta)
  stan.fit <- stan(file="../fergm-undirected.stan",
                   data=dta, chains=4, warmup=200, iter=700)
  summary(stan.fit)$summary
}
