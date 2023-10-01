#' Basic Unit Level Model (Battese-Harter-Fuller model) using Hierarchical Bayesian Approach
#'
#' @description This function gives the Hierarchical Bayesian (HB) based on a basic unit level model (Battese-Harter-Fuller model).
#'
#' @references
#' \enumerate{
#'  \item Battese, G. E., Harter, R. M., & Fuller, W. A. (1988). An error-components model for prediction of county crop areas using survey and satellite data. Journal of the American Statistical Association, 83(401), 28-36.
#'  \item Rao, J. N., & Molina, I. (2015). Small area estimation. John Wiley & Sons.
#' }
#'
#' @param formula an object of class formula that contains a description of the model to be fitted. The variables included in the formula must be contained in the data.
#' @param data_unit data frame containing the variables named in \code(formula) and \code(domain).
#' @param data_area data frame containing the variables named in \code(formula) and \code(domain). Each remaining column contains the population means of each of the p auxiliary variables for the D domains.
#' @param domain Character or formula for domain column names in unit data \code(data_unit) and area data \code(data_area). (example : "County" or ~County)
#' @param iter.update Number of updates with default 3
#' @param iter.mcmc Number of total iterations per chain with default 10000
#' @param coef a vector contains prior initial value of Coefficient of Regression Model for fixed effect with default vector of 0 with the length of the number of regression coefficients
#' @param var.coef a vector contains prior initial value of variance of Coefficient of Regression Model with default vector of 1 with the length of the number of regression coefficients
#' @param thin Thinning rate, must be a positive integer with default 2
#' @param burn.in Number of iterations to discard at the beginning with default 2000
#' @param tau.u Prior initial value of inverse of Variance of area random effect with default 1
#' @param seed number used to initialize a pseudorandom number generator (default seed = 1). The random number generator method used is "base::Wichmann-Hill".
#' @param quiet if TRUE then messages generated during compilation will be suppressed (default TRUE).
#'
#' @return The function returns a list with the following objects : Estimation \code{Est}, random effect variance \code{refVar}, beta coefficient \code{Coefficient} and MCMC result \code{result_mcmc}
#'
#' @export
#'
#' @examples
#' library(dplyr)
#'
#' Xarea <- cornsoybeanmeans %>%
#'    dplyr::select(
#'       County = CountyIndex,
#'       CornPix = MeanCornPixPerSeg,
#'       SoyBeansPix = MeanSoyBeansPixPerSeg
#'    )
#'
#' corn_model <- hb_BHF(
#'    CornHec ~ SoyBeansPix + CornPix,
#'    data_unit = cornsoybean,
#'    data_area = Xarea,
#'    domain = "County",
#'    iter.update = 20
#' )
#'

hb_BHF <- function(formula, data_unit, data_area, domain, iter.update = 3, iter.mcmc = 10000, coef, var.coef, thin = 3, burn.in = 2000, tau.u = 1, seed = 1, quiet = TRUE){
  result <- list(Est = NA, refVar = NA, coefficient = NA, result_mcmc = NA)
  formuladata <- stats::model.frame(formula, data_unit, na.action = NULL)

  # Pengecekan input ---------------------
  if (any(is.na(formuladata[, -1]))) {
    stop("Auxiliary Variables contains NA values.")
  }
  # banyaknya variabel (termasuk Beta0)
  nvar <- ncol(formuladata)

  if (!missing(var.coef)) {
    if (length(var.coef) != nvar) {
      stop("length of vector var.coef does not match the number of regression coefficients, the length must be ", nvar)
    }
    tau.b <- 1 / var.coef
  } else {
    tau.b <- 1 / rep(1, nvar)
  }

  if (!missing(coef)) {
    if (length(coef) != nvar) {
      stop("length of vector coef does not match the number of regression coefficients, the length must be ", nvar)
    }
    mu.b <- coef
  } else {
    mu.b <- rep(0, nvar)
  }
  if (iter.update < 3) {
    stop("the number of iteration updates at least 3 times")
  }

  if (quiet) {
    cli::cli_progress_bar(
      total = iter.update,
      format = "Update {iter}/{iter.update} | {cli::pb_bar} {cli::pb_percent} | ETA: {cli::pb_eta} \n"
    )
    my_env <- environment()
    default_pb <- "none"
    update_progress <- function(iter, iter.update){
      Sys.sleep(2/10000)
      cli::cli_progress_update(set = iter, .envir = my_env)
    }
  }else{
    default_pb <- "text"
    update_progress <- function(iter, iter.update){
      cli::cli_h1('Update {iter}/{iter.update}')
    }
  }


  # Model ------------------
  # Jika tidak ada NA
  if (!any(is.na(formuladata[, 1]))) {
    formuladata <- as.matrix(stats::na.omit(formuladata))

    J <- nrow(data_unit)
    d <- .get_variable(data_unit, domain)
    m <- nrow(data_area)

    X_unit <- stats::model.matrix(formula, data = as.data.frame(formuladata))
    X_unit <- as.matrix(X_unit)
    X_area <- stats::model.matrix(stats::update(formula, NULL ~ .), data = as.data.frame(data_area))
    X_area <- as.matrix(X_area)

    tau.ua <- tau.ub <- tau.ea <- tau.eb <- 1
    a.var <- 1

    my_model <- "model {
				for (j in 1:J) {
						y[j] ~ dnorm(Mu[j],tau.e)
						Mu[j] <- b[1] + sum(b[2:nvar] * x[j,]) + u[d[j]]
				}

				for(i in 1:m){
					u[i] ~ dnorm(0,tau.u)
					mu[i] <- b[1] + sum(b[2:nvar] * X[i,]) + u[i]
				}

				for (k in 1:nvar){
					 b[k] ~ dnorm(mu.b[k], tau.b[k])
				}

				tau.e ~ dgamma(tau.ea,tau.eb)
				tau.u ~ dgamma(tau.ua,tau.ub)
				a.var <- 1 / tau.u
		}"


    for (iter in 1:iter.update) {
      update_progress(iter, iter.update)

      dat <- list(
        nvar = nvar,
        J = J, m = m, d = d,
        y = formuladata[, 1],
        x = as.matrix(X_unit[, -1]),
        X = as.matrix(X_area[, -1]),
        mu.b = mu.b,
        tau.b = tau.b,
        tau.ua = tau.ua, tau.ub = tau.ub,
        tau.ea = tau.ea, tau.eb = tau.eb
      )

      inits <- list(
        u = rep(0, m),
        b = mu.b,
        tau.u = 1,
        tau.e = 1,
        .RNG.name = "base::Wichmann-Hill",
        .RNG.seed = seed
      )

      jags.m <- rjags::jags.model(
        file = textConnection(my_model),
        data = dat, inits = inits,
        n.chains = 1, n.adapt = 500,
        quiet = quiet
      )

      params <- c("mu", "a.var", "b", "tau.u", "tau.e")
      samps1 <- rjags::coda.samples(
        jags.m, params, n.iter = iter.mcmc,
        thin = thin, progress.bar = default_pb
      )
      samps11 <- stats::window(samps1, start = burn.in + 1, end = iter.mcmc - 1)
      hasil <- summary(samps11)

      a.var <- hasil$statistics[1]
      beta <- hasil$statistics[2:(nvar + 1), 1:2]
      mu.b <- beta[, 1]
      tau.b <- 1 / (beta[, 2]^2)

      tau.e <- hasil$statistics[nvar + m + 2, 1:2]
      tau.u <- hasil$statistics[nvar + m + 3, 1:2]

      tau.ea <- tau.e[1]^2 / tau.e[2]^2
      tau.eb <- tau.e[1] / tau.e[2]^2
      tau.ua <- tau.u[1]^2 / tau.u[2]^2
      tau.ub <- tau.u[1] / tau.u[2]^2
    }

    result_samps <- summary(samps1)
    b.varnames <- c('intercept', all.vars(formula[[3]]))

    result_mcmc <- samps1[, c(2:(nvar + 1))]
    colnames(result_mcmc[[1]]) <- b.varnames
    a.var <- result_samps$statistics[1]
    beta <- result_samps$statistics[2:(nvar + 1), 1:2]
    rownames(beta) <- b.varnames
    mu <- result_samps$statistics[(nvar + 2):(1 + nvar + m), 1:2]

    Estimation <- data.frame(mu)
    Quantiles <- as.data.frame(result_samps$quantiles[2:(1 + nvar + m), ])
    q_mu <- Quantiles[-c(1:nvar), ]
    q_beta <- Quantiles[1:nvar, ]
    rownames(q_beta) <- b.varnames
    beta <- cbind(beta, q_beta)
    Estimation <- data.frame(Estimation, q_mu)
    colnames(Estimation) <- c(
      "MEAN", "SD", "2.5%", "25%",
      "50%", "75%", "97.5%"
    )
    rownames(Estimation) <- .get_variable(data_area, domain)
  }else{
    # Jika ada NA
    J <- nrow(data_unit)
    m <- nrow(data_area)

    unit_sampled <- stats::na.omit(data_unit)
    M1 <- nrow(unit_sampled)
    d1 <- .get_variable(unit_sampled, domain)

    data_area$idx <- 1:m
    area_sampled <- stats::na.omit(data_area)
    # area_ns <- dplyr::filter(data_area, !idx %in% area_sampled$idx)
    area_ns <- data_area[-area_sampled$idx, ]
    m1 <- nrow(area_sampled)
    m2 <- nrow(area_ns)
    r <- c(area_sampled$idx, area_ns$idx)

    formuladata <- stats::model.frame(formula, data = unit_sampled, na.action = NULL)
    X_unit <- stats::model.matrix(formula, data = as.data.frame(formuladata))
    X_unit <- as.matrix(X_unit)

    formula_x <- stats::update(formula, NULL ~ .)
    X_area <- stats::model.matrix(formula_x, data = as.data.frame(area_sampled))
    X_area <- as.matrix(X_area)
    X_area_ns <- stats::model.matrix.lm(formula_x, data = as.data.frame(area_ns), na.action = stats::na.pass)
    X_area_ns <- as.matrix(X_area_ns)

    tau.ua <- tau.ub <- tau.ea <- tau.eb <- 1
    a.var <- 1

    my_model <- "model {
			for (j in 1:M1) {
					y[j] ~ dnorm(Mu[j], tau.e)
					Mu[j] <- b[1] + sum(b[2:nvar] * x[j,]) + u[d1[j]]
			}

			for(i in 1:m1){
			  u[i] ~ dnorm(0, tau.u)
			  mu[i] <- b[1] + sum(b[2:nvar] * X[i,]) + u[i]
			}

			for (k in 1:m2) {
				v[k] ~ dnorm(0, tau.u)
				muT[k] <- b[1] + sum(b[2:nvar] * XT[k,]) + v[k]
			}

			for (k in 1:nvar){
				 b[k] ~ dnorm(mu.b[k], tau.b[k])
			}

			tau.e ~ dgamma(tau.ea, tau.eb)
			tau.u ~ dgamma(tau.ua, tau.ub)
			a.var <- 1 / tau.u
	  }"

    for (iter in 1:iter.update) {
      update_progress(iter, iter.update)
      dat <- list(
        nvar = nvar,
        M1 = M1,
        m1 = m1, m2 = m2, d1 = d1,
        y = formuladata[, 1],
        x = as.matrix(X_unit[, -1]),
        X = as.matrix(X_area[, -1]),
        XT = as.matrix(X_area_ns[, -1]),
        mu.b = mu.b,
        tau.b = tau.b,
        tau.ua = tau.ua, tau.ub = tau.ub,
        tau.ea = tau.ea, tau.eb = tau.eb
      ) # names list of numbers

      inits <- list(
        u = rep(0, m1),
        v = rep(0, m2),
        b = mu.b,
        tau.u = 1,
        tau.e = 1
      )

      jags.m <- rjags::jags.model(
        file = textConnection(my_model),
        data = dat, inits = inits,
        n.chains = 1, n.adapt = 500, quiet = quiet
      )

      params <- c("mu", "muT", "a.var", "b", "tau.u", "tau.e")
      samps1 <- rjags::coda.samples(
        jags.m, params,
        n.iter = iter.mcmc, thin = thin,
        progress.bar = default_pb
      )
      # start is greater than n.adapt + burnin
      samps11 <- stats::window(samps1, start = burn.in + 1, end = iter.mcmc - 1)
      hasil <- summary(samps11)

      a.var <- hasil$statistics[1]
      beta <- hasil$statistics[2:(nvar + 1), 1:2]
      mu.b <- beta[, 1]
      tau.b <- 1 / (beta[, 2]^2)

      tau.e <- hasil$statistics[nvar + m + 2, 1:2]
      tau.u <- hasil$statistics[nvar + m + 3, 1:2]

      tau.ea <- tau.e[1]^2 / tau.e[2]^2
      tau.eb <- tau.e[1] / tau.e[2]^2
      tau.ua <- tau.u[1]^2 / tau.u[2]^2
      tau.ub <- tau.u[1] / tau.u[2]^2
    }


    result_samps <- summary(samps1)
    b.varnames <- c('intercept', all.vars(formula[[3]]))

    result_mcmc <- samps1[, c(2:(nvar + 1))]
    colnames(result_mcmc[[1]]) <- b.varnames
    a.var <- result_samps$statistics[1]
    beta <- result_samps$statistics[2:(nvar + 1), 1:2]
    rownames(beta) <- b.varnames
    mu <- result_samps$statistics[(nvar + 2):(1 + nvar + m), 1:2]

    Estimation <- data.frame(mu)
    Quantiles <- as.data.frame(result_samps$quantiles[2:(1 + nvar + m), ])
    q_mu <- Quantiles[-c(1:nvar), ]
    q_beta <- Quantiles[1:nvar, ]
    rownames(q_beta) <- b.varnames
    beta <- cbind(beta, q_beta)
    Estimation <- data.frame(Estimation, q_mu)
    colnames(Estimation) <- c(
      "MEAN", "SD", "2.5%", "25%",
      "50%", "75%", "97.5%"
    )
    # urutkan hasil estimasi seperti data awal
    Estimation <- dplyr::slice(Estimation, match(1:m, r))
    rownames(Estimation) <- .get_variable(data_area, domain)
  }

  result$Est <- Estimation
  result$coefficient <- beta
  result$refVar <- a.var
  result$result_mcmc <- result_mcmc
  class(result) <- 'saehb'

  graphics::par(mar = c(2, 2, 2, 2))
  coda::autocorr.plot(result_mcmc, col = "brown2", lwd = 2)
  plot(result_mcmc, col = "brown2", lwd = 2)

  cli::cli_h1('Coefficient')
  stats::printCoefmat(beta)
  return(invisible(result))
}




# Fungsi bantuan ----------------------------------------------------------
.get_variable <- function(data, variable) {
  if (length(variable) == nrow(data)) {
    return(variable)
  } else if (methods::is(variable, "character")) {
    if (variable %in% colnames(data)) {
      variable <- data[[variable]]
    }else{
      cli::cli_abort('variable "{variable}" is not found in the data')
    }
  } else if (methods::is(variable, "formula")) {
    # extract column name (class character) from formula
    variable <- data[[all.vars(variable)]]
  } else {
    cli::cli_abort('variable "{variable}" is not found in the data')
  }
  return(variable)
}
