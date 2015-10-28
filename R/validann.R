#--------------------------------------------------
#' @title Validate Artificial Neural Networks.
#'
#' @description Compute metrics and statistics for predictive, replicative
#'    and/or structural validation of artificial neural networks (ANNs).
#'
#' @param net   an object of class `ann' (as returned by function
#'    \code{\link{ann}}) or `nnet' (as returned using \code{\link[nnet]{nnet}}).
#'    This is a list object comprising information about the fitted ANN model,
#'    including values of weights, fitted target values, number of layers and
#'    numbers of nodes in each layer, for example.
#' @param obs,sim   vectors comprising observed (\code{obs}) and simulated
#'    (\code{sim}) examples of a single response variable. These vectors are
#'    used to compute model fit statistics. Optional if \code{net} is supplied
#'    (see `Details').
#' @param x   (optional) matrix, data frame or vector of input data used for
#'    fitting \code{net} object. A vector is considered to comprise examples of
#'    a single input or predictor variable. While \code{x} is optional,
#'    sensitivity analyses useful for structural validation cannot be performed
#'    if it is missing.
#' @param npar   (optional) integer; number of model parameters (ANN weights).
#'    If not supplied, several predictive validation metrics will not be
#'    computed.
# @param \dots    Arguments to be passed to different validation methods,
#     see specific formulations for details.
#' @return   list object of class `validann' with components dependent on
#'    arguments passed to \code{validann} function:
#'
#' \item{predictive}{logical; if \code{TRUE}, metrics and statistics used
#'    for predictive validation have been computed.}
#' \item{replicative}{logical; if \code{TRUE}, metrics and statistics used
#'    for replicative validation have been computed.}
#' \item{structural}{logical; if \code{TRUE}, metrics and statistics used
#'    for structural validation have been computed.}
#' \item{metrics}{a data frame consisting of metrics:
#'
#'    AME, PDIFF, MAE, ME, RMSE, R4MS4E, AIC, BIC, NSC, RAE, PEP, MARE,
#'    MdAPE, MRE, MSRE, RVE, RSqr, IoAd, CE, PI, MSLE, MSDE, IRMSE, VE,
#'    KGE, SSE and R.
#'
#'    See Dawson et al. (2007) for definitions.}
#' \item{obs_stats}{a data frame consisting of summary statistics about the
#'    \code{obs} dataset including mean, minimum, maximum, variance,
#'    standard deviation, skewness and kurtosis.}
#' \item{sim_stats}{a data frame consisting of summary statistics about the
#'    \code{sim} dataset including mean, minimum, maximum, variance,
#'    standard deviation, skewness and kurtosis.}
#' \item{residuals}{a 1-column matrix of model residuals (\code{sim - obs}).}
#' \item{resid_stats}{a data frame consisting of summary statistics about the
#'    model \code{residuals} including mean, minimum, maximum, variance,
#'    standard deviation, skewness and kurtosis.}
#' \item{ri}{a data frame consisting `relative importance' values for each input
#'    computed using the overall connection weight (ocw), modified ocw (ocw.mod)
#'    and Garson's (gars) methods. Only returned if \code{net} is supplied.
#'    See ***refs}
#' \item{y_hat}{a matrix of dimension \code{c(101, ncol(x))} of model response
#'    values indicating the local sensitivity of the model to each input in
#'    \code{x}. Only returned if \code{net} and \code{x} are supplied.
#'
#'    The response values returned in \code{y_hat} are calculated by carrying
#'    out a local sensitivity analysis, whereby the sensitivity of the model
#'    with respect to each input in \code{x} is considered successively.
#'    For each input of interest \code{x[,i]}, synthetic data are generated
#'    such that inputs \code{x[,-i]} are fixed to their mean values
#'    (as calculated from \code{x}), while input \code{x[,i]} is varied between
#'    its minimum and maximum value, increasing in increments of 1\% (giving
#'    101 synthetic values of \code{x[,i]}). These data are input into
#'    \code{net} and the corresponding model response values are computed and
#'    returned in \code{y_hat[,i]}. This process is repeated for each input
#'    variable in \code{x}. See Shahin et al. (2005) for further details.}
#' \item{rs}{a matrix of dimension \code{dim(x)} of `relative sensitivity'
#'    values for each input in \code{x} given the model output values
#'    (i.e. \code{sim}). Only returned if \code{net} and \code{x} are
#'    supplied and \code{net} is of class `ann'.
#'
#'    The values in \code{rs} are calculated according to the partial
#'    derivative (P.D.) sensitivity analysis method described in Mount et al.
#'    (2013), which involves computing the first-order partial derivatives of
#'    the ANN output with respect to each input. \code{net} must be of class
#'    `ann' in order to access partial derivatives of the hidden layer nodes as
#'    returned by \code{\link{ann}}.}
#' @details   To compute all (predictive, replicative and structural) types of
#' validation metrics and statistics, \code{net} must be supplied and must be
#' of class `ann' (as returned by \code{\link{ann}}) or `nnet' (as returned by
#' \code{\link[nnet]{nnet}}). However, a partial derivative (P.D.)
#' sensitivity analysis (useful for structural validation) will only be carried
#' out if \code{net} is of class `ann'.
#'
#' If \code{obs} and \code{sim} data are supplied, predictive and replicative
#' validation metrics are computed based on these. Otherwise, these results
#' are computed based on \code{obs} and \code{sim} datasets derived from the
#' \code{net} object (i.e. the pre-processed observed and simulated data used
#' for training \code{net}). As such, both \code{obs} and \code{sim} must be
#' supplied if validation is to be based on data not used for training or
#' unprocessed training data. If either \code{obs} or \code{sim} is specified
#' but the other isn't, both \code{obs} and \code{sim} will be derived from
#' \code{net} if supplied. Similarly, this will occur if \code{obs} and
#' \code{sim} are of different lengths.
#'
#' If \code{net} is not supplied, both \code{obs} and
#' \code{sim} are required and structural validation metrics will not be
#' computed. This may be necessary if validating an ANN model not built using
#' either the \code{\link[nnet]{nnet}} or \code{ann} functions. In this case,
#' it is necessary to also supply \code{npar} for AIC and BIC metrics to be
#' returned.
#'
#' @references
#' Dawson, C.W., Abrahart, R.J., See, L.M., 2007. HydroTest: A web-based
#'    toolbox of evaluation metrics for the standardised assessment of
#'    hydrological forecasts. Environmental Modelling & Software, 22(7),
#'    1034-1052. \url{http://dx.doi.org/10.1016/j.envsoft.2006.06.008}.
#'    \url{http://co-public.lboro.ac.uk/cocwd/HydroTest/Details.html}
#'
#' Shahin, M.A., Maier, H.R., Jaksa, M.B., 2005. Investigation
#'    into the robustness of artificial neural networks for a case study in
#'    civil engineering. MODSIM 2005 International Congress on Modelling and
#'    Simulation, December 2005,
#'    \url{http://www.mssanz.org.au/modsim05/papers/shahin_3.pdf}, 79-83.
#'
#' Mount, N.J., Dawson, C.W., Abrahart, R.J., 2013. Legitimising
#' data-driven models: exemplification of a new data-driven mechanistic
#' modelling framework. Hydrology and Earth System Sciences 17, 2827-2843.
#' \url{http://dx.doi.org/10.5194/hess-17-2827-2013}.
#'
#' @seealso \code{\link{ann}}, \code{\link{plot.validann}},
#' \code{\link{predict.ann}}
#' @examples
#' # get validation results for 1-hidden node `ann' model fitted to ar9 data
#' # based on training data.
#' # ---
#' data("ar9")
#' samp <- sample(1:1000, 200)
#' y <- ar9[samp, ncol(ar9)]
#' x <- ar9[samp, -ncol(ar9)]
#' x <- x[, c(1,4,9)]
#'
#' fit <- ann(x, y, size = 1, act_hid = "tanh", act_out = "linear", rang = 0.1)
#' results <- validann(fit, x = x)
#'
#' # get validation results for above model based on a new sample of ar9 data.
#' # ---
#' samp <- sample(1:1000, 200)
#' y <- ar9[samp, ncol(ar9)]
#' x <- ar9[samp, -ncol(ar9)]
#' x <- x[, c(1,4,9)]
#'
#' obs <- y
#' sim <- predict(fit, newdata = x)$values
#' results <- validann(fit, obs = obs, sim = sim, x = x)
#'
#' # get validation results for `obs' and `sim' data without ANN model.
#' # In this example `sim' is generated using a linear model.
#' # validann would be called in the same way if the ANN model used to generate
#' # `sim' was not available or was not of class `ann' or `nnet'.
#' # ---
#' samp <- sample(1:1000, 200)
#' y <- ar9[samp, ncol(ar9)]
#' x <- ar9[samp, -ncol(ar9)]
#' x <- as.matrix(x[, c(1,4,9)])
#' lmfit <- lm.fit(x, y)
#' sim <- lmfit$fitted.values
#' obs <- y
#' npar <- length(lmfit$coefficients)
#' results <- validann(obs = obs, sim = sim, npar = npar)
#'
#' @export
#--------------------------------------------------
validann <- function(net, ...) {

    UseMethod("validann")
}
# -------------------------------------------------------------------------------
#' @describeIn validann Useful when ANN model has not been developed using
#' either \code{\link{ann}} or \code{\link[nnet]{nnet}}. Only metrics and
#' statistics for predictive and replicative validation are computed.
#' @export
validann.default <- function(obs, sim, npar, na.rm = TRUE) {

  results <- list()

  if (missing(obs) | missing(sim)) {
    stop("Required 'obs' or 'sim' data missing")
  } else if (length(obs) != length(sim)) {
    stop("'obs' and 'sim' must be the same length")
  }
  results$predictive <- FALSE
  results$replicative <- FALSE
  results$structural <- FALSE

  # Test for predictive validity.
  #----
  if (missing(npar)) {
    npar <- NULL
  }
  valid_type <- "predictive"
  packageStartupMessage("Computing ", valid_type, " validity...")
  results_pred <- predictive_valid(obs, sim, npar, na.rm)
  results <- append(results, results_pred)
  results$predictive <- TRUE
  packageStartupMessage("Done.")

  # Test for replicative validity.
  #----
  valid_type <- "replicative"
  packageStartupMessage("Computing ", valid_type, " validity...")
  results_rep <- replicative_valid(obs, sim, na.rm)
  results <- append(results, results_rep)
  results$replicative <- TRUE
  packageStartupMessage("Done.")

  class(results) <- "validann"

  return(results)
}
# -------------------------------------------------------------------------------
#' @describeIn validann Compute validation metrics when \code{net}
#' is of class `ann'.
#' @export
validann.ann <- function(net, obs, sim, x, na.rm = TRUE) {

  results <- list()

  if (missing(obs) & missing(sim)) {
    obs <- observed(net)
    sim <- fitted(net)
  } else if (missing(obs)) {
    obs <- observed(net)
    sim <- fitted(net)
    message1 <- "'obs' data missing : 'obs' and 'sim' both derived from 'net'"
    warning(message1, call. = FALSE, immediate. = FALSE)
  } else if (missing(sim)) {
    obs <- observed(net)
    sim <- fitted(net)
    message1 <- "'sim' data missing : 'obs' and 'sim' both derived from 'net'"
    warning(message1, call. = FALSE, immediate. = FALSE)
  } else if (length(obs) != length(sim)) {
    message1 <- "'obs' and 'sim' must be the same length : "
    message2 <- "'obs' and 'sim' both derived from 'net'"
    warning(message1, message2, call. = FALSE, immediate. = FALSE)
  }
  results$predictive <- FALSE
  results$replicative <- FALSE
  results$structural <- FALSE

# Test for predictive validity.
#----
  npar <- length(net$wts)
  valid_type <- "predictive"
  packageStartupMessage("Computing ", valid_type, " validity...")
  results_pred <- predictive_valid(obs, sim, npar, na.rm)
  results <- append(results, results_pred)
  results$predictive <- TRUE
  packageStartupMessage("Done.")

# Test for replicative validity.
#----
  valid_type <- "replicative"
  packageStartupMessage("Computing ", valid_type, " validity...")
  results_rep <- replicative_valid(obs, sim, na.rm)
  results <- append(results, results_rep)
  results$replicative <- TRUE
  packageStartupMessage("Done.")

# Test for structural validity.
#----
  if (missing(x)) {
    x <- NULL
  }
  valid_type <- "structural"
  packageStartupMessage("Computing ", valid_type, " validity...")
  results_struct <- structural_valid(net, x)
  results <- append(results, results_struct)
  results$structural <- TRUE
  packageStartupMessage("Done.")

  class(results) <- "validann"
  return(results)
}
# -------------------------------------------------------------------------------
#' @describeIn validann Compute validation metrics when \code{net}
#' is of class `nnet'.
#' @export
validann.nnet <- function(net, obs, sim, x, na.rm = TRUE) {

  results <- list()

  if (missing(obs) & missing(sim)) {
    obs <- observed(net)
    sim <- fitted(net)
  } else if (missing(obs)) {
    obs <- observed(net)
    sim <- fitted(net)
    message1 <- "'obs' data missing : 'obs' and 'sim' both derived from 'net'"
    warning(message1, call. = FALSE, immediate. = FALSE)
  } else if (missing(sim)) {
    obs <- observed(net)
    sim <- fitted(net)
    message1 <- "'sim' data missing : 'obs' and 'sim' both derived from 'net'"
    warning(message1, call. = FALSE, immediate. = FALSE)
  } else if (length(obs) != length(sim)) {
    message1 <- "'obs' and 'sim' must be the same length : "
    message2 <- "'obs' and 'sim' both derived from 'net'"
    warning(message1, message2, call. = FALSE, immediate. = FALSE)
  }
  results$predictive <- FALSE
  results$replicative <- FALSE
  results$structural <- FALSE

  # Test for predictive validity.
  #----
  npar <- length(net$wts)
  valid_type <- "predictive"
  packageStartupMessage("Computing ", valid_type, " validity...")
  results_pred <- predictive_valid(obs, sim, npar, na.rm)
  results <- append(results, results_pred)
  results$predictive <- TRUE
  packageStartupMessage("Done.")

  # Test for replicative validity.
  #----
  valid_type <- "replicative"
  packageStartupMessage("Computing ", valid_type, " validity...")
  results_rep <- replicative_valid(obs, sim, na.rm)
  results <- append(results, results_rep)
  results$replicative <- TRUE
  packageStartupMessage("Done.")

  # Test for structural validity.
  #----
  if (missing(x)) {
    x <- NULL
  }
  valid_type <- "structural"
  packageStartupMessage("Computing ", valid_type, " validity...")
  results_struct <- structural_valid(net, x)
  results <- append(results, results_struct)
  results$structural <- TRUE
  packageStartupMessage("Done.")

  class(results) <- "validann"

  return(results)
}
#-------------------------------------------------------------------------------
predictive_valid <- function(obs, sim, npar = NULL, na.rm = TRUE) {

  if (is.null(npar)) {
    message1 <- "No information on model dimension provided : "
  }
  rem <- vector()
  if (length(obs) != length(sim)) {
    stop("'obs' and 'sim' must be the same length")
  } else if (any(is.na(obs))) {
    message2 <- "missing values in 'obs'"
    warning(message2, call. = FALSE, immediate. = FALSE)
    rem <- which(is.na(obs))
  } else if (any(is.na(sim))) {
    message2 <- "missing values in 'sim'"
    warning(message2, call. = FALSE, immediate. = FALSE)
    rem <- c(rem, which(is.na(sim)))
  }
  # Remove NA values if na.rm = TRUE
  if(na.rm & length(rem) > 0) {
    obs <- obs[-rem]
    sim <- sim[-rem]
  }
# Compute metrics from HydroTest
  nsamps <- length(obs)
  ame <- max(abs(sim - obs))
  pdiff <- max(sim) - max(obs)
  mae <- sum(abs(sim - obs)) / nsamps
  me <- sum(sim - obs) / nsamps
  rmse <- sqrt( sum( (sim - obs) ^ 2) / nsamps)
  r4ms4e <- ( sum( (sim - obs) ^ 4) / nsamps) ^ (1 / 4)
  if (!is.null(npar)) {
    aic <- nsamps * log(rmse) + 2 * npar
    bic <- nsamps * log(rmse) + npar * log(nsamps)
  } else {
    aic <- NA
    bic <- NA
    message3 <- "AIC and BIC not computed."
    warning(message1, message3, call. = FALSE, immediate. = FALSE)
  }
  #-----
  nsc_calc <- function(obs, sim) {
    resid <- sim - obs
    sc <- diff(sign(resid))
    sc[sc != 0] <- 1
    sum(sc)
  }
  #-----
  nsc <- nsc_calc(obs, sim)
  rae <- sum( abs(sim - obs) ) / sum( abs(obs - mean(obs)) )
  pep <- (max(sim) - max(obs)) / max(obs) * 100
  mare <- sum(abs(sim - obs) / obs) / nsamps
  mdape <- median(abs( (sim - obs) / obs) * 100)
  mre <- sum( (sim - obs) / obs) / nsamps
  msre <- sum( ( (sim - obs) / obs) ^ 2) / nsamps
  rve <- sum(sim - obs) / sum(obs)
  rsqr <- (sum( (obs - mean(obs)) * (sim - mean(sim)) ) /
           sqrt( sum( (obs - mean(obs)) ^ 2) *
                 sum( (sim - mean(sim)) ^ 2))) ^ 2
  ioad <- 1 - sum( (sim - obs) ^ 2) /
                  sum((abs(obs - mean(obs)) +
                       abs(sim - mean(obs))) ^ 2)
  ce <- 1 - sum( (sim - obs) ^ 2) /
            sum( (obs - mean(obs)) ^ 2)
  pi <- 1 - sum( (sim[-1] - obs[-1]) ^ 2) /
            sum( (obs[-1] - obs[-nsamps]) ^ 2)

  msle <- (log(sim + 1e-08) - log(obs + 1e-08)) ^ 2
  msle <- sum(msle, na.rm = TRUE) / (nsamps - sum(is.na(msle)))
  msde <- sum( ( (obs[-1] - obs[-nsamps]) -
                 (sim[-1] - sim[-nsamps])) ^ 2) / (nsamps - 1)
  delta <- obs[-1] - obs[-nsamps]
  irmse <- sqrt(sum( (delta - mean(delta)) ^ 2) / (nsamps - 1))
  irmse <- rmse / irmse
  ve <- 1 - sum(abs(sim - obs)) / sum(obs)
  alpha <- sd(sim) / sd(obs)
  beta <- mean(sim) / mean(obs)
  r <- cor(sim, obs)
  kge <- 1 - sqrt( (r - 1) ^ 2 + (alpha - 1) ^ 2 + (beta - 1) ^ 2)
  sse <- sum( (sim - obs) ^ 2)


  metrics <- data.frame(AME = ame, PDIFF = pdiff, MAE = mae, ME = me,
                        RMSE = rmse, R4MS4E = r4ms4e, AIC = aic,
                        BIC = bic, NSC = nsc, RAE = rae, PEP = pep,
                        MARE = mare, MdAPE = mdape, MRE = mre,
                        MSRE = msre, RVE = rve, RSqr = rsqr,
                        IoAd = ioad, CE = ce, PI = pi, MSLE = msle,
                        MSDE = msde, IRMSE = irmse, VE = ve,
                        KGE = kge, SSE = sse, R = r)
  obs_stats <- stats(obs)
  sim_stats <- stats(sim)

  return(list(metrics = metrics, obs_stats = obs_stats, sim_stats = sim_stats))
}
# ------------------------------------------------------------------------------
replicative_valid <- function(obs, sim, na.rm = TRUE) {

  rem <- vector()
  if (length(obs) != length(sim)) {
    stop("'obs' and 'sim' must be the same length")
  } else if (any(is.na(obs))) {
    message2 <- "missing values in 'obs'"
    warning(message2, call. = FALSE, immediate. = FALSE)
    rem <- which(is.na(obs))
  } else if (any(is.na(sim))) {
    message2 <- "missing values in 'sim'"
    warning(message2, call. = FALSE, immediate. = FALSE)
    rem <- c(rem, which(is.na(sim)))
  }
  # Remove NA values if na.rm = TRUE
  if(na.rm & length(rem) > 0) {
    obs <- obs[-rem]
    sim <- sim[-rem]
  }

  err <- (sim - obs)
  resid_stats <- stats(err)

  return(list(residuals = err, resid_stats = resid_stats))
}
# -------------------------------------------------------------------------------
structural_valid <- function(net, x = NULL) {

  results <- list()

# Compute relative importance of inputs via ocw, modified ocw and Garson's
# methods
  ninputs <- ifelse(is.null(net$n), net$nodes[1], net$n[1])
  ri_ocw <- ocw_fn(net)
  ri_gars <- garson_fn(net)
  ri <- as.data.frame(matrix(c(unlist(ri_ocw), unlist(ri_gars)),
                             ncol = ninputs, byrow = TRUE))
  if(!is.null(x)) {
    colnames(ri) <- colnames(x)
  } else {
    colnames(ri) <- paste("inp_", 1:ninputs, sep = "")
  }
  row.names(ri) <- c(names(ri_ocw), names(ri_gars))

  results$ri <- ri

  if(!is.null(x)) {
    y_hat <- sens_anal(net, x)
    colnames(y_hat) <- colnames(x)
    results$y_hat <- y_hat
  # If data$net is of class "ann", perform PD sensitivity analysis
    if (inherits(net, "ann")) {
      rs <- pd_sens_anal(net, x)
      results$rs <- rs
    } else {
      message3 <-
        "'net' not of class \"ann\" : "
      message4 <- "PD sensitivity analysis not performed."
      warning(message3, message4, call. = FALSE, immediate. = FALSE)
    }
  } else {
    message1 <- "Input data (x) missing : "
    message2 <- "No sensitivity analyses performed."
    warning(message1, message2, call. = FALSE, immediate. = FALSE)
  }

  return(results)
}
# ------------------------------------------------------------------------------
stats <- function(x) {
  stats_mean <- mean(x)
  stats_min <- min(x)
  stats_max <- max(x)
  stats_var <- var(x)
  stats_sd <- sd(x)
  stats_skew <- moments::skewness(x)
  stats_kurt <- moments::kurtosis(x)
  return(data.frame(mean = stats_mean, min = stats_min, max = stats_max,
                    var = stats_var, sd = stats_sd, skewness = stats_skew,
                    kurtosis = stats_kurt))
}
#-------------------------------------------------------------------------------
sens_anal <- function(net, x) {
  if (!(inherits(net, "nnet") | inherits(net, "ann"))) {
    stop("'net' must be of class \"nnet\" or \"ann\" to perform local
         sensitivity analysis.")
  }

  mean_x <- apply(x, 2, mean)
  x_mean <- matrix(rep(mean_x, 101), ncol = ncol(x), byrow = TRUE)
  y_hat <- vector()
  for (k in 1:ncol(x)) {
    x_tmp <- x_mean
    x_tmp[, k] <- as.vector(quantile(x[, 2], prob = seq(0, 1, by = 0.01)))
    if (inherits(net, "nnet")) {
      y_hat <- cbind(y_hat, predict(net, newdata = x_tmp,
                                    type = "raw"))
    } else if (inherits(net, "ann")) {
      y_hat <- cbind(y_hat, predict(net, newdata = x_tmp)$values)
    }
  }
  colnames(y_hat) <- colnames(x)
  return(y_hat)
}
#-------------------------------------------------------------------------------
pd_sens_anal <- function(net, x) {
  if (inherits(net, "nnet")) {
    stop("'net' must be of class \"ann\" to perform PD sensitivity analysis.")
  }
  if (!inherits(net, "ann")) stop("'net' not of class \"ann\"")
  if (is.vector(x)) {
    x <- matrix(x, ncol = 1)
  }
  npatterns <- dim(x)[1]
  ninputs <- dim(x)[2]
  O <- net$fitted.values
  if(ncol(net$derivs) == 1) {
    nhn <- 0
    out_derivs <- matrix(net$derivs[, 1], ncol = 1)
    hid_derivs <- NULL
  } else {
    out_derivs <- matrix(net$derivs[, ncol(net$derivs)], ncol = 1)
    hid_derivs <- matrix(net$derivs[, -ncol(net$derivs)],
                         ncol = ncol(net$derivs) - 1)
    nhn <- ncol(hid_derivs)
  }
  rs <- vector()
  if(nhn > 0) {
    for (i in 1:ninputs) {
      sum1 <- rep(0, npatterns)
      for (j in 1:net$nodes[2]) {
        sum1 <- sum1 + net$wts[(i - 1) * net$nodes[2] + j] *
                       hid_derivs[, j] * net$wts[net$nodes[1] *
                       net$nodes[2] + net$nodes[2] + j] *
                       out_derivs[, 1]
      }
      rs <- cbind(rs, x[, i] / O * sum1)
    }
  } else {
    for (i in 1:ninputs) {
      sum1 <- net$wts[i]
      rs <- cbind(rs, x[, i] / O * sum1)
    }
  }
  colnames(rs) <- names(x)
  return(rs)
}
#-------------------------------------------------------------------------------
ocw_fn <- function(net) {
# -------
# function for calculating overall connection weight (OCW) and
# relative importance (RI) of inputs
# -------
  if (inherits(net, "nnet")) {
    layers <- net$n
    nhn <- layers[2]
    ninp <- layers[1]
    wts <- net$wts
    act_fn <- "sigmoid"
    indices <- matrix(seq(1, layers[1] * layers[2] + layers[2]),
                      ncol = layers[2])
    out_ls <- list()
    for (i in 1:ncol(indices)) {
      out_ls[[paste("hidden", i)]] <- wts[indices[, i]]
    }
    out_ls[["out 1"]] <- wts[(max(indices) + 1):length(wts)]
    ocw <- ocw_mod <- vector()
    for (i in 1:nhn) {
      ocw_mod <- rbind(ocw_mod, tanh(out_ls[[i]][2:(ninp + 1)]) *
                         out_ls[[nhn + 1]][i + 1])
      ocw <- rbind(ocw, out_ls[[i]][2:(ninp + 1)] * out_ls[[nhn + 1]][i + 1])
    }
  } else if (inherits(net, "ann")) {
    layers <- net$nodes
    nhn <- ifelse(length(layers) == 3, layers[2], 0)
    ninp <- layers[1]
    wts <- net$wts
    act_fn <- net$act_fn[2]
    out_ls <- list()
    if(nhn > 0) {
      indices <- matrix(seq(1, layers[1] * layers[2]), ncol = layers[2],
                        byrow = TRUE)
      for (i in 1:ncol(indices)) {
        out_ls[[paste("hidden", i)]] <- wts[indices[, i]]
      }
      out_ls[["out 1"]] <- wts[(max(indices) + layers[2] + 1):length(wts)]
      ocw <- ocw_mod <- vector()
      for (i in 1:nhn) {
        ocw_mod <- rbind(ocw_mod, actfn(out_ls[[i]][1:ninp], method = act_fn) *
                           out_ls[[nhn + 1]][i])
        ocw <- rbind(ocw, out_ls[[i]][1:ninp] * out_ls[[nhn + 1]][i])
      }
      ri <- colSums(ocw)
      ri_mod <- colSums(ocw_mod)
    } else {
      out_ls[["out 1"]] <- wts
      ocw_mod <- actfn(out_ls[[1]][1:ninp], method = act_fn)
      ocw <- out_ls[[1]][1:ninp]
      ri <- ocw
      ri_mod <- ocw_mod
    }
  } else {
    stop("'net' must be of class \"nnet\" or \"ann\"")
  }

  ri_denom <- sum(abs(ri))
  ri <- ri / ri_denom
  ri <- ri * 100

  ri_denom <- sum(abs(ri_mod))
  ri_mod <- ri_mod / ri_denom
  ri_mod <- ri_mod * 100

  return(list(ocw = ri, ocw.mod = ri_mod))
}
# ------------------------------------------------------------------------------
garson_fn <- function(net) {
# -------
# Function to calculate Garson's measure of relative importance
# -------
  if (inherits(net, "nnet")) {
    layers <- net$n
    nhn <- layers[2]
    ninp <- layers[1]
    wts <- net$wts

    indices <- matrix(seq(1, layers[1] * layers[2] + layers[2]),
                      ncol = layers[2])
    out_ls <- list()
    for (i in 1:ncol(indices)) {
      out_ls[[paste("hidden", i)]] <- wts[indices[, i]]
    }
    out_ls[["out 1"]] <- wts[(max(indices) + 1):length(wts)]
    ri <- vector()
    for (i in 1:nhn) {
      sum_wi <- sum(abs(out_ls[[i]][2:(ninp + 1)]))
      sum_wo <- sum(abs(out_ls[[nhn + 1]][2:(nhn + 1)]))
      ri <- rbind(ri, abs(out_ls[[i]][2:(ninp + 1)]) / sum_wi *
                    abs(out_ls[[nhn + 1]][i + 1]) / sum_wo)
    }
  } else if (inherits(net, "ann")) {
    layers <- net$nodes
    nhn <- layers[2]
    ninp <- layers[1]
    wts <- net$wts
    indices <- matrix(seq(1, layers[1] * layers[2]), ncol = layers[2],
                      byrow = TRUE)
    out_ls <- list()
    for (i in 1:ncol(indices)) {
      out_ls[[paste("hidden", i)]] <- wts[indices[, i]]
    }
    out_ls[["out 1"]] <- wts[(max(indices) + layers[2] + 1):length(wts)]
    ri <- vector()
    for (i in 1:nhn) {
      sum_wi <- sum(abs(out_ls[[i]][1:ninp]))
      sum_wo <- sum(abs(out_ls[[nhn + 1]][1:nhn]))
      ri <- rbind(ri, abs(out_ls[[i]][1:ninp]) / sum_wi *
                    abs(out_ls[[nhn + 1]][i]) / sum_wo)
    }
  } else {
    stop("'net' must be of class \"nnet\" or \"ann\"")
  }

  # ri_mod <- rbind(ri_mod, out_ls[[i]][2:(ninp+1)] / sum.wi *
  #                         out_ls[[nhn + 1]][i+1] / sum.wo)

  ri <- colSums(ri) * 100

  return(list(gars = ri))
}
# ------------------------------------------------------------------------------
