#--------------------------------------------------
#' @title Plot ANN validation results.
#'
#' @description Plot method for objects of class `validann'. Produces a series
#'    of plots used for validating and assessing ANN models based on results
#'    returned by \code{\link{validann}}.
#'
#' @param results  object of class `validann' as returned
#'    by \code{\link{validann}}. This is a list comprising metrics and
#'    statistics that can be used for validating ANN models.
#' @param obs,sim   vectors comprising observed (\code{obs}) and simulated
#'    (\code{sim}) examples of a single response variable used for computing
#'    \code{results} object.
#' @param type  character string defining how plots should be displayed.
#'    The default is ``multi'' where multiple plots are displayed together
#'    according to whether they are used for predictive, replicative or
#'    structural validation. For ``single'', each plot is displayed on its own.
#'    If the session is interactive, the user will be asked to confirm a new
#'    page whether \code{type} is ``single'' or ``multi''
# @param \dots Arguments to be passed to plot.
#' @details   This function can be invoked by calling
#'    \code{plot(results, obs, sim)} for an object \code{results} of class
#'    `validann'.
#'
#'    To produce plots for all (predictive, replicative and structural)
#'    types of validation metrics and statistics, \code{results$predictive},
#'    \code{results$replicative} and \code{results$structural} must be
#'    \code{TRUE} and, for replicative and structural validation, corresponding
#'    results must have been successfully computed by \code{\link{validann}}
#'    and returned in object \code{results}.
#'
#'    If \code{results$predictive} is \code{TRUE}, a scatter plot, Q-Q plot and
#'    time/sample plot of observed (\code{obs}) versus predicted (\code{sim})
#'    data are produced.
#'
#'    If \code{results$replicative} is \code{TRUE} and \code{results$residuals}
#'    is not \code{NULL}, plots of the model residuals are produced including
#'    histogram, Q-Q plot (standardized residuals compared to standard normal),
#'    autocorrelation (acf), partial autocorrelation (pacf), standardized
#'    residual versus predicted output (i.e. \code{sim}) and standardized
#'    residual versus time/order of the data.
#'
#'    If \code{results$structural} is \code{TRUE} and results$y_hat is not
#'    \code{NULL}, plots of the model residuals
#'    are produced including histogram, Q-Q plot (standardized residuals
#'    compared to standard normal), autocorrelation (acf),
#'    partial autocorrelation (pacf), standardized residuals versus predicted
#'    output (i.e. \code{sim}) and standardized residual versus time/order of
#'    the data.
#'
#'    Setting \code{results$predictive}, \code{results$replicative} and/or
#'    \code{results$structural} to \code{FALSE} prior to calling
#'    \code{plot.validann} will turn off the respective validation plots.
#'
#' @seealso \code{\link{validann}}
#' @examples
#' ## Build ANN model and compute replicative and structural validation results
#' data("ar9")
#' samp <- sample(1:1000, 200)
#' y <- ar9[samp, ncol(ar9)]
#' x <- ar9[samp, -ncol(ar9)]
#' x <- x[, c(1,4,9)]
#' fit <- ann(x, y, size = 1, act_hid = "tanh", act_out = "linear", rang = 0.1)
#' results <- validann(fit, x = x)
#' obs <- observed(fit)
#' sim <- fitted(fit)
#'
#' ## Plot replicative and structural validation results to the current device
#' ## - a single page for each type of validation
#' plot(results, obs, sim)
#'
#' ## Plot results to the current device - a single page for each plot
#' plot(results, obs, sim, type = "single")
#'
#' ## Plot replicative and structural validation results to single file
#' pdf("RepStructValidationPlots.pdf")
#' plot(results, obs, sim)
#' dev.off()
#'
#' ## Get predictive validation results for above model based on a new sample
#' ## of ar9 data.
#' samp <- sample(1:1000, 200)
#' y <- ar9[samp, ncol(ar9)]
#' x <- ar9[samp, -ncol(ar9)]
#' x <- x[, c(1,4,9)]
#' obs <- y
#' sim <- predict(fit, newdata = x)$values
#' results <- validann(fit, obs = obs, sim = sim, x = x)
#'
#' ## Plot predictive results only to file
#' results$replicative <- results$structural <- FALSE
#' pdf("PredValidationPlots.pdf")
#' plot(results, obs, sim)
#' dev.off()
#'
#' @export
#--------------------------------------------------
plot.validann <- function(results, obs, sim, type = c("multi", "single"), ...) {

#  if (is.null(net) & is.null(obs) & is.null(sim)) {
  if (is.null(obs) & is.null(sim)) {
    stop("'obs' and 'sim' objects required.")
  }
  type <- match.arg(NULL, type)
  if (type == "single" && dev.interactive()) devAskNewPage(ask = TRUE)

# Predictive validity plots
# ----
  if (results$predictive == TRUE) {
    if (is.null(obs)) {
      message1 <- "'obs' data missing :"
      message2 <- "Predictive validity plots will not be produced."
      warning(message1, message2, call. = FALSE, immediate. = FALSE)
    } else if (is.null(sim)) {
      message1 <- "'sim' data missing :"
      message2 <- "Predictive validity plots will not be produced."
      warning(message1, message2, call. = FALSE, immediate. = FALSE)
    } else {
      if (type == "multi") {
        m <- rbind(c(1, 2), c(3, 3))
        layout(m)
      } else {
        m <- c(1, 1)
        layout(m)
      }
      par(oma = c(0, 0, 1, 0), mar = c(4, 4, 3, 0.3))

      # scatterplot - obs vs sim
      min_plot <- min(obs, sim)
      max_plot <- max(obs, sim)
      plot(x = obs, y = sim, type = "p", pch = 21, col = "black",
           bg = colors()[240], xlim = c(min_plot, max_plot),
           ylim = c(min_plot, max_plot), xlab = "Observed", ylab = "Predicted",
           main = "Scatter Plot", ...)
      abline(a = 0, b = 1, col = "red", lty = "dashed")


      # qq plot of obs v sim
      qqplot(obs, sim, pch = 21, col = "black", bg = colors()[240],
             xlab = "Observed", ylab = "Predicted",
             main = "Q-Q Plot")
      abline(a = 0, b = 1, col = "red", lty = "dashed")

      nsamps <- length(obs)
      plot(x = 1:nsamps, y = obs, type = "p", pch = 23, col = "black",
           bg = "black", ylim = c(min_plot, max_plot),
           xlab = "Sample", ylab = "Value", ...)
      points(x = 1:nsamps, y = sim, pch = 23, col = "black",
             bg = colors()[240], cex = 0.8)
      title(main = "Observed Vs Predicted", line = 2)
    # add legend
      par(oma = c(0, 0, 1, 0), mar = c(0, 0, 1, 0), new = TRUE)
      plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
      legend(x = "top", legend = c("Observed", "Predicted"),
             pch = c(23, 23), col = c("black", "black"),
             pt.bg = c("black", colors()[240]), pt.cex = c(1, 0.8),
             horiz = TRUE, bty = "n", inset = c(0, 0), xpd = TRUE)
      if (type == "multi") {
        if (dev.interactive()) devAskNewPage(ask = TRUE)
        title(main = "Predictive Validity", outer = TRUE)
      }
    }
  }


# Replicative validity plots
# ----
  if(results$replicative == TRUE) {
    if (is.null(sim)) {
      message1 <- "'sim' data missing :"
      message2 <- "Replicative validity plots will not be produced."
      warning(message1, message2, call. = FALSE, immediate. = FALSE)
    } else {
      if(type == "multi") {
        m <- rbind(c(1, 2), c(3, 4), c(5, 6))
        layout(m)
      } else {
        m <- c(1, 1)
        layout(m)
      }
      par(oma = c(0, 0, 1, 0), mar = c(4, 4, 3, 0.3))

      # residuals histogram
      tmp_hist <- hist(results$residuals, plot = FALSE)
      tmp_norm <- dnorm(results$residuals, mean = 0,
                        sd = results$resid_stats$sd)
      ymax <- max(tmp_hist$density, tmp_norm) * 1.2
      plot(x = tmp_hist$mids, y = tmp_hist$density, type = "h", lwd = 30,
           lend = 2, col = colors()[240], ylim = c(0, ymax), yaxs = "i",
           xlab = "Residual", ylab = "Density", main = "Residuals Histogram",
           ...)
      lines(x = sort(results$residuals), y = tmp_norm[order(results$residuals)],
            col = "red", lty = "dashed")
      if (type == "multi") devAskNewPage(ask = FALSE)

    # qq plot of residual vs normal distribution
      sd_err <- (results$residuals - results$resid_stats$mean) /
                 results$resid_stats$sd
      qqnorm(sd_err, pch = 21, col = "black", bg = colors()[240],
             xlab = "Standard Normal Quantiles", ylab = "Standardized Residual",
             main = "Residual Q-Q Plot")
      abline(a = 0, b = 1, col = "red", lty = "dashed")

    # residual autocorrelation plots
      acf_tmp <- acf(results$residuals, plot = FALSE)
      pacf_tmp <- pacf(results$residuals, plot = FALSE)

      clim <- qnorm((1 + 0.95) / 2) / sqrt(acf_tmp$n.used)
      ylim <- range(c(-clim, clim, acf_tmp$acf[, 1, 1]))
      plot(acf_tmp$lag[, 1, 1], acf_tmp$acf[, 1, 1], type = "h", ylim = ylim,
           xlab = "Lag", ylab = "ACF", main = "Residual Autocorrelation")
      abline(h = 0)
      abline(h = c(clim, -clim), col = "blue", lty = 2)

      ylim <- range(c(-clim, clim, pacf_tmp$acf[, 1, 1]))
      plot(pacf_tmp$lag[, 1, 1], pacf_tmp$acf[, 1, 1], type = "h", ylim = ylim,
           xlab = "Lag", ylab = "Partial ACF",
           main = "Residual Partial-Autocorrelation")
      abline(h = 0)
      abline(h = c(clim, -clim), col = "blue", lty = 2)



    # Standardised residuals vs simulated
      plot(x = sim, y = sd_err, type = "p", pch = 21, col = "black",
           cex = 0.8, bg = colors()[240], xlab = "Predicted Value",
           ylab = "Standardized Residual",
           main = "Residuals Vs Simulated")
      abline(h = 0, lty = "dashed", col = "red")
      abline(h = -1.96, lty = "dashed", col = "blue")
      abline(h = 1.96, lty = "dashed", col = "blue")
    # Standardised residuals vs 'time'
      plot(x = 1:length(results$residuals), y = sd_err, type = "p", pch = 21,
           col = "black", cex = 0.8, bg = colors()[240],
           xlab = "Order", ylab = "Standardized Residual",
           main = "Residuals Vs Order/Time")
      abline(h = 0, lty = "dashed", col = "red")
      abline(h = -1.96, lty = "dashed", col = "blue")
      abline(h = 1.96, lty = "dashed", col = "blue")

      if (type == "multi") {
        if (dev.interactive()) devAskNewPage(ask = TRUE)
        title(main = "Replicative Validity", outer = TRUE)
      }
    }
  }



# Structural validity plots
# ----
  if(results$structural == TRUE) {
#     if (is.null(net)) {
#       message1 <-
#         "'net' missing : "
#       message2 <- "Structural validity plots will not be produced."
#       warning(message1, message2, call. = FALSE, immediate. = FALSE)
#     } else
    if (is.null(results$y_hat) && is.null(results$rs)) {
      message1 <-
        "Sensitivity analysis results missing : "
      message2 <- "Structural validity plots will not be produced."
      warning(message1, message2, call. = FALSE, immediate. = FALSE)
    } else {
      ninputs <- ncol(results$y_hat)
#       if (inherits(net, "nnet")) {
#         layers <- net$n
#         ninputs <- layers[1]
#       } else if (inherits(net, "ann")) {
#         layers <- net$nodes
#         ninputs <- layers[1]
#       } else {
#         message1 <-
#           "'net' not of class \"ann\" or \"nnet\" : "
#         message2 <- "Structural validity plots will not be produced."
#         warning(message1, message2, call. = FALSE, immediate. = FALSE)
#       }
    }
    if (!is.null(results$y_hat)) {
      if (type == "multi") {
        rem <- ninputs %% 2
        rep <- ninputs %/% 2
        if (rem > 0) rep <- rep + 1
        rep <- min(rep, 3)
        m <- matrix(1:(rep * 2), ncol = 2, byrow = TRUE)
        layout(m)
      } else {
        m <- c(1, 1)
        layout(m)
      }
      par(oma = c(0, 0, 1, 0), mar = c(4, 4, 3, 0.3))

      for (i in 1:ninputs) {
#        maxy <- max(range(fitted(net)), results$y_hat[, i])
#        miny <- min(range(fitted(net)), results$y_hat[, i])
        maxy <- max(results$y_hat[, i])
        miny <- min(results$y_hat[, i])
        plot(x = seq(0, 100, by = 1), y = results$y_hat[, i], type = "p",
             pch = 21, col = "black", bg = colors()[240],
             ylim = c(miny, maxy), ylab = "Predicted Response",
             xlab = paste("Quantile of Input:", colnames(results$y_hat)[i]),
             main = colnames(results$y_hat)[i])
        if (dev.interactive() && (i %% (rep * 2) == 0)) {
          devAskNewPage(ask = TRUE)
        } else if (dev.interactive() && i == ninputs) {
          devAskNewPage(ask = TRUE)
        } else if (dev.interactive() && type == "single") {
          devAskNewPage(ask = TRUE)
        } else {
          devAskNewPage(ask = FALSE)
        }
        title(main = "Local Sensitivity Analysis", outer = TRUE)
      }
    }
    if (!is.null(results$rs)) {
      if (type == "multi") {
        rem <- ninputs %% 2
        rep <- ninputs %/% 2
        if (rem > 0) rep <- rep + 1
        rep <- min(rep, 3)
        m <- matrix(1:(rep * 2), ncol = 2, byrow = TRUE)
        layout(m)
      } else {
        m <- c(1, 1)
        layout(m)
      }
      par(oma = c(0, 0, 1, 0), mar = c(4, 4, 3, 0.3))

      yrange <- c(min(results$rs), max(results$rs))
      for (i in 1:ninputs) {
        plot(x = obs, y = results$rs[, i], type = "p", ylim = yrange,
             xlab = "Observed Response Value", ylab = "Relative Sensitivity",
             main = colnames(results$rs)[i])
        if (dev.interactive() && (i %% (rep * 2) == 0)) {
          devAskNewPage(ask = TRUE)
        } else if (dev.interactive() && i == ninputs) {
          devAskNewPage(ask = TRUE)
        } else if (dev.interactive() && type == "single") {
          devAskNewPage(ask = TRUE)
        } else {
          devAskNewPage(ask = FALSE)
        }
        title(main = "P.D. Sensitivity Analysis", outer = TRUE)
      }
    }

  }
  devAskNewPage(ask = FALSE)
}
#-------------------------------------------------------------------------------
