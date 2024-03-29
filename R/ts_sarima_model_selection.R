#'
#' @author Lionel Lamy
#' @title Sarima model selection
#' @concept Comp.Sarima redesigned
#'
#' @description
#' Comparison using the AIC criterion of different S-ARIMA models
#' fitted to a time series with trend and seasonality removal.
#'
#' @details
#' Displays the top x models in the console.
#' Given info are AIC, AICR (relative), BIC and the nb of parameters.
#' Returns the model with the lowest AIC if return.best is true
#'
#' @note
#' Be careful not to set the maximum values too high.
#' The data must be the original one, not differentiated.
#'
#' @param data the (univariate) time series. Must be class of ts().
#' @param max.pq vector of length 2, maximum orders for p and q.
#' @param max.PQ vector of length 2, maximum orders for P and Q.
#' @param d number of diff of lag 1 for trend elimination.
#' @param D number of diff of lag 'season' for seasonality elimination.
#' @param top the number of best models returned.
#' @param return.best (bool) : is the best AIC model returned.
#'
#' @return the model with the lowest AIC.
#'
#' @export

ts.sarima.model.selection <- function(data, max.pq = c(1, 1), max.PQ = c(1, 1), d = 1, D = 1, top = 3, return.best = F) {

  if (!is(data, "ts")) {
    warning("The argument 'data' must be a time series object!", immediate. = T)
    return(NA)
  }

  season <- frequency(data)
  # Expand grid is equivalent to nested loops
  loop <- expand.grid(p = 0:max.pq[1], q = 0:max.pq[2], P = 0:max.PQ[1], Q = 0:max.PQ[2])

  # For each (pq)x(PQ) possibility
  # Returns the AIC, BIC and the model
  results <- sapply(seq(1, nrow(loop)), function(i) {
    arr <- loop[i, ]
    temp <- arima(data, order = c(arr$p, d, arr$q), seasonal = list(order = c(arr$P, D, arr$Q), period = season))
    list(AIC = AIC(temp), BIC = BIC(temp), ARMA = temp$arma)
  })

  # Unpack the results
  AIC.table <- round(unlist(results[1, ]), 3)
  BIC.table <- round(unlist(results[2, ]), 3)

  # Relative AIC for comparison
  AICR.table <- AIC.table - min(AIC.table)

  # Order them by AICR, up to "best.top"
  AIC.top <- order(AICR.table)[1:top]

  # Print them in a readable way
  cat("TOP ", top, " AIC || MODEL : (p,d,q)x(P,D,Q)[s]\n\n", sep = "")

  sapply(AIC.top, function(i) {
    sarima <- unlist(results[3, i])
    m1 <- paste0(sarima[c(1, 6, 2)], collapse = ",") # (p,d,q)
    m2 <- paste0(sarima[c(3, 7, 4)], collapse = ",") # (P,D,Q)

    cat("(", m1, ")x(", m2, ")[", sarima[5], "]",
      " || AIC: ", format(AIC.table[i], nsmall = 3),
      " | AICR: ", format(AICR.table[i], nsmall = 3),
      " | BIC: ", format(BIC.table[i], nsmall = 3),
      " || P: ", sum(sarima[1:4]), "\n",
      sep = ""
    )
  })

  # Return the best model, based on AIC
  if(return.best){
    best <- unlist(results[3, AIC.top[1]])
    arima(data, order = c(best[1], d, best[2]), seasonal = list(order = c(best[3], D, best[4]), period = season))
  }
}
