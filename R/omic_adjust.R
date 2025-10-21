#' Rescale a vector of p-values by binomial-derived excess discoveries
#'
#' @param p Numeric vector of p-values in [0, 1].
#' @param alpha Numeric scalar in (0, 1). Default 0.05.
#' @return List with rescaled p-values and diagnostics.
#' @examples
#' set.seed(1); p <- runif(10); omic_adjust(p)$p_rescaled
#' @export
omic_adjust <- function(p, alpha = 0.05) {
  stopifnot(is.numeric(p), all(is.finite(p)), all(p >= 0), all(p <= 1))
  stopifnot(is.numeric(alpha), length(alpha) == 1L, is.finite(alpha), alpha > 0, alpha < 1)
  n <- length(p)
  expected <- n * alpha
  sd <- sqrt(n * alpha * (1 - alpha))
  upper <- expected + 1.96 * sd
  obs <- sum(p < alpha)
  excess <- max(0, obs - round(upper))
  ord <- order(p)
  ix_low <- if (excess > 0) ord[seq_len(excess)] else integer(0)
  ix_high <- setdiff(seq_len(n), ix_low)
  low_vals <- if (excess > 0) seq(0, alpha, length.out = excess + 2L)[-c(1L, excess + 2L)] else numeric(0)
  high_len <- length(ix_high)
  high_vals <- if (high_len > 0) seq(alpha, 1, length.out = high_len + 2L)[-c(1L, high_len + 2L)] else numeric(0)
  p_rescaled <- numeric(n)
  if (excess > 0) p_rescaled[ix_low]  <- low_vals
  if (high_len > 0) p_rescaled[ix_high] <- high_vals
  p_sorted <- sort(p)
  p_resorted <- sort(p_rescaled)
  p_final_sorted <- pmax(p_resorted, p_sorted)
  p_final <- numeric(n); p_final[ord] <- p_final_sorted
  list(p_rescaled = p_final, expected = expected, sd = sd, upper = upper, observed = obs, excess = excess)
}
