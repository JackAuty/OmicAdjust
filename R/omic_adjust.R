#' OmicAdjust: pragmatic p-value redistribution for exploration
#'
#' @description
#' `OmicAdjust()` estimates the expected number of false positives at level
#' `alpha` using a binomial model and a 95% upper bound. It then redistributes
#' the observed p-values so the smallest values lie in `[0, alpha]` and the
#' remainder in `[alpha, 1]`. This is intended for exploratory use.
#'
#' @details
#' The method:
#' 1. Counts `obs = sum(p < alpha)`.
#' 2. Computes the null expectation `expected = n * alpha` with
#'    `sd = sqrt(n*alpha*(1-alpha))` and a 95% upper bound `upper`.
#' 3. Treats `excess = max(0, obs - round(upper))` as putative discoveries and
#'    linearly maps the `excess` smallest p-values into `[0, alpha]`,
#'    the remainder into `[alpha, 1]`.
#' 4. Applies a monotone tweak so the rescaled values are never smaller than the
#'    original sorted p-values.
#'
#' **Important:** This is *not* an FDR procedure and does not guarantee error
#' control. Use BH / Storey q-values / Bonferroni for confirmatory analysis.
#'
#' @param p Numeric vector of p-values in `[0, 1]`. `NA`s allowed.
#' @param alpha Single numeric in `(0, 1)`. Default `0.05`.
#'
#' @return A list with:
#' - `p_rescaled`: numeric vector like `p` with `NA`s preserved
#' - `expected`, `sd`, `upper`, `observed`, `excess`: diagnostics
#'
#' @examples
#' set.seed(1)
#' p <- c(runif(900), runif(100, max = 0.01))
#' out <- OmicAdjust(p)
#' mean(out$p_rescaled < 0.05)
#'
#' @export

omic_adjust <- function(p, alpha = 0.05) {
  stopifnot(is.numeric(p), all(is.finite(p)), all(p >= 0), all(p <= 1))
  stopifnot(is.numeric(alpha), length(alpha) == 1L, is.finite(alpha), alpha > 0, alpha < 1)
  n <- length(p)
  expected <- n * alpha
  sd <- sqrt(n * alpha * (1 - alpha))
  upper <- expected + 1.96 * sd
  obs <- sum(p < alpha)
  excess <- max(0, obs - ceiling(upper))
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
