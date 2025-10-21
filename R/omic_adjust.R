#' OmicAdjust (aka `omic_adjust`): pragmatic p-value redistribution for exploration
#'
#' @description
#' Estimate expected false positives at `alpha`, then redistribute p-values so
#' the `excess` smallest lie in `[0, alpha]` and the rest in `[alpha, 1]`.
#' Exploratory only (no FDR control).
#'
#' @param p Numeric vector of unadjusted p-values in [0, 1]. `NA` allowed.
#' @param alpha Numeric scalar in (0, 1). Default 0.05.
#' @return A list; use `$p_rescaled` for the rescaled values.
#'
#' @examples
#' set.seed(1); p <- c(runif(900), runif(100, max = 0.01))
#' OmicAdjust(p)$p_rescaled
#' omic_adjust(p)$p_rescaled
#'
#' @name OmicAdjust
#' @rdname OmicAdjust
NULL

#' @rdname OmicAdjust
#' @export
omic_adjust <- function(p, alpha = 0.05) {
  stopifnot(is.numeric(p), length(alpha) == 1L, is.finite(alpha), alpha > 0, alpha < 1)
  keep <- is.finite(p) & p >= 0 & p <= 1
  p_in <- p[keep]; n <- length(p_in)

  expected <- n * alpha
  sd <- sqrt(n * alpha * (1 - alpha))
  upper <- expected + 1.96 * sd
  obs <- sum(p_in < alpha)
  excess <- max(0L, obs - ceiling(upper))

  ord <- order(p_in)
  ix_low <- if (excess > 0L) ord[seq_len(excess)] else integer(0)
  ix_high <- setdiff(seq_len(n), ix_low)

  low_vals <- if (excess > 0L) seq(0, alpha, length.out = excess + 2L)[-c(1L, excess + 2L)] else numeric(0)
  high_len <- length(ix_high)
  high_vals <- if (high_len > 0L) seq(alpha, 1, length.out = high_len + 2L)[-c(1L, high_len + 2L)] else numeric(0)

  p_rescaled <- numeric(n)
  if (excess > 0L) p_rescaled[ix_low]   <- low_vals
  if (high_len > 0L) p_rescaled[ix_high] <- high_vals

  p_final_sorted <- pmax(sort(p_rescaled), sort(p_in))

  out <- rep(NA_real_, length(p))
  out_idx <- which(keep)
  out[out_idx[order(p_in)]] <- p_final_sorted

  list(
    p_rescaled = out,
    expected = expected,
    sd = sd,
    upper = upper,
    observed = obs,
    excess = excess
  )
}

#' Alias of `omic_adjust()`.
#' @rdname OmicAdjust
#' @export
OmicAdjust <- omic_adjust
