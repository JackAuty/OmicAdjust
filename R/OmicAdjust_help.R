#' OmicAdjust (aka `omic_adjust`): pragmatic p-value redistribution for exploration
#'
#' Run your standard test to get **unadjusted** p-values, then rescale with
#' `OmicAdjust()` (or `omic_adjust()`). Intended for **exploration only**; not FDR control.
#'
#' @param p Numeric vector of unadjusted p-values in [0, 1]. `NA` allowed.
#' @param alpha Numeric scalar in (0, 1). Default 0.05. In enrichment settings
#'   a smaller alpha (e.g. 0.01) can help reveal low-frequency pathways.
#'
#' @return A list; use `$p_rescaled` for the rescaled values. Diagnostics also returned:
#'   `expected`, `sd`, `upper`, `observed`, `excess`.
#'
#' @examples
#' set.seed(1)
#' expr <- matrix(rnorm(1000*6), 1000)
#' grp  <- factor(rep(c('A','B'), each = 3))
#' p    <- apply(expr, 1, function(y) t.test(y ~ grp)$p.value)
#' out  <- OmicAdjust(p)                # alias: omic_adjust(p)
#' hist(p,            breaks = 20)
#' hist(out$p_rescaled, breaks = 20)
#'
#' ## Typical data.frame use:
#' ## df$p_OmicAdjust <- OmicAdjust(df$unadjustedPvalues)$p_rescaled
#'
#' @name OmicAdjust
#' @aliases omic_adjust
#' @export OmicAdjust
#' @export omic_adjust
NULL
