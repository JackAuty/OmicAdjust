## ======================================================================
## Omics simulation + BH vs OmicAdjust visuals (no pipes)
## ======================================================================

## Load plotting first
suppressPackageStartupMessages(library(ggplot2))
## Load your package so OmicAdjust() is available
## library(OmicAdjust)

#Graph colour
Col<-"#006699"

## ----- Reusable helpers ------------------------------------------------

# Stable -log10 that never returns Inf for p=0 due to underflow
safe_neglog10 <- function(z) -log10(pmax(z, 1e-300))

# Volcano (three panels) with shared y-axis and dotted alpha line
make_volcano_triple <- function(lfc_hat, p_raw, p_bh, p_oa,
                                alpha = 0.05,
                                col_sig = Col,  # Okabe–Ito reddish-purple
                                col_nonsig = "grey80") {

  cut_y <- -log10(alpha)

  df_raw <- data.frame(lfc = lfc_hat, p_use = p_raw, panel = "Raw p")
  df_bh  <- data.frame(lfc = lfc_hat, p_use = p_bh,  panel = "BH adjusted p")
  df_oa  <- data.frame(lfc = lfc_hat, p_use = p_oa,  panel = "OmicAdjust rescaled p")
  df     <- rbind(df_raw, df_bh, df_oa)

  df$y      <- safe_neglog10(df$p_use)
  df$signif <- df$p_use <= alpha
  df$panel  <- factor(df$panel,
                      levels = c("Raw p","BH adjusted p","OmicAdjust rescaled p"))
  y_max <- ceiling(max(df$y[is.finite(df$y)]))

  ggplot(df, aes(x = lfc, y = y, colour = signif)) +
    geom_hline(yintercept = cut_y, linetype = "dotted") +
    geom_point(alpha = 0.85, size = 1.4, stroke = 0) +
    scale_color_manual(values = c("TRUE" = col_sig, "FALSE" = col_nonsig),
                       guide = "none") +
    coord_cartesian(ylim = c(0, y_max)) +
    facet_wrap(~ panel, nrow = 1, scales = "fixed") +
    labs(x = "observed log fold-change", y = expression(-log[10](p))) +
    theme_classic(base_size = 11) +
    theme(strip.text = element_text(face = "bold"),
          panel.spacing = grid::unit(8, "pt"))
}

# 20-bin histogram from 0 to 1 with the first (0–0.05) bin coloured
plot_hist_20 <- function(x, main,
                         first_col = Col,
                         other_col = "grey80") {
  br <- seq(0, 1, length.out = 21)   # 20 equal bins => width 0.05
  h  <- hist(x, breaks = br, plot = FALSE, right = TRUE, include.lowest = TRUE)
  cols <- rep(other_col, length(h$counts))
  cols[1] <- first_col
  plot(h, col = cols, border = NA, main = main, xlab = "", xlim = c(0, 1))
  box(bty = "l")
}

# Vectorised equal-variance t-test returning p and observed difference
ttest_p_and_diff <- function(X, Y) {
  nA <- ncol(X); nB <- ncol(Y); df <- nA + nB - 2
  s2x <- apply(X, 1, var)
  s2y <- apply(Y, 1, var)
  sp2 <- ((nA - 1) * s2x + (nB - 1) * s2y) / df
  se  <- sqrt(sp2 * (1/nA + 1/nB))
  diff <- rowMeans(Y) - rowMeans(X)
  tstat <- diff / se
  p <- 2 * pt(-abs(tstat), df = df)
  list(p = p, diff = diff)          # diff is your observed log fold-change on this scale
}

## ======================================================================
## SCENARIO 1: mixture (19,500 null + 500 affected)
## ======================================================================

set.seed(42)

# Design
n_null <- 19500
n_alt  <-  500
nA <- 8; nB <- 8
alpha <- 0.05

# Effects on a log scale:
# Draw fold-changes on log2 scale (mean=1.5 makes a strong effect); then randomise sign,
# then convert to a *natural-log* shift that we actually add to Y.
sd_log2 <- 0.8
fc  <- 2^(rnorm(n_alt, mean = 2, sd = sd_log2))  # bigger mean => larger magnitudes
sgn <- sample(c(-1, 1), n_alt, replace = TRUE)
lfc_true <- sgn * log(fc)                           # natural-log shift applied to group B
delta <- c(rep(0, n_null), lfc_true)                # per-gene mean shift (A=0, B=delta)

# Simulate expression (A around 0; B shifted by delta)
m <- n_null + n_alt
X <- matrix(rnorm(m * nA, mean = 0, sd = 1), nrow = m)
Y <- matrix(rnorm(m * nB, mean = 0, sd = 1), nrow = m) + delta

# Test & p-values
tt <- ttest_p_and_diff(X, Y)
p_raw   <- tt$p
lfc_hat <- tt$diff                      # observed log fold-change on the same (ln) scale

# Multiple-testing adjustments
p_bh <- p.adjust(p_raw, "BH")
p_oa <- OmicAdjust(p_raw, alpha = alpha)$p_rescaled

# Volcano (three panels; same y-scale)
p_volc_mix <- make_volcano_triple(lfc_hat, p_raw, p_bh, p_oa, alpha)
print(p_volc_mix)

# Histograms (0–1, 20 bins; first bar coloured)
op <- par(mfrow = c(1,3))
plot_hist_20(p_raw,  main = "Raw p")
plot_hist_20(p_bh,   main = "BH adjusted p")
plot_hist_20(p_oa,   main = "OmicAdjust rescaled")
par(op)

## ======================================================================
## SCENARIO 2: full null (20,000 null; no effects)
## ======================================================================

set.seed(43)

n_null2 <- 20000
nA2 <- 8; nB2 <- 8
alpha2 <- 0.05

# No effects anywhere
delta2 <- rep(0, n_null2)

# Simulate
X2 <- matrix(rnorm(n_null2 * nA2, mean = 0, sd = 1), nrow = n_null2)
Y2 <- matrix(rnorm(n_null2 * nB2, mean = 0, sd = 1), nrow = n_null2) + delta2

# Test & p-values (under the global null, p should be ~ Uniform(0,1))
tt2 <- ttest_p_and_diff(X2, Y2)
p_raw2   <- tt2$p
lfc_hat2 <- tt2$diff

# Adjust
p_bh2 <- p.adjust(p_raw2, "BH")
p_oa2 <- OmicAdjust(p_raw2, alpha = alpha2)$p_rescaled

# Volcano (expect a "flat cloud" and very few/zero rejections)
p_volc_null <- make_volcano_triple(lfc_hat2, p_raw2, p_bh2, p_oa2, alpha2)
print(p_volc_null)

# Histograms (should look Uniform; first bar marks 0–0.05)
op <- par(mfrow = c(1,3))
plot_hist_20(p_raw2,  main = "Raw p (null)")
plot_hist_20(p_bh2,   main = "BH p (null)")
plot_hist_20(p_oa2,   main = "OmicAdjust p (null)")
par(op)
