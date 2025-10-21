
# OmicAdjust

**OmicAdjust** provides a pragmatic *p-value redistribution* for
**exploratory** analysis of large omics screens.

- Run your **standard test** to get a column of **unadjusted** p-values
  (one per gene/feature).
- Then pass that vector to `OmicAdjust()` (or the alias
  `omic_adjust()`).
- Use the returned `p_rescaled` for **plots and triage** — for
  enrichment analyses and inference.

## Installation

``` r
# install.packages("remotes")  # if needed
remotes::install_github("JackAuty/OmicAdjust")
library(OmicAdjust)
```

## Typical workflow

1.  **Compute unadjusted p-values** using your usual method (e.g.,
    per-gene t-test, limma, DESeq2, edgeR). Use the **raw p-values**
    column.
2.  **Rescale** with OmicAdjust and keep `$p_rescaled`.
3.  **Plot** histograms with **20 breaks** so the first bar spans 0–0.05
    (the “significance bar”).

``` r
# Example: per-gene t-test to create unadjusted p-values
set.seed(1)
expr <- matrix(rnorm(1000 * 6), nrow = 1000)           # 1000 genes x 6 samples
grp  <- factor(rep(c("A","B"), each = 3))
p    <- apply(expr, 1, function(y) t.test(y ~ grp)$p.value)

# Rescale (alpha defaults to 0.05; try 0.01 for enrichment-style workflows)
out <- OmicAdjust(p, alpha = 0.05)      # alias: out <- omic_adjust(p, alpha = 0.05)

# Just the rescaled values (typical data-frame usage)
# df$p_OmicAdjust <- OmicAdjust(df$unadjustedPvalues)$p_rescaled
p_adj <- out$p_rescaled

# Visual check: 20 bins -> each bar is width 0.05, so 0–0.05 is a single bar
op <- par(mfrow = c(1,2))
hist(p,     breaks = 20, main = "Raw p-values",            xlab = "p")
hist(p_adj, breaks = 20, main = "OmicAdjust (rescaled)",   xlab = "rescaled p")
par(op)
```

### Choosing `alpha`

- Default is **0.05**.  
- In **enrichment** settings (where many genes are “significant”), a
  smaller `alpha` (e.g., **0.01**) can prevent very small p-values from
  obscuring low-frequency pathways and pathways with subtle effects.

## What OmicAdjust does (brief)

At a chosen `alpha`, it computes the null expectation
(`expected = n * alpha`) with standard deviation
`sqrt(n * alpha * (1 - alpha))` and a 95% upper bound on false
positives. It counts observed `p < alpha`, then **redistributes**: the
`excess` smallest p-values go into `[0, alpha]`, the rest into
`[alpha, 1]`. A monotone tweak ensures rescaled values are never smaller
than the original sorted p-values.

## Two entry points (same behavior)

- `OmicAdjust(p, alpha = 0.05)` — TitleCase, exported
- `omic_adjust(p, alpha = 0.05)` — snake_case alias, also exported

Both names return the same list; use `$p_rescaled` for the rescaled
values.

## Output structure

``` r
str(out)
# $ p_rescaled: num [1:1000] ...
# $ expected  : num
# $ sd        : num
# $ upper     : num
# $ observed  : int
# $ excess    : int
```

## Citation

If you use OmicAdjust in a manuscript, please cite the package
repository:

> Rivers-Auty J. **OmicAdjust**: pragmatic p-value redistribution for
> exploratory omics. GitHub: JackAuty/OmicAdjust.
