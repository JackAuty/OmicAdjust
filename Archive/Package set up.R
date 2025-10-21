# Use usethis in THIS project (no new windows)
library(usethis)
proj_set(".", force = TRUE)

gitcreds::gitcreds_delete("https://github.com")

# Create a new token in your browser
usethis::create_github_token()     # copy the token shown at the end

# Paste the token when prompted:
gitcreds::gitcreds_set()

# (One-time global identity; safe to run again)
use_git_config(user.name = "JackAuty", user.email = "jack.auty@utas.edu.au")

# Initialise Git for this folder and make the first commit
# RStudio will ask to restart once; click "Yes".
use_git()


proj_set(".", force = TRUE)

# If you haven't saved a GitHub token on this machine yet, do these once:
# create_github_token()
# gitcreds::gitcreds_set()  # paste the token when prompted

# Create `JackAuty/OmicAdjust` on GitHub and push the current branch
use_github(private = FALSE)

usethis::use_readme_rmd(open = TRUE)
# 1) Re-knit README.md from README.Rmd
# (needs rmarkdown + knitr; install if missing)
# install.packages(c("rmarkdown","knitr"))
devtools::build_readme()

# 2) Stage just the README files (or everything)
system('git add README.Rmd README.md')

# 3) Commit & push
system('git commit -m "Re-knit README"')
br <- system('git branch --show-current', intern = TRUE)
system(sprintf('git push -u origin %s', br))


# Creates R/OmicAdjust_help.R (docs both names; exports both)
dir.create("R", showWarnings = FALSE)
writeLines(c(
  "#' OmicAdjust (aka `omic_adjust`): pragmatic p-value redistribution for exploration",
  "#'",
  "#' Run your standard test to get **unadjusted** p-values, then rescale with",
  "#' `OmicAdjust()` (or `omic_adjust()`). Intended for **exploration only**; not FDR control.",
  "#'",
  "#' @param p Numeric vector of unadjusted p-values in [0, 1]. `NA` allowed.",
  "#' @param alpha Numeric scalar in (0, 1). Default 0.05. In enrichment settings",
  "#'   a smaller alpha (e.g. 0.01) can help reveal low-frequency pathways.",
  "#'",
  "#' @return A list; use `$p_rescaled` for the rescaled values. Diagnostics also returned:",
  "#'   `expected`, `sd`, `upper`, `observed`, `excess`.",
  "#'",
  "#' @examples",
  "#' set.seed(1)",
  "#' expr <- matrix(rnorm(1000*6), 1000)",
  "#' grp  <- factor(rep(c('A','B'), each = 3))",
  "#' p    <- apply(expr, 1, function(y) t.test(y ~ grp)$p.value)",
  "#' out  <- OmicAdjust(p)                # alias: omic_adjust(p)",
  "#' hist(p,            breaks = 20)",
  "#' hist(out$p_rescaled, breaks = 20)",
  "#'",
  "#' ## Typical data.frame use:",
  "#' ## df$p_OmicAdjust <- OmicAdjust(df$unadjustedPvalues)$p_rescaled",
  "#'",
  "#' @name OmicAdjust",
  "#' @aliases omic_adjust",
  "#' @export OmicAdjust",
  "#' @export omic_adjust",
  "NULL"
), "R/OmicAdjust_help.R")

writeLines(c(
  "#' OmicAdjust: pragmatic p-value redistribution (exploratory)",
  "#'",
  "#' @description Tools for exploring omics p-value distributions by redistributing",
  "#' p-values relative to an expected null count at alpha.",
  "#'",
  "#' @docType package",
  "#' @name OmicAdjust-package",
  "NULL"
), "R/OmicAdjust-package.R")

devtools::document()
