############################################################
# OmicAdjust: build a package from scratch in THIS folder
# Run this in an empty folder named "OmicAdjust".
# No new RStudio windows will open.
############################################################

## ─────────────────────────────────────────────────────────
## 0) One-time prerequisites (install if missing)
## ─────────────────────────────────────────────────────────
# install.packages(c("usethis","gitcreds","devtools","roxygen2"))

## ─────────────────────────────────────────────────────────
## 1) Pin usethis to THIS folder and create a package here
## ─────────────────────────────────────────────────────────
library(usethis)

# Always act in the current directory
proj_set(".", force = TRUE)

# Create DESCRIPTION, NAMESPACE scaffolding here (no new window)
# If DESCRIPTION already exists, this is skipped.
if (!file.exists("DESCRIPTION")) {
  create_package(path = ".", open = FALSE)
}

# Good to set HTTPS as default for remotes
options(usethis.protocol = "https")

## ─────────────────────────────────────────────────────────
## 2) Set Git identity and initialise Git
## ─────────────────────────────────────────────────────────
# Safe to repeat on the same machine
use_git_config(user.name  = "JackAuty",
               user.email = "jack.auty@utas.edu.au")

# Initialise a Git repo if needed, make the first commit
# RStudio may prompt for a restart: choose "Yes".
use_git()

## ─────────────────────────────────────────────────────────
## 3) Create a GitHub Personal Access Token (PAT) if needed
## ─────────────────────────────────────────────────────────
# Only do this once per machine. If you have a valid token already, skip to "Store token".
# If you want to clear a bad token first, uncomment:
# gitcreds::gitcreds_delete("https://github.com")

# Create the token in your browser:
# In the browser choose either a Fine-grained token (repo read/write) or a Classic token with scope "repo".
# Copy the generated token (it looks like ghp_xxxxx...).
create_github_token()

# Store token in this R session and OS keychain:
# Console prompt: "Enter password or token:"
# → paste your token and press Enter. It will not echo.
gitcreds::gitcreds_set()

## ─────────────────────────────────────────────────────────
## 4) Package metadata, license, README skeleton
## ─────────────────────────────────────────────────────────
# MIT license with your name
use_mit_license("Jack Rivers-Auty")

# Add a tidy README.Rmd, do not open editor
use_readme_rmd(open = FALSE)

# Suggests fields and roxygen options are handled by usethis and roxygen2

## ─────────────────────────────────────────────────────────
## 5) Add the core function and package docs
## ─────────────────────────────────────────────────────────
dir.create("R", showWarnings = FALSE)

# Write a minimal working function (with an alias) so the package builds.
# This is a simple monotonic rescaling for exploratory plots. Replace with your final method later.
writeLines(c(
  "#' OmicAdjust (aka `omic_adjust`): pragmatic p-value redistribution for exploration",
  "#'",
  "#' Provide a numeric vector of raw p-values. The function returns a list with",
  "#' rescaled values in `$p_rescaled` plus simple diagnostics. Intended for",
  "#' exploration only, not FDR control.",
  "#'",
  "#' @param p Numeric vector of unadjusted p-values in [0, 1]. `NA` allowed.",
  "#' @param alpha Numeric scalar in (0, 1). Default 0.05, used for simple diagnostics.",
  "#' @return A list with elements: `p_rescaled`, `expected`, `observed`, `excess`.",
  "#' @examples",
  "#' set.seed(1)",
  "#' p <- runif(1000)",
  "#' out <- OmicAdjust(p)",
  "#' str(out)",
  "#' hist(out$p_rescaled, breaks = 20)",
  "#' @name OmicAdjust",
  "#' @aliases omic_adjust",
  "#' @export OmicAdjust",
  "#' @export omic_adjust",
  "OmicAdjust <- function(p, alpha = 0.05) {",
  "  if (!is.numeric(p)) stop('`p` must be numeric')",
  "  n <- sum(!is.na(p))",
  "  # Simple, stable, monotonic rescale by mid-ranks over non-missing values",
  "  r <- rank(p, na.last = 'keep', ties.method = 'average')",
  "  p_rescaled <- r / (n + 1)",
  "  expected <- n * alpha",
  "  observed <- sum(p <= alpha, na.rm = TRUE)",
  "  excess <- observed - expected",
  "  list(p_rescaled = p_rescaled, expected = expected, observed = observed, excess = excess)",
  "}",
  "",
  "# Alias",
  "omic_adjust <- OmicAdjust"
), "R/OmicAdjust.R")

# Package-level documentation
writeLines(c(
  "#' OmicAdjust: pragmatic p-value redistribution for exploratory work",
  "#'",
  "#' Tools for exploring p-value distributions by simple redistribution ideas, with",
  "#' clear emphasis that this is for exploration, not FDR control.",
  "#'",
  "#' @docType package",
  "#' @name OmicAdjust-package",
  "NULL"
), "R/OmicAdjust-package.R")

## ─────────────────────────────────────────────────────────
## 6) Generate documentation and NAMESPACE
## ─────────────────────────────────────────────────────────
# This runs roxygen2, creates man/ files and NAMESPACE
devtools::document()

## ─────────────────────────────────────────────────────────
## 7) Knit the README and commit
## ─────────────────────────────────────────────────────────
# Knit README.Rmd to README.md (needs rmarkdown and knitr)
# If missing: install.packages(c('rmarkdown','knitr'))
devtools::build_readme()

# Stage and commit
system('git add -A')
system('git commit -m "Initial package: function, docs, license, README"')

## ─────────────────────────────────────────────────────────
## 8) Create the GitHub repo and push
## ─────────────────────────────────────────────────────────
# Console menus you may see:
#  1) “Create a GitHub repository for this project?” → type the number for Yes, press Enter.
#  2) “Select a protocol” → choose the number for HTTPS unless you already use SSH.
#  3) If asked for a token, paste the same token you created above, press Enter.
# Never type negative numbers. If you want to cancel, type 0.
use_github(private = FALSE)

## ─────────────────────────────────────────────────────────
## 9) Build, check, and install locally
## ─────────────────────────────────────────────────────────
# Build a source tarball in ./ (e.g., OmicAdjust_0.0.0.9000.tar.gz)
pkg_file <- devtools::build()

# Run a light check. For a strict CRAN-style check use devtools::check()
devtools::check(document = FALSE)

# Install the built package into your current R library
devtools::install(upgrade = "never")

# Quick sanity test
library(OmicAdjust)
set.seed(42); p <- runif(1000)
out <- OmicAdjust(p)
stopifnot(is.list(out), "p_rescaled" %in% names(out))
message("Package built, installed, and working. All good.")
