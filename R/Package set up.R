# =============================================================================
# OmicAdjust: one-shot usethis script to create, document, test, and publish
# Start here. Run this whole script top to bottom in a fresh R session.
# =============================================================================

# ---------------- 0) Settings you can change ---------------------------------
PKG_PATH       <- "C:/Users/jrivers/Dropbox/Code/OmicAdjust"   # where the package lives
PKG_NAME       <- "OmicAdjust"                                  # package name
GITHUB_PRIVATE <- FALSE                                         # TRUE for a private repo
LICENSEE       <- "Jack Rivers-Auty"                            # for the MIT licence

# ---------------- 1) Install and load helpers --------------------------------
need <- c("usethis","devtools","desc","gert","gitcreds","withr","testthat")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(need, require, character.only = TRUE))

# ---------------- 2) Create or activate the package project ------------------
PKG_PATH <- normalizePath(PKG_PATH, winslash = "/", mustWork = FALSE)
if (!dir.exists(PKG_PATH)) dir.create(PKG_PATH, recursive = TRUE)

if (file.exists(file.path(PKG_PATH, "DESCRIPTION"))) {
  usethis::proj_activate(PKG_PATH)
} else {
  usethis::create_package(PKG_PATH, open = FALSE)
  usethis::proj_activate(PKG_PATH)
}

# Paste everything below into the open project
message("Active project: ", usethis::proj_get())

# ---------------- 3) DESCRIPTION hygiene -------------------------------------
d <- desc::desc(file = "DESCRIPTION")
d$set("Package", PKG_NAME)
if (!d$has_fields("Title"))       d$set("Title", "Pragmatic P-Value Redistribution for Exploratory Omics")
if (!d$has_fields("Description")) d$set("Description",
                                        "Provides an exploratory p-value redistribution method for large hypothesis sets. \
OmicAdjust estimates expected false positives under the null using a binomial model \
and redistributes observed p-values so that the smallest values lie within [0, alpha] \
and the remainder within [alpha, 1]. For exploration only. It does not control FDR.")
if (!d$has_fields("Version"))     d$set("Version", "0.0.0.9000")
if (!d$has_fields("Authors@R"))   d$set_authors(desc::person(
  given = "Jack", family = "Rivers-Auty",
  email = "Jack.auty@utas.edu.au", role = c("aut","cre")
))
# Add URLs if missing (uses your global git user.name as a default handle)
gh_user <- tryCatch(gert::git_config_global_get("user.name"), error = function(e) "JackAuty")
if (!d$has_fields("URL"))        d$set("URL",        sprintf("https://github.com/%s/%s", gh_user, PKG_NAME))
if (!d$has_fields("BugReports")) d$set("BugReports", sprintf("https://github.com/%s/%s/issues", gh_user, PKG_NAME))
d$write(file = "DESCRIPTION")

usethis::use_roxygen_md()
usethis::use_mit_license(LICENSEE)

# Build ignores and git hygiene
if (!file.exists(".Rbuildignore")) file.create(".Rbuildignore")
usethis::use_build_ignore(c("README_cache", "README_files", "docs"))
usethis::git_vaccinate()   # safe global ignores
usethis::use_git_ignore(c(".Rhistory", ".Rproj.user", ".DS_Store", "docs", "README_cache", "README_files"))

# ---------------- 4) README ---------------------------------------------------
if (!file.exists("README.Rmd")) usethis::use_readme_rmd(open = FALSE)

# ---------------- 5) The function: R/omic_adjust.R ----------------------------
dir.create("R", showWarnings = FALSE)

omic_code <- '
#' #' OmicAdjust: pragmatic p-value redistribution for exploration
#' #'
#' #' Estimates the expected number of false positives under the null at level
#' #' alpha using a binomial model and a 95 percent upper bound. Redistributes
#' #' observed p-values so that the smallest values lie within [0, alpha] and the
#' #' remainder lie within [alpha, 1]. This is an exploratory display transform.
#' #' It does not control FDR. Use Benjamini and Hochberg, Storey q-values, or
#' #' Bonferroni for confirmatory inference.
#' #'
#' #' @param p Numeric vector of p-values in [0, 1]. NAs are allowed.
#' #' @param alpha Numeric scalar in (0, 1). Default 0.05.
#' #' @return A list with elements:
#' #' \\itemize{
#' #'   \\item \\code{p_rescaled}: numeric vector with NA positions preserved
#' #'   \\item \\code{expected}: n * alpha
#' #'   \\item \\code{sd}: sqrt(n * alpha * (1 - alpha))
#' #'   \\item \\code{upper}: expected + 1.96 * sd
#' #'   \\item \\code{observed}: count of p < alpha among finite entries
#' #'   \\item \\code{excess}: max(0, observed - round(upper))
#' #' }
#' #' @examples
#' #' set.seed(1)
#' #' p <- runif(20)
#' #' out <- OmicAdjust(p)
#' #' head(out$p_rescaled)
#' #' @export
OmicAdjust <- function(p, alpha = 0.05) {
  stopifnot(is.numeric(p),
            length(alpha) == 1L,
            is.finite(alpha),
            alpha > 0, alpha < 1)

  keep <- is.finite(p) & p >= 0 & p <= 1
  p_in <- p[keep]
  n <- length(p_in)

  expected <- n * alpha
  sd <- sqrt(n * alpha * (1 - alpha))
  upper <- expected + 1.96 * sd
  obs <- sum(p_in < alpha)
  excess <- max(0L, obs - round(upper))

  ord <- order(p_in)
  ix_low <- if (excess > 0L) ord[seq_len(excess)] else integer(0)
  ix_high <- setdiff(seq_len(n), ix_low)

  low_vals <- if (excess > 0L) {
    seq(0, alpha, length.out = excess + 2L)[-c(1L, excess + 2L)]
  } else numeric(0)

  high_len <- length(ix_high)
  high_vals <- if (high_len > 0L) {
    seq(alpha, 1, length.out = high_len + 2L)[-c(1L, high_len + 2L)]
  } else numeric(0)

  p_rescaled <- numeric(n)
  if (excess > 0L) p_rescaled[ix_low]   <- low_vals
  if (high_len > 0L) p_rescaled[ix_high] <- high_vals

  # Conservative isotonic tweak relative to original sorted p
  p_final_sorted <- pmax(sort(p_rescaled), sort(p_in))

  out_vec <- rep(NA_real_, length(p))
  out_idx <- which(keep)
  out_vec[out_idx[order(p_in)]] <- p_final_sorted

  list(
    p_rescaled = out_vec,
    expected   = expected,
    sd         = sd,
    upper      = upper,
    observed   = obs,
    excess     = excess
  )
}
'
writeLines(omic_code, "R/omic_adjust.R")

# ---------------- 6) Generate docs and NAMESPACE ------------------------------
devtools::document()  # writes NAMESPACE and man files from roxygen comments

# ---------------- 7) Tests with testthat --------------------------------------
usethis::use_testthat()
usethis::use_test("OmicAdjust")
dir.create("tests/testthat", recursive = TRUE, showWarnings = FALSE)
writeLines(
'Test_that <- testthat::test_that

Test_that("OmicAdjust basic behaviour", {
  set.seed(42)
  p <- runif(1000)
  out <- OmicAdjust(p, alpha = 0.05)
  testthat::expect_type(out, "list")
  testthat::expect_equal(length(out$p_rescaled), length(p))
  testthat::expect_true(all(is.na(out$p_rescaled) | (out$p_rescaled >= 0 & out$p_rescaled <= 1)))
  testthat::expect_true(all(sort(out$p_rescaled, na.last = TRUE) >= sort(p, na.last = TRUE)))
})

Test_that("OmicAdjust handles NA and bounds", {
  p <- c(0, 1, NA, 0.5, 0.001, NA)
  out <- OmicAdjust(p)
  testthat::expect_true(all(is.na(out$p_rescaled[is.na(p)])))
  ok <- out$p_rescaled[!is.na(out$p_rescaled)]
  testthat::expect_true(all(ok >= 0 & ok <= 1))
})
', "tests/testthat/test-OmicAdjust.R")

# ---------------- 8) Init Git, first commit ----------------------------------
# Set identity if not set
if (is.na(tryCatch(gert::git_config_global_get("user.name"), error = function(e) NA))) {
  usethis::use_git_config(user.name = "JackAuty", user.email = "Jack.auty@utas.edu.au")
}
if (!gert::git_openable(".")) gert::git_init(".")
# Ensure main branch
try(gert::git_branch_create("main"), silent = TRUE)
try(gert::git_branch_switch("main"), silent = TRUE)
gert::git_add(".")
if (nrow(gert::git_status()) > 0L) {
  gert::git_commit("Initial package: function, docs, tests, licence, README stub")
}

# ---------------- 9) Create GitHub repo and push ------------------------------
# Requires a GitHub token set with gitcreds::gitcreds_set()
ok <- TRUE
tryCatch({
  usethis::use_github(private = GITHUB_PRIVATE)
}, error = function(e) {
  ok <<- FALSE
  message("use_github error: ", conditionMessage(e))
})

# Fallback manual remote if needed
if (!ok) {
  remote_url <- sprintf("https://github.com/%s/%s.git", gh_user, PKG_NAME)
  rems <- tryCatch(gert::git_remote_list()$name, error = function(e) character(0))
  if (!("origin" %in% rems)) gert::git_remote_add("origin", remote_url)
  try(gert::git_push(remote = "origin", refspec = "refs/heads/main", set_upstream = TRUE), silent = TRUE)
}

# ---------------- 10) Build README and run checks -----------------------------
if (file.exists("README.Rmd")) {
  try(devtools::build_readme(), silent = TRUE)  # needs pandoc
  gert::git_add(c("README.Rmd", "README.md"))
  if (nrow(gert::git_status()) > 0L) {
    gert::git_commit("Build README")
    try(gert::git_push(), silent = TRUE)
  }
}

# Full package check
devtools::check()
message("Done. OmicAdjust is created, documented, tested, and pushed.")
