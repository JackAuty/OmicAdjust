# 1) Activate usethis here (no new windows)
usethis::proj_set(".", force = TRUE)
usethis::proj_sitrep()  # should now show an active usethis project

# 2) Build docs (NAMESPACE + man/)
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::document()

# 3) Git init if needed, make sure you are on main, commit everything
if (!requireNamespace("gert", quietly = TRUE)) install.packages("gert")
library(gert)

if (!gert::git_openable(".")) gert::git_init(".")
if (!("main" %in% gert::git_branch_list()$name)) gert::git_branch_create("main")
gert::git_branch_switch("main")

gert::git_add(".")
if (nrow(gert::git_status()) > 0L) {
  gert::git_commit("OmicAdjust: function, docs, tests, licence, README")
}

# 4) Push to GitHub
# Make sure you have a PAT stored once per machine: gitcreds::gitcreds_set()
if (!requireNamespace("usethis", quietly = TRUE)) install.packages("usethis")

if (!("origin" %in% gert::git_remote_list()$name)) {
  # Create the repo on GitHub and push
  usethis::use_github(private = FALSE)
} else {
  # Remote already exists, just push
  gert::git_push(remote = "origin", set_upstream = TRUE)
}

# 5) Optional README build and final push
if (file.exists("README.Rmd")) {
  try(devtools::build_readme(), silent = TRUE)  # needs pandoc
  gert::git_add(c("README.Rmd", "README.md"))
  if (nrow(gert::git_status()) > 0L) {
    gert::git_commit("Build README")
    try(gert::git_push(), silent = TRUE)
  }
}
