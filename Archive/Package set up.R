# Turn this folder into a git repo, add everything, make first commit
library(gert)
git_init(".")
git_add(".")
git_commit("Initial OmicAdjust package commit")

# Create the GitHub repo and push (uses your saved GitHub token)
usethis::use_github(private = FALSE)
