# Use usethis in THIS project (no new windows)
library(usethis)
proj_set(".", force = TRUE)

# (One-time global identity; safe to run again)
use_git_config(user.name = "JackAuty", user.email = "jack.auty@utas.edu.au")

# Initialise Git for this folder and make the first commit
# RStudio will ask to restart once; click "Yes".
use_git()
