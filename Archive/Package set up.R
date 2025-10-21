# Use usethis in THIS project (no new windows)
library(usethis)
proj_set(".", force = TRUE)

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
