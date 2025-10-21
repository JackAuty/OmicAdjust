# make sure usethis is available
install.packages("usethis")  # skip if already installed
library(usethis)

# ensure we're operating in this project (no new windows)
proj_set(".", force = TRUE)

# (one-time) set your Git identity, globally
use_git_config(user.name = "JackAuty", user.email = "jack.auty@utas.edu.au")

# initialise Git here and make the first commit
use_git()  # RStudio will likely ask to restart once; click "Yes"

usethis::use_git_remote("origin", url = NULL, overwrite = TRUE)

use_github(private = FALSE)
