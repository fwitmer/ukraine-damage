##########################################################################
# 29 Dec 2025
#  load given library and install it if needed
#
##########################################################################

load_install_lib <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    options(timeout = 300)  # 5 minutes
    cat(paste(pkg, "not installed\n"))
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

