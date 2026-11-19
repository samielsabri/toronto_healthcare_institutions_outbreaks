# Posit Public Package Manager: prebuilt Linux binaries for fast, reproducible installs
options(
  repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/noble/latest"),
  HTTPUserAgent = sprintf(
    "R/%s R (%s)", getRversion(),
    paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
  )
)
source("renv/activate.R")
