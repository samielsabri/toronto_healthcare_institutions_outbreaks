# Source the project's R/ functions. testthat runs with tests/testthat as the
# working directory, so the project root is two levels up.
PROJECT_ROOT <- normalizePath(file.path("..", ".."))
for (f in list.files(file.path(PROJECT_ROOT, "R"), pattern = "\\.R$", full.names = TRUE)) {
  source(f, local = FALSE)
}
root_path <- function(...) file.path(PROJECT_ROOT, ...)
