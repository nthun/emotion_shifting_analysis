# Read the third batch from the zip directly
# From the vroom help page
read_all_zip <- function(file, ...) {
  filenames <- unzip(file, list = TRUE)$Name
  vroom(purrr::map(filenames, ~ unz(file, .x)), ...)
}
