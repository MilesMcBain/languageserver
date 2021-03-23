test_that("configurable source globs works", {
  skip_on_cran()

  test_project_dir <- file.path(tempdir(), "test_project")
  create_source_dir(test_project_dir, "R")
  create_source_dir(test_project_dir, "parse")
  create_source_dir(test_project_dir, "no-parse")

  client <- language_client(working_dir = test_project_dir)

  unlink(test_project_dir, recursive = TRUE)
})

create_source_file <- function(path, name = "test") {
  source_file <- file.path(path, paste0("test_", name, "_file.R"))
  if (file.exists(source_file)) unlink(source_file)
  file.create(source_file)
  writeLines(
    c(
      paste0("test_", name, "_a <- function(x) { x + 1 }"),
      paste0("test_", name, "_b <- 1")
    ),
    source_file
  )
}

create_source_dir <- function(path, name = "test") {
  source_dir <- file.path(path, name)
  if (dir.exists(source_dir)) unlink(source_dir, recursive = TRUE)
  dir.create(source_dir)
  create_source_file(file.path(path, name), name)
}
