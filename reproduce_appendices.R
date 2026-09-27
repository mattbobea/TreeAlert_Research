#!/usr/bin/env Rscript

# Reproduce appendix analyses without modifying the R1 manuscript or the
# main electricity/heart-rate analysis files.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) != 1L) stop("Run with: Rscript reproduce_appendices.R")

root_dir <- dirname(normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE))
setwd(root_dir)

required_packages <- c("rmarkdown", "readr", "dplyr", "lubridate", "forecast")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                               logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
  stop("Install required packages: ", paste(missing_packages, collapse = ", "))
}

render_dir <- tempfile("treealert_appendix_render_")
dir.create(render_dir, recursive = TRUE)

render_appendix <- function(input) {
  message("Rendering ", input)
  rmarkdown::render(input = input, output_dir = render_dir,
                    knit_root_dir = root_dir,
                    envir = new.env(parent = globalenv()), quiet = FALSE)
}

# Appendix A: probabilistic heart-rate analysis.
render_appendix("Heart_Rate/Heart_Rate_Probabilistic_Main.Rmd")

# Appendix E: representative Scenario B analysis and replication sensitivity.
dir.create("Simulation_Scenario_B/data", recursive = TRUE, showWarnings = FALSE)
render_appendix("Simulation_Scenario_B/code/B_Simulation_Main.Rmd")

status <- system2(file.path(R.home("bin"), "Rscript"),
                  "Simulation_Scenario_B/code/run_scenario_b_cv.R")
if (!identical(status, 0L)) stop("Scenario B cross-validation failed.")

render_appendix("Simulation_Scenario_B/code/B_Simulation_Sensitivity.Rmd")
message("Appendix A and Appendix E reproduction completed.")
