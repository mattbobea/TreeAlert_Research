#!/usr/bin/env Rscript

# Rebuild the electricity and heart-rate example analyses and synchronize the
# figures used by Latex_Files/JBA_R1.tex.

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
if (length(file_arg) != 1L) {
  stop("Run this file with Rscript, for example: Rscript reproduce_examples.R")
}
script_path <- normalizePath(sub("^--file=", "", file_arg), mustWork = TRUE)
root_dir <- dirname(script_path)

required_packages <- c(
  "readr", "tidyr", "feasts", "tsibble", "fable", "fabletools",
  "dplyr", "lubridate", "ggplot2", "ggtext", "gridExtra", "slider",
  "scales", "rpart", "rpart.plot", "rmarkdown", "knitr", "forecast"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0L) {
  stop(
    "Install the missing R packages before running this script: ",
    paste(missing_packages, collapse = ", ")
  )
}

render_dir <- tempfile("treealert_render_")
dir.create(render_dir, recursive = TRUE)

render_example <- function(relative_path) {
  input_path <- file.path(root_dir, relative_path)
  if (!file.exists(input_path)) stop("Missing example source: ", input_path)
  message("Rendering ", relative_path)
  set.seed(3001L)
  rmarkdown::render(
    input = input_path,
    output_file = paste0(tools::file_path_sans_ext(basename(input_path)), ".html"),
    output_dir = render_dir,
    knit_root_dir = root_dir,
    envir = new.env(parent = globalenv()),
    quiet = FALSE
  )
}

copy_outputs <- function(mapping) {
  for (i in seq_len(nrow(mapping))) {
    source_path <- file.path(root_dir, mapping$source[[i]])
    target_path <- file.path(root_dir, mapping$target[[i]])
    if (!file.exists(source_path)) stop("Expected figure was not produced: ", source_path)
    dir.create(dirname(target_path), recursive = TRUE, showWarnings = FALSE)
    ok <- file.copy(source_path, target_path, overwrite = TRUE)
    if (!ok) stop("Could not synchronize figure: ", target_path)
    message("  synchronized ", mapping$target[[i]])
  }
}

render_example(file.path("Electricity", "Electricity_Main.Rmd"))
copy_outputs(data.frame(
  source = c(
    "Electricity/images/Electricity.png",
    "Electricity/images/Electricity_Lift_Train.png",
    "Electricity/images/Electricity_Lift_Test.png",
    "Electricity/images/electricity_kfold_cv_lift.png",
    "Electricity/images/sensitivity/elec_sensitivity_train_lift.png",
    "Electricity/images/sensitivity/elec_sensitivity_test_lift.png",
    "Electricity/images/sensitivity/elec_split_sensitivity_train_lift.png",
    "Electricity/images/sensitivity/elec_split_sensitivity_test_lift.png",
    "Electricity/images/sensitivity/elec_k_rule_sensitivity_lift.png",
    "Electricity/images/sensitivity/elec_threshold_sensitivity_lift.png"
  ),
  target = c(
    "Latex_Files/Figures/Electricity.png",
    "Latex_Files/Figures/Electricity_Lift_Train.png",
    "Latex_Files/Figures/Electricity_Lift_Test.png",
    "Latex_Files/Figures/electricity_kfold_cv_lift.png",
    "Latex_Files/Figures/elec_sensitivity_train_lift_fig15.png",
    "Latex_Files/Figures/elec_sensitivity_test_lift.png",
    "Latex_Files/Figures/elec_split_sensitivity_train_lift.png",
    "Latex_Files/Figures/elec_split_sensitivity_test_lift.png",
    "Latex_Files/Figures/elec_k_rule_sensitivity_lift.png",
    "Latex_Files/Figures/elec_threshold_sensitivity_lift.png"
  ),
  stringsAsFactors = FALSE
))

render_example(file.path("Heart_Rate", "Heart_Rate_Main.Rmd"))
copy_outputs(data.frame(
  source = c(
    "Heart_Rate/images/hr_mae_viz.png",
    "Heart_Rate/images/hr_mae_lift_train.png",
    "Heart_Rate/images/hr_mae_lift_test.png",
    "Heart_Rate/images/hr_mae_kfold_cv_lift.png",
    "Heart_Rate/images/sensitivity/hr_mae_sensitivity_train_lift.png",
    "Heart_Rate/images/sensitivity/hr_mae_sensitivity_test_lift.png",
    "Heart_Rate/images/sensitivity/hr_mae_split_sensitivity_train_lift.png",
    "Heart_Rate/images/sensitivity/hr_mae_split_sensitivity_test_lift.png",
    "Heart_Rate/images/sensitivity/hr_mae_k_rule_sensitivity_lift.png",
    "Heart_Rate/images/sensitivity/hr_mae_threshold_sensitivity_lift.png"
  ),
  target = c(
    "Latex_Files/Figures/hr_viz.png",
    "Latex_Files/Figures/HR_Lift_Train.png",
    "Latex_Files/Figures/HR_Lift_Test.png",
    "Latex_Files/Figures/hr_mae_kfold_cv_lift.png",
    "Latex_Files/Figures/hr_sensitivity_train_lift.png",
    "Latex_Files/Figures/hr_sensitivity_test_lift.png",
    "Latex_Files/Figures/hr_split_sensitivity_train_lift.png",
    "Latex_Files/Figures/hr_split_sensitivity_test_lift.png",
    "Latex_Files/Figures/hr_k_rule_sensitivity_lift.png",
    "Latex_Files/Figures/hr_mae_threshold_sensitivity_lift.png"
  ),
  stringsAsFactors = FALSE
))

message("Example analyses and LaTeX figure assets were rebuilt successfully.")
message("Rendered HTML files are in: ", render_dir)
session_info_path <- file.path(root_dir, "reproduction_session_info.txt")
writeLines(capture.output(sessionInfo()), session_info_path)
message("R session information written to: ", session_info_path)
