# TreeAlert reproduction guide

This guide reproduces the empirical demonstrations in Section 4 of the paper
and the supporting analyses in Appendices A–E. The introduction, conceptual
scenarios, and other material before Section 4 are not intended to be
reproduced. The optional Section 3.5 rule-visualization illustration is
covered separately below.

## 1. Setup

Run commands from the repository root. Use R 4.x and install the packages
listed in `reproduce_examples.R`:

```r
install.packages(c(
  "readr", "tidyr", "feasts", "tsibble", "fable", "fabletools",
  "dplyr", "lubridate", "ggplot2", "ggtext", "gridExtra", "slider",
  "scales", "rpart", "rpart.plot", "rmarkdown", "knitr", "forecast"
))
```

The shared TreeAlert implementation is in `TA_functions/TA_functions.R` and
the rule-interpretation functions are in `TA_functions/Rephrase_Rule_Func.R`.
The analyses use fixed seeds where resampling or simulation is involved.

## 2. Section 4 — Demonstrations of TreeAlert

### 2.1 Example 1 — Electricity demand

Source analysis: `Electricity/Electricity_Main.Rmd`

Input data: `Electricity/Elec_data/time_series_15min_singleindex.csv.gz`

The script reads the compressed file automatically. If an uncompressed file
named `time_series_15min_singleindex.csv` is present, it is used instead.

Run the electricity analysis as part of the complete reproduction command:

```text
Rscript reproduce_examples.R
```

The analysis uses the Germany–Luxembourg load and operational forecast
columns, constructs temporal features, partitions the series chronologically,
fits the TreeAlert regression tree, extracts the retained rules, and computes
lift and sensitivity results.

Generated analysis outputs are written to:

- Figures: `Electricity/images/`
- Sensitivity figures: `Electricity/images/sensitivity/`
- Cross-validation results: `Electricity/results/electricity_kfold_cv_lift.csv`
- Training-sample validation results: `Electricity/results/electricity_training_sample_cv_lift.csv`

The figures used in the submission PDF are synchronized to
`Latex_Files/Figures/`, including:

| Paper content | Generated source | LaTeX asset |
|---|---|---|
| Electricity series and errors | `Electricity/images/Electricity.png` | `Latex_Files/Figures/Electricity.png` |
| Training/test lift | `Electricity/images/Electricity_Lift_Train.png`, `Electricity_Lift_Test.png` | `Latex_Files/Figures/Electricity_Lift_Train.png`, `Electricity_Lift_Test.png` |
| 20-fold lift distributions | `Electricity/images/electricity_kfold_cv_lift.png` | `Latex_Files/Figures/electricity_kfold_cv_lift.png` |
| Hyperparameter sensitivity | `Electricity/images/sensitivity/elec_*sensitivity*_lift.png` | corresponding `Latex_Files/Figures/elec_*` assets |
| Split, rule-count, and quantile sensitivity | `Electricity/images/sensitivity/elec_*_lift.png` | corresponding `Latex_Files/Figures/elec_*` assets |

### 2.2 Example 2 — Heart rate

Source analysis: `Heart_Rate/Heart_Rate_Main.Rmd`

Input data: `Heart_Rate/HR_data/heart_combined.rds`

Run it with:

```text
Rscript reproduce_examples.R
```

The analysis uses the point-forecast residual and mean-absolute-error lift
setting reported in Section 4.2. It constructs temporal and contextual
features, performs the chronological split, fits TreeAlert, extracts rules,
and computes lift and sensitivity results.

Generated outputs are written to:

- Figures: `Heart_Rate/images/`
- Sensitivity figures: `Heart_Rate/images/sensitivity/`
- Cross-validation results: `Heart_Rate/results/heart_rate_kfold_cv_lift.csv`
- Training-sample validation results: `Heart_Rate/results/heart_rate_training_sample_cv_lift.csv`

The main-paper assets are synchronized to `Latex_Files/Figures/`:

| Paper content | Generated source | LaTeX asset |
|---|---|---|
| Heart-rate series and errors | `Heart_Rate/images/hr_mae_viz.png` | `Latex_Files/Figures/hr_viz.png` |
| Training/test lift | `Heart_Rate/images/hr_mae_lift_train.png`, `hr_mae_lift_test.png` | `Latex_Files/Figures/HR_Lift_Train.png`, `HR_Lift_Test.png` |
| 20-fold lift distributions | `Heart_Rate/images/hr_mae_kfold_cv_lift.png` | `Latex_Files/Figures/hr_mae_kfold_cv_lift.png` |
| Hyperparameter, split, rule-count, and quantile sensitivity | `Heart_Rate/images/sensitivity/hr_mae_*` | corresponding `Latex_Files/Figures/hr_*` assets |

## 3. Optional Section 3.5 illustration

The additional rule-selection/visualization illustration is not required for
the main Section 4 reproduction. The current paper’s implementation is
represented by the electricity and heart-rate analysis objects and the
alternative positive/negative rule example in
`Electricity/Appendix_Elect__positive_Negative_Rules.Rmd`.

That legacy file contains historical absolute paths and should be treated as
an optional reference rather than the primary reproduction entry point. The
portable shared functions used by the paper are in `TA_functions/`.

## 4. Appendix A — Probabilistic forecasting of heart rate

Source: `Heart_Rate/Heart_Rate_Probabilistic_Main.Rmd`

Inputs:

- `Heart_Rate/HR_data/heart_combined.rds`
- `Heart_Rate/HR_data/heart_new_data.rds`

Set `PROB_SCORE <- "winkler_score"` for the paper’s Winkler-score analysis,
then render the R Markdown file from the repository root. The script writes
the probabilistic figures to `Heart_Rate/images/`, using the
`hr_winkler_score_` prefix. The corresponding submission assets are
`Latex_Files/Figures/winkler_lift_train_tf.jpg`,
`winkler_lift_test_tf.jpg`, and `winkler_full.png`.

The electricity application does not have a probabilistic reproduction because
the source operational forecasts are point forecasts and the forecasting model
is not disclosed.

## 5. Appendix B — Rule selection for over- and under-forecasting

Appendix B is a rule-selection variant of the electricity analysis. It retains
the strongest positive and negative mean-error terminal nodes separately. The
underlying rule extraction and interpretation functions are in
`TA_functions/TA_functions.R` and `TA_functions/Rephrase_Rule_Func.R`.

The appendix assets are:

- Rules: `Latex_Files/Figures/Elec_3x3_both.png` and the corresponding table in
  `Latex_Files/JBA_R1.tex`
- Supporting electricity figures: `Latex_Files/Figures/elec_actual_predicted.png`,
  `elec_residual_plot.png`, and `Elec_3x3_actual_fit.png`

Because the historical Appendix B R Markdown file contains non-portable paths,
the README should be used with the portable main electricity analysis and the
shared rule-selection functions when recreating this appendix.

## 6. Appendix C — Sensitivity analysis

Appendix C is generated from the sensitivity sections of the main analysis
files. It covers:

1. Tree hyperparameters (`cp`, minimum bucket size, and maximum depth)
2. Temporal train/test split choices
3. Temporal feature granularity
4. Number of retained rules (`k`)
5. Top-quantile size used for lift evaluation

Run:

```text
Rscript reproduce_examples.R
```

The source figures are saved under:

- `Electricity/images/sensitivity/`
- `Heart_Rate/images/sensitivity/`

The synchronized appendix assets are the `elec_*` and `hr_*` sensitivity
figures in `Latex_Files/Figures/`. The feature-granularity values displayed in
the appendix table are produced by the feature-sensitivity objects in the two
main R Markdown analyses.

## 7. Appendix D — Randomized 20-fold cross-validation

The shared implementation is `tree_alert_kfold_cross_validation()` in
`TA_functions/TA_functions.R`. The main analyses call it with:

- `n_folds = 20`
- `n_groups = 20`
- fixed `fold_seed = 3001`
- mean-absolute-error lift
- an unchanged external test set across folds

The complete reproduction command is:

```text
Rscript reproduce_examples.R
```

CSV summaries are written to:

- `Electricity/results/electricity_kfold_cv_lift.csv`
- `Heart_Rate/results/heart_rate_kfold_cv_lift.csv`

The corresponding figures are `electricity_kfold_cv_lift.png` and
`hr_mae_kfold_cv_lift.png` in both the analysis image directories and the
LaTeX figure directory.

## 8. Appendix E — Scenario B lunar-calendar simulation

Representative simulation source:
`Simulation_Scenario_B/code/B_Simulation_Main.Rmd`

Cross-validation source:
`Simulation_Scenario_B/code/run_scenario_b_cv.R`

Replication-sensitivity source:
`Simulation_Scenario_B/code/B_Simulation_Sensitivity.Rmd`

The data-generating process is implemented in
`Simulation_Scenario_B/code/generate_actual_and_forecasts_data.R`. It records
the generated weekly series and ARIMA metadata under
`Simulation_Scenario_B/data/`.

Run the representative study from the repository root by rendering
`B_Simulation_Main.Rmd`. Run the 20-fold cross-validation with:

```text
Rscript Simulation_Scenario_B/code/run_scenario_b_cv.R
```

Run the broader replication sensitivity analysis by rendering
`Simulation_Scenario_B/code/B_Simulation_Sensitivity.Rmd`. Set
`SCENARIO_B_REPLICATION_SEEDS` to a comma-separated list to control the
replication seeds.

Outputs are written to:

- Data: `Simulation_Scenario_B/data/`
- Results: `Simulation_Scenario_B/results/`
- Figures: `Simulation_Scenario_B/images/`
- Paper assets: `Latex_Files/Figures/scenario_b_*.png`

Important reproducibility parameters are recorded in
`scenario_b_arima_metadata.csv`, `scenario_b_cv_seeds.csv`, and the generated
cross-validation CSV files. The representative paper values should be checked
against the current manuscript after running the scripts, because changing the
simulation seed, ARIMA implementation, or aggregation convention changes the
results.

## 9. Rebuilding the submission PDF

After regenerating the figures, compile the manuscript from `Latex_Files/`:

```text
cd Latex_Files
latexmk -pdf JBA_R1.tex
```

The source archive for Overleaf is `Latex_Files/JBA_R1_Overleaf.zip`. The
pre-generated figures are included so the manuscript can be compiled without
first running the R analyses.

## 10. Reproduction validation

The validation checklist is:

1. Run `Rscript reproduce_examples.R` from a clean R session.
2. Confirm that each generated paper figure is byte-identical to its
   synchronized copy in `Latex_Files/Figures/`.
3. Compare the generated rule tables, selected hyperparameters, flagged-period
   counts, lift values, and cross-validation summaries with the values in
   `Latex_Files/JBA_R1.tex`.
4. Run the Scenario B scripts and compare its generated data, selected rules,
   lift values, and 20-fold distributions with Appendix E.
5. Compile `JBA_R1.tex` and confirm that all figure paths resolve.

`reproduce_examples.R` records the local R and package versions in
`reproduction_session_info.txt` after a successful run.

### Current validation status

The main-example render completed successfully in a clean R session. The
generated figures were byte-identical to the synchronized copies used by
`JBA_R1.tex`, and all manuscript image paths resolved.

The electricity analysis now reproduces the manuscript values: 2,119 training
observations, 696 test observations, selected parameters `cp = 0`, minimum
bucket size 4, maximum depth 8, and first-ventile lift 1.693870 in training
and 1.381551 in testing (reported in the paper as 1.69 and 1.38). Its
20-fold summary matches Appendix D to the reported precision.

The current heart-rate analysis produces 3,240 training observations, 572 test
observations, training lift 2.63, and test lift 2.85, matching the rounded
values reported in Section 4.2. The heart-rate 20-fold summary also matches
the appendix values to the reported precision.

The Scenario B representative outputs reproduce the reported rounded values:
731 training observations, 157 test observations, training lift 2.25, and test
lift 2.16. Its seed and ARIMA metadata are retained in
`Simulation_Scenario_B/data/` and its cross-validation results are retained in
`Simulation_Scenario_B/results/`.
