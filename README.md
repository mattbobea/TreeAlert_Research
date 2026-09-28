# TreeAlert Research

Code and data for reproducing the TreeAlert results reported in the paper.
Run all commands from this repository's root directory. No local author paths
are required.

## 1. Install R packages

Use R 4.4.2 where possible. Install the required packages once:

```r
install.packages(c(
  "readr", "tidyr", "feasts", "tsibble", "fable", "fabletools",
  "dplyr", "lubridate", "ggplot2", "ggtext", "gridExtra", "slider",
  "scales", "rpart", "rpart.plot", "rmarkdown", "knitr", "forecast"
))
```

The tested package versions are recorded in `reproduction_session_info.txt`.

## 2. Reproduce the paper

Run the main electricity and heart-rate analyses, including sensitivity and
20-fold cross-validation results:

```bash
Rscript reproduce_examples.R
```

Run the probabilistic heart-rate and Scenario B appendix workflows:

```bash
Rscript reproduce_appendices.R
```

The scripts recreate figures and result tables under `Electricity/`,
`Heart_Rate/`, and `Simulation_Scenario_B/`. They also place the paper figure
files in `Latex_Files/Figures/` for direct comparison with the manuscript.

Set `SCENARIO_B_REPLICATION_SEEDS` to a comma-separated list to run selected
Scenario B replication seeds; the default is `1001:1100`.

## 3. Repository contents

- `Electricity/`: electricity analysis and included input data.
- `Heart_Rate/`: point-forecast and probabilistic heart-rate analyses and data.
- `Simulation_Scenario_B/`: Scenario B simulation and validation workflows.
- `TA_functions/`: shared TreeAlert functions.
- `reproduce_examples.R`: main-paper analyses and sensitivity checks.
- `reproduce_appendices.R`: appendix workflows.

All included input data are local to the repository. Generated files can be
deleted and recreated by rerunning the scripts.
