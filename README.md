# TreeAlert reproducibility guide

This repository contains the code and data for the TreeAlert demonstrations
and supporting analyses in the paper. All commands below use paths relative to
the repository root; they do not depend on the original author's computer.

## Quick start

Install R 4.4.2 or a compatible R 4.x release. Then install the packages used
by the tested environment:

```r
install.packages(c(
  "readr", "tidyr", "feasts", "tsibble", "fable", "fabletools",
  "dplyr", "lubridate", "ggplot2", "ggtext", "gridExtra", "slider",
  "scales", "rpart", "rpart.plot", "rmarkdown", "knitr", "forecast"
))
```

From the repository root, reproduce the main electricity and heart-rate
analyses, including sensitivity and cross-validation outputs:

```text
Rscript reproduce_examples.R
```

Reproduce Appendix A and the Scenario B appendices with:

```text
Rscript reproduce_appendices.R
```

The Scenario B wrapper creates its generated-data directory automatically.
Set `SCENARIO_B_REPLICATION_SEEDS` to a comma-separated list to run selected
replication seeds; the default is `1001:1100`.

## Tested environment

The successful validation run used R 4.4.2 and these package versions:

| Package | Version | Package | Version |
|---|---:|---|---:|
| readr | 2.1.5 | tidyr | 1.3.1 |
| feasts | 0.4.1 | tsibble | 1.1.6 |
| fable | 0.4.1 | fabletools | 0.5.0 |
| dplyr | 1.1.4 | lubridate | 1.9.4 |
| ggplot2 | 3.5.1 | ggtext | 0.1.2 |
| gridExtra | 2.3 | slider | 0.3.2 |
| scales | 1.3.0 | rpart | 4.1.23 |
| rpart.plot | 3.1.2 | rmarkdown | 2.29 |
| knitr | 1.49 | forecast | 8.23.0 |

Package updates can change numerical results or figure rendering. For exact
replication, use the versions above. The complete recorded session is in
`reproduction_session_info.txt`.

## Repository map

- `Electricity/`: electricity data, analysis, figures, and result tables.
- `Heart_Rate/`: point-forecast and probabilistic heart-rate analyses.
- `Simulation_Scenario_B/`: Scenario B data generator, analyses, and outputs.
- `TA_functions/`: shared TreeAlert and rule-interpretation functions.
- `reproduce_examples.R`: main examples, sensitivity analyses, and Appendix C/D outputs.
- `reproduce_appendices.R`: Appendix A and Appendix E workflows.
- `Latex_Files/JBA/R1/`: R1 manuscript source and LaTeX build assets.
- `Latex_Files/Figures/`: figures synchronized for the manuscript.
- `APPENDIX_REPRODUCIBILITY.md`: appendix-specific instructions and limitations.

## Data and outputs

The electricity input is tracked as
`Electricity/Elec_data/time_series_15min_singleindex.csv.gz`; the analysis
loads it automatically. Heart-rate inputs are under `Heart_Rate/HR_data/`.

Generated analysis outputs are written to the corresponding `images/` and
`results/` directories. The main reproduction script also synchronizes the
figures used by the manuscript into `Latex_Files/Figures/`.

## Appendix coverage

- Appendix A: probabilistic heart-rate forecasting; reproduced by
  `reproduce_appendices.R`.
- Appendix B: positive/negative electricity-rule selection. The repository
  retains the historical source and assets, but that legacy R Markdown file
  contains non-portable paths and requires manual path replacement.
- Appendix C: sensitivity analyses; reproduced by `reproduce_examples.R`.
- Appendix D: randomized 20-fold cross-validation; reproduced by
  `reproduce_examples.R`.
- Appendix E: Scenario B simulation, cross-validation, and replication
  sensitivity; reproduced by `reproduce_appendices.R`.

## Rebuilding the paper

After generating the figures, build the R1 manuscript from the repository root:

```text
cd Latex_Files/JBA/R1
latexmk -pdf JBA_R1.tex
```

The LaTeX source and bibliography style are in that directory. The manuscript
uses figures from `../../Figures/`.

## Validation

The electricity reproduction should select `cp = 0`, minimum bucket size 4,
and maximum depth 8, with first-ventile lift of approximately 1.693870 in
training and 1.381551 in testing. The paper reports these as 1.69 and 1.38.

The scripts use fixed seeds for resampling and simulation. Compare generated
parameters, lift values, rule tables, CSV summaries, and figures with the
reported paper outputs when validating a new environment.
