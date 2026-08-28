# Reproducing the TreeAlert examples

The electricity and heart-rate analyses used for the paper are driven by the
two R Markdown files below:

- `Electricity/Electricity_Main.Rmd`
- `Heart_Rate/Heart_Rate_Main.Rmd`

From the repository root, run:

```text
Rscript reproduce_examples.R
```

This command renders both analyses, regenerates their figures, and synchronizes
the figures used by `Latex_Files/JBA_R1.tex` into `Latex_Files/Figures`. The
runner also records the R version and package versions in
`reproduction_session_info.txt`, which should be retained with the archived
code and data.

The command handles the compressed electricity input automatically. The
electricity data are tracked as
`Electricity/Elec_data/time_series_15min_singleindex.csv.gz`; the loader uses
the compressed file automatically and also accepts the uncompressed `.csv`
file if it is present. The heart-rate `.rds` inputs are tracked in
`Heart_Rate/HR_data`.

The scripts use fixed seeds for the cross-validation procedures. The main R
packages required are `readr`, `tidyr`, `feasts`, `tsibble`, `fable`,
`fabletools`, `dplyr`, `lubridate`, `ggplot2`, `ggtext`, `gridExtra`, `slider`,
`scales`, `rpart`, `rpart.plot`, `rmarkdown`, and `knitr`. Install them with
your preferred R package manager before running the reproduction script.

After regenerating the figures, compile the submission PDF from the LaTeX
directory, for example with:

```text
cd Latex_Files
latexmk -pdf JBA_R1.tex
```

The generated HTML files are written to a temporary directory and are not
needed for compiling the paper. The pre-generated figure files remain in the
repository so that the LaTeX manuscript can be compiled without first running
R.
