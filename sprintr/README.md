# sprintr — Reproducible Numerical Studies

This repository contains code to reproduce the numerical studies for the paper **Reluctant Interaction Modeling in Generalized Linear Models** (Lu & Yu), using the accompanying R package **`sprintr`**. It generates all synthetic experiments and plots used to evaluate the proposed interaction-screening framework for GLMs and its comparisons to baselines.


## What the code does (at a glance)

This code spans a comprehensive grid of settings across logistic, Poisson, and other GLMs with pairwise interactions, varying the relative strength of main effects and interactions (signal ratio), the correlation among main effects, the sample size, and the dimensionality, under different interaction structures including **strong hierarchy, weak hierarchy, mixed, and anti-hierarchy**.

- **Model families**: logistic, Poisson, and other GLMs with pairwise interactions  
- **Signal control**: knobs for the strength of main effects vs. interactions and for the correlation among main effects  
- **Interaction structures**: strong/weak hierarchy, mixed, and anti-hierarchy  
- **Outputs**: predictive performance (e.g., AUC, deviance), interaction selection metrics (e.g., TPR), runtimes.

## What’s inside

The repository is organized by model family and study type. Each subfolder includes:
- one or more `*_sim_run_*.R` scripts to generate data and run simulations; and
- a `*_plot_generate.R` (or similarly named) script to summarize results and create figures.

### 1) Binomial (logistic) simulations
- `binomial_simulations/different_ratio/`  
  Experiments that vary the **relative signal between main effects and interactions** (the main-effect–interaction ratio).
- `binomial_simulations/different_corr/`  
  Experiments that vary the **correlation structure among main effects** (from independent to highly correlated designs).

### 2) Poisson simulations
- `poisson_simulations/different_ratio/`  
  Experiments that vary the **relative signal between main effects and interactions** under a Poisson response model.

### 3) Theory-proof simulations
- `theory_proof_simulations/compare_with_oracle/`  
  Side-by-side comparison between the proposed screening strategy and an oracle baseline.  
- `theory_proof_simulations/increasing_corr/`  
  Studies of how **increasing correlation** among main effects impacts the selection of meaningful interactions and overall prediction.
- `theory_proof_simulations/increasing_n/`  
  Studies of how **increasing sample size** affects main-effect fit error, interaction recovery, and predictive deviance.

### 4) High-dimensional studies
- `high-dim_studies_simulations/`  
  Scaling experiments in higher ambient dimensions, focusing on computational efficiency and recovery properties of the selected interactions.

## How to run

### 0) Set up R and packages

- **R** ≥ 4.2 is recommended.
- Install the proposed method’s package:
  ```r
  install.packages("devtools")
  devtools::install_github("hluscience/sprintr")
````

* Baselines commonly used in the studies:

  ```r
  install.packages(c(
    "glmnet",
    "glinternet",
    "hierNet",
    "RAMP"
  ))
  ```

  (Not all packages are needed for every script; some Poisson studies compare only non-hierarchical baselines.)

### 1) Reproduce a single study

From a subfolder, run the simulation followed by the plotting script. For example:

```r
# Logistic: vary main-effect–interaction ratio
setwd("binomial_simulations/different_ratio")
source("ratio_sim_run_anti.R")     # generates results files under ./results/
source("ratio_plot_generate.R")    # writes plots to ./figures/
```

### 2) End-to-end reproduction

Each subfolder is self-contained. To reproduce **all** studies, run the `*_sim_run_*.R` scripts in each subfolder first (these will write results), then run the corresponding `*_plot_generate.R` scripts to create PDFs.


## Citing

If you use this repository or its results, please cite the paper and the R package `sprintr`.

