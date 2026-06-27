# Character Clustering Methods and Multi-Dimensional Statistical Analysis of Charles Dickens’s *Oliver Twist*

This repository hosts the complete computational pipeline engineered in R for the structural, directional, and compositional analysis of character architecture in Charles Dickens's *Oliver Twist*. 

---

## Analytical Pipeline

The codebase is split into sequential processing modules:

1. **Unsupervised Landscape Stratification:** 
   * `01_OT_clustering.R` – Handles string-scrubbing, narrative noise reduction, and runs a 3-center $K$-means routine.
   * `02_OT_subclusters.R` – Implements micro-node character subclustering.
   * `03_OT_tests.R` – Evaluates baseline dataset distributions, including Pareto power-law validation.

2. **Longitudinal Vector Spaces & Character Joints:** 
   * `04_OT_jp:jd_csv.R` – Tracks spatial segregation, co-presence data, and Spearman rank couplings across chapter blocks.
   * `04_OT_jp:jd_figures.R` – Generates the corresponding structural vector visualizations and spatial friction metrics.

3. **Stochastic & Distance Trajectories:** 
   * `05_OT_rhythms_part1.R` & `05_OT_rhythms_part2.R` – Model narrative rhythm dynamics and character absence spells.
   * `06_OT_mahalanobis.R` & `06_OT_mahalanobis_withoutOliver.R` – Compute localized squared Mahalanobis distances ($D^2$) to identify global anomaly scores and plot-disruptor profiles across full and restricted baselines.

4. **Calculus & Compositional Foundations:** 
   * `07_OT%_QDA.R` – Executes supervised Quadratic Discriminant Analysis for migration probabilities across character tiers.
   * `07_OT%_mahalanobis.R` – Integrates compositional metrics with multivariate distance testing.
   * `07_OT%_riemann.R` & `08_OT_riemann.R` – Calculate dynamic character mass and continuous narrative presence via Riemann geometric integrations.

---

## 🛠️ Environment & Dependencies

The pipeline was built and compiled using R (version 4.6.0). To replicate the models and compile the vector visualizations, ensure you have the following packages installed:

```r
install.packages(c("tidyverse", "compositions", "survival", "MASS", 
                   "cluster", "ggplot2", "ggrepel", "scales", "readxl"))
