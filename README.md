# Character Clustering Methods and Multi-Dimensional Statistical Analysis of Charles Dickens’s *Oliver Twist*

This repository hosts the complete computational pipeline engineered in R for the structural, directional, and compositional analysis of character architecture in Charles Dickens's *Oliver Twist*. 

---

## 📊 Analytical Pipeline

The codebase is split into sequential processing modules, moving from initial token matrix normalization to unsupervised clustering, stochastic trajectory mapping, and supervised canonical reductions:

1. **Unsupervised Landscape Stratification:** Script `01` handles string-scrubbing, removes narrative noise/leaks, and runs a 3-center $K$-means routine alongside Pareto power-law distributions.
2. **Longitudinal Vector Spaces:** Scripts `02`, `03`, and `04` track spatial segregation vs. asymmetric predatory stalking across chapter blocks using Spearman rank coupling and micro-node subclustering.
3. **Stochastic & Distance Trajectories:** Scripts `05` and `06` compute localized squared Mahalanobis distances ($D^2$) and model absence spells using non-parametric Kaplan-Meier survival hazard functions.
4. **Calculus & Compositional Foundations:** Scripts `07`, `08`, and `09` calculate dynamic mass via Riemann geometric integrations, resolve simplex singularities via Isometric Log-Ratio (ILR) transformations, and execute supervised Multiple Discriminant Analysis (MDA).

---

## 🛠️ Environment & Dependencies

The pipeline was built and compiled using **R (version 4.6.0)**. To replicate the models and compile the vector visualizations, ensure you have the following packages installed:

```r
install.packages(c("tidyverse", "compositions", "survival", "MASS", 
                   "cluster", "ggplot2", "ggrepel", "scales", "readxl"))
