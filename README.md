# Multimorbidity progession and disease networks 

This repository contains the analytic code and data processing pipelines used in the study "Ten-year population-based assessment of multimorbidity burden progression in a regional cohort of 5.5 million adults". The study investigates multimorbidity progression using longitudinal population-wide electronic health records, develops predictive models for transition to high clinical complexity, and constructs directed disease co-occurrence networks for the most prevalent chronic conditions.

---

## Study Overview

The analysis is based on a retrospective cohort of **5.5 million adults** living in Catalonia (Spain), followed for 10 years (2013–2022) using linked data from the **Catalan Health Surveillance System (CHSS)**. Multimorbidity burden was quantified using the **Adjusted Morbidity Groups (AMG)** index, a validated measure of clinical complexity widely used for population health stratification.

The study includes:
- Predictive modelling of the transition from *low/moderate* to *high/very high* morbidity burden (AMG ≥P80) using GLM, random forest, neural networks, and XGBoost.
- Construction of 20 **directed disease co-occurrence networks** based on temporally ordered disease onset at the individual level.
- Identification of sequential patterns of multimorbidity and pre-/post-transition interactions.

---

## Contact

**Damià Valero-Bover**  
Email: damiavalero@catsalut.cat
Affiliation: Catalan Health Service (CatSalut)
**Jordi Piera-Jiménez**
Email: jpiera@catsalut.cat
Affiliation: Catalan Health Service (CatSalut)

---

## Software and Dependencies

All analyses were conducted using **R version 4.4.2**.

Key packages:
- `data.table` for high-performance data processing  
- `glmnet`, `randomForest`, `nnet`, `xgboost` for predictive modelling  
- `visNetwork` for interactive disease network visualization  
- `ggplot2`, `cowplot` for figures and graphical outputs  

Package versions are listed in `/code/session_info.txt`.

---

---


Shield: [![CC BY-NC-SA 4.0][cc-by-nc-sa-shield]][cc-by-nc-sa]
 
This work is licensed under a
[Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License][cc-by-nc-sa].
 
[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]
 
[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png
[cc-by-nc-sa-shield]: https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg
