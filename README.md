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

## Network explorer

To facilitate interactive exploration of the disease co-occurrence networks presented in the manuscript, we provide two complementary Shiny applications. These tools allow users to inspect network structure, node and edge attributes, and temporal patterns underlying multimorbidity progression.

- **Global disease network**  
  Interactive visualization of the overall disease co-occurrence network, providing a population-level view of multimorbidity structure and dominant disease trajectories. This application allows users to dynamically configure how the network is displayed, including the number of primary and secondary connections shown, enabling customised exploration of disease associations and network complexity.  
  https://dvalero.shinyapps.io/multimorbidity_disease_network
  
- **Disease-centred networks**  
  Interactive exploration of disease-centred directed networks, each focused on one of the most prevalent chronic conditions. Users can examine secondary and tertiary conditions, edge directionality (temporal ordering), and whether associations predominantly occur before or after transition to high multimorbidity burden (AMG ≥ P80).  
  https://dvalero.shinyapps.io/multimorbidity_disease_centred_networks

These applications are intended as exploratory and hypothesis-generating tools that complement the static figures included in the manuscript and supplementary materials.

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


---

---


Shield: [![CC BY-NC-SA 4.0][cc-by-nc-sa-shield]][cc-by-nc-sa]
 
This work is licensed under a
[Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License][cc-by-nc-sa].
 
[![CC BY-NC-SA 4.0][cc-by-nc-sa-image]][cc-by-nc-sa]
 
[cc-by-nc-sa]: http://creativecommons.org/licenses/by-nc-sa/4.0/
[cc-by-nc-sa-image]: https://licensebuttons.net/l/by-nc-sa/4.0/88x31.png
[cc-by-nc-sa-shield]: https://img.shields.io/badge/License-CC%20BY--NC--SA%204.0-lightgrey.svg
