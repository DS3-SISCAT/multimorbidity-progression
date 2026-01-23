
# Methods

---

### 1. AMG Risk Stratification

1. **Adjusted Morbidity Group (AMG)**:  
   The AMG is a numerical measure estimating clinical complexity based on weighted sums of chronic and acute conditions.  

   - The population is stratified into four AMG risk groups:
     - **Low Risk**: Healthy stage, up to the 50th percentile.
     - **Moderate Risk**: 50th to 80th percentiles.
     - **High Risk**: 80th to 95th percentiles.
     - **Very High Risk**: Above the 95th percentile.

   - The primary outcome is crossing the 80th percentile of the AMG index.

---

### 2. Diseases

#### Chronic Diseases Using CCS Codes

The Clinical Classifications Software (CCS) is a standardized coding system developed by the Agency for Healthcare Research and Quality (AHRQ) that groups over 14,000 International Classification of Diseases (ICD) codes into clinically meaningful categories. The system simplifies the analysis of healthcare data by organizing ICD codes into 285 mutually exclusive disease categories, enabling efficient identification and comparison of conditions across populations and healthcare settings.

The input data for network creation includes over 22 million chronic disease records from the Catalan population (approximately 8 million inhabitants), represented using CCS codes. Each CCS code associated with a chronic disease appears only once per patient, ensuring no duplicates. The study period extends up to December 31, 2023.

---

#### Disease Selection

**Top 20 Selection**:  
Diseases are ranked based on their prevalence, and the top 20 most prevalent diseases are selected for further analysis.

---

#### Disease Classification

A variable is created to classify diseases based on their occurrence relative to the stratified threshold, defined as the 80th percentile of the AMG risk index:

- **Pre risk strata change**: Diseases occurring before the threshold (80th percentile of the AMG index).  
- **Post risk strata change**: Diseases occurring after the threshold (80th percentile of the AMG index).

To evaluate the distribution of diseases across these thresholds, the **Strata Ratio** is calculated for each disease in the network. This ratio represents the proportion of disease prevalence in the Pre-Threshold period relative to the total prevalence across both periods. The calculation is performed as follows:

1. **Group by Disease**:  
   For each disease, the prevalence is aggregated into two groups:
   - \(N_{Pre}\): Number of cases in the Pre-Threshold period.
   - \(N_{Post}\): Number of cases in the Post-Threshold period.

2. **Compute the Ratio**:  
   The ratio is calculated using the formula:

   $$
   Strata Ratio = \frac{N_{Pre}}{N_{Pre} + N_{Post}}
   $$

   The resulting value ranges between:
   - **1**: All cases occur in the Pre-Threshold period.  
   - **0**: All cases occur in the Post-Threshold period.

This ratio provides insight into whether a disease is predominantly associated with the Pre or Post strata within the subpopulation formed by patients with the selected chronic disease.



---

### 3. Directed Disease Networks

1. **Definition of Directionality**:  
   The directionality of edges in directed disease networks is determined by the temporal progression of diseases at the individual patient level.  

   - For a patient with $n$ diseases, all possible directed pairs are generated based on the chronological order of disease appearance:
     - For a sequence of diseases $D_1, D_2, \ldots, D_n$, all pairs $(D_i, D_j)$ where $i < j$ are included.  

   - The earlier disease in a pair is the origin, and the later disease is the destination.

2. **Generation of Directed Connections**:  
   Directed connections are classified by the type of stratum between the two diseases:
   - **Pre-Transition**: Most connections occur in the Pre-Threshold stratum.
   - **Post-Transition**: Most connections occur in the Post-Threshold stratum.
   - **Mixed-Transition**: Connections are distributed across both strata, or no stratum dominates.

---

### Risk Ratio

   The Risk Ratio quantifies the likelihood of observing a pair of diseases affecting the same patient.  
   The formula is:  

   $$
   RR_{ij} = \frac{C_{ij} \times N}{P_i \times P_j}
   $$  

   Where:  
   - \(C_{ij}\): Number of patients affected by both diseases \(i\) and \(j\).  
   - \(N\): Total number of patients in the population.  
   - \(P_i, P_j\): Prevalences of diseases \(i\) and \(j\), respectively.

---

### 5. Filtering by Bonferroni Correction and Minimum Cases

1. **Bonferroni Correction**:  
   A Bonferroni threshold (p < α/m) is applied to identify statistically significant connections.

2. **Minimum Case Threshold**:  
   Only connections with at least 50 cases (\(N \geq 50\)) and significant Bonferroni-adjusted p-values are retained.

---

### 6. Network Creation and Visualization

1. **Connection Selection**:  
   - **Primary Connections** (\(N = 10\)):  
     The top 10 most important connections for the subpopulation’s primary disease are selected based on the highest Adjusted Risk Ratio.
   - **Secondary Connections** (\(N = 20\)):  
     From the 10 primary connections, the top 20 connections with other diseases are selected based on the nominal Risk Ratio.

2. **Node and Edge Attributes**:  
   - **Nodes**: Represent diseases and include attributes such as prevalence and stratified ratios. Size is scaled to the prevalence of the disease.
   - **Edges**: Represent connections and include attributes such as number of connections and risk ratio. Size is scaled to the adjusted risk ratio of the disease pairs. The edges are color-coded to indicate the type of relationship:
     - **Pre**: Pre-Transition connections.
     - **Post**: Post-Transition connections.
     - **Mixed**: Mixed-Transition connections.

3. **Interactive Network Generation**:  
   The interactive networks are created using the `visNetwork` package.

---
