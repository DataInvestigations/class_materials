# Data Description: City lizards are more social

**Authors:** Avery L. Maune, Tobias Wittenbreder, Duje Lisičić, Barbara A. Caspers, Ettore Camerlenghi, Isabel Damas-Moreira

---

A total of 13 data files were used to construct and analyze social networks, including:

`P1_GBI.csv` - `P6_GBI.csv`: Group-by-individual matrices

`P1_attributes.csv` - `P6_attributes.csv`: Attribute metadata

`sna_data.csv`: Combined dataset including calculated social metrics

Only individuals observed ≥3 times (N = 69) are included in these files.

Additional files included:

`associations_data.csv`: Contains data for all marked individuals (N = 94), used to analyze observation and association counts.

`population_density.csv`: Lizard count data from line transect surveys conducted over 10 days across all size transects, used to estimate population density.

`script_Maune_etal_SNA.R`: Full R script used for social network construction and statistical analysis

## Data files

### 1. Group-by-individual matrices

Binary matrices showing individual co-occurrence in observed social groups.

**Files:** `P1_GBI.csv`, `P2_GBI.csv`, `P3_GBI.csv`, `P4_GBI.csv`, `P5_GBI.csv`, `P6_GBI.csv`

**Details**: Rows = observation events; Columns = individual lizards; 1 = present; 0 = absent

### 2. Attribute tables

Metadata for each individual included in the social network analysis, including id, transect, habitat, and sex. The row order corresponds to the columns in the GBI matrices.\
**Files:** `P1_attributes.csv` `P2_attributes.csv` `P3_attributes.csv` `P4_attributes.csv` `P5_attributes.csv` `P6_attributes.csv`

### 3. Social network dataset

Combined data for all individuals (N = 69), including calculated social network metrics: `binary_degree` and `weighted_degree`. Social network metrics were calculate from the GBI matrices and added to the corresponding attribute tables, which were then combined into a single dataset for analysis.\
**File:** `sna_data.csv`

### 4. Association data

Frequency of observations and associations for all marked individuals (N = 94). Includes id, transect, habitat, the total number of associations (`asso_count`) and total observation events (`obs_count`) per individual.\
**File:** `associations_data.csv`

### 5. Population density data

Counts of lizards observed during 10 line transect surveys per site. Used to estimate population density across urban and non-urban habitats.\
**File:** `population_density.csv`

### 6. R code

Contains all R code used in the social network construction and statistical analysis.\
**File:** `script_Maune_etal_SNA.R`

---

## Variables

| Variable          | Description                                                                 |
| ----------------- | --------------------------------------------------------------------------- |
| `id`              | Unique lizard identifier                                                    |
| `transect`        | Transect ID (`P1`–`P6`)                                                     |
| `habitat`         | Habitat type: `urban` or `non_urban`                                        |
| `sex`             | Sex class: `F` (female), `M` (male), or `Sub` (subadult)                    |
| `asso_count`      | Number of associations per individual (with marked or unmarked individuals) |
| `obs_count`       | Number of observations per individual                                       |
| `no_liz`          | Number of lizards counted during a single transect survey                   |
| `binary_degree`   | Number of unique social connections per individual (network metric)         |
| `weighted_degree` | Sum of edge weights per individual (network metric)                         |

