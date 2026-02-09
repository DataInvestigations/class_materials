# Population density and timing of breeding mediate effects of early life conditions on recruitment

[https://doi.org/10.5061/dryad.rxwdbrvm1](https://doi.org/10.5061/dryad.rxwdbrvm1)

## Description of the data and file structure

### Files and variables

In all files, missing values are represented by "NA."

#### File: demography.csv

**Description:** This dataset contains the annual population density on the study site.

##### Variables

* year: Year.
* annual_density: The annual population density in birds/ha, defined as the number of breeding adults on the study site divided by the area of the study site (10.7 ha).

#### File: recruit\_data\_final.csv

**Description:** Dataset used for the analysis of factors influencing whether a fledgling recruited into the breeding population. Each record is one individual fledgling.

##### Variables

* nest_key: The identification key of the individual's natal nest.
* year: The year in which the individual was born.
* eggs_laid: The clutch size (number of eggs) in the individual's natal nest.
* female_id: The band number of the individual's mother.
* male_id: The band number of the individual's father.
* found_date: The date the individual's natal nest was found.
* hatch_date: The date the eggs in the individual's natal nest hatched.
* eggs_hatched: The number of eggs in the individual's natal nest that hatched (brood size).
* fledglings: The number of young that fledged from the individual's natal nest.
* fledge_date: The date the first young fledged from the individual's natal nest.
* clutch_number: Whether the individual's natal nest was a first brood (1), second brood (2), or replacement clutch (indicated with an R). Uncertainty in assignment is indicated with an E (for "estimated").
* found_day: Ordinal day on which the individual's natal nest was found.
* hatch_day: Ordinal day on which the eggs in the individual's natal nest hatched.
* first_egg_day: Ordinal day on which the first egg was laid in the individual's natal nest.
* female_status: Mating status of the individual's mother. MO = monogamously mated, PG 1 = primary female of a polygynous male, PG 2 = secondary female of a polygynous male.
* female_age: Age in years of the individual's mother.
* female_age_code: Age code of the individual's mother. SY = a yearling female breeding for the first time (1 year old). ASY = older than yearling (> 1 year old).
* male_age: Age in years of the individual's father.
* male_age_code: Age code of the individual's father. SY = a yearling male breeding for the first time (1 year old). ASY = older than yearling (> 1 year old).
* popn_size: Number of breeding adults on the study site in the year the individual was born.
* annual_density: Population density in birds/ha on the study site in the year the individual was born (number of breeding adults divided by the area of the study site, 10.7 ha).
* band_id: The individual's band number (uniquely identifying).
* sex: The sex of the individual (unknown for most individuals). Individuals known to be male or female are represented by the codes "M" and "F" respectively. When the sex of the individual is thought to be male or female but is uncertain, it is represented by "M?" and "F?". Individuals of unknown sex are represented by the code "U" or a missing value ("NA").
* weight: Mass of the individual in grams.
* age_in_days: Age of the individual in days when banded. All individuals were banded at 7 days old.
* recruited: Whether the individual ever bred or was recaptured in the study area (logical, true or false).
* fledge_date_noNA: Fledge date with missing dates estimated (see the related article, Mueller et al. *Biology Letters*, for details).
* recruited.f: Whether the individual ever bred or was recaptured in the study area (binary, 1 if recruited, 0 if not).
* clutch_num: Whether the nest is a first brood (1) or second brood (2).
* fledge_day: Ordinal day on which the first young fledged from the individual's natal nest.

## Code/software

The attached code was used to run all analyses and generate all figures. Analyses were conducted using R (v. 4.4.0; R Core Team 2023). Models were fit using glmmTMB (v. 1.1.9; Brooks et al. 2017) and diagnostics checked using DHARMa (v. 0.4.6; Hartig 2022). Pairwise comparisons between levels of categorical variables were conducted using emmeans, adjusted for multiple comparisons using the Tukey method (v. 1.10.1; Lenth 2023).
