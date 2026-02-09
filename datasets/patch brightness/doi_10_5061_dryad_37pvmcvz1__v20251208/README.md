# A plumage patch signaling occupancy is shaped by social environment

Dataset DOI: [10.5061/dryad.37pvmcvz1](10.5061/dryad.37pvmcvz1)

## Description of the data and file structure

Cliff swallows exhibit a distinct patch of white plumage on their foreheads, which we investigate as a potential visual signal of occupancy. This study utilizes museum specimens collected in 1982-2024. Enclosed data details various aspects of bird specimens and their patches, for use in modeling key relationships. 

### Files and variables

#### File: 5NovPatchData.csv

**Description:** Dataset used for the main study findings (See StatsOverview.Rmd), details information collected for each bird specimen. 

##### Variables

* Year: The year the bird was collected.
* Date: The day of the year the bird was collected, 1-365 (or 1-366 in leap years)
* ID: Bird identification code
* Area: The area of the forehead patch, measured in mm^2^. Measurements made in Fiji 

  using color thresholding, or the lasso tool when thresholding included non-patch image information
* Colony.Site: Numeric identification code for the colony site the bird was collected from
* Colony.Size: Number of birds calculated or counted to be living at a colony site
* Sex: The sex of each bird: M = Male, F = Female
* year2: The year of collection of the bird, squared
* forehead: Median value of 5 percent reflectance spectral measurements (B2) of the 

  white forehead patch feathers, measured in the range 300-700nm, and presented as a decimal (0-100). 
* Cat.Year: Duplicate of the data listed in column labeled "Year" for categorization as factor for supplemental analyses. 

#### File: FinalPatchAreaRepeat.csv

**Description:** Dataset used only to assess repeatability using finalized methods of the values for area of the patch. 

##### Variables

* ID: Bird identification code
* Original Image: Area (mm^2^) measurement from the original image and measurement of the forehead patch. 
* Repeat on Original Image: Area (mm^2^) measurement taken on the same image, months later, to confirm accuracy of area measurement via repeatability calculations.

#### File: MasterPatchAreaRepeat.csv

**Description:** Dataset used in exploration of the best method to quantify repeatabiilty for area of the patch. Note that blank entries indicate that the specimen was not remeasured under the corresponding column condition. 

##### Variables

* ID: Bird identification code
* Original Image: Area (mm^2^) measurement from the original image and measurement of the forehead patch.
* Repeat on New Image: Area (mm^2^) new measurement taken months later on a new image also taken months later, to confirm accuracy of area measurement via repeatability calculations.
* Repeat on Original Image: Area (mm^2^) measurement taken on the same image, months later, to confirm accuracy of area measurement via repeatability calculations.

#### File: mergeRawBrightRepOrig.csv

**Description:** Dataset used only to assess repeatability of the values for brightness of the forehead patch. 

##### Variables

* RowNumber: Row number of data file, useful for data wrangling. 
* ID: Bird identification number. 
* SeqNum: For each bird, 5 measurements of the patch were taken. This column indicates whether each data point was taken 1-5. 
* RepFore: Repeat measurement, taken months later, of the forehead feathers (B2) listed in column[OrigFore]. 
* OrigFore: Original measurement of percent reflectance spectral measurements (B2) of the 

  white forehead patch feathers, measured in the range 300-700nm, and presented as a decimal (0-100). 

#### File: noAggForeheadBrightness.csv

**Description:** Unaggregated measures of brightness used for deciding method of summary for the measurement. 

##### Variables

* RowNumber: Row number of data file, useful for data wrangling. 
* ID: Bird identification number. There are 5 entries per ID number, to reflect 5 replicate measures of each specimen. 
* forehead: Measurement of percent reflectance spectral measurements (B2) of the 

  white forehead patch feathers, measured in the range 300-700nm, and presented as a scaled decimal (0-100). 

#### File: StatsOverview.Rmd

**Description:** An R markdown file containing the code for statistical analyses and graphs presented in the manuscript. The code references other files included in the dataset. 

#### File: SupportStats.Rmd

**Description:** A supporting R markdown file to StatsOverview.Rmd, with initial and exploratory testing, and supplemental mixed model analyses. 

#### File: Hannah_cliff_swallow.zip

**Description:** A zip folder containing raw spectral read data. Each subfolder is labeled with a bird specimen ID number, and contains 15 measurement files, those labeled "forehead" are associated with the forehead brightness measurements associated with the study (percent reflectance spectral measurements (B2) of the white forehead patch feathers, measured in the range 300-700nm). 

#### File: Forehead_field_observations.xlsx

**Description:** Data on transient cliff swallow visits to nests, collected 2-6 July 2025. 

##### Variables

* Nestno : nest identification number
* Status: B, owner with darkened forehead; W, no owners with darkened foreheads; U, nest failed with no owners
* Count: Number of transient birds' visits to each nest in the hour observation period
* Session: sequentially numbered observation period (each one hour in length)

  <br />

#### File: SelectionDifferentialDataannotatedBA.csv

**Description:** Subset of large dataset, with samples annotated as collected in the 14 years prior to the selection event (before, 'B') or collected in the 14 years following the selection event (after, 'A'). Dataset is referenced in calculation of selection differentials. 

##### Variables

* Year: Year of collection of the bird. 
* BorA: Designation of before or after selection event in 1996. Specimens collected in the 14 years prior to 1996 (1982-1995) are marked 'B,' and specimens collected in the 14 years following the event (1997-2010) are marked 'A.'
* Area: The area of the forehead patch, measured in mm^2^.
* forehead: Median value of 5 percent reflectance spectral measurements (B2) of the 

  white forehead patch feathers, measured in the range 300-700nm, and presented as a decimal (0-100).

