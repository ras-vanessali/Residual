# Residual
This program is to calculate the residual adjusters for any C, CS or CSM that we create schedule. For each schedule, this program will return the residual factor and best/worst econ factor for both retail and auction.

## Prerequisites
a)  You need to have R studio installed in your computer. 
b)  Download the most recent `SchedulesManagement.xlsx` from git or valuation share folder.
c)  Install the following R libraries:
```
RODBC
readxl
tibble
dplyr
latticeExtra
tidyr
stringr
lubridate
```
## Data scope
### what bi.views in ras_sas been used?
```
BI.CustomerAssetsCost
BI.Comparables
BI.Classifications
BI.AppraisalBookUsageAdjCoefficients
BI.AppraisalBookClassificationValues
BI.AppraisalBookResidFactorsMKT
```
### what data been used?
#### residual factor:
- uploadData: Cost data from BI.CustomerAssetsCost, rolling 12 years with cost greater than 10; purchase in new and is used for ABCost

#### retail econ factor:
- machine age between 3 to 10 
- cost/CurrentABCost between .5 and 2
- SP/M1value between .6 and 1.4
- SP/Cost less than 1
- Cost greater than 5
--------------------------------------
- recessionYr.ret: saleyear = 2019
- currentYr.ret: rolling 12 months sales data
- bestYr.ret: choose among 2016, 2017 and 2018 



