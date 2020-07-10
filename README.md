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
BI.AppraisalBookClassificationValuesUSNA
BI.AppraisalBookResidFactorsMKT
```
### what data been used?
What categories do we create residual value? - categories which we generate schedule, but the inherit mapping is not the same. bRA, RbA and AbR means nothing in residual.  
a) residual factor:
- uploadData: Cost data from BI.CustomerAssetsCost, rolling 12 years with cost greater than 10; purchase in new and is used for ABCost

b) econ factor:
- machine age between 3 to 10 
- cost/CurrentABCost between .5 and 2
- SP/M1value between .6 and 1.4
- SP/Cost less than 1
- Cost greater than 5
--------------------------------------
#### retail econ factor:
- worst: saleyear = 2019
- current: rolling 12 months sales data
- best: choose the best economic market from year greather or equal to than 2015 and less than or equal to current year if passed June. If not, less than or equal to prior year. 

#### auction econ factor:
- worst: average schedules of 3/31/2009 to 12/31/2009, age between 3 to 10.
- current: most recent publish (M1)
- best: average schedules of either 2016 or 2017 or 2018; pick the best by comparison

#### recency factor for econ factors (retail)
- the factor will applied on the econ factor before capped 
- the calculation uses the rolling 12 months data which been used for current
- how: depreciate the average SP/cost of 12 months by 5.5 months, `avg12m = avg(sp/cost) * .99^5.5` for each model year, and compare the results to average SP/cost of the latest month `r = avgrecent/avg12m`. Finally, do the weighted average across years to get the recency factor




