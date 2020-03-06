## change 11/30/2019
#1. residual factor: data pull by acquisition date 12/1/2006 to 11/30/2019 ok
#2. econ factor: match up by age instead of year ok
#3. use avg avg with proper weight instead of sum avg ok
#4. best year for different schedule, check 2018, 2017, 2016: max(T12 vs 2018, T12 vs 2017) ok

##change 1/3/2020
# add auction econ factors

## coming: Add 2019 econ factors
############################## Install packages #################################

library(RODBC)
library(RODBCext)
library(lattice)
library(latticeExtra)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(readxl)
library(stringr)

############################## Declare inputs #####################################

DBserver = 'production'
#DBserver = 'rasquery'

CountryCode='USA'
stdInd=3
target_Age = 8
num_threshold = 20

GlobalList<-c(313,	6,	2509,	15,	29,	315,	360,	451,	316,	362)
MoM_cap=0.02

cap_resid_low = 1.02
cap_resid_hi = 1.25

WorstEcon_cap =.98
BestEcon_cap = 1.0005

Econ_gap = 0.15
publishDate<-Sys.Date() - days(day(Sys.Date()))
#publishDate='2019-11-30'

## file path & read file name 
file_path = "H:/Projects/75_ResidualCurve/2020Jan"
setwd(file_path)  
excelfile = '20200211 SchedulesManagement.xlsx'

################################## Load data #####################################

### Cost data
channel<-odbcConnect(DBserver)
uploadData<-sqlQuery(channel,"
                                         
 SET NOCOUNT ON                    
	Declare @dateEffective DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-1, -1) AS date)
	Declare @dateStart DATE = 	CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-145, -1) AS date)
	
SELECT [CustomerAssetId]
      ,[CustomerId]
      ,[CustomerName]
      ,[EquipNo]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[MakeId]
      ,[MakeName]
      ,[ModelId]
      ,[ModelName]
      ,[ModelYear]
      ,[AcquisitionDate]
      ,[Cost]
      ,[CurrentABCost]
	    ,CurrentABCost/Cost as CostRatio
      ,CAST(YEAR(@dateEffective)-ModelYear + (MONTH(@dateEffective)-6)/12.00 as decimal(10,4))  as Age
	   ,CASE                    
             WHEN ([CurrentABCost]/Cost > 1.25+0.125*(YEAR(@dateEffective)-ModelYear + (MONTH(@dateEffective)-6)/12.00)
			    OR [CurrentABCost]/Cost < 0.75+0.015*(YEAR(@dateEffective)-ModelYear + (MONTH(@dateEffective)-6)/12.00)) THEN 'EcxRegr'
             ELSE 'inUse' END AS 'Flag'
  FROM [ras_sas].[BI].[CustomerAssetsCost] 
  WHERE  [IsUsedForABCost]='Y' and [IsPurchasedUsed]='N' 
    AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
    AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
		AND NOT ([SubcategoryId] in (2806,2808,2001,2636) and makeid=31 and ModelName not like 'XQ%')  
	AND AcquisitionDate > @dateStart and AcquisitionDate<=@dateEffective
    AND CurrentABCost is not NULL 
	  AND Cost>10
	  AND NOT(CustomerId = 178 And (Equipno like 'N%' or Equipno like '%NF'))")

##################################### Retail Econ Factor ###########################################
recessionYr.ret<-sqlQuery(channel," SET NOCOUNT ON
select  CategoryId,CategoryName, SubcategoryId,SubcategoryName, MakeId, MakeName,ModelYear,cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,0)) as yearAge, avg(SalePrice/Cost) SPCost,Count(*) Units
from [ras_sas].[BI].Comparables 
where saletype = 'retail' and IsUsedForComparables='y'
	  and SaleYear = 2009
	  and YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 >3
	  and YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 <10
	  and cost /CurrentABCost<2 and cost/CurrentABCost>.5
	  and SalePrice/M1PrecedingFmv <1.4 and SalePrice/M1PrecedingFmv >.6
	  and SalePrice/Cost < 1
	  and COST>5
	  AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
      AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
      AND NOT ([SubcategoryId] in (2806,2808,2001,2636) and makeid=31 and ModelName not like 'XQ%')
group by CategoryId,CategoryName, SubcategoryId,SubcategoryName, MakeId, MakeName,ModelYear,cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,0))	")



currentYr.ret<-sqlQuery(channel," SET NOCOUNT ON
Declare @dateStart DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-1,GETDATE()))-1, -1) as date)
Declare @dateEnd DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-1, -1) AS date)

SELECT  CategoryId,CategoryName, SubcategoryId,SubcategoryName,MakeId, MakeName, ModelYear, cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,0)) as yearAge,avg(SalePrice/Cost) SPCost,Count(*) Units
from [ras_sas].[BI].Comparables 
where saletype = 'retail' and IsUsedForComparables='y'
	  and SaleDate >@dateStart and SaleDate<=@dateEnd
	  and YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 >3
	  and YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 <10
	  and cost /CurrentABCost<2 and cost/CurrentABCost>.5
	  and SalePrice/Cost < 1
	  and SalePrice/M1PrecedingFmv <1.4 and SalePrice/M1PrecedingFmv >.6
	  and COST>5
	  AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
      AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
      AND NOT ([SubcategoryId] in (2806,2808,2001,2636) and makeid=31 and ModelName not like 'XQ%')
group by CategoryId,CategoryName, SubcategoryId,SubcategoryName, MakeId, MakeName, ModelYear,cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,0))	")


bestYr.ret<-sqlQuery(channel," SET NOCOUNT ON
	 select  CategoryId,CategoryName, SubcategoryId,SubcategoryName, MakeId, MakeName, SaleYear, ModelYear, cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,0)) as yearAge,avg(SalePrice/Cost) SPCost,Count(*) Units
from [ras_sas].[BI].Comparables 
where saletype = 'retail' and IsUsedForComparables='y'
	  and SaleYear between 2016 and 2018
	  and YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 >3
	  and YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 <10
	  and cost /CurrentABCost<2 and cost/CurrentABCost>.5
	  and SalePrice/Cost < 1
	  and SalePrice/M1PrecedingFmv <1.4 and SalePrice/M1PrecedingFmv >.6
	  and COST>5
	  AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
      AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
      AND NOT ([SubcategoryId] in (2806,2808,2001,2636) and makeid=31 and ModelName not like 'XQ%')
group by CategoryId,CategoryName, SubcategoryId,SubcategoryName, MakeId, MakeName, SaleYear,ModelYear,cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,0))	")



##################################### Auction Econ Factor ###########################################
recessionYr.auc<-sqlQuery(channel," SET NOCOUNT ON
SELECT [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[MakeId]
      ,[MakeName]
      ,[PublishYear]
      ,[ModelYear]
      ,avg([FlvSchedulePercentage]) AvgFlv
      ,avg([FmvSchedulePercentage]) AvgFmv
  FROM [ras_sas].[BI].[AppraisalBookClassificationValues]
  where AppraisalBookPublishDate between '2009-03-31' and '2009-12-31' and ModelId is null AND ModelYear between PublishYear-10 and PublishYear-3
   AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
   AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
   Group By [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[MakeId]
      ,[MakeName]
	  ,[PublishYear]
      ,[ModelYear]")


currentYr.auc<-sqlQuery(channel," SET NOCOUNT ON
Declare @EffectiveDate Date = CAST(DATEADD(MONTH, DATEDIFF(MONTH, 0, GETDATE())-1, -1) AS date)

SELECT [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[MakeId]
      ,[MakeName]
      ,[PublishYear]
      ,[ModelYear]
      ,avg([FlvSchedulePercentage]) AvgFlv
      ,avg([FmvSchedulePercentage]) AvgFmv
  FROM [ras_sas].[BI].[AppraisalBookClassificationValues]

  where [AppraisalBookPublishDate] = @EffectiveDate and ModelId is null AND ModelYear between PublishYear-10 and PublishYear-3
   AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
   AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
   Group By [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[MakeId]
      ,[MakeName]
	  ,[PublishYear]
      ,[ModelYear]	")


bestYr.auc<-sqlQuery(channel," SET NOCOUNT ON
	 SELECT [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[MakeId]
      ,[MakeName]
      ,[PublishYear]
      ,[ModelYear]
      ,avg([FlvSchedulePercentage]) AvgFlv
      ,avg([FmvSchedulePercentage]) AvgFmv
  FROM [ras_sas].[BI].[AppraisalBookClassificationValues]
  where publishyear between 2016 and 2018 and ModelId is null AND ModelYear between PublishYear-10 and PublishYear-3
   AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
   AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
   Group By [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[MakeId]
      ,[MakeName]
	  ,[PublishYear]
      ,[ModelYear]")


### import a table with all classifications
AllClass<-sqlQuery(channel,"
SET NOCOUNT ON
Drop TABLE IF EXISTS #CSlevel
  SELECT [ClassificationId]
      ,[CategoryId]
      ,[SubcategoryId]
  INTO #CSlevel
  FROM [ras_sas].[BI].[Classifications]
  where MakeId IS NULL AND SubcategoryId IS NOT NULL
  
Drop TABLE IF EXISTS #Clevel
SELECT [ClassificationId]
      ,[CategoryId]
  INTO #Clevel
  FROM [ras_sas].[BI].[Classifications]
  where SubcategoryId IS NULL AND CategoryId IS NOT NULL


SELECT BIC.[ClassificationId]
      ,BIC.[CategoryId]
      ,BIC.[CategoryName]
      ,BIC.[SubcategoryId]
      ,BIC.[SubcategoryName]
      ,BIC.[MakeId]
      ,BIC.[MakeName]
	  ,CSL.[ClassificationId] CS_ClassId
	  ,CL.[ClassificationId] C_ClassId
  FROM (select * from [ras_sas].[BI].[Classifications] where ModelId IS NULL And NOT(Categoryid IN (220,1948,21) OR CategoryName LIKE 'DO NOT USE%')) BIC
  LEFT JOIN #CSlevel  CSL
  on BIC.CategoryId = CSL.CategoryId AND BIC.SubcategoryId = CSL.SubcategoryId
  LEFT JOIN #Clevel CL
  on BIC.CategoryId = CL.CategoryId
  order by BIC.[CategoryName]
      ,BIC.[SubcategoryName]
      ,BIC.[MakeName]
")


### Import last month values
LastMonth<-sqlQuery(channel," SET NOCOUNT ON
Declare @EffectiveDate date = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-2, -1) AS date)
SELECT [MarketCode]
      ,[ClassificationID] ClassificationId
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[MakeId]
      ,[MakeName]
      ,[ResidSf]
      ,RetailEconSfBest	
      ,RetailEconSfWorst	
      ,AuctionEconSfBest	
      ,AuctionEconSfWorst
      ,[AppraisalBookIssueID]
      ,[AppraisalBookPublishDate]
  FROM [ras_sas].[BI].[AppraisalBookResidFactorsMKT]
  WHERE [AppraisalBookPublishDate] = @EffectiveDate           
                    ")

################################## Functions ####################################

sched.aggr<-function(data.df,in.df,stype,target){
  retsele.best<-c('Schedule','SaleYear','ModelYear', 'yearAge', 'Units','SPCost')
  retsele.other<-c('Schedule','ModelYear', 'yearAge', 'Units','SPCost')
  aucsele<-c('Schedule','PublishYear' ,'ModelYear', 'AvgFlv','AvgFmv')
  
  Catlevel<-in.df %>% filter(Level2 =='Category') %>% select(-SubcategoryId,-MakeId)
  Subcatlevel<-in.df %>% filter(Level2 == "SubcatGroup") %>% select(-MakeId)
  Makelevel<-in.df %>% filter(Level2 =='Make')
  
  if (stype == 'Retail'){
    if(target == 'best'){
      cat <- merge(data.df,Catlevel, by=c('CategoryId')) %>% select(retsele.best) 
      subcat <- merge(data.df,Subcatlevel, by=c('CategoryId',"SubcategoryId")) %>% select(retsele.best) 
      make <- merge(data.df,Makelevel, by=c('CategoryId',"SubcategoryId",'MakeId')) %>% select(retsele.best) }
    else{
      cat <- merge(data.df,Catlevel, by=c('CategoryId')) %>% select(retsele.other) 
      subcat <- merge(data.df,Subcatlevel, by=c('CategoryId',"SubcategoryId")) %>% select(retsele.other) 
      make <- merge(data.df,Makelevel, by=c('CategoryId',"SubcategoryId",'MakeId')) %>% select(retsele.other) }
    }
  
  else {
    cat <- merge(data.df,Catlevel, by=c('CategoryId')) %>% select(aucsele) 
    subcat <- merge(data.df,Subcatlevel, by=c('CategoryId',"SubcategoryId")) %>% select(aucsele) 
    make <- merge(data.df,Makelevel, by=c('CategoryId',"SubcategoryId",'MakeId')) %>% select(aucsele)
  }
  return(rbind(cat,subcat,make))
}


bestEconfunc<- function(best.df,cur.df,year){
  
  result <- merge(best.df %>% filter(SaleYear ==year),cur.df,by=c('Schedule','yearAge')) %>%
    mutate(ratio = avg.x/avg.y, units_N = totalunits.x + totalunits.y) %>%
    group_by(Schedule) %>%
    summarise(avg.r = sum(units_N * ratio)/sum(units_N),
              bestEcon_n = sum(totalunits.x),
              current_n = sum(totalunits.y),
              sy = year) %>%
    filter(bestEcon_n >num_threshold & current_n >num_threshold)
  return(result)
}
  
inherit.fact<-function(df){
  
  col<-c('ClassificationId','avg.r','CS_ClassId', 'C_ClassId')
  
  glob <- merge(merge(df,comb_Out,by='Schedule'), AllClass,by='ClassificationId') %>%
    filter(CategoryId.x %in% GlobalList & Level2 =='Category' & !is.na(avg.r)) %>%
    summarise(mean(avg.r))
  
  
  start.df<-merge(merge(df,comb_Out,by='Schedule'), AllClass,by='ClassificationId') %>% select(col)
  
  non_na.df <- start.df %>% filter(!is.na(avg.r))
  
  # M level inherit CS level
  m.ih.cs <-merge(start.df %>% filter(is.na(avg.r) & !is.na(CS_ClassId)) %>% select(ClassificationId,CS_ClassId,C_ClassId)
                  ,non_na.df %>% select(c('ClassificationId',avg.r)),by.x='CS_ClassId',by.y='ClassificationId') %>% select(col)
  
  # CSlevel inherit C level 
  cs.ih.c<- merge(anti_join(start.df,rbind(non_na.df,m.ih.cs),by='ClassificationId') %>% select(-avg.r)
                  ,non_na.df %>% select(c('ClassificationId',avg.r)),by.x='C_ClassId',by.y='ClassificationId') %>% select(col)
  
  # remaining inherit global
  ih.glob<- anti_join(start.df,rbind(non_na.df,m.ih.cs,cs.ih.c),by='ClassificationId') %>%
    mutate(avg.r = as.factor(glob))
  
  return(rbind(non_na.df,m.ih.cs,cs.ih.c,ih.glob))}


### build a function to do MoM limitation 
MoMlimit <- function(last_month,current_month,limit){
  upline = last_month + limit
  btline = last_month - limit
  result = ifelse(is.na(last_month), current_month,pmin(upline,pmax(btline,current_month)))
  return(result)
}
################################### Read input file ##########################################
### load the inputfeed file
In<-data.frame(read_excel(excelfile,sheet='In')) %>% filter(Country==CountryCode) %>% select(-ClassificationId,-Plot,-CategoryName,-SubcategoryName,-MakeName,-CSMM,-ValidSchedule)
InR<-data.frame(read_excel(excelfile,sheet='InR')) %>% filter(Country==CountryCode) %>% select(-ClassificationId,-Plot,-CategoryName,-SubcategoryName,-MakeName,-CSMM,-ValidSchedule,-CheckJoin) %>% 
  filter(BorrowType=='RetailBorrowAuction')

inAll <- rbind(In,InR %>% select(-BorrowSchedule,-BorrowType))

SchedFullList<-inAll %>% select(Schedule) %>% distinct()

Out<-data.frame(read_excel(excelfile,sheet='Out')) %>% filter(Country==CountryCode)
OutR<-data.frame(read_excel(excelfile,sheet='OutR')) %>% filter(Country==CountryCode & str_detect(Schedule,' RbA ')) 
### Application tab
comb_Out<-rbind(Out %>% select(ClassificationId, Schedule, CategoryId,SubcategoryId, SubcategoryName,Level2,Plot),OutR %>% select(ClassificationId, Schedule, CategoryId,SubcategoryId,SubcategoryName,Level2,Plot))


######################################### Residual factors #################################################
### break down ###
Catlevel<-inAll %>% filter(Level2 =='Category') %>% select(-SubcategoryId,-MakeId)
Subcatlevel<-inAll %>% filter(Level2 == "SubcatGroup") %>% select(-MakeId)
Makelevel<-inAll %>% filter(Level2 =='Make')

### Join in Category level 
CatData <- merge(uploadData,Catlevel, by=c("CategoryId")) %>%
  select(CategoryId, CategoryName, SubcategoryId,SubcategoryName,MakeId, MakeName, ModelId, ModelName, ModelYear,everything())

### Join in Subcategory Level
SubcatData <- merge(uploadData,Subcatlevel, by=c('CategoryId',"SubcategoryId")) %>%
  select(CategoryId, CategoryName, SubcategoryId,SubcategoryName,MakeId, MakeName, ModelId, ModelName, ModelYear,everything())

### Join in Make Level
MakeData<-merge(uploadData,Makelevel, by=c('CategoryId',"SubcategoryId","MakeId")) %>%
  select(CategoryId, CategoryName, SubcategoryId,SubcategoryName,MakeId, MakeName, ModelId, ModelName, ModelYear,everything())


### Combine C, CS and CSM levels of data into one & exclude bad data
unionData = rbind(CatData,SubcatData,MakeData) %>% filter(Age>=2 & Age<=10)

### data exclusion by some criteria
Datainput<-unionData %>%
  #  filter(Flag2=='inUse' & Flag1=='inUse') %>%
  filter(Flag=='inUse') %>%
  group_by(Schedule) %>%
  filter(Age <= mean(Age) + stdInd*sd(Age) & Age >= mean(Age) - stdInd*sd(Age)) #%>%
#filter(CostRatio <= mean(CostRatio) + stdInd*sd(CostRatio) & CostRatio>= mean(CostRatio) - stdInd*sd(CostRatio)) 
# group_by(Schedule,Age) %>%
# filter(CostRatio >= quantile(CostRatio,0.25) - stdInd*IQR(CostRatio) & CostRatio <= quantile(CostRatio,0.75) + stdInd*IQR(CostRatio)) 



### Fitting the regression 
SchedFullList = data.frame(Datainput) %>% select(Schedule) %>% distinct()
N=dim(SchedFullList)[1]

factor<-rep(0,N)
n<-rep(0,N)

for (j in 1:N){
  
  sched_data <- subset(Datainput,Datainput$Schedule==SchedFullList[j,1])
  
  if (dim(sched_data)[1] >=30) {
    ## fit the regression and force the line go through (0,1)
    fit <-lm(log(CostRatio) ~ Age, sched_data)
    
    cooksd <- cooks.distance(fit)
    influential <- as.numeric(names(cooksd)[(cooksd > 20*mean(cooksd, na.rm=T))])
    
    if (length(influential)==0){
      model <-lm(log(CostRatio) ~ 0 + I(Age), sched_data)
    }
    else{
      remain  = sched_data[-influential, ]
      model <-lm(log(CostRatio) ~ 0 + I(Age), remain)
    }
    
    factor[j]<-exp(coef(model)[1]*target_Age)
    n[j]<-ifelse(dim(remain)[1] ==0, dim(sched_data)[1], dim(remain)[1])
    
  }
}  

out<-data.frame(SchedFullList,n,factor)

## cap the residual factor
cap_out <- out %>% 
  mutate(cap_factor = pmax(cap_resid_low,pmin(cap_resid_hi,factor)))

## join to apply the factors to all classes in out tab
join_out<-merge(comb_Out,cap_out,by='Schedule',all.x=T) %>%
  mutate(MarketCode='USNA') %>%
  select(MarketCode, ClassificationId, cap_factor) %>%
  rename(ResidSF = cap_factor)



################################# Retail Econ Factor ###################################
# recession
comb_recession<-sched.aggr(recessionYr.ret,inAll,'Retail','') %>%
  group_by(Schedule, yearAge) %>%
  summarise(totalunits = sum(Units),
            avg = sum(Units*SPCost)/sum(Units)) 

# best
comb_best<-sched.aggr(bestYr.ret,inAll,'Retail','best')  %>%
  group_by(Schedule,SaleYear, yearAge) %>%
  summarise(totalunits = sum(Units),
            avg = sum(Units*SPCost)/sum(Units))

# current
comb_current<-sched.aggr(currentYr.ret,inAll,'Retail','') %>%
  group_by(Schedule, yearAge) %>%
  summarise(totalunits = sum(Units),
            avg = sum(Units*SPCost)/sum(Units)) 




############## Worst Econ
## join worst year and current year & calculate the worst econ factor
worstEcon_calc<-merge(comb_recession,comb_current,by=c('Schedule','yearAge')) %>%
  mutate(ratio = avg.x/avg.y, units_N = totalunits.x + totalunits.y) %>%
  group_by(Schedule) %>%
  summarise(avg.r = sum(units_N * ratio)/sum(units_N),
            recession_n = sum(totalunits.x),
            current_n = sum(totalunits.y)) %>%
  filter(recession_n >num_threshold & current_n >num_threshold)

## join to the schedule list
WorstEcon_out<-merge(worstEcon_calc,SchedFullList,by='Schedule',all.y=T) 

## results table
WE_outvf<-inherit.fact(WorstEcon_out) %>%
  mutate(SFWorstRetail = pmin(avg.r, WorstEcon_cap)) %>%
  select(ClassificationId,SFWorstRetail)


############## Best Econ 
### calculate the best econ rate using 2018 as best year
comb_best.2018 = bestEconfunc(comb_best,comb_current,2018)

### calculate the best econ rate using 2017 as best year
comb_best.2017 = bestEconfunc(comb_best,comb_current,2017)

### calculate the best econ rate using 2016 as best year
comb_best.2016 = bestEconfunc(comb_best,comb_current,2016)

### compare the three years and pick the max
bestYr.ret.pick<-rbind(comb_best.2018,comb_best.2017,comb_best.2016) %>%
  group_by(Schedule) %>%
  slice(which.max(avg.r))

BestEcon_out<-merge(bestYr.ret.pick,SchedFullList,by='Schedule',all.y=T) %>% select(Schedule,avg.r)

## combine & cap
BE_outvf<-inherit.fact(BestEcon_out) %>%
  mutate(SFBestRetail = pmax(BestEcon_cap,avg.r)) %>%
  select(ClassificationId,SFBestRetail)






################################# Auction Econ Factor ###################################

## recession
comb_recession.auc<-sched.aggr(recessionYr.auc,inAll,'Auction','') %>%
  group_by(Schedule, PublishYear) %>%
  summarise(avg.auc=mean(AvgFlv),
            avg.ret=mean(AvgFmv)) %>%
  mutate(recession.r = avg.auc/avg.ret)

## best
comb_best.auc<-sched.aggr(bestYr.auc,inAll,'Auction','') %>%
  group_by(Schedule, PublishYear) %>%
  summarise(avg.auc=mean(AvgFlv),
            avg.ret=mean(AvgFmv)) %>%
  mutate(best.r = avg.auc/avg.ret)

## current
comb_current.auc<-sched.aggr(currentYr.auc,inAll,'Auction','') %>%
  group_by(Schedule, PublishYear) %>%
  summarise(avg.auc=mean(AvgFlv),
            avg.ret=mean(AvgFmv)) %>%
  mutate(current.r = avg.auc/avg.ret) 


############## Worst Econ
## join worst year and current year & calculate the worst econ factor
worstEcon_calc.auc<-merge(comb_recession.auc,comb_current.auc,by=c('Schedule')) %>%
  mutate(avg.r = pmin(1,recession.r/current.r)) %>%
  select(Schedule,avg.r)

WorstEcon_out.auc<-merge(worstEcon_calc.auc,SchedFullList,by='Schedule',all.y=T) 


## combine
WE_outvf.auc<-merge(inherit.fact(WorstEcon_out.auc),WE_outvf,by='ClassificationId') %>%
  mutate(SFWorstAuction = as.numeric(avg.r) * as.numeric(SFWorstRetail)) %>%
  select(ClassificationId,SFWorstAuction)


############## Best Econ 
BestEcon_calc.auc<-merge(comb_best.auc,comb_current.auc,by=c('Schedule')) %>%
  mutate(avg.r = pmax(1,best.r/current.r)) %>%
  select(Schedule,PublishYear.x,avg.r)


BestYref.auc<-merge(bestYr.ret.pick %>% select(Schedule,sy),BestEcon_calc.auc,by.x=c('Schedule','sy'),by.y=c('Schedule','PublishYear.x'))

BestEcon_out.auc<-merge(BestYref.auc,SchedFullList,by='Schedule',all.y=T) %>% select(-sy)


## combine & cap
BE_outvf.auc<-merge(inherit.fact(BestEcon_out.auc),BE_outvf,by='ClassificationId') %>%
  mutate(SFBestAuction = as.numeric(avg.r) * as.numeric(SFBestRetail)) %>%
  select(ClassificationId,SFBestAuction)




################################# Output file ###################################


## join best and worst econ factor; adjust worst econ factor with a limit gap to best econ factor
residTb<-merge(merge(merge(merge(join_out,BE_outvf,by='ClassificationId'), WE_outvf ,by='ClassificationId'), BE_outvf.auc,by='ClassificationId'),WE_outvf.auc,by='ClassificationId') %>%
  # best and worst are at least .15 apart. fix best and twist worst
  mutate(SFWorstRetail = pmin(SFWorstRetail, as.numeric(SFBestRetail) - Econ_gap),SFWorstAuction = pmin(SFWorstAuction, as.numeric(SFBestAuction) - Econ_gap)) %>%
  select(MarketCode,	ClassificationId,everything())


#### last month values 
LMtb <-LastMonth %>% select(MarketCode,ClassificationId,ResidSf,RetailEconSfBest, RetailEconSfWorst, AuctionEconSfBest, AuctionEconSfWorst) %>%
  rename(Residlm = ResidSf)

## MoM limit applied - get the upload file ready

share_page<- merge(residTb,LMtb,by=c('MarketCode','ClassificationId'),all.x=T) %>%
  mutate(ResidSF = MoMlimit(Residlm,ResidSF,MoM_cap),	
         SFBestRetail= MoMlimit(RetailEconSfBest,SFBestRetail,MoM_cap),
         SFWorstRetail = MoMlimit(RetailEconSfWorst,SFWorstRetail,MoM_cap),
         SFBestAuction = MoMlimit(AuctionEconSfBest,SFBestAuction,MoM_cap),
         SFWorstAuction =	MoMlimit(AuctionEconSfWorst,SFWorstAuction,MoM_cap)) %>%
  mutate(MoMResid = ResidSF/Residlm-1,
         MoMBestRet = as.numeric(SFBestRetail)-RetailEconSfBest,
         MoMWorstRet = as.numeric(SFWorstRetail)-RetailEconSfWorst,
         MoMBestAuc = as.numeric(SFBestAuction)-AuctionEconSfBest,
         MoMWorstAuc = as.numeric(SFWorstAuction)-AuctionEconSfWorst)

residualOutput<- share_page %>%
  select(MarketCode, ClassificationId, ResidSF, SFBestRetail, SFWorstRetail, SFBestAuction, SFWorstAuction)


## explore the upload file


write.csv(residualOutput,paste('ResidualFactor',format(Sys.time(), "%Y%m%d%H%M"),'VL.csv',sep=''),row.names=FALSE)

write.csv(share_page,paste('share_page',format(Sys.time(), "%Y%m%d%H%M"),'VL.csv',sep=''),row.names=FALSE)
