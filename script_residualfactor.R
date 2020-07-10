## change 11/30/2019
#1. residual factor: data pull by acquisition date 12/1/2006 to 11/30/2019
#2. econ factor: match up by age instead of year
#3. use avg avg with proper weight instead of sum avg
#4. best year for different schedule, check 2018, 2017, 2016: max(T12 vs 2018, T12 vs 2017)

##change 1/3/2020
# add auction econ factors

## change 4/30
#1. add recency factor for retail side. 
#2. change the view from CV to inprogress for auction side to use the current month schedule
#3. change the way to weight the average.
############################## Install packages #################################

library(RODBC)
library(RODBCext)
library(lattice)
library(latticeExtra)
library(dplyr)
library(tidyr)
library(tibble)
library(lubridate)
library(xlsx)
library(readxl)
library(stringr)

############################## Declare inputs #####################################

DBserver = 'production'
#DBserver = 'rasquery'

CountryCode='USA'
stdInd=3
target_Age = 8
num_threshold = 20

halfyear.shift<-5.5

GlobalList<-c(313,	6,	2509,	15,	29,	315,	360,	451,	316,	362)
MoM_cap=0.02

cap_resid_low = 1.02
cap_resid_hi = 1.25

WorstEcon_cap =.98
BestEcon_cap = 1.03

Econ_gap = 0.15
publishDate<-Sys.Date() - days(day(Sys.Date()))
MbefpubDate<-publishDate%m-% months(1)

GlobalClassId=1
## file path & read file name 
file_path = "C:/Users/vanessa.li/Documents/GitHub/Residual"
setwd(file_path)  
excelfile = '20200623 SchedulesManagement.xlsx'

##################################### Load data #####################################
runSQL<-parse('SQLqueries.r')
eval(runSQL)

starttime<-Sys.time()
channel<-odbcConnect(DBserver)
uploadData<-sqlQuery(channel,uploadData.sql)
recessionYr.ret<-sqlQuery(channel,recessionYr.ret.sql)
currentYr.ret<-sqlQuery(channel,currentYr.ret.sql)
bestYr.ret<-sqlQuery(channel,bestYr.ret.sql)
recessionYr.auc<-sqlQuery(channel,recessionYr.auc.sql)
currentYr.auc<-sqlQuery(channel,currentYr.auc.sql)
bestYr.auc<-sqlQuery(channel,bestYr.auc.sql)
AllClass<-sqlQuery(channel,AllClass.sql)
LastMonth<-sqlQuery(channel,LastMonth.sql)
endtime<-Sys.time()
endtime-starttime

## run functions
runfunc<-parse('functions.r')
eval(runfunc)
################################### Read input file ##########################################
### load the inputfeed file
In<-data.frame(read.xlsx(excelfile,sheetName='In')) %>% filter(Country==CountryCode) %>% select(-ClassificationId,-Plot,-CategoryName,-SubcategoryName,-MakeName,-CSMM,-ValidSchedule,-CanadaPlots)
InR<-data.frame(read.xlsx(excelfile,sheetName='InR')) %>% filter(Country==CountryCode) %>% select(-ClassificationId,-Plot,-CategoryName,-SubcategoryName,-MakeName,-CSMM,-ValidSchedule,-CheckJoin) %>% 
  filter(BorrowType=='RetailBorrowAuction')

inAll <- rbind(In,InR %>% select(-BorrowSchedule,-BorrowType))

SchedFullList<-inAll %>% select(Schedule) %>% distinct()

Out<-data.frame(read.xlsx(excelfile,sheetName='Out')) %>% filter(Country==CountryCode)
OutR<-data.frame(read.xlsx(excelfile,sheetName='OutR')) %>% filter(Country==CountryCode & str_detect(Schedule,' RbA ')) 
### Application tab
comb_Out<-rbind(Out %>% select(ClassificationId, Schedule, SubcategoryId, SubcategoryName,Level2,Plot),OutR %>% select(ClassificationId, Schedule, SubcategoryId,SubcategoryName,Level2,Plot))


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

factorresid<-rep(0,N)
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
    
    factorresid[j]<-exp(coef(model)[1]*target_Age)
    n[j]<-ifelse(dim(remain)[1] ==0, dim(sched_data)[1], dim(remain)[1])
    
  }
}  

outregression<-data.frame(SchedFullList,n,factorresid) %>%
  rename(factor=factorresid)

resid.global = pmax(cap_resid_low,pmin(cap_resid_hi,globvalue(outregression))) 

## cap the residual factor
cap_out <- outregression %>% 
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
comb_best<-sched.aggr(bestYr.ret,inAll,'Retail','best') %>%
  group_by(Schedule,SaleYear, yearAge) %>%
  summarise(totalunits = sum(Units),
            avg = sum(Units*SPCost)/sum(Units))

joinretail_cur<-sched.aggr(currentYr.ret,inAll,'Retail','current')
# current
comb_current<-joinretail_cur %>%
  group_by(Schedule, yearAge) %>%
  summarise(totalunits = sum(Units),
            avg = sum(Units*SPCost)/sum(Units)) 

################ Calculate the recency factor ####################3

comb_currentMonth<-merge(joinretail_cur %>% 
  filter(as.Date(SaleMonth) %in% c(MbefpubDate, publishDate)) %>%
  group_by(Schedule, ModelYear) %>%
  summarise(units.recent = sum(Units),
            factorecent = sum(Units*SPCost)/sum(Units)),
  joinretail_cur %>% 
    group_by(Schedule, ModelYear) %>%
    summarise(totalunits = sum(Units),
              avg = sum(Units*SPCost)/sum(Units)),by=c('Schedule','ModelYear')) %>%
  mutate(depr = avg*(.99)^halfyear.shift) %>%
  mutate(factor = depr/factorecent,
         mincounts = pmin(totalunits,units.recent),
         sumcounts = totalunits+units.recent)
         
avgMinN.recentF <- comb_currentMonth %>%
  group_by(Schedule) %>%
  summarise(avg.mincount = mean(mincounts))


recent.ret.factor<-merge(comb_currentMonth, avgMinN.recentF,by='Schedule')%>% 
  mutate(weight = (mincounts/avg.mincount)*sumcounts) %>%
  group_by(Schedule) %>%
  summarise(recFactor_v0 = sum(factor*weight) /sum(weight),
            n = sum(units.recent)) %>%
  ## cap the factor by number of data
  mutate(recFactor_v1 = ifelse(n>num_threshold, 1*recFactor_v0, ((num_threshold-n)+n*recFactor_v0)/num_threshold)) %>%
  mutate(recFactor = ifelse(recFactor_v1<1,pmax(((num_threshold-n)+n*.8)/num_threshold,recFactor_v1),pmin(((num_threshold-n)+n*1.2)/num_threshold,recFactor_v1)))


############## Worst Econ
## calculate the ratio of recession year/ current year, as well as min unit counts and sum unit counts 
worstEcon_calc_s1<-merge(comb_recession,comb_current,by=c('Schedule','yearAge')) %>%
  mutate(ratio = avg.x/avg.y,
         mincounts = pmin(totalunits.x , totalunits.y),
         sumcounts = totalunits.x + totalunits.y)

## average the min unit counts
avgMincounts<-worstEcon_calc_s1 %>%
  group_by(Schedule) %>%
  summarise(avg.mincount = mean(mincounts))

## join the above table back and do the weighted average
worstEcon_calc<-merge(worstEcon_calc_s1,avgMincounts,by = 'Schedule') %>%
  mutate(units_N = (mincounts/avg.mincount)*sumcounts) %>%
  group_by(Schedule) %>%
  summarise(factor = sum(units_N * ratio)/sum(units_N),
            recession_n = sum(totalunits.x),
            current_n = sum(totalunits.y)) %>%
  filter(recession_n >num_threshold & current_n >num_threshold)


## join to the schedule list
WorstEcon_out<-merge(worstEcon_calc,SchedFullList,by='Schedule',all.y=T) 


retRecent_out <- merge(recent.ret.factor,comb_Out,by='Schedule',all.y=T) %>% select(ClassificationId,recFactor)
retRecent_out[is.na(retRecent_out)]<-1

## results table
WE_outvf<-merge(inherit.fact(WorstEcon_out),retRecent_out,by='ClassificationId',all.y=T) %>%
  mutate(SFWorstRetail = pmin(as.numeric(factor)*recFactor, WorstEcon_cap)) %>%
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
  slice(which.max(factor))

BestEcon_out<-merge(bestYr.ret.pick,SchedFullList,by='Schedule',all.y=T) %>% select(Schedule,factor)



## combine & cap
BE_outvf<-merge(inherit.fact(BestEcon_out),retRecent_out,by='ClassificationId',all.y=T) %>%
  mutate(SFBestRetail = pmax(BestEcon_cap,as.numeric(factor)*recFactor)) %>%
  select(ClassificationId,SFBestRetail)



################################# Auction Econ Factor ###################################

## recession
comb_recession.auc<-sched.aggr(recessionYr.auc,inAll,'Auction','') %>%
  group_by(Schedule, PublishYear) %>%
  summarise(avg.auc=mean(AvgFlv),
            factoret=mean(AvgFmv)) %>%
  mutate(recession.r = avg.auc/factoret)

## best
comb_best.auc<-sched.aggr(bestYr.auc,inAll,'Auction','') %>%
  group_by(Schedule, PublishYear) %>%
  summarise(avg.auc=mean(AvgFlv),
            factoret=mean(AvgFmv)) %>%
  mutate(best.r = avg.auc/factoret)

## current
comb_current.auc<-sched.aggr(currentYr.auc,inAll,'Auction','') %>%
  group_by(Schedule, PublishYear) %>%
  summarise(avg.auc=mean(AvgFlv),
            factoret=mean(AvgFmv)) %>%
  mutate(current.r = avg.auc/factoret) 


############## Worst Econ
## join worst year and current year & calculate the worst econ factor
worstEcon_calc.auc<-merge(comb_recession.auc,comb_current.auc,by=c('Schedule')) %>%
  mutate(factor = pmin(WorstEcon_cap,recession.r/current.r)) %>%
  select(Schedule,factor)

WorstEcon_out.auc<-merge(worstEcon_calc.auc,SchedFullList,by='Schedule',all.y=T) 


## combine
WE_outvf.auc<-merge(inherit.fact(WorstEcon_out.auc),WE_outvf,by='ClassificationId') %>%
  mutate(SFWorstAuction = as.numeric(factor) * as.numeric(SFWorstRetail)) %>%
  select(ClassificationId,SFWorstAuction)


############## Best Econ 
BestEcon_calc.auc<-merge(comb_best.auc,comb_current.auc,by=c('Schedule')) %>%
  mutate(factor = pmax(BestEcon_cap,best.r/current.r)) %>%
  select(Schedule,PublishYear.x,factor)


BestYref.auc<-merge(bestYr.ret.pick %>% select(Schedule,sy),BestEcon_calc.auc,by.x=c('Schedule','sy'),by.y=c('Schedule','PublishYear.x'))

BestEcon_out.auc<-merge(BestYref.auc,SchedFullList,by='Schedule',all.y=T) %>% select(-sy)


## combine & cap
BE_outvf.auc<-merge(inherit.fact(BestEcon_out.auc),BE_outvf,by='ClassificationId',all.y=T) %>%
  mutate(SFBestAuction = as.numeric(factor) * as.numeric(SFBestRetail)) %>%
  select(ClassificationId,SFBestAuction)



##### Data Validation

dim(join_out)[1]==dim(BE_outvf)[1]
dim(join_out)[1]==dim(WE_outvf)[1]
dim(join_out)[1]==dim(BE_outvf.auc)[1]
dim(join_out)[1]==dim(WE_outvf.auc)[1]
################################# Output file ###################################


## join best and worst econ factor; adjust worst econ factor with a limit gap to best econ factor
residTb<-merge(merge(merge(merge(join_out,BE_outvf,by='ClassificationId'), WE_outvf ,by='ClassificationId'), BE_outvf.auc,by='ClassificationId',all.x=T),WE_outvf.auc,by='ClassificationId',all.x = T) 

## global values
globalvalue<-data.frame(ClassificationId = GlobalClassId,
                        MarketCode='USNA',
                        ResidSF = resid.global,
                        SFBestRetail = globvalue(BestEcon_out),
                        SFWorstRetail = globvalue(WorstEcon_out),
                        SFBestAuction = globvalue(BestEcon_out.auc),
                        SFWorstAuction = globvalue(WorstEcon_out.auc))

residTb_glob<-rbind(globalvalue,residTb) %>%
  # best and worst are at least .15 apart. fix best and twist worst
  mutate(SFWorstRetail = pmin(SFWorstRetail, as.numeric(SFBestRetail) - Econ_gap),SFWorstAuction = pmin(SFWorstAuction, as.numeric(SFBestAuction) - Econ_gap)) %>%
  select(MarketCode,	ClassificationId,everything())
#### last month values 
LMtb <-LastMonth %>% select(MarketCode,ClassificationId,ResidSf,RetailEconSfBest, RetailEconSfWorst, AuctionEconSfBest, AuctionEconSfWorst) %>%
  rename(Residlm = ResidSf)

## MoM limit applied - get the upload file ready

share_page<- merge(residTb_glob,LMtb,by=c('MarketCode','ClassificationId'),all.x=T) %>%
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

write.xlsx(share_page,paste('share_page',format(Sys.time(), "%Y%m%d%H%M"),'VL.xlsx',sep=''),row.names=FALSE,sheetName = 'Sheet1')
