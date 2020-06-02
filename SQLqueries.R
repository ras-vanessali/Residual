uploadData.sql<-"
                                         
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
      ,[CurrentABCostUSNA]
	    ,CurrentABCostUSNA/Cost as CostRatio
      ,CAST(YEAR(@dateEffective)-ModelYear + (MONTH(@dateEffective)-6)/12.00 as decimal(10,4))  as Age
	   ,CASE                    
             WHEN (CurrentABCostUSNA/Cost > 1.25+0.125*(YEAR(@dateEffective)-ModelYear + (MONTH(@dateEffective)-6)/12.00)
			    OR CurrentABCostUSNA/Cost < 0.75+0.015*(YEAR(@dateEffective)-ModelYear + (MONTH(@dateEffective)-6)/12.00)) THEN 'EcxRegr'
             ELSE 'inUse' END AS 'Flag'
  FROM [ras_sas].[BI].[CustomerAssetsCost] 
  WHERE  [IsUsedForABCostUSNA]='Y' and [IsPurchasedUsed]='N' 
    AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
    AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
		AND NOT ([SubcategoryId] in (2806,2808,2001,2636) and makeid=31 and ModelName not like 'XQ%')  
	AND AcquisitionDate > @dateStart and AcquisitionDate<=@dateEffective
    AND CurrentABCostUSNA is not NULL 
	  AND Cost>10
	  AND NOT(CustomerId = 178 And (Equipno like 'N%' or Equipno like '%NF'))"

##################################### Retail Econ Factor ###########################################
recessionYr.ret.sql<-" SET NOCOUNT ON
select  CategoryId,CategoryName, SubcategoryId,SubcategoryName, MakeId, MakeName,ModelYear,cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,0)) as yearAge, avg(SalePrice/Cost) SPCost,Count(*) Units
from [ras_sas].[BI].Comparables 
where saletype = 'retail' and IsUsedForComparablesUSNA='y'
	  and SaleYear = 2009
	  and YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 >3
	  and YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 <10
	  and cost /CurrentABCostUSNA<2 and cost/CurrentABCostUSNA>.5
	  and SalePrice/M1PrecedingFmv <1.4 and SalePrice/M1PrecedingFmv >.6
	  and SalePrice/Cost < 1
	  and COST>5
	  AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
      AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
      AND NOT ([SubcategoryId] in (2806,2808,2001,2636) and makeid=31 and ModelName not like 'XQ%')
group by CategoryId,CategoryName, SubcategoryId,SubcategoryName, MakeId, MakeName,ModelYear,cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,0))	"



currentYr.ret.sql<-" SET NOCOUNT ON
Declare @dateStart DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, DATEADD(year,-1,GETDATE()))-1, -1) as date)
Declare @dateEnd DATE = CAST(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-1, -1) AS date)
SELECT  CategoryId,CategoryName, SubcategoryId,SubcategoryName,MakeId, MakeName, ModelYear, EOMONTH(SaleDate) as SaleMonth,
cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,0)) as yearAge,avg(SalePrice/Cost) SPCost,Count(*) Units
from [ras_sas].[BI].Comparables 
where saletype = 'retail' and IsUsedForComparablesUSNA='y'
	  and SaleDate >@dateStart and SaleDate<=@dateEnd
	  --and SaleDate>='2020-03-01' and SaleDate<='2020-03-31'
	  and YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 >3
	  and YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 <10
	  and cost /CurrentABCostUSNA<2 and cost/CurrentABCostUSNA>.5
	  and SalePrice/Cost < 1
	  and SalePrice/M1PrecedingFmv <1.4 and SalePrice/M1PrecedingFmv >.6
	  and COST>5
	  AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
      AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
      AND NOT ([SubcategoryId] in (2806,2808,2001,2636) and makeid=31 and ModelName not like 'XQ%')
group by CategoryId,CategoryName, SubcategoryId,SubcategoryName, MakeId, MakeName, ModelYear,EOMONTH(SaleDate),cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,0))	"


bestYr.ret.sql<-" SET NOCOUNT ON
	 select  CategoryId,CategoryName, SubcategoryId,SubcategoryName, MakeId, MakeName, SaleYear, ModelYear, cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,0)) as yearAge,avg(SalePrice/Cost) SPCost,Count(*) Units
from [ras_sas].[BI].Comparables 
where saletype = 'retail' and IsUsedForComparablesUSNA='y'
	  and SaleYear between 2016 and 2018
	  and YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 >3
	  and YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 <10
	  and cost /CurrentABCostUSNA<2 and cost/CurrentABCostUSNA>.5
	  and SalePrice/Cost < 1
	  and SalePrice/M1PrecedingFmv <1.4 and SalePrice/M1PrecedingFmv >.6
	  and COST>5
	  AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
      AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
      AND NOT ([SubcategoryId] in (2806,2808,2001,2636) and makeid=31 and ModelName not like 'XQ%')
group by CategoryId,CategoryName, SubcategoryId,SubcategoryName, MakeId, MakeName, SaleYear,ModelYear,cast(YEAR(SaleDate)-ModelYear + (MONTH(SaleDate)-6)/12.00 as decimal(10,0))	"



##################################### Auction Econ Factor ###########################################
recessionYr.auc.sql<-" SET NOCOUNT ON
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
  FROM [ras_sas].[BI].[AppraisalBookClassificationValuesUSNA]
  where AppraisalBookPublishDate between '2009-03-31' and '2009-12-31' 
  and ModelId is null 
  AND ModelYear between PublishYear-10 and PublishYear-3
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
      ,[ModelYear]"


currentYr.auc.sql<-" SET NOCOUNT ON

SELECT [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[MakeId]
      ,[MakeName]
      ,year(DATEADD(MONTH, DATEDIFF(MONTH, -1, GETDATE())-1, -1)) as PublishYear
      ,[ModelYear]
      ,avg([FlvSchedulePercentage]) AvgFlv
      ,avg([FmvSchedulePercentage]) AvgFmv
  FROM [ras_sas].[BI].[ClassificationValuesInProgressUSNA]

  where ModelId is null AND ModelYear between year(GETDATE()) -10 and year(GETDATE())-3
   AND CategoryId Not In (220,	1948,	18,	4,	1949,	234,	21,	31,	2733,	2706,	2718,	2692,	2724,	2674,	2700,	2708)
   AND MakeId NOT in (58137,78) --Miscellaneous,Not Attributed,Various
   Group By [ClassificationId]
      ,[CategoryId]
      ,[CategoryName]
      ,[SubcategoryId]
      ,[SubcategoryName]
      ,[MakeId]
      ,[MakeName]
      ,[ModelYear]	"


bestYr.auc.sql<-" SET NOCOUNT ON
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
  FROM [ras_sas].[BI].[AppraisalBookClassificationValuesUSNA]
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
      ,[ModelYear]"


### import a table with all classifications
AllClass.sql<-"
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
      ,BIC.[MakeName]"


### Import last month values
LastMonth.sql<-" SET NOCOUNT ON
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
  WHERE [AppraisalBookPublishDate] = @EffectiveDate "
