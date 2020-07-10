################################## Functions ####################################

sched.aggr<-function(data.df,in.df,stype,target){
  retsele.best<-c('Schedule','SaleYear','ModelYear', 'yearAge', 'Units','SPCost')
  retsele.current<-c('Schedule','ModelYear', 'yearAge', 'SaleMonth','Units','SPCost')
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
    else if(target == 'current'){
      cat <- merge(data.df,Catlevel, by=c('CategoryId')) %>% select(retsele.current) 
      subcat <- merge(data.df,Subcatlevel, by=c('CategoryId',"SubcategoryId")) %>% select(retsele.current) 
      make <- merge(data.df,Makelevel, by=c('CategoryId',"SubcategoryId",'MakeId')) %>% select(retsele.current) }
    else{
      cat <- merge(data.df,Catlevel, by=c('CategoryId')) %>% select(retsele.other) 
      subcat <- merge(data.df,Subcatlevel, by=c('CategoryId',"SubcategoryId")) %>% select(retsele.other) 
      make <- merge(data.df,Makelevel, by=c('CategoryId',"SubcategoryId",'MakeId')) %>% select(retsele.other) }
  }
  
  else {
    cat <- merge(data.df,Catlevel, by=c('CategoryId')) %>% select(all_of(aucsele)) 
    subcat <- merge(data.df,Subcatlevel, by=c('CategoryId',"SubcategoryId")) %>% select(all_of(aucsele)) 
    make <- merge(data.df,Makelevel, by=c('CategoryId',"SubcategoryId",'MakeId')) %>% select(all_of(aucsele))
  }
  return(rbind(cat,subcat,make))
}


bestEconfunc<- function(best.df,cur.df,year){
  
  jointable <- merge(best.df %>% filter(SaleYear ==year),cur.df,by=c('Schedule','yearAge')) %>%
    mutate(ratio = avg.x/avg.y, 
           mincounts = pmin(totalunits.x , totalunits.y),
           sumcounts = totalunits.x + totalunits.y)
  
  mincount<-jointable %>%
    group_by(Schedule) %>%
    summarise(avg.mincount =mean(mincounts))
  
  result <- merge(jointable,mincount,by='Schedule') %>%
    mutate(units_N = (mincounts/avg.mincount)*sumcounts) %>%
    group_by(Schedule) %>% 
    summarise(factor = sum(units_N * ratio)/sum(units_N),
              bestEcon_n = sum(totalunits.x),
              current_n = sum(totalunits.y),
              sy = year) %>%
    filter(bestEcon_n >num_threshold & current_n >num_threshold)
  return(result)
}


inherit.fact<-function(df){
  
  col<-c('ClassificationId','factor','CS_ClassId', 'C_ClassId')
  
  glob <- merge(merge(df,comb_Out,by='Schedule'), AllClass,by='ClassificationId') %>%
    filter(CategoryId %in% GlobalList & Level2 =='Category' & !is.na(factor)) %>%
    summarise(mean(factor))
  
  
  start.df<-merge(merge(df,comb_Out,by='Schedule'), AllClass,by='ClassificationId') %>% select(col)
  
  non_na.df <- start.df %>% filter(!is.na(factor))
  
  # M level inherit CS level
  m.ih.cs <-merge(start.df %>% filter(is.na(factor) & !is.na(CS_ClassId)) %>% select(ClassificationId,CS_ClassId,C_ClassId)
                  ,non_na.df %>% select(c('ClassificationId',factor)),by.x='CS_ClassId',by.y='ClassificationId') %>% select(col)
  
  # CSlevel inherit C level 
  cs.ih.c<- merge(anti_join(start.df,rbind(non_na.df,m.ih.cs),by='ClassificationId') %>% select(-factor)
                  ,non_na.df %>% select(c('ClassificationId',factor)),by.x='C_ClassId',by.y='ClassificationId') %>% select(col)
  
  # remaining inherit global
  ih.glob<- anti_join(start.df,rbind(non_na.df,m.ih.cs,cs.ih.c),by='ClassificationId') %>%
    mutate(factor = as.factor(glob))
  
  return(rbind(non_na.df,m.ih.cs,cs.ih.c,ih.glob))}

## create the global value
globvalue<-function(df){
  glob <- merge(merge(df,comb_Out,by='Schedule'), AllClass,by='ClassificationId') %>%
    filter(CategoryId %in% GlobalList & Level2 =='Category' & !is.na(factor)) %>%
    summarise(mean(factor))
  return(as.numeric(glob))
}


### build a function to do MoM limitation 
MoMlimit <- function(last_month,current_month,limit){
  upline = last_month + limit
  btline = last_month - limit
  result = ifelse(is.na(last_month), current_month,pmin(upline,pmax(btline,current_month)))
  return(result)
}