# This gets the operational results for the pumped storage
#   converts these to hourly detail
#   also has power (GW) results

#20 June 2017
library(reshape2)
library(ggplot2)
library(scatterplot3d)
library(gdxrrw)
library(dplyr)
library(XLConnect)
library(rpivotTable)


localDir = 'C:/Users/01425453/Google Drive/Work/Projects Current/SANEDI Dist costs/R regional elec profiles/'# this folder
gdxLocation = 'C:/AnswerTIMESv6/Gams_WrkTI/Gamssave/' 

#mapping of techs and commodities
mapPRC = read.csv(paste(localDir,'/mapPRC.csv',sep ='')) # Process + Tech.Description + Sector + Subsector + Subsubsector
mapCOM = read.csv(paste(localDir,'/mapCOM.csv',sep =''))

source(paste(localDir,'loadUsefulFunctions.R',sep = ''))

modellist =  c('SATIM_SA15','RUN1CPT28') 
N = length(modellist)
tmp2 = data.frame()


for(i in seq(1,N)){
  gdxname = modellist[i]
  DBPath = paste(c(gdxLocation,gdxname,'.gdx'),collapse = '')
  print(DBPath)
  loadTsTablefromDB(DBPath)
  
  FOUT =rgdx.param(DBPath,'F_OUT')
  FIN =rgdx.param(DBPath,'F_IN')
  
  names(FOUT) = c('Region','V_Year','Year','Process','Commodity','Timeslice','F_OUT')
  names(FIN) = c('Region','V_Year','Year','Process','Commodity','Timeslice','F_IN')
  
  technames = c('EPDD-N',
                'EPDW-N',
                'EPT-N',
                'EPP-N',
                'EPDD-E',
                'EPDW-E',
                'EPT-E',
                'EPP-E',
                'ESTOREDUM') #name of tech we want to look at
  colnames = c('Year','Process','Commodity','Timeslice','F_OUT','F_IN','Region')
  
  Process_name = c('Storage','Storage_Weekly','Turbine','Pump','Storage','Storage_Weekly','Turbine','Pump','Simple Storage')
  Process = technames
  dfname = data.frame(Process,Process_name)
  
  tech_fout = FOUT[FOUT$Process%in% technames,names(FOUT)%in% colnames]
  tech_fin = FIN[FIN$Process%in% technames,names(FIN)%in% colnames]
  
  tmpcheck = merge(tech_fout,dfname,all.x = T)
  tmpcheck[grepl('-E$',tmpcheck$Process),'vintage'] = 'Existing'
  tmpcheck[grepl('-N$',tmpcheck$Process),'vintage'] = 'New'
  
  print(gdxname)
  #checkPumpRunningTurbines(tmpcheck)
  
  
  tmpin = makeHourlyDetail(tech_fin)
  tmpout = makeHourlyDetail(tech_fout)
  
  tmp = rbind(tmpin,tmpout)
  
  
  tmp = merge(tmp,dfname,all.x = T)
  tmp[grepl('-E$',tmp$Process),'vintage'] = 'Existing'
  tmp[grepl('-N$',tmp$Process),'vintage'] = 'New'
  
  tmp$model = gdxname
  
  if(i!= 1){
    print('appending dataframes of different model results')
    tmp2 = rbind(tmp,tmp2)
  }else{
    tmp2 = tmp
  }
}
tmp = tmp2

rpivotTable(tmp, rows = c('Year','Season','vintage','Process_name','flow_dir'),
            cols = c('Day','Block'),
            aggregatorName = 'Average',
            vals = 'flow_amount')

#make df of results
myyear = 2030
colnames = c('flow_dir','Process_name','Year','flow_amount','Season','Day','Block')
tmp = tmp[tmp$Year == myyear,(names(tmp)%in% colnames)]
#tmp$flow_amount = round(tmp$flow_amount,digits = 3)
cast_tmp = dcast(tmp,Year+Season+Day+Process_name+flow_dir~Block,mean,value.var = 'flow_amount',fun.aggregate = mean,na.rm = T)
cast_tmp[is.na(cast_tmp)] = ''
View(cast_tmp  )
