# This gets the operational results any specified technology
#   converts these to hourly detail
#   also has power (GW) results

#20 June 2017
#can now safely look at multiple scenario results. 

library(reshape2)
library(ggplot2)
library(scatterplot3d)
library(gdxrrw)
library(dplyr)
library(XLConnect)
library(rpivotTable)


localDir = 'C:/Users/01425453/Documents/R/DispatchProfilesAnalyses/'# this folder
gdxLocation = 'C:/SATIMGE_02/SATM/Gams_WrkTI-PAMS/Gamssave/' 

resourcesPath = paste(localDir,'resources',sep = '')
TSfilepath = paste(resourcesPath,'/timeslice_data_8ts.xlsx',sep = '')

#list of model results to comapre.  
modellist =  c('REFU')
# Names of technologies to look at. 
technames = c('ETRANS')
sectornames  = '.*'

#mapping of techs and commodities
mapPRC = read.csv(paste(resourcesPath,'/mapPRC.csv',sep ='')) # Process + Tech.Description + Sector + Subsector + Subsubsector
mapCOM = read.csv(paste(resourcesPath,'/mapCOM.csv',sep =''))

#load functions
source(paste(resourcesPath,'/functions_oct2017.R',sep =''))

N = length(modellist)
tmp = tmp2 = data.frame()


for(i in seq(1,N)){
  gdxname = modellist[i]
  DBPath = paste(c(gdxLocation,gdxname,'.gdx'),collapse = '')
  print(gdxname)
  
  loadTsTablefromDB(DBPath)
  
  FOUT =rgdx.param(DBPath,'F_OUT')
  FIN =rgdx.param(DBPath,'F_IN')
  
  names(FOUT) = c('Region','V_Year','Year','Process','Commodity','Timeslice','F_OUT')
  names(FIN) = c('Region','V_Year','Year','Process','Commodity','Timeslice','F_IN')
  
  FOUT = addPRCmap(FOUT)
  FIN = addPRCmap(FIN)
  
  tech_fout = FOUT[grepl(technames,FOUT$Process)&grepl(sectornames,FOUT$Sector),names(FOUT) != 'V_Year']
  tech_fin = FIN[grepl(technames,FIN$Process)&grepl(sectornames,FIN$Sector),names(FIN)!= 'V_Year']
  #drop PRWENV
  tech_fout = tech_fout[tech_fout$Commodity != 'PWRENV',]
  #checkPumpRunningTurbines(tmpcheck)
  tmpin = tmpout = data.frame()
  tmpin = makeHourlyDetail(tech_fin)
  tmpout = makeHourlyDetail(tech_fout)
  
  tmp = rbind(tmpin,tmpout)
  
  tmp$model = gdxname
  
  if(i!= 1){
    print('appending dataframes of different model results')
    tmp2 = rbind(tmp,tmp2)
  }else{
    tmp2 = tmp
  }
}
tmp = tmp2

options(viewer = NULL) # stop rstudio from opening in viewer tab - then it opens in browser. 
#create rpivottable. 
rpivotTable(tmp, rows = c('Year','Season','Process','Tech.Description','flow_dir'),
            cols = c('Day','Block','hour'),
            aggregatorName = 'Average',
            vals = 'flow_amount')

