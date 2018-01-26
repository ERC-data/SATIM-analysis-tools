# functions for doing calculations of pumped storage profiles and others
# 4 july

getColourGroup = function(path,df1,id){
  #returns a vector with the associated colour coding from the colour codes csv file
  #path is the path to the CSV file and must include name of the CSV file
  #df1 is the data frame that we want to get the unique item names from the CSV file from.
  # id is the name of the column in df1 which we want to compare with our colour tables. 
  print('getting colour set')
  colourTable = read.csv(path,header =T)
  
  colourGroup = as.vector(df1[id])
  
  colourGroup= unique(df1[id])
  
  #append the unique list with the associated colour code
  colourGroup = merge(colourGroup,colourTable, by.x = id, by.y = "Item",all.x = T,sort = F, incompatible = "#000000")
  
  rownames(colourGroup) = colourGroup[,id] #make row names the name of technologies
  colourGroup = colourGroup['ColourCode'] # remove the now redundant extra column
  #transpose
  mycolours = t(colourGroup)
  mycolours = as.vector(mycolours) # make back into vector
  names(mycolours) = rownames(colourGroup) # it appears that R is finickey with this
  print('done gettign colour set')
  return(mycolours)
}
loadTsTablefromDB <- function(DBPath){
  print('Loading TS fractions from GDX')
  #get the timeslices and their fractions:
  # Obtain gdx file info
  info <- gdxInfo(DBPath, dump = FALSE, returnDF = TRUE)
  sets <- info$set$name
  params <- info$parameters$name
  
  if("G_YRFR" %in% params){
    print("getting G_YRFR")
    tsTable = rgdx.param(DBPath,'G_YRFR') #FIX - replace gdxname_ts with the gdxname - the gdx file should have the gyrfr so dont need another gdx file  
    tsTable = tsTable[,c(2,3)]
    names(tsTable) = c('Timeslice','Fraction')
  }else{
    print('fetching G_YRFR from resource folder')
    tsTable = read.csv(paste(resourcesPath,'/tsTable.csv',sep =''))
  }
  # make a column for each ts id
  tsTable$B = substr(as.character(tsTable$Timeslice),5,6)
  tsTable$S = substr(as.character(tsTable$Timeslice),1,2)
  tsTable$D = substr(as.character(tsTable$Timeslice),3,4)
  assign("tsTable",tsTable,.GlobalEnv) #load into the global environment. instead of returning it. 
  #return(tsTable)
}

makeHourlyDetail <- function(tmp){  
  
  if('F_IN' %in% names(tmp)){
    print('input')
    tmp$flow_dir = 'in'
    tmp$flow_amount = tmp$F_IN
    names(tmp)[names(tmp) == 'F_IN'] = 'Flow'
    
  }else{
    print(
      'output'
    )
    tmp$flow_dir = 'out'
    tmp$flow_amount = tmp$F_OUT
    names(tmp)[names(tmp) == 'F_OUT'] = 'Flow'
    
  }
  
  if('S3'%in% unique(substr(as.character(tmp$Timeslice),1,2))|'D2'%in%unique(substr(as.character(tmp$Timeslice),3,4))){
    tspath = paste(localDir,"/resources/timeslice_data20.xlsx",sep = '') # location of user defined Timeslices'
    print('Model is 20 timeslices')
  }else{
    tspath = paste(localDir,"/resources/timeslice_data_8ts.xlsx",sep = '') # location of user defined Timeslices'
    print('Model is 8 Timelices')
  }
  
  blockids = getblockidsDataset(tspath)
  m = max(nchar(as.character(tmp$Timeslice)))
  
  if(sum(nchar(as.character(tmp$Timeslice)) < m)>0){
    print('Some entries have weekly timeslices')
    print('converting these to full timeslices')
    
    tmp2 = tmp[nchar(as.character(tmp$Timeslice)) < m ,]
    print(unique(tmp2$Process))
    
    
    print('convert energy to power using TS fractions and time')
    
    tmp2 = merge(tmp2,tsTable,by = 'Timeslice') #add ts details to the power table  
    tmp2$GW = tmp2$Flow*277.78/(8760*tmp2$Fraction)
    
    tmp2 = merge(tmp2,tsTable,by = 'S') #this adds to the df the missing timeslices that are at a finer resolution
    
    names(tmp2)[grepl('Timeslice.y',names(tmp2))] = 'Timeslice'#rename
    #names(tmp2)[names(tmp2)=='B'] = 'Block'
    
    tmp2 = tmp2[nchar(as.character(tmp2$Timeslice))==m,]#drop all those that are not full ts length
    print('done extrapolating to full ts detail')
    #tmp2 = tmp2[,names(tmp) ]
    
    
    #tmp3$Block = substr(as.character(tmp3$Timeslice),5,6)
    #tmp2 = tmp2[,names(tmp3)]
    #tmp = rbind(tmp2,tmp3)
  }
  #get those that were full  timeslice
  print('convert energy to power using TS fractions and time')
  tmp3 = tmp[(nchar(as.character(tmp$Timeslice)) ==m),]
  tmp3 = merge(tmp3,tsTable,by = 'Timeslice')
  tmp3$GW = tmp3$Flow*277.78/(8760*tmp3$Fraction)
  colnames = c(names(tmp),'GW')
  if(is.null(tmp2[1,1])){
    #in case the if statement above does not get called. 
    tmp2 = data.frame()
  }else{
    tmp2 = tmp2[,colnames]
  }
  
  tmp3 = tmp3[,colnames]
  
  #recombine the now extrapolated to full ts data to the others that didnt need to be extrapolated
  
  tmp = rbind(tmp2,tmp3)
  
  tmp$B = substr(as.character(tmp$Timeslice),5,6)
  tmp$S = substr(as.character(tmp$Timeslice),1,2)
  tmp$D = substr(as.character(tmp$Timeslice),3,4)
  
  print('add hourly detail from blocks')
  tmp = merge(tmp,blockids)       
  
  tmp$model = gdxname
  
  return(tmp)
}

getblockidsDataset <- function(TSfilepath){
  print('fecthing timeslice and making blockids dataset')
  
  #GET timeslice data in the right format from excel file. 
  
  ts_table_seasons = readWorksheetFromFile(TSfilepath,sheet = 'Seasons')
  ts_table_days = readWorksheetFromFile(TSfilepath,sheet = 'Days')
  
  #convert ts_table_days into long format: expanding over wday
  ts_table_days2 = data.frame()
  for(i in seq(1,length(ts_table_days$SeasonName))){
    #loop over each row in ts_table_days
    #for each row, split all 'wday' into a vector
    #make a column in new matrix with duplicate 'Season' in first column but with all wday in second column and assing 
    wday = as.character(strsplit(ts_table_days[i,'wday'],',')[[1]])
    SeasonName= sprintf(ts_table_days[i,'SeasonName'],seq(1,length(wday)))
    
    tmp = data.frame(SeasonName,wday) #create new dataframe
    tmp$DayID = ts_table_days[i,'DayID']#add the dayID
    tmp$DayTypeName = ts_table_days[i,'DayTypeName'] #add the daytypename detail
    tmp = cbind(tmp,ts_table_days[i,-c(1,2,3,4)]) #add all the hours for this row
    
    ts_table_days2 = rbind(ts_table_days2,tmp)# add this to the new dataframe
    tmp = 0
  }
  ts_table_days = ts_table_days2
  x = ts_table_seasons[,names(ts_table_seasons)%in%c('SeasonName','SeasonID')]
  x = x[!duplicated(x),]
  ts_table_days = merge(ts_table_days,x,by = 'SeasonName')
  x = melt(ts_table_days,id.vars = c('SeasonName','SeasonID','wday','DayID','DayTypeName'))
  names(x)[names(x)%in%c('variable','value')] = c('hour','BlockID')
  blockids = x
  blockids$hour = as.numeric(gsub('X','',blockids$hour))#remove those silly X's
  
  #convert blockids to long format with SDB format
  blockids = blockids[,names(blockids)%in% c('SeasonID','DayID','BlockID','hour')]
  blockids$Timeslice = paste('S',blockids$SeasonID,sep = '')
  blockids$Timeslice = paste(blockids$Timeslice,'D',sep = '')
  blockids$Timeslice = paste(blockids$Timeslice,blockids$DayID,sep = '')
  blockids$Timeslice = paste(blockids$Timeslice,'B',sep = '')
  blockids$Timeslice = paste(blockids$Timeslice,blockids$BlockID,sep = '')
  
  blockids = blockids[names(blockids)%in% c('Timeslice','hour')]
  
  blockids = blockids[(!duplicated(blockids)),]
  
  return(blockids)
}
addPRCmap <- function(db){
  #this function gets the mapPRC dataset from the csv file which should be in your workdir
  #and merges it with db
  print('Adding process mapping')
  
  db = merge(db,mapPRC,all = FALSE)
  return(db)
}
addCOMmap <- function(db){
  #this function gets the mapPRC dataset from the csv file which should be in your workdir
  #and merges it with db
  print('Adding mapping for commodities')
  
  db = merge(db,mapCOM,all = FALSE)
  return(db)
}
