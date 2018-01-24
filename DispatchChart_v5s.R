# 22 March 2016
# Bryce Mccall
# 
# Dispatch charting 
# version 5 works with the slightly modified GAMS output gdx's which now have timeslice detail added to 
# the FLO_OUT
# the 5s version works with the added scenario dimension to the databases



library(reshape2)
library(ggplot2)
library(scatterplot3d)
library(gdxrrw)
library(dplyr)

#FUNCTIONS
getColourGroup <- function(path,df1,id){
  #returns a vector with the associated colour coding from the colour codes csv file
  #path is the path to the CSV file and must include name of the CSV file
  #df1 is the data frame that we want to get the unique item names from the CSV file from.
  # id is the name of the column in df1 which we want to compare with our colour tables. 
  print('getting colour set')
  colourTable <- read.csv(path,header =T)
  
  colourGroup <- as.vector(df1[id])
  
  colourGroup<- unique(df1[id])
  
  #append the unique list with the associated colour code
  colourGroup <- merge(colourGroup,colourTable, by.x = id, by.y = "Item",all.x = T,sort = F, incompatible = "#000000")
  
  rownames(colourGroup) <- colourGroup[,id] #make row names the name of technologies
  colourGroup <- colourGroup['ColourCode'] # remove the now redundant extra column
  #transpose
  mycolours <- t(colourGroup)
  mycolours <- as.vector(mycolours) # make back into vector
  names(mycolours) <- rownames(colourGroup) # it appears that R is finickey with this
  print('done gettign colour set')
  return(mycolours)
}


getPrcMap <- function(sector){
  #this returns the mapping df you need to map names
  #it needs mapPRC
  # sector is the sector you are looking at, eg 'Power'. can be a vector of sectors
  
  df = subset(mapPRC,mapPRC$Sector %in% sector)
  
  df = df[,(names(df)%in%c('Process','Technology','Sector'))]
  return(df)
} 

# Initiate GAMS library access
GAMS_lib_dir <- 'C:/GAMS/win64/24.1' # location of your GAMS main directory. 
igdx(GAMS_lib_dir) # connect to the GAMS library.

# declare directories *************************

workDir <- 'C:/Users/01425453/Google Drive/Work/SANEDI DSM/R codes/Transport results' 
resultsDir <- paste(workDir,"/Charts/",sep ='')
#resultsDir = 'C:/Users/01425453/Google Drive/Work/SANEDI DSM/R codes'

# location of gdx Database
DBname <- 'tra_results_withTSdetail2.gdx'  # this one is the one with all the data we are going to process
DBnameT <- 'RUN1_ALLBIO.gdx'# this one is going to be used to fetch the tstable (G-yrfr) 

DBPath <- paste(workDir,DBname,sep ="/")
DBPathT <- paste('C:/AnswerTIMESv6/Gams_WrkTI-MC/Gamssave/',DBnameT,sep = '') # to get the tsTable

#get mapping table from gdx 
mapPRC = rgdx.set(DBPath,'mPRC')
names(mapPRC) = c('Process','Sector','Subsector','Technology','Process2')

#get the dispatch ordering of technologies 
disorder <- as.data.frame(read.csv(paste(workDir,"ColourCoding.csv",sep ="/"),header = T))
disorder <- disorder[!(is.na(disorder$Dispatch)),]
disorder <- disorder[,-2] #drop colour code column
names(disorder) <- c('Technology','Dispatch')


tsHours <- as.data.frame(read.csv(paste(workDir,"TShours.csv",sep ="/"),header = T))

# *************************


#USER VARIABLES

myyear <- 2050 # year we want to look at
myseas <- c('S2') #The Season we want to look at
myscen = 'S1' # the scenario we want to look at

#THE DISPATCH CHARTING CODE
#######


#read in data
  FOUT <-rgdx.param(DBPath,'FLO_OUT_S')
  names(FOUT)<- c('Year','Process','Output','TimeSlice','Scenario','Value')
  
  FIN <-rgdx.param(DBPath,'FLO_IN_S')
  names(FIN)<- c('Year','Process','Input','TimeSlice','Scenario','Value')
  
  CAP <- rgdx.param(DBPath,'CAPL2')
  names(CAP)<- c('Year','Sector','Technology','Scenario','Cap')
  
  #get the timeslices and their fractions:
  tsTable <- rgdx.param(DBPathT,'G_YRFR')
  tsTable <- tsTable[,c(2,3)]
  names(tsTable) <- c('TimeSlice','Fraction')
  
  # make a column for each ts id
  tsTable$B <- substr(as.character(tsTable$TimeSlice),5,6)
  tsTable$S <- substr(as.character(tsTable$TimeSlice),1,2)
  tsTable$D <- substr(as.character(tsTable$TimeSlice),3,4)
  
  # GET POWER DATA ONLY
  pwrData <- subset(FOUT,FOUT$Output == 'ELCC') #this is energy. 
  pwrData <- pwrData[,!(names(pwrData) %in% c('Output'))] #remove the column with ELCC in it
  
  capData <- subset(CAP,CAP$Season == 'S1')
  capData <- capData[,-4]
  capData <- subset(capData,capData$Sector == 'Power')
  capData <- capData[,-2]
  
  #this is transmission into subsectors:
  xtrandata <- subset(FIN,substr(as.character(FIN$Process),1,1) == 'X') 
  xtrandata <- xtrandata[grepl("*ELC",xtrandata$Output),]

#add col with length of TS so we can pick out those which are not defined for each ts
pwrData$tslen <- nchar(as.character(pwrData$TimeSlice)) 

#convert energy to power ****************************
  
  #add in the timeslice fraction for the associated timeslice id
  pwrData$tsfr <-tsTable[match(pwrData$TimeSlice, tsTable$TimeSlice),2] 
  
  #add a power column with the calculation
  pwrData$Value <- pwrData$Value*277.78/(8760*pwrData$tsfr) # this is GW

#       ****************************

# Extract the year we want to look at and ********************
# extend all data entries for processes into the full ts detail:
  
  dat6 <- subset(pwrData,pwrData$Year == myyear & pwrData$tslen == 6)
  dat6 <- dat6[,((names(dat6) %in% c('Process','TimeSlice','Value')))]#drop the original names column)]
  
  dat <- subset(pwrData,pwrData$Year == myyear & pwrData$tslen != 6)
  dat <- dat[,-1] #drop the year since it is for a single year given by user
  
  dat$B <- substr(as.character(dat$TimeSlice),5,6)
  dat$S <- substr(as.character(dat$TimeSlice),1,2)
  dat$D <- substr(as.character(dat$TimeSlice),3,4)
  
  tmp <- merge(dat,tsTable,by = 'S') #merge 
  colnames <- c('Process','TimeSlice.y','Value')
  tmp <- tmp[,((names(tmp) %in% colnames))]#drop the original names column)]
  tmp <- subset(tmp,nchar(as.character(tmp$TimeSlice.y)) == 6 ) # remove all the rows not with tslen =6
  names(tmp) <- c('Process','Value','TimeSlice') #basically renaming timeslice.y to timeslice
  
  #Add the two datasets together
  fullpwr <- rbind(dat6,tmp)

# Add name mapping to pwr dataframe *******************************
  
  #the mapping
  mappwr = getPrcMap('Power')
  
  mappwr <- merge(mappwr,disorder)
  
  fullpwr <- merge(fullpwr,mappwr, by = 'Process') #insert a column which has the new name for each process into the main df. 
  #fullpwr <- pwrData[,-2]#remove the first column 'TechCode'
  
  fullpwr <- fullpwr[,!(names(fullpwr) %in% c('Process'))]#drop the original names column 'Process'
  
  #now to sum all the common named technologies: 
  fullpwr <- fullpwr %>% 
    group_by(Technology,TimeSlice,Dispatch) %>% 
    summarise(GW = sum(Value))
  
  fullpwr2 <- merge(tsHours,fullpwr) #put together the two dataframes - adding in hours to the data for each ts

#         ****************
  
  # Capacity factor data      **************************
  capData <- subset(capData,capData$Year == myyear)
  capData <- capData[,-1]
  
  CFdata <- merge(capData,fullpwr, by = c('Technology'))
  CFdata$cf <- CFdata$GW/CFdata$Cap
  #add hourly detail
  CFdata <- merge(tsHours,CFdata)

#         ****************


#MAKE THE DISPATCH CHART OF CAPACITY
  
  plotdat <- fullpwr2
  picname <- paste('Power_dispatch',paste(myyear,paste(myseas,'.png',sep = ''),sep ='_'),sep= '_')
  plotdat <- subset(plotdat,substr(as.character(plotdat$TimeSlice),1,2) %in% myseas) #get subset of only this season(s)
  
  #order the df by output (largest first)
  plotdat <- plotdat[with(plotdat,order(Dispatch)),]
  
  p1 <- ggplot(plotdat,aes(x = Hour, y = GW, fill = Technology)) +geom_bar(stat = 'identity')+
    scale_fill_manual(values = getColourGroup(paste(workDir,"ColourCoding.csv",sep ="/"),plotdat, 'Technology'))+
    ggtitle(paste(myyear,myseas,sep ='-')) + ylab('Power')
  
  #Write this image to file:
  setwd(resultsDir)
  png(picname)
  print(p1)
  dev.off()
  setwd(workDir)

#MAKE THE DISPATCH CHART OF CAPACITY FACTORS
  
  plotdat <- CFdata
  picname <- paste("Capacity_factor",paste(myyear,paste(myseas,'.png',sep = ''),sep ='_'),sep = "_")
  plotdat <- subset(plotdat,substr(as.character(plotdat$TimeSlice),1,2) %in% myseas) #get subset of only this season(s)
  
  #order the df by output (largest first)
  plotdat <- plotdat[with(plotdat,order(Dispatch)),]
  
  p1 <- ggplot(plotdat,aes(x = Hour, y = cf, group = Technology, colour = Technology)) +geom_line(size = 1)+
    ggtitle(paste(myyear,myseas,sep ='-')) + ylab('CF')+
    scale_colour_manual(values = getColourGroup(paste(workDir,"ColourCoding.csv",sep ="/"),plotdat, 'Technology'))
  
  #Write this image to file:
  setwd(resultsDir)
  png(picname)
  print(p1)
  dev.off()
  setwd(workDir)

# Making demand side dispatch of electricity by sector ******************
  
  #get the processes which use electricity
  resdirtmp <- resultsDir #store original path to reassign later
  resultsDir <- paste(resultsDir,'/Sectors/',sep ='')
    
    #get all processes that use ELC
    elecprocs <- FIN[grepl('*ELC*',FIN$Input),]
    
    # get the mapping of processes to end use for sectors of interest
    mapDmd = getPrcMap(c('Commerce','Residential','Industry'))
    names(mapDmd)[3] = 'EndUse'
    
    #get dispatch ordering for the sectors
    disorder2 <-disorder
    names(disorder2) <- c('Sector','Dispatch') # so we can merge by column
    mapDmd <- merge(mapDmd,disorder2)
    
    #put these together
    distElc <- merge(elecprocs,mapDmd,by ='Process')
    distElc <- distElc[,!(names(distElc)%in%c('Process','Input'))] #drop these two columns
    
    distElc <- distElc %>% 
      group_by(Year,Sector,EndUse,TimeSlice,Dispatch,Scenario) %>% 
      summarise(Value = sum(Value))
  
    #add hourly detail
    distElc <- merge(distElc,tsHours)
      
    #convert to power:
    distElc$tsfr <- tsTable[match(distElc$TimeSlice, tsTable$TimeSlice),2]# add in the ts fractions
    distElc$Value <- distElc$Value*277.78/(8760*distElc$tsfr) # this is GW
    # end of demand side dispatch of elec by sector ******************
    
  plotdat <- subset(distElc,distElc$Year == myyear)
  
  picname <- paste("Sector_dispatch",paste(myyear,paste(myseas,'.png',sep = ''),sep ='_'),sep = "_")
  
  plotdat <- subset(plotdat,substr(as.character(plotdat$TimeSlice),1,2) %in% myseas) #get subset of only this season(s)
  plotdat = subset(plotdat,plotdat$Scenario == myscen)
  #order the df by output (largest first)
  plotdat <- plotdat[with(plotdat,order(Dispatch)),]
  
  p1 <- ggplot(plotdat,aes(x = Hour, y = Value, fill = Sector)) +geom_bar(stat = 'identity')+
    ggtitle(paste(myyear,myseas,sep ='-')) + ylab('Power')+
    scale_fill_manual(values = getColourGroup(paste(workDir,"ColourCoding.csv",sep ="/"),plotdat, 'Sector'))
  
  #Write this image to file:
  setwd(resultsDir)
  png(picname)
  print(p1)
  dev.off()
  setwd(workDir)
  
# End of making sector dispatch chart ************************
  
  
# make charts of by sector and end use *************
  unqSects <- unique(plotdat$Sector)
  print('looping over each sector and printing a chart for end use dispatch')
  dat <- plotdat #going to be using the dataset from above workings
  
  for (s in seq(1,length(unqSects)))
  {#loop over each sector and make a chart for its dispatch
    sect <- unqSects[s]
    
    picname <- paste(paste("Sector_",sect,sep = ''),paste(myyear,paste(myseas,'.png',sep = ''),sep ='_'),sep = "_")
    
    plotdat <- subset(dat,dat$Sector == sect)
    plotdat <- plotdat[with(plotdat,order(EndUse)),]

    
    p1 <- ggplot(plotdat,aes(x = Hour, y = Value, group = EndUse, colour = EndUse)) +geom_line(size = 2)+
      ggtitle(paste(sect,paste(myyear,myseas,sep ='-'),sep="_")) + ylab('Power')+
      scale_colour_manual(values = getColourGroup(paste(workDir,"ColourCoding.csv",sep ="/"),plotdat, 'EndUse'))
    
    #Write this image to file:
    setwd(resultsDir)
    png(picname)
    print(p1)
    dev.off()
    setwd(workDir)
  }
  
  resultsDir <- resdirtmp #putting the name back
  
# end of sector end use charting ***************

  
#END OF DISPATCH CHARTING CODE
