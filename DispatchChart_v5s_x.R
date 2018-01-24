# 22 March 2016
# Bryce Mccall
# 
# Dispatch charting 
# version 5 works with the slightly modified GAMS output gdx's which now have timeslice detail added to 
# the FLO_OUT
# 
#


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
resultsDir <- 'C:/Users/01425453/Google Drive/SATIM/SANEDI Projects/R code and Model outputs/Results'
resultsSect = paste(resultsDir,'/Demand Sectors/',sep ='')
resultsPwr = paste(resultsDir,'/Power/',sep ='')

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

myyear <- 2020 # year we want to look at
myseas <- c('S2') #The Season we want to look at
myscen = 'S1' # the scenario we want to look at

title_add = paste(myyear,myseas,myscen,sep = '_')

#THE DISPATCH CHARTING CODE
#######


#read in data  ***********

  FOUT <-rgdx.param(DBPath,'FLO_OUT_S')
  names(FOUT)<- c('Year','Process','Output','TimeSlice','Scenario','Value')
  
  FIN <-rgdx.param(DBPath,'FLO_IN_S')
  names(FIN)<- c('Year','Process','Input','TimeSlice','Scenario','Value')
  
  #get the timeslices and their fractions:
  tsTable <- rgdx.param(DBPathT,'G_YRFR')
  tsTable <- tsTable[,c(2,3)]
  names(tsTable) <- c('TimeSlice','Fraction')
    # make a column for each ts id
    tsTable$B <- substr(as.character(tsTable$TimeSlice),5,6)
    tsTable$S <- substr(as.character(tsTable$TimeSlice),1,2)
    tsTable$D <- substr(as.character(tsTable$TimeSlice),3,4)
  
#ENd of reading in data ***********
    
  
#get the scenario we want to look at
FOUT = subset(FOUT,FOUT$Scenario == myscen)
FIN = subset(FIN,FIN$Scenario == myscen)

  #EXTRACT FROM THE DATAFRAMES:
    
    # GET POWER DATA
    pwrData <- subset(FOUT,FOUT$Output == 'ELCC') #this is energy. 
    pwrData <- pwrData[,!(names(pwrData) %in% c('Output'))] #remove the column with ELCC in it
    
    #add col with length of TS so we can pick out those which are not defined for each ts
    pwrData$tslen <- nchar(as.character(pwrData$TimeSlice)) 
    
    #convert energy to power ****
      
      #add in the timeslice fraction for the associated timeslice id
      pwrData$tsfr <-tsTable[match(pwrData$TimeSlice, tsTable$TimeSlice),2] 
      
      #add a power column with the calculation
      pwrData$Value <- pwrData$Value*277.78/(8760*pwrData$tsfr) # this is GW
    
    #       ****

# Extract the year we want to look at and ********************
# extend all data entries for processes into the full ts detail:
  
  pwr_s6 <- subset(pwrData,pwrData$Year == myyear & pwrData$tslen == 6)
  pwr_s6 <- pwr_s6[,((names(pwr_s6) %in% c('Process','TimeSlice','Value')))]#drop the original names column)]
  
  pwr_s2 <- subset(pwrData,pwrData$Year == myyear & pwrData$tslen != 6)
  pwr_s2 <- pwr_s2[,-1] #drop the year since it is for a single year given by user
  
  pwr_s2$B <- substr(as.character(pwr_s2$TimeSlice),5,6)
  pwr_s2$S <- substr(as.character(pwr_s2$TimeSlice),1,2)
  pwr_s2$D <- substr(as.character(pwr_s2$TimeSlice),3,4)
  
  tmp <- merge(pwr_s2,tsTable,by = 'S') #add ts details to the power table  
  colnames <- c('Process','TimeSlice.y','Value')
  tmp <- tmp[,((names(tmp) %in% colnames))]#drop the original names column)]
  tmp <- subset(tmp,nchar(as.character(tmp$TimeSlice.y)) == 6 ) # remove all the rows not with tslen =6
  names(tmp) <- c('Process','Value','TimeSlice') #basically renaming timeslice.y to timeslice
  
  #Add the two datasets together
  fullpwr <- rbind(pwr_s6,tmp)

  # Add name mapping to pwr dataframe 
  
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

  #MAKE THE DISPATCH CHART OF CAPACITY
  
    plotdat <- fullpwr2
    picname <- paste('Power_dispatch',paste(title_add,'.png',sep = ''),sep ='_')
    chart_title = title_add
    
    plotdat <- subset(plotdat,substr(as.character(plotdat$TimeSlice),1,2) %in% myseas) #get subset of only this season(s)
    
    #order the df by output (largest first)
    plotdat <- plotdat[with(plotdat,order(Dispatch)),]
    
    p1 <- ggplot(plotdat,aes(x = Hour, y = GW, fill = Technology)) +geom_bar(stat = 'identity')+
      scale_fill_manual(values = getColourGroup(paste(workDir,"ColourCoding.csv",sep ="/"),plotdat, 'Technology'))+
      ggtitle(chart_title) + ylab('GW')
    
    #Write this image to file:
    setwd(resultsPwr)
    png(picname)
    print(p1)
    dev.off()
    setwd(workDir)

#         ****************

        
# Make chart of sector demand profiles (Shows res+com+ind) ******************
  
  #get all those processes that use ELC for something
  elc_prcs = FIN[grepl('*ELC*',FIN$Input),]
  # get the year we want to look at
  elc_prcs = elc_prcs[elc_prcs$Year == myyear,]
  elc_prcs = elc_prcs[,-1] #drop the year column
  
  #extract the subsectors we want to look at and add mapping and other data
    # get the mapping of processes to end use for sectors of interest
    mapDmd = getPrcMap(c('Commerce','Residential','Industry'))
    names(mapDmd)[3] = 'EndUse' #only doing this since its demand sectors we looking at
    
    #get dispatch ordering for the sectors
    disorder2 <-disorder
    names(disorder2) <- c('Sector','Dispatch') # so we can merge by column
    mapDmd <- merge(mapDmd,disorder2)
    
  # add dispatch ordering to df - this also removes all those that aren't in the sector list in mapDmd
  elc_prcs= merge(elc_prcs,mapDmd, by = 'Process')
  
  
  #add tshours
  elc_prcs = merge(elc_prcs,tsHours)
  
  #convert to power:
  elc_prcs$tsfr <- tsTable[match(elc_prcs$TimeSlice, tsTable$TimeSlice),2]# add in the ts fractions
  elc_prcs$Value <- elc_prcs$Value*277.78/(8760*elc_prcs$tsfr) # this is GW
  
  
  # end of demand side dispatch of elec by sector ******************
  
  picname <- paste("Sector_dispatch",paste(title_add,'.png',sep = ''),sep ='_')
  chart_title = title_add
  
  plotdat = elc_prcs
  
  plotdat <- subset(plotdat,substr(as.character(plotdat$TimeSlice),1,2) %in% myseas) #get subset of only this season(s)
  
  #order the df by output (largest first)
  plotdat <- plotdat[with(plotdat,order(Dispatch)),]
  
  p1 <- ggplot(plotdat,aes(x = Hour, y = Value, fill = Sector)) +geom_bar(stat = 'identity')+
    ggtitle(chart_title) + ylab('GW')+
    scale_fill_manual(values = getColourGroup(paste(workDir,"ColourCoding.csv",sep ="/"),plotdat, 'Sector'))
  
  #Write this image to file:
  setwd(resultsSect)
  png(picname)
  print(p1)
  dev.off()
  setwd(workDir)
  
# End of making sector dispatch chart ************************
  
  
# make charts of by sector and end use *************
  
  dat <- subset(elc_prcs,substr(as.character(elc_prcs$TimeSlice),1,2) %in% myseas) #get subset of only this season(s)
  
  unqSects <- unique(dat$Sector)
  
  
  for (s in seq(1,length(unqSects)))
  {#loop over each sector and make a chart for its dispatch
    sect <- unqSects[s]
    
    picname <- paste(paste("Sector_",sect,sep = ''),paste(title_add,'.png',sep = ''),sep ='_')
    chart_title = title_add
    
    plotdat <- subset(dat,dat$Sector == sect)
    plotdat <- plotdat[with(plotdat,order(EndUse)),]
    
    #sum over all enduses (aggregating new/existing com or L,M,H income residential)
    plotdat = plotdat %>% 
      group_by(EndUse,TimeSlice,Dispatch,Hour) %>% 
      summarise(Value = sum(Value))
    
    p1 <- ggplot(plotdat,aes(x = Hour, y = Value, group = EndUse, colour = EndUse)) +geom_line(size = 1.2)+
      ggtitle(chart_title) + ylab('GW')+
      scale_colour_manual(values = getColourGroup(paste(workDir,"ColourCoding.csv",sep ="/"),plotdat, 'EndUse'))
    
    #Write this image to file:
    setwd(resultsSect)
    png(picname)
    print(p1)
    dev.off()
    setwd(workDir)
  }
  
# end of sector end use charting ***************

  
#END OF DISPATCH CHARTING CODE
