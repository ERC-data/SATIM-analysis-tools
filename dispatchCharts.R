#this script gets electricity profiles for sectors and charts them

library(reshape2)
library(ggplot2)
library(scatterplot3d)
library(gdxrrw)
library(dplyr)

getPrcMap <- function(sector){
  #this returns the mapping df you need to map names
  #it needs mapPRC
  # sector is the sector you are looking at, eg 'Power'. can be a vector of sectors
  
  df = subset(mapPRC,mapPRC$Sector %in% sector)
  
  df = df[,(names(df)%in%c('Process','Technology','Sector'))]
  return(df)
} 

# declare directories *************************

localDir <- 'C:/Users/01425453/Documents/R/dispatchProfilesAnalyses/' 
resultsDir <- paste(localDir,'OutputCharts/',sep = '')

resultsSect = paste(resultsDir,'/Demand Sectors/',sep ='')
resultsPwr = paste(resultsDir,'/Power/',sep ='')

# location of gdx Database
DBname <- 'tra_results_withTSdetail2.gdx'  # this one is the one with all the data we are going to process
DBPath <- paste(localDir,DBname,sep ="/")

#get the dispatch ordering of technologies 
disorder <- as.data.frame(read.csv(paste(resourcesPath,"ColourCoding.csv",sep ="/"),header = T))
disorder <- disorder[!(is.na(disorder$Dispatch)),]
disorder <- disorder[,-2] #drop colour code column
names(disorder) <- c('Technology','Dispatch')

tsHours <- as.data.frame(read.csv(paste(localDir,"TShours.csv",sep ="/"),header = T))

# *************************


#USER VARIABLES

myyear <- 2050 # year we want to look at
myseas <- c('S2') #The Season we want to look at
myscen = 'S1' # the scenario we want to look at

title_add = paste(myyear,myseas,sep = '_')

#THE DISPATCH CHARTING CODE
#######

#MAKE THE DISPATCH CHART OF CAPACITY
    
    fullpwr = FOUT[FOUT$Year == myyear&FOUT$Sector == 'Power'&grepl('^ELC',FOUT$Commodity)&substr(as.character(FOUT$Timeslice),1,2) %in% myseas,]
    fullpwr = makeHourlyDetail(fullpwr)
    fullpwr = fullpwr %>% group_by(Subsector,hour)%>%summarise(GW = mean(GW))
      
    plotdat = fullpwr
      
      picname <- paste('Power_dispatch',paste(title_add,'.png',sep = ''),sep ='_')
     
      #order the df by output (largest first)
      plotdat = merge(plotdat,disorder,by.x = 'Subsector',by.y = 'Technology',all.x = T)
      plotdat <- plotdat[with(plotdat,order(Dispatch)),]
      
      p1 <- ggplot(plotdat,aes(x = hour, y = GW, fill = Subsector)) +geom_bar(stat = 'identity',width =1)+
        scale_fill_manual(values = getColourGroup(paste(resourcesPath,"ColourCoding.csv",sep ="/"),plotdat, 'Subsector'))+
        ggtitle(title_add) + ylab('GW')
      
      #Write this image to file:
      setwd(resultsPwr)
      png(picname)
      print(p1)
      dev.off()
      setwd(localDir)
      
  tmp = FOUT[FOUT$Commodity =='ELCC'&FOUT$Year == myyear&(as.character(substr(FOUT$Timeslice,1,2))==myseas),]
  tmp = makeHourlyDetail(tmp)
  
  tmp = FOUT[FOUT$Process == 'ETRANS'&FOUT$Year == myyear&(as.character(substr(FOUT$Timeslice,1,2))==myseas),]
  tmp = makeHourlyDetail(tmp)
  
  
#         ****************

        
# Make chart of sector demand profiles (Shows res+com+ind ect) ******************
  
  #get all those processes that use ELC for something
  elcs = 'RESELC|INDELC|COMELC|TRAELC|UPSELC|AGRELC'
  elc_prcs = FOUT[grepl('^X.*ELC',FOUT$Process)&(FOUT$Commodity != 'INDELC'),]
  elc_prcs = addPRCmap(elc_prcs)
  elc_prcs$Tech.Description = gsub( " .*$", "",elc_prcs$Tech.Description)
  elc_prcs$Sector = elc_prcs$Tech.Description
  elc_prcs = elc_prcs %>%group_by(Year,Sector,Timeslice)%>%summarise(F_OUT = sum(F_OUT))# group together
  elc_prcs = makeHourlyDetail(elc_prcs)
  
  # end of demand side dispatch of elec by sector ******************
  
  picname <- paste("SectorDemand_profile",paste(title_add,'.png',sep = ''),sep ='_')
  chart_title = paste(title_add,' flow out of X...ELCs')
  
  plotdat = elc_prcs
  
  plotdat = plotdat[plotdat$Year == myyear&substr(as.character(plotdat$Timeslice),1,2)%in% myseas,]
  #order the df by output (largest first)
  #plotdat <- plotdat[with(plotdat,order(Dispatch)),]
  
  p1 <- ggplot(plotdat,aes(x = hour, y = GW, fill = Sector)) +geom_bar(stat = 'identity',width = 1)+
    ggtitle(chart_title) + ylab('GW')+
    scale_fill_manual(values = getColourGroup(paste(resourcesPath,"ColourCoding.csv",sep ="/"),plotdat, 'Sector'))
  
  #Write this image to file:
  setwd(resultsSect)
  png(picname)
  print(p1)
  dev.off()
  setwd(localDir)
  
# End of making sector dispatch chart ************************
  
  
# make charts of sector and end use *************
  elc_prcs = FIN[grepl('RESELC|COMELC',FIN$Commodity),]
  
  dat = subset(elc_prcs,substr(as.character(elc_prcs$Timeslice),1,2) %in% myseas) #get subset of only this season(s)
  dat = makeHourlyDetail(dat)
  tmp = read.csv(paste(resourcesPath,'ColourCoding.csv',sep ='/'))
  tmp = tmp[,-2]
  dat = merge(dat,tmp,all.x = T,by.x = 'Subsubsector',by.y = 'Item')
  unqSects <- unique(dat$Sector)
  tmpcheck = data.frame()
  
  for (s in seq(1,length(unqSects)))
  {#loop over each sector and make a chart for its dispatch
    sect <- unqSects[s]
    
    picname <- paste(paste(sect,"_Sector",sep = ''),paste(title_add,'.png',sep = ''),sep ='_')
    chart_title = title_add
    
    plotdat <- subset(dat,dat$Sector == sect)
    plotdat <- plotdat[with(plotdat,order(Subsubsector)),]
    
    #sum over all enduses (aggregating new/existing com or L,M,H income residential)
    plotdat = plotdat %>% 
      group_by(Subsubsector,Timeslice,Dispatch,hour) %>% 
      summarise(GW = sum(GW))
    
    p1 <- ggplot(plotdat,aes(x = hour, y = GW, group = Subsubsector, colour = Subsubsector)) +geom_line(size = 1.2)+
      ggtitle(chart_title) + ylab('GW')+
      scale_colour_manual(values = getColourGroup(paste(resourcesPath,"ColourCoding.csv",sep ="/"),plotdat, 'Subsubsector'))
    
    #Write this image to file:
    setwd(resultsSect)
    png(picname)
    print(p1)
    dev.off()
    setwd(localDir)
    tmp =plotdat[plotdat$Hour == 1,]
    tmpcheck = append(tmpcheck,max(tmp$GW))
    
  }
