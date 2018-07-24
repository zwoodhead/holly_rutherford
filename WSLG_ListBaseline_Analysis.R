#####################################################
## Script to subtract LG baseline task from SG / WG
## Before calculating LI value at the subject level
#####################################################
## July 2018

#####################################################
## Getting Started

require(dplyr)      # For first function
library("reshape2") # For melt function
library("yarrr")    # For pirateplot function

dir <- "C:/Users/zwoodhead/Dropbox/Project A2/Doppler_Tasks/WG_SG_LG/"

# Set parameters
premarker=-7                          # baseline start
samplingrate=25                       # number of samples per second
poistart=7                            # period of interest start
poiend=17                             # period of interest end
poistartpoints=poistart*samplingrate
poiendpoints=poiend*samplingrate
baseoffset=-premarker*samplingrate
rangestart=baseoffset+poistartpoints
rangeend=baseoffset+poiendpoints
nsubj <- 31                           # number of subjects to analyse
ncond <- 2                            # number of conditions (Sentences and Words)

myLI= matrix(1,nsubj,ncond)            # create empty matrices to fill with results
mylatency=matrix(1,nsubj,ncond)
colnames(myLI) <- c('SG_v_LG', 'WG_v_LG')
colnames(mylatency) <- c('SG_v_LG', 'WG_v_LG')


#####################################################
## Loop through subjects
subnums <- c(1:24, 26:32)
nsub <- length(subnums)
for (s in 1:nsub)
{ mysubj <- subnums[s]
  mysubjname <- paste0(0,mysubj)
  
  if (as.numeric(mysubj) < 10){
    mysubjname <- paste0('00',mysubj)
  }
  SG_data <- read.csv(paste0(dir,'Rawmeans_',mysubjname,'_SentGen.csv'))  # Read in data from csv files
  WG_data <- read.csv(paste0(dir,'Rawmeans_',mysubjname,'_WordGen.csv'))
  LG_data <- read.csv(paste0(dir,'Rawmeans_',mysubjname,'_ListGen.csv'))
  
  
  #####################################################
  ## Loop through conditions (Sentences and Words)
  
  for(c in 1:ncond)
  
    # Create difference waves, e.g. SG minus LG  
  {if(c==1)
    {LRdiff = SG_data$meanDiff - LG_data$meanDiff}    # Subtract SG-LG data
    if(c==2)
    {LRdiff = WG_data$meanDiff - LG_data$meanDiff}  # Subtract WG-LG data
    
    # Identify peak of difference wave
    mymax=max(LRdiff[rangestart:rangeend])  # Highest difference between task and baseline
    mymin=min(LRdiff[rangestart:rangeend])  # Smallest difference between task and baseline
    mylatpeak=mymax
    myside=1
    
    if(-mymin>mymax)
    {myside=-1 #R biased LI
    mylatpeak=mymin
    } #R peak > L peak
    
    # Calculate LI
    mytimepeak=first(which(LRdiff==mylatpeak))
    mylatency[s,c]=(mytimepeak-baseoffset)/samplingrate  # record latency of peak
    mypeakrange=seq(mytimepeak-25,mytimepeak+25)              # take range of 25 samples before and after the peak
    myLI[s,c]=as.numeric(format(mean(LRdiff[mypeakrange]),digits=3)) # LI is average within this range around the peak
    
  } # finish loop through conditions (c)
  
} # finish loop through participants (mysubj)


#####################################################
## Save data

results <- cbind(myLI, mylatency)
colnames(results) <- c('LI_SGvLG','LI_WGvLG','Latency_SGvLG','Latency_WGvLG')
write.csv(results,file='WSLG_ListBaseline_Data.csv')



# #####################################################
# ## Make pirate plot
# 
# LI_data <- data.frame('ID' = as.factor(matrix(1:nsubj)), # melt needs to know what the factors are
#                       'SG_v_LG' = myLI[,1],
#                       'WG_v_LG' = myLI[,2])
# 
# LI_data_long <- melt(LI_data)         # pirateplot uses data in long format
# colnames(LI_data_long) <- c("ID","Contrast","LI_value")
# 
# pirateplot(formula = LI_value ~ Contrast,
#            data = LI_data_long,
#            xlab = "Contrast",
#            ylab = "LI Value",
#            main = "Pirate Plot of LI Values")

