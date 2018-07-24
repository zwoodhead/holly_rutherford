#####################################################
## Script to subtract LG baseline task from SG / WG
## Before calculating LI value at the subject level
#####################################################
## 09/10/2016

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
conds <- c('WordGen','SentGen','ListGen')
ncond <- length(conds)                # number of conditions (Sentences and Words)

myLI= matrix(1,nsubj,ncond)            # create empty matrices to fill with results
colnames(myLI) <- conds



#####################################################
## Loop through subjects
subnums <- c(1:24, 26:32)
nsub <- length(subnums)
for (s in 1:nsub)
{mysubj <- subnums[s]

  mysubjname <- paste0(0,mysubj)
  if (as.numeric(mysubj) < 10){
    mysubjname <- paste0('00',mysubj)
  }

  #####################################################
  ## Loop through conditions (Sentences and Words)
  
  for(cond in 1:ncond)
  {
    # Read in data
    mydata <- read.csv(paste0(dir,'Rawmeans_',mysubjname,'_',conds[cond],'.csv'))

    # Calculate mean L-R diff within POI
  
    myLI[s,cond] <- mean(mydata$meanDiff[rangestart:rangeend])
  } 
}
    
 

#####################################################
## Save data

write.csv(myLI,file='WSLG_POImean_Data.csv')



# #####################################################
# ## Make pirate plot

LI_data <- data.frame('ID' = as.factor(matrix(1:nsubj)), # melt needs to know what the factors are
                      'WG' = myLI[,1],
                      'SG' = myLI[,2],
                      'LG' = myLI[,3])

LI_data_long <- melt(LI_data)         # pirateplot uses data in long format
colnames(LI_data_long) <- c("ID","Contrast","LI_value")

pirateplot(formula = LI_value ~ Contrast,
           data = LI_data_long,
           xlab = "Contrast",
           ylab = "LI Value",
           main = "Pirate Plot of LI Values")

