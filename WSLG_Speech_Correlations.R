#####################################################
## Script to test correlations between LI values and
## number of words produced in CANDICE A2a (Holly)
#####################################################
## 23/07/18

#####################################################
## Getting Started

mydir <- 'C:/Users/zwoodhead/Dropbox/Project A2/Doppler_Tasks/WG_SG_LG/'
mydata <- read.csv(paste0(mydir,'WSLG_AnalysisData.csv'))
moredata <- read.csv(paste0(mydir,'WSLG_Participant_Info.csv'))

alldata <- cbind(mydata$LI_List, mydata$LI_Sent, mydata$LI_Word, 
                 moredata$LG_words, moredata$SG_words, moredata$WG_words)
colnames(alldata) <- c('LI_List', 'LI_Sent', 'LI_Word', 
                       'Speech_List', 'Speech_Sent', 'Speech_List')

cor.test(alldata[,1:3], alldata[,4:6], method='spearman')

collapsedata <- cbind(
  c(alldata[,1], alldata[,2], alldata[,3]),
  c(alldata[,4], alldata[,5], alldata[,6]))

colnames(collapsedata) <- c('LI', 'Speech')

cor(collapsedata[,1], collapsedata[,2], method='spearman')