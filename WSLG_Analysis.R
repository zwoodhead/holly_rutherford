##R_doppler_analyse
##SENTENCE GENERATION
# This version was adapted on 22/06/2017 by Alex Wilson for analysis of the Sentence Generation Task. 

# NOTES FOR USER 
# 1. This program waits for user input, so runs best if run from source using Ctrl+Shift+S.
# 2. Specify your directory on line 37.
# 3. The script reads in an excel spreadsheet that lists participants, and indicates
#    trials to be excluded because of a procedural error, e.g. talking during rest.
#    Code each trial as 1 for include, 0 for exclude and 9 for not completed 
#    (i.e. because experiment finished early). Laterality statistics for each condition 
#    are written to the same document at the end of the script. 
#    The spreadsheet is specified on line 94, and needs to be present in the directory.
# 4. Select the subject you wish to analyse on line 54. Indicate the row they
#    occupy in the spreadsheet. Ignore the title row, so count the first subject as row one.
# 5. Note that subject files should be saved in the form 000.1.exp, where 000 is subject
#    ID and 1 (or 2) is run number.
# 6. A second excel document provides the order of conditions. Specified on line 89.
# 7. Two doppler files are read in for each participant, one from each run.
#    Do not merge files because the script will do that for you. There may be practice
#    trials in each run. The script looks for them and, if present, drops them from analysis.
# 6. The program is not set up to deal with anomalous data, but will write comments to 
#    the spreadsheet if, for instance, intervals between markers are too short, or 
#    there are more markers than expected. These issues can then be investigated.
# 7. You can view plots at different stages in the analysis by altering the controls
#    on lines 66-71. If initialdatacheck (line 67) is set to 1, the user can view the raw 
#    data for each epoch and enter keyboard commands to manually exclude trials
#    retained by the script, or manually re-include ones rejected unnecessarily.
#-------------------------------------------------------------------------------------------

library(xlsx) #install.packages(c("xlsx","dplyr"))
require(dplyr)

#------------------------------------------------------
# Specify directory. CHANGE TO YOUR LOCAL DATA DIRECTORY
#------------------------------------------------------
dir<-("C:/Users/zwoodhead/Dropbox/Project A2/Doppler_Tasks/WG_SG_LG/")


filename<-'WSLG_AnalysisData.csv' # File lists all subjects and trial inclusions/exclusions
fileloc<-paste(dir,filename,sep='')
filelist<-read.csv(fileloc) # reads file
filelist$Filename<-as.character(filelist$Filename) # unfactor these columns to avoid later difficulties
filelist$Comment<-as.character(filelist$Comment) 

#------------------------------------------------------
# Set task parameters 
#------------------------------------------------------

# Description of Paradigm. There are three tasks: sentence generation (describing a picture
# stimulus), word generation (letter-cued verbal fluency), and list generation (of numbers 1-10).
# There are 60 epochs (each 30s) with 20 of each task in a pseudorandomized order.
# The paradigm is split into two runs of 30 epochs each.

# Timings for Epoch. The marker occurs at the start of the Clear Mind phase.
# Participant clears their mind >> 0-3s
# Silent generation >> 3-15s
# Overt reporting of generated material >> 15-20s
# Rest >> 20-30s
## The Period of Interest is set from 7s to 17s
## The baseline is set to -5s to -1s (i.e. just before the marker)

condition_list <- c('SentGen','ListGen','WordGen')
conditionfile<-'WSLG_TrialConditions.csv' # file giving codes for condition
conditionloc<-paste(dir,conditionfile,sep="");
conditions<-read.csv(conditionloc);
conditions<-conditions$Condition # extract necessary info into a vector for easy use

# Timings in secs
premarker=-7 # epoch start
basestart=-5 # baseline start
baseend=0 # baseline end
poistart=7 # period of interest start
poiend=17 # period of interest end
postmarker=25 # end of epoch  

baselinecorrect=1 # correct for baseline
extremehi=140 # define values for rejecting bad epochs 
extremelo=60 # (% above/below mean of 100 in normed/corrected data)

samplingrate=25
heartratemax = 125  # Maximum heartrate that would be expected, in bpm
peakdiffmin = 60/heartratemax * samplingrate # The minumum number of samples expected between heartbeats, based on heartratemax

prepoints=premarker*samplingrate # these define the data point marking parts of the epoch
basestartpoint=basestart*samplingrate
baseendpoint=baseend*samplingrate
poistartpoints=poistart*samplingrate
poiendpoints=poiend*samplingrate
postpoints=postmarker*samplingrate

runs=2 #note: if this is altered, coding for the concatenation of data from runs will also need to be adapted
trialsperrun=30
practicerun1=6 #num practice trials in runs 1
praticerun2=3 #and 2
numconditions=3 #num conditions across paradigm



#######################################################################################
# Start Analysis
#######################################################################################

for (mysub in 1:31)  # Can do a loop through multiple subjects, e.g. 1:31
{
  #------------------------------------------------------
  # Toggle the following initialdatacheck values to view 
  # aspects of analysis. Once satisfied, can set all to 
  # zero for fast analysis. But it is recommended to 
  # inspect all trials and if need be manually override
  # automated rejection
  #------------------------------------------------------
  
  briefinspect=0; #set to 1 to see a sample of the file to check markers are there
  initialdatacheck=1; #set to 1 toview raw data for each epoch
  initialdatacheck1=0; # set to 1 to view epochs after normalisation
  initialdatacheck2=0; #set to 1 to view epochs after heartbeat Correction
  initialdatacheck3=0; # set to 1 to visualise after baseline correction
  initialdatacheck4=0; # set to 1 to plot average for each subject
  
  #-----------------------------------------------------
  # Subject-specific parameters
  #-----------------------------------------------------
  
  mysubnum<-paste(filelist[mysub,1])
  mysubname <- paste0('0',mysubnum)
  if (as.numeric(mysubnum) < 10){
    mysubname <- paste0('00',mysubnum)
  }
  
  mycomment<-filelist[mysub,62] # opens up the comment, for adding additional details throughout analysis 
  

  #--------------------------------------------------------
  # Extract data and markers for each run
  #--------------------------------------------------------
  for(r in 1:runs)
  {
    #read exp file for this run
    myname<-paste(mysubname,".",r,".exp",sep="")
    dataloc<-paste(dir,myname,sep="")
    dat<-read.table(dataloc, skip = 6,  header =FALSE, sep ='\t') #read .exp file in to table
    wantcols=c(2,3,4,9) #csec, L, R,marker #select columns of interest and put in shortdat
    
    shortdat=data.frame(dat[,wantcols])
    colnames(shortdat)=c("csec","L","R","marker")
    
    #downsample to 25 Hz by taking every 4th point
    rawdata=filter(shortdat, row_number() %% 4 == 0) 
    
    allpts=nrow(rawdata) # total N points in long file
    rawdata[,1]=(seq(from=1,to=allpts*4,by=4)-1)/100 #create 1st column which is time in seconds from start
    colnames(rawdata)=c("sec","L","R","marker")
    
    #-------------------------------------------------------
    # Brief plot of 1500 pts to check all OK; range here is arbitrary
    #-------------------------------------------------------
    if (briefinspect==1)
    {
      x=rawdata$sec[3000:5000]
      y=rawdata$L[3000:5000]
      z=rawdata$R[3000:5000]
      w=rawdata$marker[3000:5000]
      
      plot(x,y, type="n") #set up plot - doesn't actually plot anything
      lines(x,y,col="red")
      lines(x,z,col="blue")
      lines(x,w)
      cat("Press [enter] to continue")
      line <- readline()
      #This should show left (red) and right (blue) channels and some markers in black
    }
    
    #-----------------------------------------------------------
    #Now find markers; place where we go from low to high value
    #-----------------------------------------------------------
    mylen=nrow(rawdata);
    markerplus=c(0 ,rawdata$marker);
    markerchan=c(rawdata$marker,0); #create vectors with offset of one
    markersub=markerplus-markerchan;#start of marker indicated by large difference between consecutive data points
    meanmarker<-mean(rawdata$marker)
    markersize<-meanmarker+5*sd(rawdata$marker)
    origmarkerlist=which(markersub>markersize)
    norigmarkers=length(origmarkerlist)
    
    if(origmarkerlist[length(origmarkerlist)]>(mylen-postpoints))
    {myaddcomment<-paste('. Short last epoch in run',r)
    mycomment<-paste(mycomment,myaddcomment)
    } # indicates if there is a short last epoch; this may need disposing of
    
    excessmarkers=norigmarkers-trialsperrun
    # indicates if there are excess markers; hopefully these are practice trials. 
    # If the quantity does not indicate this, a comment is made in the 'Dispose of practice trials' 
    # section below. Also, check there aren't fewer than expected, and comment on this
    if (excessmarkers<0)
    {mycomment<-paste(mycomment,'. Fewer markers than expected')
    }
    
    # Check that markers are at least 29 s apart
    intervals=c(rawdata$sec[origmarkerlist],10000)-c(0,rawdata$sec[origmarkerlist])
    intervals=intervals[2:(length(intervals)-1)]
    # Ignore first and last values since these are arbitrary; other intervals should be around 30s 
    # but may be longer if recording interrupted. Shorter intervals indicate there have been spurious 
    # markers which will need dealing with
    if(min(intervals<29))
    {myaddcomment<-paste('. Possible spurious markers in run',r)
    mycomment<-paste(mycomment,myaddcomment)
    }
    
    #---------------------------------------------------------
    # Look for practice trials, and dispose of them
    # Also identify unexpected extra markers; at present the
    # script is set to drop any excess markers from beginning of file 
    #---------------------------------------------------------
    if(r==1)
    {if(excessmarkers>0)
    {if(excessmarkers==practicerun1)
    {run1markerlist<-origmarkerlist[(excessmarkers+1):(length(origmarkerlist))]
    mycomment<-paste(mycomment,'. Practice trials run 1 found and removed')
    }
      else
      {myaddcomment<-paste(excessmarkers,'. Unexpected markers found in run 1, investigate')
      mycomment<-paste(mycomment,myaddcomment)
      run1markerlist<-origmarkerlist[(excessmarkers+1):(length(origmarkerlist))] #If unexpected extra markers
      # found, drop earlier ones until maximum possible markers are retained; then continue
      }
    }
      else
      {run1markerlist<-origmarkerlist
      } 
      datRun1=dat # rename these, since they will be rewritten in the 2nd iteration of the loop
      shortdatRun1=shortdat
      rawdataRun1=rawdata
      origmarkerlistRun1=origmarkerlist
      intervalsRun1=intervals
    }
    if(r==2)
    {if(excessmarkers>0)
    {if(excessmarkers==practicerun2)
    {run2markerlist<-origmarkerlist[(excessmarkers+1):(length(origmarkerlist))]
    mycomment<-paste(mycomment,'. Practice trials run 2 found and removed')
    }
      else
      {myaddcomment<-paste(excessmarkers,'. Unexpected markers found in run 2, investigate')
      mycomment<-paste(mycomment,myaddcomment)
      run2markerlist<-origmarkerlist[(excessmarkers+1):(length(origmarkerlist))] #If unexpected extra markers
      } # found, drop earlier ones until maximum possible markers are retained; then continue
    }
      else
      {run2markerlist<-origmarkerlist
      }
      datRun2=dat
      rm(dat)
      shortdatRun2=shortdat
      rm(shortdat)
      rawdataRun2=rawdata
      origmarkerlistRun2=origmarkerlist
      intervalsRun2=intervals
    }
    
  } #end of loop extracting markers for each run
  
  #--------------------------------------------------------
  # Put together data from separate runs
  #--------------------------------------------------------
  rawdata=rbind(rawdataRun1,rawdataRun2) # concatenates the data from the two runs 
  markerlist=c(run1markerlist,run2markerlist)
  filelist[mysub,63]=length(origmarkerlistRun1)+length(origmarkerlistRun2) #adds number of original markers 
  #(i.e. including any practice markers) to table that will be saved at the end
  
  #---------------------------------------------------------
  # Make vector indicating trials to be be included/excluded
  # based on behaviour, and drop markers for trials not completed
  #---------------------------------------------------------
  myinclude=filelist[mysub,(2:(1+length(markerlist)))]
  myinclude=as.numeric(myinclude)
  #need as numeric to prevent it being a data frame, which creates problems later
  
  myremove=which(myinclude==9)#9 indicates trial not given
  if(length(myremove)>0)
  {markerlist=markerlist[-myremove]
  conditions=conditions[-myremove]
  }
  nmarkers=length(markerlist)
  
  #----------------------------------------------------------
  # Settings for initial screening to remove signal dropout
  #----------------------------------------------------------
  interpolatebad=2;#set to 1 to replace brief dropout/spiking with mean value for that channel
  #number specified here is max number of bad datapoints corrected for
  
  #-----------------------------------------------------------
  # identify extreme values; can also check each epoch visually
  #------------------------------------------------------------
  droprej=rep(0,2) ;spikerej=droprej #initialise spikerej and droprej with zero
  mymax=max(rawdata[,2:3])
  intmax=100*(1+round(mymax/100))
  
  droprej[1]=quantile(rawdata$L,.0001)
  droprej[2]=quantile(rawdata$R,.0001)
  spikerej[1]=quantile(rawdata$L,.9999)
  spikerej[2]=quantile(rawdata$R,.9999)
  
  for(i in 1:2)
  {if(droprej[i]<1)
  {droprej[i]=1 #value cannot be 0 or less! Lowest droprej value is 1
  }
  }#droprej gives lower limit of signal for L and R channels below which rejected
  
  #----------------------------------------------------------
  # epoch the accepted trials into an array
  # This has 4 dimensions; trials,points, L/R, raw/normalised/heartcorr/baselined
  #-----------------------------------------------------------
  myepoched <- array(0, dim=c(nmarkers,postpoints-prepoints+1,2,4)) # Full epochs: from premarker to postmarker
  mybit=matrix(data = NA, nrow = postpoints-prepoints, ncol = 2)  # Just take one marker at a time
  
  for(mym in 1:nmarkers) # for trials
  {index1=markerlist[mym]+prepoints
  index2=markerlist[mym]+postpoints
  myepoched[mym,,1,1]=rawdata[index1:index2,2] #L side
  myepoched[mym,,2,1]=rawdata[index1:index2,3] #R side
  
  
  for(i in 1:2) # for left and right sides
  {rejpoints<-numeric(0) #coerces vector rejpoints to zero length between iterations 
  mybit[,i]=myepoched[mym,1:(postpoints-prepoints),i,1]
  thisbit=mybit[,i]
  rejpoints=c(rejpoints,which(thisbit < droprej[i])) ; # Identifies timepoints below lower threshold
  rejpoints=c(rejpoints, which(thisbit>spikerej[i])); # Identifies timepoints above upper threshold
  rejpoints=c(rejpoints,which(is.na(thisbit))) #triggered if epoch too short
  #gives list of points where signal indicates dropout or spiking
  if(length(rejpoints)>interpolatebad)
  {myinclude[mym]=-1; #flag with -1; denotes drop this epoch; triggered by either channel
  }
  if(length(rejpoints)==interpolatebad)
  {dropoint=rejpoints;#if just one value abnormal, identify it as dropoint
  myepoched[mym,dropoint,i,1]=mean(myepoched[mym,,i,1])  #and substitute the mean for this channel
  }
  }
  if(mym==1)
  {badpoints=rejpoints
  }
  if(mym>1)
  {badpoints=c(badpoints,rejpoints)# keeps record of points with dropout/spiking between iterations
  }
  
  #-------------------------------------------
  # See epoch-by-epoch plots of raw data
  #-------------------------------------------
  timeline=rawdata$sec[1:(postpoints-prepoints+1)] #timeline used in all plots: full epoch length

  if(initialdatacheck==1) #set initialdatacheck to zero to avoid plotting
  { myplotbit <- myepoched[mym, , ,1]
    #first plot the old values with no correction
    myylim <- range(c(range(na.omit(myplotbit[,1])),range(na.omit(myplotbit[,2]))))
    
    # Plot axes and data for that epoch. Left = Red, Right = Blue
    plot(timeline+premarker,myplotbit[,1],type="n",xlab='time (secs)',ylab='velocity',ylim=myylim)
    lines(timeline+premarker,myplotbit[,1],col="red")
    lines(timeline+premarker,myplotbit[,2],col="blue")
    
    #then overplot the corrected values in different colours
    lines(timeline+premarker,myepoched[mym,,1,1],col='pink')
    lines(timeline+premarker,myepoched[mym,,2,1],col='lightblue')
    abline(v=basestart)
    abline(v=baseend)
    abline(v=poistart)
    abline(v=poiend)
    
    mytitle=paste(mysubname, 'Trial:', mym,'Include = ',myinclude[mym])
    title(mytitle)
    location1<-range(mybit)[2]-20
    location2<-range(mybit)[2]-80
    text(0,location1,'Red/blue values in POI have been overwritten with mean',cex=.7)
    text(0,location2,'1 = included; 0 = pre-excluded, -1 = rejected',cex=.7);
    cat("Press 9 for manual exclusion. Press 8 to retain excluded (-1). To retain current inclusion/exclusion status, press 1")
    myoverride <- as.integer(readline(prompt = ""))
    
    # These if statements process the user's manual responses 
    if(is.na(myoverride)) # If the user presses enter but fails to press a number,
    {myoverride=1  # myoverride is coded as '1'to prevent crashing.
    }              # This means exclusion/inclusion is retained according to the automated system. 
    if(myoverride>1)
    {if(myoverride==9)
    {myinclude[mym]=-1}
      if(myoverride==8)
      {myinclude[mym]=1
      }
      myaddcomment<-paste('. Manual override',myoverride,'trial',mym)
      mycomment<-paste(mycomment,myaddcomment)}
  }
  
  } #next epoch
  
  #--------------------------------------------------------
  # Remove deleted epochs (originals in origdata; 
  # myepoched updated so only has retained epochs)
  #--------------------------------------------------------
  filelist[mysub,64]=length(badpoints) # add number of spiking/dropout points to table for saving
  keepmarkers=which(myinclude==1)
  origdata=myepoched #keep this so can reconstruct
  myepoched=myepoched[keepmarkers,,,] #file with only accepted epochs
  conditions1=conditions[keepmarkers] #update trial by condition list
  nmarkers2=length(keepmarkers)
  
  #---------------------------------------------------------
  # Normalise to mean of 100 (see Deppe et al, 2004)
  # Multiply by 100 and divide by overall mean value
  # ensures results are independent of angle of insonation
  #----------------------------------------------------------
  meanL=mean(myepoched[,,1,1])
  meanR=mean(myepoched[,,2,1])
  myepoched[,,1,2]=(100*myepoched[,,1,1])/meanL #last dim of myepoched is 2 for the normalised data
  myepoched[,,2,2]=(100*myepoched[,,2,1])/meanR
  #NB. don't use zscore: need to retain variance difference
  # so make mean 100, but preserve variance
  
  #---------------------------------------------------------
  # See plots of normed epochs
  #---------------------------------------------------------
  if(initialdatacheck1==1)
  {for(mym in 1:nmarkers2)
  {plot(timeline+premarker,myepoched[mym,1:(postpoints-prepoints+1),1,2],type="n",xlab='time (secs)',ylab='velocity')
    lines(timeline+premarker,myepoched[mym,1:(postpoints-prepoints+1),1,2],col='pink')
    lines(timeline+premarker,myepoched[mym,1:(postpoints-prepoints+1),2,2],col='lightblue')
    title('After normalization')
    cat("Press [enter] to continue")
    line <- readline()
  }
  }
  
  #----------------------------------------------------------
  # Find heart beat markers and put corrected values in col 3
  # of 4th dimension of myepoched
  #----------------------------------------------------------
  #Find peaks with moving window, looking for segments that peak
  mypts=dim(myepoched)[2]
  
  for(mym in 1:nmarkers2)
  {peaklist=numeric(0)
  thisbit=myepoched[mym,,1,2]

  
  for(i in seq(6,mypts-6,2))
  {if(
    (thisbit[i] > thisbit[i-5])      # Check that ith value is greater than the value 5 back
    && (thisbit[i-1] > thisbit[i-5]) # Check that the previous value is greater than the value 5 back
    && (thisbit[i] > thisbit[i+5])   # Check that the ith value is greater than the value 5 ahead
    && (thisbit[i+1]>thisbit[i+5]))  # Check that the next value is greater than the value 5 ahead
  {peaklist=c(peaklist,i)
  }
  }
  
  pdiff<-peaklist[2:length(peaklist)]-peaklist[1:(length(peaklist)-1)] # pdiff is a list of the number of samples between peaks
  badp<-which(pdiff<peakdiffmin) # badp is a list of the pdiff values that are less than peakdiffmin
  peaklist<-peaklist[-(badp+1)] # update peaklist, removing peaks identified by badp
  peaklist=c(1,peaklist,mypts) #top and tail the list with end values
  
  peakn=length(peaklist)
  for (p in 1:(peakn-1))
  {myrange=seq(peaklist[p],peaklist[p+1])
  thisheart1=mean(myepoched[mym,myrange,1,2])
  thisheart2=mean(myepoched[mym,myrange,2,2])
  myepoched[mym,myrange,1,3]=thisheart1
  myepoched[mym,myrange,2,3]=thisheart2
  }
  }
  
  #----------------------------------------------------------
  # See plot after heartbeat correction
  #----------------------------------------------------------
  if (initialdatacheck2==1)
  {for(mym in 1:nmarkers2 )
  {plot(timeline+premarker,myepoched[mym,1:(postpoints-prepoints+1),1,3],type="n",xlab='time (secs)',ylab='velocity')
    lines(timeline+premarker,myepoched[mym,1:(postpoints-prepoints+1),1,3],col='pink')
    lines(timeline+premarker,myepoched[mym,1:(postpoints-prepoints+1),2,3],col='lightblue')
    mytitle=paste('Trial after heart beat correction, condition =',conditions1[mym])
    title(mytitle)
    cat ("Press [enter] to continue")
    line <- readline()
  }
  }
  
  #----------------------------------------------------------
  # Find mean for baseline and subtract this.
  # This amounts to baseline correction...
  #----------------------------------------------------------
  nepochbase=nmarkers2
  basepoints=(basestartpoint-prepoints):(baseendpoint-prepoints) #all baseline points within epoch
  if (baselinecorrect==1)
  {for (mym in 1:nmarkers2)
  {basemeanL=mean(myepoched[mym,basepoints,1,3]) #last dim is 3, which is HB corrected
  basemeanR=mean(myepoched[mym,basepoints,2,3])
  myepoched[mym,,1,4]=100+myepoched[mym,,1,3]-basemeanL #last dim 4 is HB and baseline
  myepoched[mym,,2,4]=100+myepoched[mym,,2,3]-basemeanR
  }
  }
  
  
  
  #--------------------------------------------------------
  # Plot after HB correction and baseline correction
  #--------------------------------------------------------
  if(initialdatacheck3==1)
  {for(mym in 1:nmarkers2 )
  {plot(timeline+premarker,myepoched[mym,1:(postpoints-prepoints+1),1,4],type="n",xlab='time (secs)',ylab='velocity')
    lines(timeline+premarker,myepoched[mym,1:(postpoints-prepoints+1),1,4],col='red')
    lines(timeline+premarker,myepoched[mym,1:(postpoints-prepoints+1),2,4],col='blue')
    mytitle=paste('Trial after baseline correction, condition =',conditions1[mym])
    title(mytitle)
    text(-5,110,'blue=R\n red=L\n',cex=.75)
    cat ("Press [enter] to continue")
    line <- readline()
  }
  }
  
  #--------------------------------------------------------
  # Find and exclude epochs with extreme values in period 
  # between start of baseline and end of POI
  #---------------------------------------------------------
  
  keepepoch=rep(1,nmarkers2) #initialise for inclusions
  for(mym in 1:nmarkers2)
  {extremerange=c(which(myepoched[mym,1:(poiendpoints-basestartpoint),1:2,4]>extremehi),which(myepoched[mym,1:(poiendpoints-basestartpoint),1:2,4]<extremelo))
  if(length(extremerange)>0 )
  {keepepoch[mym]=0
  }
  if(mym==1)
  {allextreme=extremerange
  }
  if(mym>1)
  {allextreme=c(allextreme,extremerange) #keeps record of extreme values across trials
  }
  }
  acceptableepochs=which(keepepoch==1)
  conditions2=conditions1[acceptableepochs]
  filelist[mysub,65]=length(allextreme) #report number of extreme values across trials
  
  #--------------------------------------------------------
  # Get grand average and summary stats. 
  # Plot overall laterality curve.
  #--------------------------------------------------------
  finalepochs=myepoched[acceptableepochs,,,4]
  
  myN=dim(finalepochs)[1] #initialise vectors for laterality stats
  myLI=1
  mylatency=1
  myse=1
  lowCI=1
  hiCI=1
  lateralised=1
  myLIodd=1
  myLIeven=1
  myLI_mean=1
  myse_mean=1
  
  # Select relevant trials for each condition
  for(c in 1:numconditions)
  {if(c==1)
  {sentgen=which(conditions2==1)
  sentenceset=finalepochs[sentgen,,1:2]
  finalset=sentenceset
  myN[c]=length(sentgen)
  }
    if(c==2)
    {listgen=which(conditions2==2)
    listset=finalepochs[listgen,,1:2]
    finalset=listset
    myN[c]=length(listgen)
    }
    if(c==3)
    {wordgen=which(conditions2==3)
    wordset=finalepochs[wordgen,,1:2]
    finalset=wordset
    myN[c]=length(wordgen)
    }
    
    # Extract summary statistics
    Lmean <- apply(finalset[,,1], c(2), mean)
    Rmean <- apply(finalset[,,2],c(2),mean)
    LRdiff=Lmean-Rmean
    odds<-seq(from=1,to=myN[c],by=2)
    evens<-seq(from=2,to=myN[c],by=2)
    Lmeanodd<-apply(finalset[odds,,1],c(2),mean)
    Lmeaneven<-apply(finalset[evens,,1],c(2),mean)
    Rmeanodd<-apply(finalset[odds,,2],c(2),mean)
    Rmeaneven<-apply(finalset[evens,,2],c(2),mean)
    LRdiffodd<-Lmeanodd-Rmeanodd
    LRdiffeven<-Lmeaneven-Rmeaneven
    
    #Compute LI etc
    rangestart=poistartpoints - prepoints
    rangeend=poiendpoints - prepoints
    mymax=max(LRdiff[rangestart:rangeend])
    mymin=min(LRdiff[rangestart:rangeend])
    myside=1; mylatpeak=mymax
    if(-mymin>mymax)
    {myside=-1 #R biased LI
    mylatpeak=mymin
    } #R peak > L peak
    mytimepeak=first(which(LRdiff==mylatpeak))
    mylatency[c]=(mytimepeak+prepoints)/samplingrate #need to subtract points for baseline
    mypeakrange=seq(mytimepeak-25,mytimepeak+25) #actual points ie includes baseline
    myLI[c]=as.numeric(format(mean(LRdiff[mypeakrange]),digits=3))
    myLIeven[c]=mean(LRdiffeven[mypeakrange]) #NB LI for even and odd computed at same peak as full LI
    myLIodd[c]=mean(LRdiffodd[mypeakrange])
    
    indLI=numeric(0)#initialise null vector
    for (m in 1:myN[c])
    {indLI=c(indLI,mean(finalset[m,mypeakrange,1]-finalset[m,mypeakrange,2]))
    }
    mysd=sd(indLI)
    myse[c]=as.numeric(format(mysd/sqrt(myN[c]),digits=3))
    lowCI[c]=as.numeric(format(myLI[c]-myside*myse[c]*1.96,digits=3))
    hiCI[c]=as.numeric(format(myLI[c]+myside*myse[c]*1.96,digits=3))
    lateralised[c]=myside
    if((myside*lowCI[c])<0) {lateralised[c]=0}
    latdir=c("R","bilat","L")
    mylatdir=latdir[lateralised[c]+2]
    
    #----------------------------------------------------------
    #Plot overall  laterality curve
    #----------------------------------------------------------
    if (initialdatacheck4==1)
    {timelinelong=rawdata$sec[1:(postpoints-prepoints+1)]+premarker
    plot(timelinelong,Lmean, type="n",ylab='mean blood flow',xlab='time(s)',ylim=c(90,120)) #set up plot - doesn't actually plot anything
    lines(timelinelong,Lmean,col='red')
    lines(timelinelong,Rmean,col='blue')
    lines(timelinelong,(100+LRdiff),col='black')
    abline(v = poistart, lty = 2, col = 'green')
    abline(v = poiend, lty = 2, col = 'green')
    abline(v = basestart, lty = 2)
    abline(v = baseend, lty = 2)
    abline(v = mylatency, lty = 2, col = 'magenta')
    
    text(-4,105,'blue=R\n red=L\n black=(L-R) +100',cex=.75)
    if(c==1){mytitle=paste(mysubname,'Sentence Generation')}
    if(c==2){mytitle=paste(mysubname,'List Generation')}
    if(c==3){mytitle=paste(mysubname,'Word Generation')}
    title(mytitle)
    cat ("Press [enter] to continue")
    line <- readline()
    }
    
    #Prepare stats for individual condition for saving to file
    if(c==1)
    {filelist[mysub,66:74]=c(myN[c],myLI[c],myse[c],mylatency[c],lowCI[c],hiCI[c],lateralised[c],myLIodd[c],myLIeven[c])
    averageddata <- data.frame("Sent_L" = Lmean,
                               "Sent_R" = Rmean)
    }
    if(c==2)
    {filelist[mysub,75:83]=c(myN[c],myLI[c],myse[c],mylatency[c],lowCI[c],hiCI[c],lateralised[c],myLIodd[c],myLIeven[c])
    averageddata$List_L <- Lmean
    averageddata$List_R <- Rmean
    }
    if(c==3)
    {filelist[mysub,84:92]=c(myN[c],myLI[c],myse[c],mylatency[c],lowCI[c],hiCI[c],lateralised[c],myLIodd[c],myLIeven[c])
    averageddata$Word_L <- Lmean
    averageddata$Word_R <- Rmean
    }

  
  # Print averaged epoch data to file
  
  
  if(length(keepmarkers)>=8){
    epochlength <- length(Lmean)
    mymeanLR<-data.frame(matrix(ncol=5,nrow=epochlength))
    mymeanLR[,1]<-as.integer(substring(mysubname,1,3))
    alltime<-seq(from=premarker, to=postmarker, by=.04)
    mymeanLR[,2]<-alltime
    mymeanLR[,3]<-Lmean
    mymeanLR[,4]<-Rmean
    mymeanLR[,5]<-Lmean-Rmean
    colnames(mymeanLR)<-c("ID", "time", "Lmean", "Rmean", "meanDiff")
    csvFile2<-paste0(dir, "Rawmeans_", mysubname, "_", condition_list[c], ".csv",sep="")
    write.csv(mymeanLR, csvFile2, row.names=F)
  }
    
  }# closes loop after stats for all three conditions has been calculated

#------------------------------------------------------
# Opportunity to add a comment after looking at the plots.
# Then writes data to file.
#------------------------------------------------------


#write.table(filelist, outfileloc, sep="\t",row.names=FALSE) #alternative for tab-sep
write.csv(filelist,fileloc,row.names=FALSE)
  
} # End of subject loop
