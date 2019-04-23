


# Written by Nathan Muncy 4/4/19

### Notes
#
# 1) Run this script from terminal via "Rscript <scriptName>.R s1234"
#   where s1234 is the intended subject number 
#
# 2) This script will create a subject dir, and place a randomized
#   stimulus file in that dir to be read by python script
# 
# 3) Randomization occurs at:
#   - selection of stimuli
#   - assignment to output df
#   - seed for NA search in output df


library("openxlsx")


### Set variables
parDir <- paste0(getwd(),"/..")
workDir <- paste0(parDir,"/Participants/")
stimDir <- paste0(parDir,"/Stimuli/")
winPath <- "Stimuli\\"




### Stimuli per block
#
# Food High = 5 Targ, 10 Lure, 2|3 Foil
# Food Low = 5 Targ, 10 Lure, 2|3 Foil
# Object = 10 Targ, 20 Lure, 5 Foil
#
# 70 stimuli per block, 60 will repeat -> block length = 130 trials
# 70 stimuli * 6 blocks = 420 stimuli

n.Blocks <- 6

n.FHTarg <- n.FLTarg <- 5
n.FHLure <- n.FLLure <- 10
n.FFoil <- 5

n.OTarg <- 10
n.OLure <- 20
n.OFoil <- 5

n.Repeat <- n.FHTarg+n.FLTarg+n.OTarg+n.FHLure+n.FLLure+n.OLure
n.All <- n.Repeat+n.FFoil+n.OFoil




### Import terminal argument, make subjDir
args <- commandArgs(TRUE)
# args <- "p004"
subj <- as.character(args[1])
subjDir <- paste0(workDir, subj)
dir.create(file.path(subjDir))




### Functions
# Iterate until it detects the needed two positions or reaches max iterations
SeedNA.Function <- function(x,y){
  max.Iter <- 200; count <- 1
  while(count <= max.Iter){
    seed <- sample(which(is.na(y[,1]),arr.ind=T),1)
    lag.pos <- seed + x
    if(lag.pos <= dim(y)[1]){
      if(is.na(y[seed,1])==T && is.na(y[lag.pos,1])==T){
        return(list(seed,lag.pos))
        break 
      }
    }
    count <- count+1
  }
  return("Fail")
}

# Build stim list
Build.Function <- function(Targ,Lure,Foil){

  for(x in c("Targ","Lure","Foil")){
    assign(paste0(x,".h"),paste0(winPath,get(x)))
  }
  hold.all <- c(Targ.h,Lure.h,Foil.h)
  hold.label <- c(rep("Targ",n.FHTarg+n.FLTarg+n.OTarg),rep("Lure",n.FHLure+n.FLLure+n.OLure),rep("Foil",n.FFoil+n.OFoil))

  h.df <- matrix(NA,nrow=n.All,ncol=3)
  h.df[,1] <- hold.all
  h.df[,2] <- hold.label
  hold.rand <- sample(c(rep(6,(n.Repeat/2)), rep(12,(n.Repeat/2))),n.Repeat)
  ccc<-1; for(xx in 1:dim(h.df)[1]){
    if(h.df[xx,2]=="Targ" || h.df[xx,2]=="Lure"){
      h.df[xx,3] <- hold.rand[ccc]
      ccc <- ccc+1
    }else{
      h.df[xx,3] <- 0
    }
  }
  return(h.df)
}




### Make lists of random stimuli
n.TotalAll <- n.Blocks*(n.OTarg+n.OLure+n.OFoil+2*(n.FHTarg+n.FHLure)+n.FFoil)

# read in stimulus lists
stim.FHS <- as.matrix(read.delim(paste0(stimDir,"FH_single.txt"),header=F))
stim.FHP <- as.matrix(read.delim(paste0(stimDir,"FH_pair.txt"),header=F))
stim.FLS <- as.matrix(read.delim(paste0(stimDir,"FL_single.txt"),header=F))
stim.FLP <- as.matrix(read.delim(paste0(stimDir,"FL_pair.txt"),header=F))
stim.OS <- as.matrix(read.delim(paste0(stimDir,"O_single.txt"),header=F))
stim.OP <- as.matrix(read.delim(paste0(stimDir,"O_pair.txt"),header=F))

# randomize singles, so any stimulus can be Target or Foil
h.FHS <- sample(1:dim(stim.FHS)[1],dim(stim.FHS)[1])
h.FLS <- sample(1:dim(stim.FLS)[1],dim(stim.FLS)[1])
h.OS <- sample(1:dim(stim.OS)[1],dim(stim.OS)[1])

# select random subset of pairs - Lures
h.FHP <- sample(1:dim(stim.FHP)[1],n.Blocks*n.FHLure)
h.FLP <- sample(1:dim(stim.FLP)[1],n.Blocks*n.FLLure)
h.OP <- sample(1:dim(stim.OP)[1],n.Blocks*n.OLure)

# make Targ/Lure/Foil vectors
v.FHLure <- h.FHP
v.FLLure <- h.FLP
v.OLure <- h.OP

v.FHTarg <- h.FHS[1:(n.Blocks*n.FHTarg)]
v.FLTarg <- h.FLS[1:(n.Blocks*n.FLTarg)]
v.OTarg <- h.OS[1:(n.Blocks*n.OTarg)]

v.FHFoil <- h.FHS[(n.Blocks*n.FHTarg+1):((n.Blocks*n.FHTarg)+(n.Blocks*n.FFoil)/2)]
v.FLFoil <- h.FLS[(n.Blocks*n.FLTarg+1):((n.Blocks*n.FLTarg)+(n.Blocks*n.FFoil)/2)]
v.OFoil <- h.OS[(n.Blocks*n.OTarg+1):((n.Blocks*n.OTarg)+(n.Blocks*n.OFoil))]

# patch - make single list of Food H/L Foils, randomize, make foil list
tmp.FFoil <- c(stim.FHS[v.FHFoil,1],stim.FLS[v.FLFoil,1])
hold.rand <- sample(1:length(tmp.FFoil),length(tmp.FFoil))
stim.FFoil <- tmp.FFoil[hold.rand]




### Build Blocks

# set counters
c.FHT <- c.FLT <- c.OT <- c.FHL <- c.FLL <- c.OL <- c.FF <- c.OF <- 1
cc.FHT <- cc.FLT <- n.FHTarg
cc.FHL <- cc.FLL <- n.FLLure
cc.FF <- n.FFoil
  
cc.OT <- n.OTarg; cc.OL <- n.OLure; cc.OF <- n.OFoil

hold.na <- rep(NA,2)
for(j in 1:n.Blocks){
  
  ## build list
  # extract number of stim per type per block
  hold.FHTarg <- stim.FHS[v.FHTarg[c.FHT:cc.FHT],1]
  hold.FLTarg <- stim.FLS[v.FLTarg[c.FLT:cc.FLT],1]
  hold.OTarg <- stim.OS[v.OTarg[c.OT:cc.OT],1]
  
  hold.FHLure <- stim.FHP[v.FHLure[c.FHL:cc.FHL],1]
  hold.FLLure <- stim.FLP[v.FLLure[c.FLL:cc.FLL],1]
  hold.OLure <- stim.OP[v.OLure[c.OL:cc.OL],1]
  
  hold.OFoil <- stim.OS[v.OFoil[c.OF:cc.OF],1]
  hold.FFoil <- stim.FFoil[c.FF:cc.FF]
  
  # randomise lists for type (Food,Object)
  hold.T <- sample(c(hold.FHTarg,hold.FLTarg,hold.OTarg),n.FHTarg+n.FLTarg+n.OTarg)
  hold.L <- sample(c(hold.FHLure,hold.FLLure,hold.OLure),n.FHLure+n.FLLure+n.OLure)
  hold.F <- sample(c(hold.FFoil,hold.OFoil),n.FFoil+n.OFoil)

  # update counters
  c.FHT <- c.FHT+n.FHTarg; c.FLT <- c.FLT+n.FLTarg; c.OT <- c.OT+n.OTarg
  c.FHL <- c.FHL+n.FHLure; c.FLL <- c.FLL+n.FLLure; c.OL <- c.OL+n.OLure
  c.FF <- c.FF+n.FFoil; c.OF <- c.OF+n.OFoil
  
  cc.FHT <- cc.FHT+n.FHTarg; cc.FLT <- cc.FLT+n.FLTarg; cc.OT <- cc.OT+n.OTarg
  cc.FHL <- cc.FHL+n.FHLure; cc.FLL <- cc.FLL+n.FLLure; cc.OL <- cc.OL+n.OLure
  cc.FF <- cc.FF+n.FFoil; cc.OF <- cc.OF+n.OFoil
  
  
  ## build each block
  # iterate till success
  status <- 0; while(status != 1){
    
    # construct stimulus list - this randomization is what makes or breaks the DF construction
    hold.input <- Build.Function(hold.T,hold.L,hold.F)
    
    ## fill output file
    hold.output <- matrix(NA,nrow=(2*(n.Repeat)+n.FFoil+n.OFoil),ncol=6)
    hold.rand <- sample(1:dim(hold.output)[1],dim(hold.output)[1])
    
    for(i in 1:dim(hold.input)[1]){
      
      # find first NA, lag, positions 1 & 2
      hold.FNA <- as.numeric(min(which(is.na(hold.output[,1]))))
      
      # fill at random
      hold.lag <- as.numeric(hold.input[i,3])
      hold.pos1 <- as.numeric(hold.rand[i])
      hold.pos2 <- hold.lag + hold.pos1
      
      # if lag
      if(hold.lag != 0){
        
        # if pos2 is in df
        if(hold.pos2 <= as.numeric(dim(hold.output)[1])){
          
          # if pos1 & pos2 are empty
          if(is.na(hold.output[hold.pos1,1])==T && is.na(hold.output[hold.pos2,1])==T){
            hold.output[hold.pos1,] <- c(hold.input[i,], "No", hold.na)
            hold.output[hold.pos2,] <- c(hold.input[i,], "Yes", hold.na)
            
          # find suitable empty rows  
          }else{
            func.out <- SeedNA.Function(hold.lag,hold.output)
            if(length(func.out) > 1){
              hold.output[as.numeric(func.out[1]),] <- c(hold.input[i,],"No", hold.na)
              hold.output[as.numeric(func.out[2]),] <- c(hold.input[i,],"Yes", hold.na)
            }
          }
          
        # find empty rows in df     
        }else{
          func.out <- SeedNA.Function(hold.lag,hold.output)
          if(length(func.out) > 1){
            hold.output[as.numeric(func.out[1]),] <- c(hold.input[i,],"No", hold.na)
            hold.output[as.numeric(func.out[2]),] <- c(hold.input[i,],"Yes", hold.na)
          }
        }
        
      # if no lag
      }else{
        
        # if pos1 is empty
        if(is.na(hold.output[hold.pos1,1])==T){
          hold.output[hold.pos1,] <- c(hold.input[i,],"No", hold.na)
        }else{
          hold.output[hold.FNA,] <- c(hold.input[i,],"No", hold.na)
        }
      }
    }
    
    ## Exit while-loop if DF is constructed successfully
    n.na<-0; for(i in 1:dim(hold.output)[1]){
      if(is.na(hold.output[i,4])==T){
        n.na <- n.na+1
      }
    }
    if(n.na == 0 ){
      status <- 1
    }
  }

    
  # switch a-b for repeat, 
  for(i in 1:dim(hold.output)[1]){
    if(grepl("Yes",hold.output[i,4])==T && grepl("Lure",hold.output[i,2])==T){
      hold.output[i,1] <- gsub("a.jpg","b.jpg",hold.output[i,1])
    }
  }
  

  # StimValue column
  # (TH = 61, TL = 62, TO = 63, LH = 71, LL = 72, OL = 73, FH = 91, FL = 92, FO = 93)
  for(i in 1:dim(hold.output)[1]){
    if(grepl("Targ",hold.output[i,2])==T && grepl("F_H_",hold.output[i,1])==T){
      hold.output[i,5] <- 61
    }else if(grepl("Targ",hold.output[i,2])==T && grepl("F_L_",hold.output[i,1])==T){
      hold.output[i,5] <- 62
    }else if(grepl("Targ",hold.output[i,2])==T && grepl("O_",hold.output[i,1])==T){
      hold.output[i,5] <- 63
    }else if(grepl("Lure",hold.output[i,2])==T && grepl("F_H_",hold.output[i,1])==T){
      hold.output[i,5] <- 71
    }else if(grepl("Lure",hold.output[i,2])==T && grepl("F_L_",hold.output[i,1])==T){
      hold.output[i,5] <- 72
    }else if(grepl("Lure",hold.output[i,2])==T && grepl("O_",hold.output[i,1])==T){
      hold.output[i,5] <- 73
    }else if(grepl("Foil",hold.output[i,2])==T && grepl("F_H_",hold.output[i,1])==T){
      hold.output[i,5] <- 91
    }else if(grepl("Foil",hold.output[i,2])==T && grepl("F_L_",hold.output[i,1])==T){
      hold.output[i,5] <- 92
    }else if(grepl("Foil",hold.output[i,2])==T && grepl("O_",hold.output[i,1])==T){
      hold.output[i,5] <- 93
    }
  }
  
  
  # write correct response column (1=Old, 2=Sim, 3=New)
  for(i in 1:dim(hold.output)[1]){
    if(hold.output[i,5]==61 || hold.output[i,5]==62 || hold.output[i,5]==63){
      hold.output[i,6] <- 1
    }else if(hold.output[i,5]==71 || hold.output[i,5]==72 || hold.output[i,5]==73){
      hold.output[i,6] <- 2
    }else if(hold.output[i,5]==91 || hold.output[i,5]==92 || hold.output[i,5]==93){
      hold.output[i,6] <- 3
    }
  }
  
  
  # write out
  trialNum <- 1:(2*(n.Repeat)+n.FFoil+n.OFoil)
  hold.output <- cbind(trialNum,hold.output)
  fileName <- paste0(subj,"_B",j,"_stimuli.xlsx")
  colnames(hold.output) <- c("Trial","StimFile","StimType","Lag","Repeat", "StimValue","CorResp")
  write.xlsx(hold.output,paste0(subjDir,"/",fileName), sheetName = "Sheet1", col.names = TRUE, row.names = F, append = FALSE)
}



# ### Manually create short practice block of 10 trials
# stim.train <- all.stim[v.Train[1:n.Train]]
# hold.train <- matrix(NA,nrow=12,ncol=5)
# hold.train[,1] <- 1:12
# 
# hold.train[1:7,2] <- paste0(winPath,stim.train[1:7])
# hold.train[8,2] <- paste0(winPath,stim.train[4])
# hold.train[9:11,2] <- paste0(winPath,stim.train[8:10])
# hold.train[12,2] <- paste0(winPath,stim.train[1])
# 
# hold.train[1:12,3] <- "Foil"
# hold.train[1,3] <- "Targ"
# hold.train[4,3] <- "Lure"
# hold.train[8,3] <- "Lure"
# hold.train[12,3] <- "Targ"
# 
# hold.train[1:12,4] <- 0
# hold.train[1,4] <- 12
# hold.train[4,4] <- 4
# hold.train[8,4] <- 4
# hold.train[12,4] <- 12
# 
# hold.train[1:12,5] <- "No"
# hold.train[8,5] <- "Yes"
# hold.train[12,5] <- "Yes"
# 
# # switch a-b for repeat
# for(i in 1:dim(hold.train)[1]){
#   if(grepl("Yes",hold.train[i,5])==T){
#     hold.train[i,2] <- gsub("a.jpg","b.jpg",hold.train[i,2])
#   }
# }
# 
# fileName <- paste0(subj,"_Train_stimuli.xlsx")
# colnames(hold.train) <- c("Trial","File","StimType","Lag","Repeat")
# write.xlsx(hold.train,paste0(subjDir,"/",fileName), sheetName = "Sheet1", col.names = TRUE, row.names = F, append = FALSE)



