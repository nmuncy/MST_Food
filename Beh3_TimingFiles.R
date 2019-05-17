### load data and packages
library(stringr)
library(openxlsx)


inputDir <- "/Volumes/Vedder/FoodMST/Analyses/behAnalysis/all_data/converted/"
outputDir <- "/Volumes/Vedder/FoodMST/Analyses/behAnalysis/timing_files/"

txtList <- list.files(path = inputDir, pattern = ".*.txt")
txtList <- t(txtList)

# j <- txtList[1]
for(j in txtList){
  
  # data_raw <- read.delim(paste0("~/Desktop/",j),sep=" ")
  data_raw <- read.delim(paste0(inputDir,j),sep=" ")
  
  # strip off training
  ind_start <- as.numeric(grep("Test:", data_raw[,1]))
  data_raw <- data_raw[-(1:ind_start),]
  row.names(data_raw) <- 1:dim(data_raw)[1]
  
  # blocks
  ind_blockList <- grep("BlockList:",data_raw[,1])
  num_blocks <- as.numeric(length(ind_blockList))
  
  # pull data positions
  ind_stimImage <- grep("StimImage:", data_raw[,1])
  ind_stimType <- grep("StimValue:", data_raw[,1])
  ind_stimOnset <- grep("Stim.OnsetTime:", data_raw[,1])
  ind_corr <- grep("Correct:", data_raw[,1])
  ind_resp <- grep("Stim.RESP:", data_raw[,1])

  
  # split into blocks
  for (i in 1:length(ind_blockList)) {
    
    if(i ==1){
      ind_block <- which(ind_stimImage < ind_blockList[i])
    }else{
        ind_block <- which(ind_stimImage < ind_blockList[i] & ind_stimImage >=ind_blockList[i-1]) 
    }
    
    # get info
    hold_stimType <- as.numeric(as.character(data_raw[ind_stimType[ind_block],2]))
    hold_stimOnset <- as.numeric(as.character(data_raw[ind_stimOnset[ind_block],2])); hold_base <- hold_stimOnset[1]
    hold_resp <- as.numeric(as.character(data_raw[ind_resp[ind_block],2]))
    hold_corr <- as.numeric(as.character(data_raw[ind_corr[ind_block],2]))
    
    # set up matrices
    behAll_resp <- behSep_resp <- matrix(0,nrow=length(ind_block),ncol=1)
    
    # set up behavior outputs
    for(stim in c("All","O","H","L")){
      for(beh in c("Hit","Miss","XT_New","LFA","LCR","XL_New","XF_FA","XF_Miss","FCR","NR")){
        assign(paste0("time_",stim,"_",beh),NA)
      }
    }

    
    ## determine behavior - split for stim types
    # (TH = 61, TL = 62, TO = 63, LH = 71, LL = 72, OL = 73, FH = 91, FL = 92, FO = 93)
    # (1 = old, 2 = sim, 3 = new)
    
    # k is T/L/F
    for( k in c(6:7,9)){
      
      # r is response
      for( r in 1:3){
        
        # Targets/Lures only
        indAll <- which(grepl(k,hold_stimType) & hold_resp==r & hold_corr!=3)
        
        tmp_timeAll <- hold_stimOnset[indAll]
        hold_timeAll <- round((tmp_timeAll-hold_base)/1000,1)
        
        # type is high and low calorie and object
        for( type in 1:3){
          indSep <- which(grepl(paste0(k,type),hold_stimType) & hold_resp==r & hold_corr!=3)
          
          tmp_timeSep <- hold_stimOnset[indSep]
          hold_timeSep <- round((tmp_timeSep-hold_base)/1000,1)
          
          # Target
          if(k == 6){
            if(r==1){
              behAll_resp[indAll] <- "Hit"; time_All_Hit <- hold_timeAll
              if(type==1){ behSep_resp[indSep] <- "H_Hit"; time_H_Hit <- hold_timeSep}
              if(type==2){ behSep_resp[indSep] <- "L_Hit"; time_L_Hit <- hold_timeSep}
              if(type==3){ behSep_resp[indSep] <- "O_Hit"; time_O_Hit <- hold_timeSep}
            }
            if(r==2){
              behAll_resp[indAll] <- "Miss"; time_All_Miss <- hold_timeAll
              if(type==1){ behSep_resp[indSep] <- "H_Miss"; time_H_Miss <- hold_timeSep}
              if(type==2){ behSep_resp[indSep] <- "L_Miss"; time_L_Miss <- hold_timeSep}
              if(type==3){ behSep_resp[indSep] <- "O_Miss"; time_O_Miss <- hold_timeSep}
            }
            if(r==3){
              behAll_resp[indAll] <- "XT_New"; time_All_XT_New <- hold_timeAll
              if(type==1){ behSep_resp[indSep] <- "XHT_New"; time_H_XT_New <- hold_timeSep}
              if(type==2){ behSep_resp[indSep] <- "XLT_New"; time_L_XT_New <- hold_timeSep}
              if(type==3){ behSep_resp[indSep] <- "XOT_New"; time_O_XT_New <- hold_timeSep}
            }
          }
          
          
          # Lure
          if(k == 7){
            if(r==1){
              behAll_resp[indAll] <- "LFA"; time_All_LFA <- hold_timeAll
              if(type==1){ behSep_resp[indSep] <- "H_LFA"; time_H_LFA <- hold_timeSep}
              if(type==2){ behSep_resp[indSep] <- "L_LFA"; time_L_LFA <- hold_timeSep}
              if(type==3){ behSep_resp[indSep] <- "O_LFA"; time_O_LFA <- hold_timeSep}
            }
            if(r==2){
              behAll_resp[indAll] <- "LCR"; time_All_LCR <- hold_timeAll
              if(type==1){ behSep_resp[indSep] <- "H_LCR"; time_H_LCR <- hold_timeSep}
              if(type==2){ behSep_resp[indSep] <- "L_LCR"; time_L_LCR <- hold_timeSep}
              if(type==3){ behSep_resp[indSep] <- "O_LCR"; time_O_LCR <- hold_timeSep}
            }
            if(r==3){
              behAll_resp[indAll] <- "XL_New"; time_All_XL_New <- hold_timeAll
              if(type==1){ behSep_resp[indSep] <- "XHL_New"; time_H_XL_New <- hold_timeSep}
              if(type==2){ behSep_resp[indSep] <- "XLL_New"; time_L_XL_New <- hold_timeSep}
              if(type==3){ behSep_resp[indSep] <- "XOL_New"; time_O_XL_New <- hold_timeSep}
            }
          }
         
          
          # Foils
          if(k == 9){
            
            indAll <- which(grepl(k,hold_stimType) & hold_resp==r) 
            indSep <- which(grepl(paste0(k,type),hold_stimType) & hold_resp==r)
            
            
            
            if(r==1){
              behAll_resp[indAll] <- "XF_FA"
              if(type==1){ behSep_resp[indSep] <- "XHF_FA"}
              if(type==2){ behSep_resp[indSep] <- "XLF_FA"}
              if(type==3){ behSep_resp[indSep] <- "XOF_FA"}
            }
            if(r==2){
              behAll_resp[indAll] <- "XF_Miss"
              if(type==1){ behSep_resp[indSep] <- "XHF_Miss"}
              if(type==2){ behSep_resp[indSep] <- "XLF_Miss"}
              if(type==3){ behSep_resp[indSep] <- "XOF_Miss"}
            }
            if(r==3){
              behAll_resp[indAll] <- "FCR"
              if(type==1){ behSep_resp[indSep] <- "H_FCR"}
              if(type==2){ behSep_resp[indSep] <- "L_FCR"}
              if(type==3){ behSep_resp[indSep] <- "O_FCR"}
            }
          }
        
          # First Presentations
          indAllFirst <- which(hold_resp==r & hold_corr==3)
          indSepFirst <- which(grepl(type,hold_stimType) & hold_resp==r & hold_corr==3)
          if(r==1){
            behAll_resp[indAllFirst] <- "XF_FA"
            if(type==1){ behSep_resp[indSepFirst] <- "XHF_FA"}
            if(type==2){ behSep_resp[indSepFirst] <- "XLF_FA"}
            if(type==3){ behSep_resp[indSepFirst] <- "XOF_FA"}
          }
          if(r==2){
            behAll_resp[indAllFirst] <- "XF_Miss"
            if(type==1){ behSep_resp[indSepFirst] <- "XHF_Miss"}
            if(type==2){ behSep_resp[indSepFirst] <- "XLF_Miss"}
            if(type==3){ behSep_resp[indSepFirst] <- "XOF_Miss"}
          }
          if(r==3){
            behAll_resp[indAllFirst] <- "FCR"
            if(type==1){ behSep_resp[indSepFirst] <- "H_FCR"}
            if(type==2){ behSep_resp[indSepFirst] <- "L_FCR"}
            if(type==3){ behSep_resp[indSepFirst] <- "O_FCR"}
          }
        }
      }
    }
    
    behAll_resp[which(behAll_resp=="0")] <- "999"
    behSep_resp[which(behSep_resp=="0")] <- "999"
    assign(paste0("behAll_resp_block",i),behAll_resp)
    assign(paste0("behSep_resp_block",i),behSep_resp)
    
  } # for blocks
} # for participant
              
          
            