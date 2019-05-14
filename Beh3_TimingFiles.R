### load data and packages
library(stringr)
library(openxlsx)


inputDir <- "/Volumes/Vedder/FoodMST/Analyses/behAnalysis/all_data/converted/"
outputDir <- "/Volumes/Vedder/FoodMST/Analyses/behAnalysis/timing_files/"

txtList <- list.files(path = inputDir, pattern = ".*.txt")
txtList <- t(txtList)

# j <- txtList[1]
for(j in txtList){
  data_raw <- read.delim(paste0("~/Desktop/",j),sep=" ")
  #data_raw <- read.delim(paste0(inputDir,j),sep=" ")
  
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
  ind_corr <- grep("Correct:", data_raw[,1])
  ind_resp <- grep("Stim.RESP:", data_raw[,1])

  
  # split into blocks
  for (i in 1:length(ind_blockList)) {
    
    if(i ==1){
      ind_block <- which(ind_stimImage < ind_blockList[i])
      }else{
        ind_block <- which(ind_stimImage < ind_blockList[i] & ind_stimImage >=ind_blockList[i-1]) }
    
    hold_stimType <- as.numeric(as.character(data_raw[ind_stimType[ind_block],2]))
    hold_resp <- as.numeric(as.character(data_raw[ind_resp[ind_block],2]))
    hold_corr <- as.numeric(as.character(data_raw[ind_corr[ind_block],2]))
    
    # set up matrices
    behAll_resp <- behSep_resp <- matrix(0,nrow=length(ind_block),ncol=1)
    
    ## determine behavior - split for stim types
    # (TH = 61, TL = 62, TO = 63, LH = 71, LL = 72, OL = 73, FH = 91, FL = 92, FO = 93)
    # (1 = old, 2 = sim, 3 = new)
    
    for( k in c(6:7,9)){
      # r is response
      for( r in 1:3){
        indAll <- which(grepl(k,hold_stimType) & hold_resp==r & hold_corr!=3)
        
        # type is high and low calorie and object
        for( type in 1:3){
          indSep <- which(grepl(paste0(k,type),hold_stimType) & hold_resp==r & hold_corr!=3)
          # Target
          if(k == 6){
            if(r==1){
              behAll_resp[indAll] <- "Hit"
              if(type==1){ behSep_resp[indSep] <- "H_Hit"}
              if(type==2){ behSep_resp[indSep] <- "L_Hit"}
              if(type==3){ behSep_resp[indSep] <- "O_Hit"}
            }
            if(r==2){
              behAll_resp[indAll] <- "Miss"
              if(type==1){ behSep_resp[indSep] <- "H_Miss"}
              if(type==2){ behSep_resp[indSep] <- "L_Miss"}
              if(type==3){ behSep_resp[indSep] <- "O_Miss"}
            }
            if(r==3){
              behAll_resp[indAll] <- "XT_New"
              if(type==1){ behSep_resp[indSep] <- "XHT_New"}
              if(type==2){ behSep_resp[indSep] <- "XLT_New"}
              if(type==3){ behSep_resp[indSep] <- "XOT_New"}
            }
          }
          
          # Lure
          if(k == 7){
            if(r==1){
              behAll_resp[indAll] <- "LFA"
              if(type==1){ behSep_resp[indSep] <- "H_LFA"}
              if(type==2){ behSep_resp[indSep] <- "L_LFA"}
              if(type==3){ behSep_resp[indSep] <- "O_LFA"}
            }
            if(r==2){
              behAll_resp[indAll] <- "LCR"
              if(type==1){ behSep_resp[indSep] <- "H_LCR"}
              if(type==2){ behSep_resp[indSep] <- "L_LCR"}
              if(type==3){ behSep_resp[indSep] <- "O_LCR"}
            }
            if(r==3){
              behAll_resp[indAll] <- "XL_New"
              if(type==1){ behSep_resp[indSep] <- "XHL_New"}
              if(type==2){ behSep_resp[indSep] <- "XLL_New"}
              if(type==3){ behSep_resp[indSep] <- "XOL_New"}
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
              
          
            