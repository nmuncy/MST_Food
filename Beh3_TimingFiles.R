##### ----- load data and packages ----- #####
library(stringr)
library(openxlsx)

inputDir <- "/Volumes/Vedder/FoodMST/Analyses/behAnalysis/all_data/"
outputDir <- "/Volumes/Vedder/FoodMST/Analyses/behAnalysis/timing_files/"

txtList <- list.files(path = inputDir, pattern = ".txt")
txtList <- t(txtList)

writeTiming <- 1
flag <- c(F,T,T) # to append or not (for blocks)

##### ----- populating output matrices ----- #####
# j <- txtList[1]

# for each participant
for(j in txtList){
  tmp <- gsub("^.*?-", "", j);  subNum <- gsub("-.*?$","",tmp)
  data_raw <- data.frame(read.delim2(paste0(inputDir,j),sep=c("\t"),comment.char = "<",fileEncoding ="UCS-2LE" ))
  
  # strip off training
  ind_start <- as.numeric(grep("Test: ", data_raw[,1]))
  data_raw <- data_raw[-(1:ind_start),]
  
  # blocks
  ind_blockList <- grep("BlockList:",data_raw)
  num_blocks <- as.numeric(length(ind_blockList))
  
  # pull data positions
  ind_stimImage <- grep("StimImage:", data_raw)
  ind_stimType <- grep("StimValue:", data_raw)
  ind_stimOnset <- grep("Stim.OnsetTime:", data_raw)
  ind_fixOnset <- grep("Fixation.OnsetTime:", data_raw)
  ind_corr <- grep("Correct:", data_raw)
  ind_resp <- grep("Stim.RESP:", data_raw)
  
  
  ##### ----- Spilt into Blocks ----- #####
  for (i in 1:length(ind_blockList)) {
    
    if(i ==1){
      ind_block <- which(ind_stimImage < ind_blockList[i])
    }else{
      ind_block <- which(ind_stimImage < ind_blockList[i] & ind_stimImage >=ind_blockList[i-1]) 
    }
    
    # get info for each block
    hold_stimType <- as.numeric(sub("StimValue: ", "",data_raw[ind_stimType[ind_block]]))
    hold_stimOnset <- as.numeric(sub("Stim.OnsetTime: ", "",data_raw[ind_stimOnset[ind_block]]))
    hold_base <- hold_stimOnset[1]
    hold_resp <- as.numeric(sub("Stim.RESP: ", "", data_raw[ind_resp[ind_block]]))
    hold_corr <- as.numeric(sub("Correct: ", "",data_raw[ind_corr[ind_block]]))
    hold_stimImage <- sub("StimImage: ", "",data_raw[ind_stimImage[ind_block]])
    
    ##### ----- First Presentations ----- #####
    # First presentations (for both targets and lures; not including foils)
    for( k in 1:2){
      for( r in 1:3){
        if( k == 1){
          # overall
          indAll <- which( (grepl(6,hold_stimType)  | grepl(7,hold_stimType))& hold_resp==r & hold_corr==3)
          pre_name <- "All"
        }else{
          # food 
          indAll <- which( (grepl(6,hold_stimType)  | grepl(7,hold_stimType) ) & !grepl(3,hold_stimType) & hold_resp==r & hold_corr==3)
          pre_name <- "Food"
        }
        
        tmp_timeAll <- hold_stimOnset[indAll]
        hold_timeAll <- round((tmp_timeAll-hold_base)/1000,1)
        outname <- ifelse(r==1,paste0(pre_name,"_1stPresent_Old.txt"), ifelse(r==2,paste0(pre_name,"_1stPresent_Sim.txt"),paste0(pre_name,"_1stPresent_Hit.txt")))
        
        if(length(hold_timeAll)==0){
          beh_out <- "*"
        } else{ beh_out <- paste0(hold_timeAll,":1.5")}
        
        write.table(t(beh_out),paste0(outputDir,"sub-",subNum,"_",outname),quote=F,row.names=F,col.names=F,append=flag[i])
        
      } 
    }
    # High, Low, Object  first presentations
    for( stim in 1:3){ # High, Low, Object  
      pre_name <- ifelse(stim==1,"H",ifelse(stim==2,"L","O"))
      type1 <- paste0(6,stim)
      type2 <- paste0(7,stim)
      # r is response  
      for( r in 1:3){
        indAll <- which( (grepl(type1,hold_stimType)  | grepl(type2,hold_stimType))& hold_resp==r & hold_corr==3)
        
        tmp_timeAll <- hold_stimOnset[indAll]
        hold_timeAll <- round((tmp_timeAll-hold_base)/1000,1)
        
        outname <- ifelse(r==1,paste0(pre_name,"_1stPresent_Old.txt"), ifelse(r==2,paste0(pre_name,"_1stPresent_Old.txt"),paste0(pre_name,"_1stPresent_Hit.txt")))
        
        if(length(hold_timeAll)==0){
          beh_out <- "*"
        } else{ beh_out <- paste0(hold_timeAll,":1.5")}
        
        write.table(t(beh_out),paste0(outputDir,"sub-",subNum,"_",outname),quote=F,row.names=F,col.names=F,append=flag[i])
        
      }
      
    }
    
    ##### ----- Encoding preceeding Test ----- #####
    # preceding all Behaviors
    for( k in 6:7){
      # r is response
      for( r in 1:3){
        
        indAll <- which(grepl(k,hold_stimType) & hold_resp==r & hold_corr!=3)
        indPrec <- rep(0,length(indAll))
        count <- 0
        for(p in hold_stimImage[indAll]){
          count <- count + 1
          if(k == 7) p <- sub("b.jpg","",p)
          
          indPrec[count] <- grep(p,hold_stimImage)[1]
        }
        
        indPrec <- sort(indPrec)
        tmp_timeAll <- hold_stimOnset[indPrec]
        hold_timeAll <- round((tmp_timeAll-hold_base)/1000,1)
        
        if(r==1 & k==6){outname <- "All_Prec_Hit.txt"}
        if(r==2 & k==6){outname <- "All_Prec_Miss.txt"}
        if(r==3 & k==6){outname <- "All_Prec_XTarg_New.txt"} # Target called New
        
        if(r==1 & k==7){outname <- "All_Prec_LFA.txt"}
        if(r==2 & k==7){outname <- "All_Prec_LCR.txt"}
        if(r==3 & k==7){outname <- "All_Prec_XLure_New.txt"} # Similiar called New
        
        if(length(hold_timeAll)==0){
          beh_out <- "*"
        } else{ beh_out <- paste0(hold_timeAll,":1.5")}
        
        write.table(t(beh_out),paste0(outputDir,"sub-",subNum,"_",outname),quote=F,row.names=F,col.names=F,append=flag[i])
        
      }
      
    }
    # preceding behaviors for High, Low, and Object
    for( k in c(6:7)){
      for( stim in 1:3){ # High, Low, Object  
        pre_name <- ifelse(stim==1,"H",ifelse(stim==2,"L","O"))
        type <- paste0(k,stim)
        
        # r is response  
        for( r in 1:3){
          indAll <- which(grepl(type,hold_stimType) & hold_resp==r & hold_corr!=3)
          indPrec <- rep(0,length(indAll))
          count <- 0
          for(p in hold_stimImage[indAll]){
            count <- count + 1
            if(k == 7) p <- sub("b.jpg","",p)
            
            indPrec[count] <- grep(p,hold_stimImage)[1]
          }
          
          indPrec <- sort(indPrec)
          
          tmp_timeAll <- hold_stimOnset[indPrec]
          hold_timeAll <- round((tmp_timeAll-hold_base)/1000,1)
          
          # naming convention
          if(r==1 & k==6){outname <- paste0(pre_name,"_Prec_Hit.txt")}
          if(r==2 & k==6){outname <- paste0(pre_name,"_Prec_Miss.txt")}
          if(r==3 & k==6){outname <- paste0(pre_name,"_Prec_XTarg_New.txt")} # Target called New
          
          if(r==1 & k==7){outname <- paste0(pre_name,"_Prec_LFA.txt")}
          if(r==2 & k==7){outname <- paste0(pre_name,"_Prec_LCR.txt")}
          if(r==3 & k==7){outname <- paste0(pre_name,"_Prec_XLure_New.txt")} # Similiar called New
          
          if(length(hold_timeAll)==0){
            beh_out <- "*"
          } else{ beh_out <- paste0(hold_timeAll,":1.5")}
          
          
          write.table(t(beh_out),paste0(outputDir,"sub-",subNum,"_",outname),quote=F,row.names=F,col.names=F,append=flag[i])
          
        }
        
      }
    }
    
    # preceding behaviors for Food
    for( k in c(6:7)){
      # r is response
      for( r in 1:3){
        type1 <- paste0(k,"1") 
        type2 <- paste0(k,"2") 
        indAll <- which( (grepl(type1,hold_stimType) | grepl(type2,hold_stimType) )  & hold_resp==r & hold_corr!=3)
        
        indPrec <- rep(0,length(indAll))
        count <- 0
        for(p in hold_stimImage[indAll]){
          count <- count + 1
          if(k == 7) p <- sub("b.jpg","",p)
          
          indPrec[count] <- grep(p,hold_stimImage)[1]
        }
        
        indPrec <- sort(indPrec)
        tmp_timeAll <- hold_stimOnset[indPrec]
        hold_timeAll <- round((tmp_timeAll-hold_base)/1000,1)
        
        if(r==1 & k==6){outname <- "Food_Prec_Hit.txt"}
        if(r==2 & k==6){outname <- "Food_Prec_Miss.txt"}
        if(r==3 & k==6){outname <- "Food_Prec_XTarg_New.txt"} # Target called New
        
        if(r==1 & k==7){outname <- "Food_Prec_LFA.txt"}
        if(r==2 & k==7){outname <- "Food_Prec_LCR.txt"}
        if(r==3 & k==7){outname <- "Food_Prec_XLure_New.txt"} # Similiar called New
        
        if(length(hold_timeAll)==0){
          beh_out <- "*"
        } else{ beh_out <- paste0(hold_timeAll,":1.5")}
        
        write.table(t(beh_out),paste0(outputDir,"sub-",subNum,"_",outname),quote=F,row.names=F,col.names=F,append=flag[i])
        
      }
      
    }
    
    
    ##### ----- Test ----- #####
    ## determine behavior - split for stim types
    # (TH = 61, TL = 62, TO = 63, LH = 71, LL = 72, LO = 73, FH = 91, FL = 92, FO = 93)
    # (1 = old, 2 = sim, 3 = new)
    
    # k is Target/Lure/Foil
    # for All_*
    for( k in c(6:7,9)){
      # r is response
      for( r in 1:3){
        
        indAll <- which(grepl(k,hold_stimType) & hold_resp==r & hold_corr!=3)
        if(k==9){
          indAll <- which(grepl(k,hold_stimType) & hold_resp==r & hold_corr==3)
        }
        tmp_timeAll <- hold_stimOnset[indAll]
        hold_timeAll <- round((tmp_timeAll-hold_base)/1000,1)
        
        if(r==1 & k==6){outname <- "All_Hit.txt"}
        if(r==2 & k==6){outname <- "All_Miss.txt"}
        if(r==3 & k==6){outname <- "All_XTarg_New.txt"} # Target called New
        
        if(r==1 & k==7){outname <- "All_LFA.txt"}
        if(r==2 & k==7){outname <- "All_LCR.txt"}
        if(r==3 & k==7){outname <- "All_XLure_New.txt"} # Similiar called New
        
        if(r==1 & k==9){outname <- "All_XFoil_FA.txt"}
        if(r==2 & k==9){outname <- "All_XFoil_Miss.txt"}
        if(r==3 & k==9){outname <- "All_FCR.txt"} 
        if(length(hold_timeAll)==0){
          beh_out <- "*"
        } else{ beh_out <- paste0(hold_timeAll,":1.5")}
        
        write.table(t(beh_out),paste0(outputDir,"sub-",subNum,"_",outname),quote=F,row.names=F,col.names=F,append=flag[i])
        
      }
      
    }
    
    # the for-loop for High, Low, and Object
    for( k in c(6:7,9)){
      for( stim in 1:3){ # High, Low, Object  
        pre_name <- ifelse(stim==1,"H",ifelse(stim==2,"L","O"))
        type <- paste0(k,stim)
        
        # r is response  
        for( r in 1:3){
          indAll <- which(grepl(type,hold_stimType) & hold_resp==r & hold_corr!=3)
          if(k==9){
            indAll <- which(grepl(type,hold_stimType) & hold_resp==r & hold_corr==3)
          }
          tmp_timeAll <- hold_stimOnset[indAll]
          hold_timeAll <- round((tmp_timeAll-hold_base)/1000,1)
          
          # naming convention
          if(r==1 & k==6){outname <- paste0(pre_name,"_Hit.txt")}
          if(r==2 & k==6){outname <- paste0(pre_name,"_Miss.txt")}
          if(r==3 & k==6){outname <- paste0(pre_name,"_XTarg_New.txt")} # Target called New
          
          if(r==1 & k==7){outname <- paste0(pre_name,"_LFA.txt")}
          if(r==2 & k==7){outname <- paste0(pre_name,"_LCR.txt")}
          if(r==3 & k==7){outname <- paste0(pre_name,"_XLure_New.txt")} # Similiar called New
          
          if(r==1 & k==9){outname <- paste0(pre_name,"_XFoil_FA.txt")}
          if(r==2 & k==9){outname <- paste0(pre_name,"_XFoil_Miss.txt")}
          if(r==3 & k==9){outname <- paste0(pre_name,"_FCR.txt")} 
          
          if(length(hold_timeAll)==0){
            beh_out <- "*"
          } else{ beh_out <- paste0(hold_timeAll,":1.5")}
          
          write.table(t(beh_out),paste0(outputDir,"sub-",subNum,"_",outname),quote=F,row.names=F,col.names=F,append=flag[i])
          
        }
        
      }
    }
    
    # for food only
    for( k in c(6:7,9)){
      # r is response
      for( r in 1:3){
        type1 <- paste0(k,"1") 
        type2 <- paste0(k,"2") 
        indAll <- which( (grepl(type1,hold_stimType) | grepl(type2,hold_stimType) )  & hold_resp==r & hold_corr!=3)
        if(k==9){
          indAll <- which((grepl(type1,hold_stimType) | grepl(type2,hold_stimType) ) & hold_resp==r & hold_corr==3)
        }
        
        tmp_timeAll <- hold_stimOnset[indAll]
        hold_timeAll <- round((tmp_timeAll-hold_base)/1000,1)
        
        if(r==1 & k==6){outname <- "Food_Hit.txt"}
        if(r==2 & k==6){outname <- "Food_Miss.txt"}
        if(r==3 & k==6){outname <- "Food_XTarg_New.txt"} # Target called New
        
        if(r==1 & k==7){outname <- "Food_LFA.txt"}
        if(r==2 & k==7){outname <- "Food_LCR.txt"}
        if(r==3 & k==7){outname <- "Food_XLure_New.txt"} # Similiar called New
        
        if(r==1 & k==9){outname <- "Food_XFoil_FA.txt"}
        if(r==2 & k==9){outname <- "Food_XFoil_Miss.txt"}
        if(r==3 & k==9){outname <- "Food_FCR.txt"} 
        
        if(length(hold_timeAll)==0){
          beh_out <- "*"
        } else{ beh_out <- paste0(hold_timeAll,":1.5")}
        
        write.table(t(beh_out),paste0(outputDir,"sub-",subNum,"_",outname),quote=F,row.names=F,col.names=F,append=flag[i])
        
      }
      
    }
    
  } # end of block
  
} # end of participant 


