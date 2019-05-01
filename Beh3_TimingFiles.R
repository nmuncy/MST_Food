### load data and packages
library(stringr)



inputDir <- "/Volumes/Vedder/FoodMST/Analyses/behAnalysis/all_data/converted/"
outputDir <- "/Volumes/Vedder/FoodMST/Analyses/behAnalysis/timing_files/"

txtList <- list.files(path = inputDir, pattern = ".*.txt")
txtList <- t(txtList)

for(j in txtList){
  
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
  ind_corr <- grep("Correct:", data_raw[,1])
  ind_resp <- grep("Stim.RESP:", data_raw[,1])
  
  # correct for multiple responses - assumes no double response on first trial
  #### does this work?
  cor_range <- as.numeric(ind_stimImage[2]-ind_stimImage[1])
  if(length(ind_resp) != length(ind_stimImage)){
    for(i in 1:length(ind_resp)){
      holdA <- as.numeric(ind_resp[i])
      holdB <- as.numeric(ind_resp[i+1])
      if(holdB-holdA != cor_range){
        ind_resp <- ind_resp[-(i+1)]
      }
    }
  }
  if(length(ind_resp) != length(ind_stimImage)){
    print("Problem in number of responses")
    break
  }
  
  # extract useful info
  hold_stimImage <- as.character(data_raw[ind_stimImage,2])
  hold_stimType <- as.numeric(as.character(data_raw[ind_stimType,2]))
  hold_corr <- as.numeric(as.character(data_raw[ind_corr,2]))
  hold_resp <- as.numeric(as.character(data_raw[ind_resp,2]))
  
  # # determine behavior
  # for(i in 1:length(hold_stimImage)){
  #   
  # }
}