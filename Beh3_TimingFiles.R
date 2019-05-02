### load data and packages
library(stringr)



inputDir <- "/Volumes/Vedder/FoodMST/Analyses/behAnalysis/all_data/converted/"
outputDir <- "/Volumes/Vedder/FoodMST/Analyses/behAnalysis/timing_files/"

txtList <- list.files(path = inputDir, pattern = ".*.txt")
txtList <- t(txtList)

# j <- txtList[1]
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
  
  
  # split into blocks
  var_list <- c("stimImage","stimType","corr","resp")
  
  for (i in 1:length(ind_blockList)) {
    for (k in var_list) {
      assign(paste0("ind_",k,"_block",i),NA)
    }
  }

  for(i in 1:length(ind_stimImage)){
   for(k in 1:length(ind_blockList)){
     if(as.numeric(ind_stimImage[i]) < ind_blockList[k]){
       for(m in var_list){
         assign(paste0("ind_",m,"_block",k),c(get(paste0("ind_",m,"_block",k)),as.numeric(get(paste0("ind_",m))[i])))
       }
       break
     }
   } 
  }
  
  for (i in 1:length(ind_blockList)) {
    for (k in var_list) {
      assign(paste0("ind_",k,"_block",i),get(paste0("ind_",k,"_block",i))[-1])
    }
  }
  
  
  # extract useful info
  for(i in 1:length(ind_blockList)){
    for(k in var_list){
      if(k != "stimImage"){
        assign(paste0("hold_",k,"_block",i),as.numeric(as.character(data_raw[get(paste0("ind_",k,"_block",i)),2])))
      }else{
        assign(paste0("hold_",k,"_block",i),as.character(data_raw[get(paste0("ind_",k,"_block",i)),2]))
      }
    }
  }

  
  ## determine behavior - split for stim types
  # (TH = 61, TL = 62, TO = 63, LH = 71, LL = 72, OL = 73, FH = 91, FL = 92, FO = 93)
  # (1 = old, 2 = sim, 3 = new)
  # for(i in 1:length(ind_blockList)){
  #   
  # }
}









