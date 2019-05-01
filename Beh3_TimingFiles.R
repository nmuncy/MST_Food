### load data and packages
library(stringr)



inputDir <- "/Volumes/Vedder/FoodMST/Analyses/behAnalysis/all_data/converted/"
outputDir <- "/Volumes/Vedder/FoodMST/Analyses/behAnalysis/timing_files/"

txtList <- list.files(path = inputDir, pattern = ".*.txt")
