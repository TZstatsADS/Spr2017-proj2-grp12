setwd('/Users/Zoe/Documents/Spring2017/GR5243/MyPrjs/')
library(data.table)
library(dplyr)

#read all 19 filenames
folder.path <- "CollegeScorecard_Raw_Data/"
filenames <- list.files(path = folder.path, pattern = "*.csv")
fnames <- paste(folder.path, filenames, sep = "")
load('Spr2017-proj2-grp12/output/selectedColsNames.RData')
is.nullstr <- function(x) {
  return (x == "NULL")
}#check null

#preprocess ith file
shrink.data <- function(i,fnames){
  target <- fread(fnames[i])
  target <- tbl_df(target)
  #select columns with <0.25 missing rate conditional on rows with non-null admission rate
  target <- target%>%filter(ADM_RATE!="NULL")%>%select_(.dots = cols)
  high.missing.cols <- names(target%>%select(which(colMeans(is.nullstr(.)) > 0.25)))
  save_name <- paste('Spr2017-proj2-grp12/data/selected_',strsplit(fnames[i],'/')[[1]][2],sep = '')
  write.csv(target,save_name)
  print(paste(c('Following columns have missing rate more than 0.25:',high.missing.cols)))
}

# preprocess recent 10-years' data
for (i in 10:19){
  shrink.data(i,fnames)
  print(paste(str(i),'th file has been preprocessed.',sep = ''))
}



# null.to.na <- function(x) {
#   if ((x=="NULL")) {
#     result <- NA
#   }
#   else {
#     result <- x
#   }
#   return(result)
# }#replace "NUll" with na
#target$ADM_RATE <- lapply(target$ADM_RATE,null.to.na)
# target <- target%>%filter(ADM_RATE!='NULL')%>%select(which(colMeans(is.nullstr(.)) < 0.25),ADM_RATE)
# names(target)
# ind <- c(1,2,4,5,6,7,9,10,11,12,13,18,19,20,21,22,23,24,25:34,35,36,37:73,265,266:284,288:300,301,302,303,304,305:315,344:363,368:371,452,482,487,488,459:462,454)
# cols <- names(target)[ind]
# extrat <- c("MD_EARN_WNE_P6","SD_EARN_WNE_P10","COUNT_WNE_INC1_P6","COUNT_WNE_INC2_P6","COUNT_WNE_INC3_P6")
# cols <- c(cols,extrat)
# save(cols,file = 'Spr2017-proj2-grp12/output/selectedColsNames.RData')


