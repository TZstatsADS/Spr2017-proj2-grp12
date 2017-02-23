setwd('/Users/Zoe/Documents/Spring2017/GR5243/MyPrjs/')
library(data.table)
library(dplyr)

# #read all 19 filenames
# folder.path <- "CollegeScorecard_Raw_Data/"
# filenames <- list.files(path = folder.path, pattern = "*.csv")
# fnames <- paste(folder.path, filenames, sep = "")
# load('Spr2017-proj2-grp12/output/selectedColsNames.RData')
# is.nullstr <- function(x) {
#   return (x == "NULL")
# }#check null

# #preprocess ith file
# shrink.data <- function(i,fnames){
#   target <- fread(fnames[i])
#   target <- tbl_df(target)
#   #select columns with <0.25 missing rate conditional on rows with non-null admission rate
#   target <- target%>%filter((ADM_RATE!="NULL")&(ADM_RATE>0))%>%select_(.dots = cols)
#   high.missing.cols <- names(target%>%select(which(colMeans(is.nullstr(.)) > 0.25)))
#   save_name <- paste('Spr2017-proj2-grp12/output/selected_',strsplit(fnames[i],'/')[[1]][2],sep = '')
#   write.csv(target,save_name)
#   print(paste(c('Following columns have missing rate more than 0.25:',high.missing.cols)))
# }

# # preprocess recent 10-years' data
# for (i in 10:19){
#   shrink.data(i,fnames)
#   print(paste(str(i),'th file has been preprocessed.',sep = ''))
# }

#popularity against value_added
# target <- fread('Spr2017-proj2-grp12/output/selected_MERGED2014_15_PP.csv')
target <- fread('Spr2017-proj2-grp12/output/newest_data.csv')

# compute popularity
target$UGDS <- as.numeric(as.character(target$UGDS))
target$ADM_RATE <- as.numeric(as.character(target$ADM_RATE))
target$popularity <- target$UGDS/(target$ADM_RATE)
# is.na(target$popularity) <- do.call(cbind,lapply(target$popularity, is.infinite))#replace Inf with NA

# compute value_added(defined as salary/family_income ratio)
ref_tgt <- fread('Spr2017-proj2-grp12/output/selected_MERGED2009_10_PP.csv')
ref_tgt$MD_EARN_WNE_P6 <- as.numeric(as.character(ref_tgt$MD_EARN_WNE_P6)) 
target$MD_FAMINC <- as.numeric(as.character(target$MD_FAMINC))
new_df <- target%>%select(UNITID,MD_FAMINC)%>%inner_join((ref_tgt%>%select(UNITID,MD_EARN_WNE_P6)))
new_df$value_added <- new_df$MD_EARN_WNE_P6/new_df$MD_FAMINC
is.na(new_df$value_added) <- do.call(cbind,lapply(new_df$value_added, is.infinite))#replace Inf with NA
target <- target%>%left_join((new_df%>%select(UNITID,value_added)))
#write.csv(target,'Spr2017-proj2-grp12/output/newest_data.csv')

# compute value_added_dif(defined as salary-family_income difference)
ref_tgt <- fread('Spr2017-proj2-grp12/output/selected_MERGED2009_10_PP.csv')
ref_tgt$MD_EARN_WNE_P6 <- as.numeric(as.character(ref_tgt$MD_EARN_WNE_P6)) 
#target$MD_FAMINC <- as.numeric(as.character(target$MD_FAMINC))
new_df <- target%>%select(UNITID,MD_FAMINC)%>%inner_join((ref_tgt%>%select(UNITID,MD_EARN_WNE_P6)))
new_df$value_added_dif <- new_df$MD_EARN_WNE_P6 - new_df$MD_FAMINC
#is.na(new_df$value_added) <- do.call(cbind,lapply(new_df$value_added, is.infinite))#replace Inf with NA
target <- target%>%left_join((new_df%>%select(UNITID,value_added_dif)))
write.csv(target,'Spr2017-proj2-grp12/output/newest_data.csv')

# for verification, columbia univeristy id is 190150, harvard university id is 166027, stanford university 243744, NYU 193900, Fordham University 191241, MIT 166683

# compute diversity 
# standard.ratio <- c(WHITE=0.637,Black=0.122,HISP=0.163,AIAN=0.007,ASIAN=0.047,NHPI=0.0015,NRA=0.0625,WOMEN=0.5)
standard.ratio <- c(WHITE=0.632,Black=0.139,HISP=0.129,AIAN=0.011,ASIAN=0.068,NHPI=0.0015,NRA=0.022,WOMEN=0.5)

target <- target%>%mutate_each(funs(as.character),c(UGDS_WHITE:UGDS_NHPI,UGDS_NRA,UGDS_WOMEN))
target <- target%>%mutate_each(funs(as.numeric),c(UGDS_WHITE:UGDS_NHPI,UGDS_NRA,UGDS_WOMEN))
target$diversity<-1/(target%>%select(c(UGDS_WHITE:UGDS_NHPI,UGDS_NRA,UGDS_WOMEN))%>%sweep(2,standard.ratio,'/')%>%sweep(2,1,'-')%>%sweep(2,2,'^')%>%rowSums())
target$diversity <- (target$diversity - min(target$diversity))/(max(target$diversity-min(target$diversity)))
# write.csv(target,'Spr2017-proj2-grp12/output/newest_data.csv')

# compute new variable as is.major
target <- fread('Spr2017-proj2-grp12/output/newest_data.csv')
# mid <- target%>%select(PCIP01:PCIP52)%>%mutate_if(is.character,as.numeric)
# major_cols <- names(target%>%select(PCIP01:PCIP52))
# target[,major_cols] <- mid
# cluster colleges by major distribution
to.cluster <- target%>%select(PCIP01:PCIP52)%>%na.omit()%>%scale()
wss <- (nrow(to.cluster)-1)*sum(apply(to.cluster,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(to.cluster, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
k <- 12#sse is acceptably small when k is larger than 12
# K-Means Cluster Analysis
fit <- kmeans(to.cluster, k) # k cluster solution
# get cluster means 
aggregate(to.cluster,by=list(fit$cluster),FUN=mean)
# append cluster assignment
to.cluster <- data.frame(to.cluster, fit$cluster)
target$Nth_cluster <- to.cluster$fit.cluster



# # regress value_added on selected variables
# # selected <- names(target)[c(12,28:64,66:75,78,81:84,86,88:90,94,95,97,98,107,108,109)]
# # save(selected,file = 'Spr2017-proj2-grp12/output/selected_for_regression.RData')
# # load('Spr2017-proj2-grp12/output/selected_for_regression.RData')
# load('Spr2017-proj2-grp12/output/most_important_predictors.RData')
# # to.regress <- target%>%select_(.dots=selected)
# to.regress <- target%>%select_(.dots=sub_cols)
# to.regress <- to.regress%>%mutate_if(is.character,as.numeric)
# to.regress$REGION <- as.factor(to.regress$REGION)
# #to.regress$LOCALE <- as.factor(to.regress$LOCALE)
# to.regress$Nth_cluster <- as.factor(to.regress$Nth_cluster)
# to.regress$LOCALE <- as.factor(to.regress$LOCALE)
# # mod <- lm(value_added_dif ~ . -MD_FAMINC - FAMINC_IND -UGDS -GRAD_DEBT_MDN -RPY_3YR_RT, data = (to.regress%>%na.omit()))
# # summary(mod)
# # sub.to.regress <- to.regress[c(1,40:53,55,57:58,61:66)]
# # sub_cols <- names(sub.to.regress)
# # sub_cols <- sub_cols[c(1,11:24)]
# # save(sub_cols,file = 'Spr2017-proj2-grp12/output/most_important_predictors.RData')
# mod <- lm(value_added_dif ~ . - value_added,data = to.regress)
# summary(mod)



# # Stepwise Regression
# library(MASS)
# sub.to.regress <- to.regress%>%na.omit()
# sub_mod <- lm(value_added_dif ~ . - value_added,data = sub.to.regress)
# step <- stepAIC(sub_mod, direction="both")
# step$anova # display results
# final_cols <- c("REGION","AVGFACSAL","PCTFLOAN","DEBT_MDN","FIRST_GEN","popularity","diversity","LOCALE","Nth_cluster")
# #save(final_cols,file = 'Spr2017-proj2-grp12/output/final_predictors.RData')


# # All Subsets Regression
# library(leaps)
# to.best <- to.regress[,c(final_cols,"value_added_dif")]
# best_mod<-regsubsets(value_added_dif ~ . ,data=to.best ,nbest=10,really.big = T)
# # view results
# summary(best_mod)
# # plot a table of models showing variables in each model.
# # models are ordered by the selection statistic.
# plot(best_mod,scale="r2")
# # plot statistic by subset size
# library(car)
# subsets(best_mod, statistic="rsq")

# find out adjusted r square trends.
target <- fread('Spr2017-proj2-grp12/output/newest_data.csv')
first_ind <- c(1:(dim(target)[2]))[names(target)=="UNITID"]
target <- target[,first_ind:(dim(target)[2])]
load('Spr2017-proj2-grp12/output/final_predictors.RData')
to.regress <- target%>%select_(.dots=c(final_cols,"value_added_dif"))
#transform character columns to numeric
mid <- target%>%select(PCIP01:PCIP52)%>%mutate_if(is.character,as.numeric)
# major_cols <- names(target%>%select(PCIP01:PCIP52))
# target[,major_cols] <- mid
full <- lm(value_added_dif~.,data = to.regress) 
names(to.regress)

















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



