packages.used=c("dplyr", "xlsx", "leaflet", "RColorBrewer", "scales", "lattice", "shiny","plotly", "ggplot2", "fmsb", "shinyjs","plyr")

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

# load packages
library(dplyr)
library(xlsx)
library(plyr)

# Data Loading

dataOriginal <- read.csv("../output/newest_data.csv", header = T, stringsAsFactors = F)
#newtable <- read.csv("../output/newtable.csv", header = T, stringsAsFactors = F)
#load("../output/predictors_rank.RData")


# preprocess data
dataOriginal$OTHERS = dataOriginal$UGDS_ASIAN + dataOriginal$UGDS_AIAN  + dataOriginal$UGDS_NHPI + dataOriginal$UGDS_UNKN + dataOriginal$UGDS_2MOR
dataOriginal$TUITIONFEE_IN[dataOriginal$TUITIONFEE_IN=="NULL"]="-1"
dataOriginal$TUITIONFEE_OUT[dataOriginal$TUITIONFEE_OUT=="NULL"]="-1"

dataRecent <- dataOriginal %>%
  select(
    UID = UNITID,
    Name = INSTNM,
    City = CITY,
    State = STABBR,
    Lat = LATITUDE,
    Long = LONGITUDE,
    Zip = ZIP,
    Link = INSTURL,
    AdmRate = ADM_RATE_ALL,
    Cost = TUITIONFEE_OUT,
    TuitionIN = TUITIONFEE_IN,
    International = UGDS_NRA,
    Gender.Men = UGDS_MEN,
    Gender.Women = UGDS_WOMEN,
    White = UGDS_WHITE,
    Black = UGDS_BLACK,
    Asian = UGDS_ASIAN,
    Hispanic = UGDS_HISP,
    Other = OTHERS,
    one_of(names(dataOriginal[1,29:65])),
    Popularity = popularity
  ) %>%
  mutate(
    AdmRate = as.numeric(AdmRate),
    City = as.character(City),
    State = as.character(State),
    Cost = as.numeric(as.character(Cost)),
    TuitionIN = as.numeric(as.character(TuitionIN)),
    Zip = as.character(Zip),
    Link = as.character(Link),
    White = White/(White+Black+Asian+Hispanic+Other),
    Black = Black/(White+Black+Asian+Hispanic+Other),
    Asian = Asian/(White+Black+Asian+Hispanic+Other),
    Hispanic = Hispanic/(White+Black+Asian+Hispanic+Other),
    Other = Other/(White+Black+Asian+Hispanic+Other),
    RadiusPop = (Popularity-mean(Popularity[Popularity<70000]))/sd(Popularity[Popularity<70000])*100+750
  )


####################################################
# for predictor analysis
####################################################
# set a newtable for data exploration.
# transform data format
newtable <- dataOriginal %>%
  select(
    UID = UNITID,
    Name = INSTNM,
    AdmRate = ADM_RATE_ALL,
    AdmRateGroup = adm_group,
    Cost = TUITIONFEE_OUT,
    TuitionIN = TUITIONFEE_IN,
    ValueAddedByRatio = value_added,
    ValueAddedByDifference = value_added_dif,
    FamilyIncomeGroup = familyIncome_group,
    FirstGeneration = FIRST_GEN,
    Region = REGION,
    Diversity = diversity,
    Popularity = popularity,
    SchoolType = Nth_cluster,
    FacultySalary = AVGFACSAL,
    Debt = DEBT_MDN,
    Urbanization = LOCALE,
    PercentageOfLoan = PCTFLOAN,
    FamilyIncome = MD_FAMINC
  )
newtable$Locale<-newtable$Urbanization
newtable$School <- newtable$SchoolType
newtable$FamilyIncomeGroup <- factor(mapvalues(newtable$FamilyIncomeGroup, from=c(1, 2, 3), to=c("Low", "Median", "High")))
newtable$AdmRateGroup <- factor(newtable$AdmRateGroup)
newtable$SchoolType <- factor(newtable$SchoolType)
newtable$Region <- factor(newtable$Region)
newtable$Urbanization <- factor(newtable$Urbanization)

# functions and variables for use
nms <- c("Name","AdmRate","AdmRateGroup","ValueAddedByRatio","ValueAddedByDifference","FamilyIncomeGroup","FirstGeneration","Region","Diversity","Popularity","SchoolType","FacultySalary","Debt"     ,"Urbanization","PercentageOfLoan","FamilyIncome")
adjR.square <- as.data.frame(list(adj.r.square = c(0.5871,0.6316,0.6625,0.6878,0.7042,0.7099,0.7152,0.7204,0.7214), predictor=nms[c(7:15)]))
adjR.square$predictor <- factor(adjR.square$predictor,levels=adjR.square$predictor[order(adjR.square$adj.r.square)])
number_ticks <- function(n) {function(limits) pretty(limits, n)}

numeric_cols <- c("AdmRate","Cost","TuitionIN","ValueAddedByRatio","ValueAddedByDifference","FirstGeneration","Diversity","Popularity","FacultySalary","Debt","PercentageOfLoan","FamilyIncome")
newtable[,numeric_cols] <- newtable%>%select_(.dots = numeric_cols)%>%mutate_if(is.character,as.numeric)
newtable$ValueAddedByRatio <- log(newtable$ValueAddedByRatio)
#########################################################
# for predictor analysis 
#########################################################




# change name of majors for easier matching in shiny app filtering
indexMajStart <- 20
indexMajStop <- 55
names(dataRecent)[indexMajStart:indexMajStop] <- apply(matrix(1:(indexMajStop-indexMajStart+1)),1,function(x)paste("MAJPER",x))

load("../output/selectedColsNames.RData")
vars <- read.xlsx2("../data/CollegeScorecardDataDictionary.xlsx", sheetName = "Variables", stringsAsFactors=F)
vars <- vars[vars[,1]!="",]
selectedCols = vars[vars$VARIABLE.NAME %in% cols,]

majors <- substring(vars[substring(vars$VARIABLE.NAME,1,4)=="PCIP",1],34)[-c(37,38)]
cities <- levels(factor(dataRecent$City))
universities <- as.character(dataRecent$Name)

# Choices of levels for input variables 
adm_rate_th <- c(0.15,0.30,0.5)

# Change the missing data to zero
for (i in 1:nrow(newtable)){
  newtable$ValueAddedByRatio[i]<-ifelse(is.na(newtable$ValueAddedByRatio[i]), 0, newtable$ValueAddedByRatio[i])
  newtable$Popularity[i]<-ifelse(is.na(newtable$Popularity[i])==T, 0, newtable$Popularity[i])
  newtable$Debt[i]<-ifelse(is.na(newtable$Debt[i])==T, 0, newtable$Debt[i])
}

# Get the range of variable for radar chart
max_sel<-max(newtable$AdmRate)
max_pop<-max(newtable$Popularity)
max_deb<-max(newtable$Debt)
max_val<-max(newtable$ValueAddedByRatio)
max_div<-max(newtable$Diversity)
min_sel<-min(newtable$AdmRate)
min_pop<-min(newtable$Popularity)
min_deb<-min(newtable$Debt)
min_val<-min(newtable$ValueAddedByRatio)
min_div<-min(newtable$Diversity)
max_radar<- c(max_sel, max_pop, max_deb, max_val, max_div)
min_radar<- c(min_sel, min_pop, min_deb, min_val, min_div)
gender<-c("Gender")
ethnicity<-c("Ethnicity")
