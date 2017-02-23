library(dplyr)
library(xlsx)

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
    one_of(names(dataOriginal[1,29:65]))
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
    Other = Other/(White+Black+Asian+Hispanic+Other)
  )


#set a newtable for data exploration.

newtable <- dataOriginal %>%
  select(
    UID = UNITID,
    Name = INSTNM,
    AdmRate = ADM_RATE_ALL,
    AdmRateGroup = adm_group,
    Cost = TUITIONFEE_OUT,
    TuitionIN = TUITIONFEE_IN,
    ValueAddedbyRatio = value_added,
    ValueAddedbyDifference = value_added_dif,
    FamilyIncomeGroup = familyIncome_group,
    FirstGeneration = FIRST_GEN,
    Region = REGION,
    Diversity = diversity,
    Popularity = popularity,
    SchoolType = Nth_cluster,
    FacultySalary = AVGFACSAL,
    Debt = DEBT_MDN,
    Urbanization = LOCALE,
    PercentageofLoan = PCTFLOAN
  )
#it's dangerous to use index by specified constants
nms <- names(newtable)[c(2,4,7:18)]
first_ind <- c(1:(dim(newtable)[2]))[names(newtable)=="AdmRate"]
#newtable[, c(4:13)] <- sapply(newtable[, c(4:13)], as.numeric)
newtable[,c(first_ind:dim(newtable)[2])] <- newtable%>%select(AdmRate:PercentageofLoan)%>%mutate_if(is.character,as.numeric)
newtable$ValueAddedbyRatio <- log(newtable$ValueAddedbyRatio)
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
  newtable$ValueAddedbyRatio[i]<-ifelse(is.na(newtable$ValueAddedbyRatio[i]), 0, newtable$ValueAddedbyRatio[i])
  newtable$Popularity[i]<-ifelse(is.na(newtable$Popularity[i])==T, 0, newtable$Popularity[i])
  newtable$Debt[i]<-ifelse(is.na(newtable$Debt[i])==T, 0, newtable$Debt[i])
}

# Get the range of variable for radar chart
max_sel<-max(newtable$AdmRate)
max_pop<-max(newtable$Popularity)
max_deb<-max(newtable$Debt)
max_val<-max(newtable$ValueAddedbyRatio)
max_div<-max(newtable$Diversity)
min_sel<-min(newtable$AdmRate)
min_pop<-min(newtable$Popularity)
min_deb<-min(newtable$Debt)
min_val<-min(newtable$ValueAddedbyRatio)
min_div<-min(newtable$Diversity)
max_radar<- c(max_sel, max_pop, max_deb, max_val, max_div)
min_radar<- c(min_sel, min_pop, min_deb, min_val, min_div)
gender<-c("Gender")
ethnicity<-c("Ethnicity")

# Change the Locale int variable to char 
loc<-function(l){
  if (l==11||l==12||l==13||l==14){
    return("City")
  }
  if (l==21||l==22||l==23||l==24){
    return("Suburb")
  }
  if (l==31||l==32||l==33||l==34){
    return("Town")
  }
  if (l==41||l==42||l==43||l==44){
    return("Rural")
  }
  else{
    return("Unknown")
  }
}
for(i in 1:nrow(newtable)){
  newtable$Locale[i]<-loc(newtable$Urbanization[i])
}