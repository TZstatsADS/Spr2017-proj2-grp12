library(dplyr)
library(xlsx)

# Data Loading

dataOriginal <- read.csv("../output/newest_data.csv", header = T, stringsAsFactors = F)
#newtable <- read.csv("../output/newtable.csv", header = T, stringsAsFactors = F)


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
    Cost = TUITIONFEE_OUT,
    TuitionIN = TUITIONFEE_IN,
    ValueAddedbyRatio= value_added,
    ValueAddedbyDifference= value_added_dif,
    FacultySalary= AVGFACSAL,
    PercentageofLoan= PCTFLOAN,
    Debt= DEBT_MDN,
    FirstGeneration= FIRST_GEN,
    Popularity= popularity,
    Diversity= diversity
  )

nms <- names(newtable)[4:13]
newtable[, c(4:13)] <- sapply(newtable[, c(4:13)], as.numeric)
# change name of majors for easier matching in shiny app filtering
indexMajStart <- 20
indexMajStop <- 55
names(dataRecent)[indexMajStart:indexMajStop] <- apply(matrix(1:(indexMajStop-indexMajStart+1)),1,function(x)paste("MAJPER",x))

load("../output/selectedColsNames.RData")
vars <- read.xlsx2("../data/CollegeScorecardDataDictionary.xlsx", sheetName = "Variables", stringsAsFactors=F)
vars <- vars[vars[,1]!="",]
selectedCols = vars[vars$VARIABLE.NAME %in% cols,]

majors <- substring(vars[substring(vars$VARIABLE.NAME,1,4)=="PCIP",1],34)
cities <- levels(factor(dataRecent$City))
universities <- as.character(dataRecent$Name)

# Choices of levels for input variables 
adm_rate_th <- c(0.15,0.30,0.5)