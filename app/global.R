library(dplyr)
library(xlsx)

# Data preprocessing

dataOriginal <- read.csv("../output/newest_data.csv", header = T)

# preprocess data
dataOriginal$OTHERS = dataOriginal$UGDS_ASIAN + dataOriginal$UGDS_AIAN  + dataOriginal$UGDS_NHPI + dataOriginal$UGDS_UNKN + dataOriginal$UGDS_2MOR

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
    Cost = COSTT4_A,
    International = UGDS_NRA,
    Gender.Men = UGDS_MEN,
    Gender.Women = UGDS_WOMEN,
    White = UGDS_WHITE,
    Black = UGDS_BLACK,
    Asian = UGDS_ASIAN,
    Hispanic = UGDS_HISP,
    Other = OTHERS
  ) %>%
  mutate(
    AdmRate = as.numeric(AdmRate),
    City = as.character(City),
    State = as.character(State),
    Cost = as.integer(Cost),
    Zip = as.character(Zip),
    Link = as.character(Link),
    White = White/(White+Black+Asian+Hispanic+Other),
    Black = Black/(White+Black+Asian+Hispanic+Other),
    Asian = Asian/(White+Black+Asian+Hispanic+Other),
    Hispanic = Hispanic/(White+Black+Asian+Hispanic+Other),
    Other = Other/(White+Black+Asian+Hispanic+Other)
  )

load("../output/selectedColsNames.RData")
vars <- read.xlsx2("../data/CollegeScorecardDataDictionary.xlsx", sheetName = "Variables", stringsAsFactors=F)
vars <- vars[vars[,1]!="",]
selectedCols = vars[vars$VARIABLE.NAME %in% cols,]

majors <- substring(vars[substring(vars$VARIABLE.NAME,1,4)=="PCIP",1],34)
cities <- levels(factor(dataRecent$City))
universities <- as.character(dataRecent$Name)
