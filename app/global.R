library(dplyr)
library(xlsx)

# Original
allzips <- readRDS("superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

cleantable <- allzips %>%
  select(
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    Rank = rank,
    Score = centile,
    Superzip = superzip,
    Population = adultpop,
    College = college,
    Income = income,
    Lat = latitude,
    Long = longitude
  )

# My data

data09 <- read.csv("../data/selected_MERGED2009_10_PP.csv", header = T)
data14 <- read.csv("../data/selected_MERGED2014_15_PP.csv", header = T)

# preprocess data
data14$OTHERS = data14$UGDS_ASIAN + data14$UGDS_AIAN  + data14$UGDS_NHPI + data14$UGDS_UNKN + data14$UGDS_2MOR

dataRecent <- data14 %>%
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
