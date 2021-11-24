library(tidycensus)
library(stringr)
library(tidyverse)
library(plotly)

# US state, regions, and divisions from GitHub
states <- read.csv('https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv')

# State and county populations from Census Bureau
# statePops <- get_estimates(geography = "state", product = "population", output="wide")
statePops <- get_estimates(geography = "state", product = "housing", output="wide", year=2019)
countyPops <- get_estimates(geography = "county", product = "housing", output="wide", year=2019)

###### RENT ########

#### United States ####
# pulling in the categories for the B25070 table
# https://censusreporter.org/tables/B25070/
BurdenUS <- get_acs(geography = "us",
                        variables = c(rentBurdenTotal = "B25070_001",
                                      rentBurden3035 = "B25070_007",
                                      rentBurden3540 = "B25070_008",
                                      rentBurden4050 = "B25070_009",
                                      rentBurden50Plus = "B25070_010",
                                      rentBurdenNC = "B25070_011",
                                      gini="B19083_001",
                                      ownBurdenTotal = "B25091_002",
                                      ownBurden3035 = "B25091_008",
                                      ownBurden3540 = "B25091_009",
                                      ownBurden4050 = "B25091_010",
                                      ownBurden50Plus = "B25091_011",
                                      ownBurdenNC = "B25091_012"),
                        year = 2019,
                        survey = "acs1",
                        output = "wide") %>%
  mutate(rentBurdenPerc = (rentBurden3035E + rentBurden3540E + rentBurden4050E + rentBurden50PlusE) / (rentBurdenTotalE - rentBurdenNCE),
         ownBurdenPerc = (ownBurden3035E + ownBurden3540E + ownBurden4050E + ownBurden50PlusE) / (ownBurdenTotalE - ownBurdenNCE))

#### States ####
# pulling in the categories for the B25070 table
# https://censusreporter.org/tables/B25070/
BurdenStates <- get_acs(geography = "state",
                            variables = c(rentBurdenTotal = "B25070_001",
                                          rentBurden3035 = "B25070_007",
                                          rentBurden3540 = "B25070_008",
                                          rentBurden4050 = "B25070_009",
                                          rentBurden50Plus = "B25070_010",
                                          rentBurdenNC = "B25070_011",
                                          gini="B19083_001",
                                          ownBurdenTotal = "B25091_002",
                                          ownBurden3035 = "B25091_008",
                                          ownBurden3540 = "B25091_009",
                                          ownBurden4050 = "B25091_010",
                                          ownBurden50Plus = "B25091_011",
                                          ownBurdenNC = "B25091_012"),
                            year = 2019,
                            survey = "acs1",
                            output = "wide") %>%
  mutate(rentBurdenPerc = round((rentBurden3035E + rentBurden3540E + rentBurden4050E + rentBurden50PlusE) / (rentBurdenTotalE - rentBurdenNCE) * 100,2),
         giniE = round(giniE, 2),
         ownBurdenPerc = round((ownBurden3035E + ownBurden3540E + ownBurden4050E + ownBurden50PlusE) / (ownBurdenTotalE - ownBurdenNCE) * 100,2))

# merging rent cost data with the population data
BurdenStates <- merge(BurdenStates, statePops, by=c("NAME", "GEOID"))
BurdenStates <- merge(states, BurdenStates, by.x="State", by.y="NAME")

BurdenStates <- BurdenStates %>%
  select(State, GEOID, State.Code, Region, Division, giniE, HUEST, rentBurdenPerc, ownBurdenPerc)

write.csv(BurdenStates, "BurdenStates.csv")

#### Counties ####
# pulling in the categories for the B25070 table
# https://censusreporter.org/tables/B25070/
BurdenCounties <- get_acs(geography = "county",
                            variables = c(rentBurdenTotal = "B25070_001",
                                          rentBurden3035 = "B25070_007",
                                          rentBurden3540 = "B25070_008",
                                          rentBurden4050 = "B25070_009",
                                          rentBurden50Plus = "B25070_010",
                                          rentBurdenNC = "B25070_011",
                                          gini="B19083_001",
                                          ownBurdenTotal = "B25091_002",
                                          ownBurden3035 = "B25091_008",
                                          ownBurden3540 = "B25091_009",
                                          ownBurden4050 = "B25091_010",
                                          ownBurden50Plus = "B25091_011",
                                          ownBurdenNC = "B25091_012"),
                            year = 2019,
                            survey = "acs5",
                            output = "wide") %>%
  mutate(rentBurdenPerc = round((rentBurden3035E + rentBurden3540E + rentBurden4050E + rentBurden50PlusE) / (rentBurdenTotalE - rentBurdenNCE) * 100,2),
         giniE = round(giniE, 2),
         ownBurdenPerc = round((ownBurden3035E + ownBurden3540E + ownBurden4050E + ownBurden50PlusE) / (ownBurdenTotalE - ownBurdenNCE) * 100,2))

# merging rent cost data with the population data
BurdenCounties <- merge(BurdenCounties, countyPops, by=c("GEOID","NAME"), all.x=T)

BurdenCounties$County<- str_split(BurdenCounties$NAME,", ") %>% sapply("[", 1)
BurdenCounties$State <- str_split(BurdenCounties$NAME,", ") %>% sapply("[", 2)

BurdenCounties <- merge(states, BurdenCounties, by.x="State", by.y="State")

BurdenCounties <- BurdenCounties %>%
  select(County, State, GEOID, State.Code, Region, Division, giniE, HUEST, rentBurdenPerc, ownBurdenPerc)

#write.csv(BurdenCounties, "BurdenCounties.csv")

######## YEARLY ########

yearlyFull <- data.frame()

for (i in 2006:2019){
  yearlyBurdenStates <- get_acs(geography = "state",
                              variables = c(rentBurdenTotal = "B25070_001",
                                            rentBurden3035 = "B25070_007",
                                            rentBurden3540 = "B25070_008",
                                            rentBurden4050 = "B25070_009",
                                            rentBurden50Plus = "B25070_010",
                                            rentBurdenNC = "B25070_011",
                                            gini="B19083_001",
                                            ownBurdenTotal = "B25091_002",
                                            ownBurden3035 = "B25091_008",
                                            ownBurden3540 = "B25091_009",
                                            ownBurden4050 = "B25091_010",
                                            ownBurden50Plus = "B25091_011",
                                            ownBurdenNC = "B25091_012"),
                              year = i,
                              survey = "acs1",
                              output = "wide") %>%
    mutate(rentBurdenPerc = round((rentBurden3035E + rentBurden3540E + rentBurden4050E + rentBurden50PlusE) / (rentBurdenTotalE - rentBurdenNCE) * 100,2),
           ownBurdenPerc = round((ownBurden3035E + ownBurden3540E + ownBurden4050E + ownBurden50PlusE) / (ownBurdenTotalE - ownBurdenNCE) * 100,2),
           giniE = round(giniE, 2),
           year = i) %>%
    select("NAME", "GEOID", "year", "rentBurdenPerc", "ownBurdenPerc", "giniE") %>%
    rename(State = NAME)

  yearlyFull <- rbind(yearlyFull, yearlyBurdenStates)
}

#write.csv(yearlyFull, "BurdenYearly.csv")


usFull <- data.frame()

for (i in 2006:2019){
  yearlyBurdenUS <- get_acs(geography = "us",
                                variables = c(rentBurdenTotal = "B25070_001",
                                              rentBurden3035 = "B25070_007",
                                              rentBurden3540 = "B25070_008",
                                              rentBurden4050 = "B25070_009",
                                              rentBurden50Plus = "B25070_010",
                                              rentBurdenNC = "B25070_011",
                                              gini="B19083_001",
                                              ownBurdenTotal = "B25091_002",
                                              ownBurden3035 = "B25091_008",
                                              ownBurden3540 = "B25091_009",
                                              ownBurden4050 = "B25091_010",
                                              ownBurden50Plus = "B25091_011",
                                              ownBurdenNC = "B25091_012"),
                                year = i,
                                survey = "acs1",
                                output = "wide") %>%
    mutate(rentBurdenPerc = round((rentBurden3035E + rentBurden3540E + rentBurden4050E + rentBurden50PlusE) / (rentBurdenTotalE - rentBurdenNCE) * 100,2),
           ownBurdenPerc = round((ownBurden3035E + ownBurden3540E + ownBurden4050E + ownBurden50PlusE) / (ownBurdenTotalE - ownBurdenNCE) * 100,2),
           giniE = round(giniE, 2),
           year = i) %>%
    select("NAME", "GEOID", "year", "rentBurdenPerc", "ownBurdenPerc", "giniE")
  
  usFull <- rbind(usFull, yearlyBurdenUS)
}
