############################################################

############################################################

## xG Modified Elo Rating System

## James Turner

## Applied Social Data Science MSc Dissertation

## Dataset: https://www.kaggle.com/datasets/christianmcb/football-expected-goals-match-statistics?resource=download

## Loop Script

#############################################################

#############################################################

## Load Libraries

library(tidyverse)
library("SciViews")

## Read Data

library(worldfootballR)

PL1415 <- understat_league_match_results(league = "EPL", season_start_year = 2014)
PL1516 <- understat_league_match_results(league = "EPL", season_start_year = 2015)
PL1617 <- understat_league_match_results(league = "EPL", season_start_year = 2016)
PL1718 <- understat_league_match_results(league = "EPL", season_start_year = 2017)
PL1819 <- understat_league_match_results(league = "EPL", season_start_year = 2018)
PL1920 <- understat_league_match_results(league = "EPL", season_start_year = 2019)
PL2021 <- understat_league_match_results(league = "EPL", season_start_year = 2020)
PL2122 <- understat_league_match_results(league = "EPL", season_start_year = 2021)


epl <- rbind(PL1415,PL1516,PL1617,PL1718,PL1819, PL1920,PL2021, PL2122)

write.csv(epl, "epl.csv")

epl <- epl %>% select(season, datetime,home_team,away_team,home_xG, away_xG, home_goals, away_goals)

epl <- epl %>% rename(Season = season,
                      Date = datetime,
                      Home.Team = home_team,
                                Away.Team = away_team,
                                Home.Expected.Goals = home_xG,
                                Away.Expected.Goals = away_xG,
                                Home.Goals = home_goals,
                                Away.Goals = away_goals)

## Removing time from datetime

epl$Date <- as.Date(epl$Date)

## Adding First Elo ratings

epl$EloHomeAtt <- NA
epl$EloHomeDef <- NA
epl$EloAwayAtt <- NA
epl$EloAwayDef <- NA

epl$EloHomeAtt[epl$Date < "2014-08-26"] <- 1500
epl$EloHomeDef[epl$Date < "2014-08-26"] <- 1500
epl$EloAwayAtt[epl$Date < "2014-08-26"] <- 1500
epl$EloAwayDef[epl$Date < "2014-08-26"] <- 1500


epl <- epl %>% 
  mutate(HID = case_when(
    Home.Team == "Manchester United" ~ "1",
    Home.Team == "Tottenham" ~ "2",
    Home.Team == "Bournemouth" ~ "3",
    Home.Team == "Aston Villa" ~ "4",
    Home.Team == "Everton" ~ "5",
    Home.Team == "Watford" ~ "6",
    Home.Team == "Leicester" ~ "7",
    Home.Team == "Sunderland" ~ "8",
    Home.Team == "Chelsea" ~ "9",
    Home.Team == "Swansea" ~ "10",
    Home.Team == "Newcastle United" ~ "11",
    Home.Team == "Southampton" ~ "12",
    Home.Team == "Arsenal" ~ "13",
    Home.Team == "West Ham" ~ "14",
    Home.Team == "Stoke" ~ "15",
    Home.Team == "Liverpool" ~ "16",
    Home.Team == "West Bromwich Albion" ~ "17",
    Home.Team == "Manchester City" ~ "18",
    Home.Team == "Norwich" ~ "19",
    Home.Team == "Crystal Palace" ~ "20",
    Home.Team == "Hull" ~ "21",
    Home.Team == "Burnley" ~ "22",
    Home.Team == "Middlesbrough" ~ "23",
    Home.Team == "Queens Park Rangers" ~ "24",
    Home.Team == "Brighton" ~ "25",
    Home.Team == "Huddersfield" ~ "26",
    Home.Team == "Fulham" ~ "27",
    Home.Team == "Wolverhampton Wanderers" ~ "28",
    Home.Team == "Cardiff" ~ "29",
    Home.Team == "Sheffield United" ~ "30",
    Home.Team == "Leeds" ~ "31",
    Home.Team == "Brentford" ~ "32"
  ))

epl <- epl %>% 
  mutate(AID = case_when(
    Away.Team == "Manchester United" ~ "1",
    Away.Team == "Tottenham" ~ "2",
    Away.Team == "Bournemouth" ~ "3",
    Away.Team == "Aston Villa" ~ "4",
    Away.Team == "Everton" ~ "5",
    Away.Team == "Watford" ~ "6",
    Away.Team == "Leicester" ~ "7",
    Away.Team == "Sunderland" ~ "8",
    Away.Team == "Chelsea" ~ "9",
    Away.Team == "Swansea" ~ "10",
    Away.Team == "Newcastle United" ~ "11",
    Away.Team == "Southampton" ~ "12",
    Away.Team == "Arsenal" ~ "13",
    Away.Team == "West Ham" ~ "14",
    Away.Team == "Stoke" ~ "15",
    Away.Team == "Liverpool" ~ "16",
    Away.Team == "West Bromwich Albion" ~ "17",
    Away.Team == "Manchester City" ~ "18",
    Away.Team == "Norwich" ~ "19",
    Away.Team == "Crystal Palace" ~ "20",
    Away.Team == "Hull" ~ "21",
    Away.Team == "Burnley" ~ "22",
    Away.Team == "Middlesbrough" ~ "23",
    Away.Team == "Queens Park Rangers" ~ "24",
    Away.Team == "Brighton" ~ "25",
    Away.Team == "Huddersfield" ~ "26",
    Away.Team == "Fulham" ~ "27",
    Away.Team == "Wolverhampton Wanderers" ~ "28",
    Away.Team == "Cardiff" ~ "29",
    Away.Team == "Sheffield United" ~ "30",
    Away.Team == "Leeds" ~ "31",
    Away.Team == "Brentford" ~ "32"
  ))


func.data <- epl %>% select(Date,Home.Team, Away.Team, HID,AID, Home.Expected.Goals, Away.Expected.Goals,
                            Home.Goals, Away.Goals,
                            EloHomeAtt, EloHomeDef, EloAwayAtt, EloAwayDef, Season)

func.data <- func.data %>% 
  arrange(Date)

## Removing objects not needed

rm(PL1415)

rm(PL1516)
rm(PL1617)
rm(PL1718)
rm(PL1819)
rm(PL1920)
rm(PL2021)
rm(PL2122)



##########################################################################

## Adding Parameters to dataset

##########################################################################

## OAH

func.data <- func.data %>% 
  mutate(OAH = case_when(
    func.data$Home.Goals > 3.5 ~ 1,
    func.data$Home.Goals == 3 ~ 0.875,
    func.data$Home.Goals == 2 ~ 0.75,
    func.data$Home.Goals == 1 ~ 0.50,
    func.data$Home.Goals == 0 ~ 0
  ))

## ODA

func.data <- func.data %>% 
  mutate(ODA = case_when(
    func.data$Home.Goals > 3.5 ~ 0,
    func.data$Home.Goals == 3 ~ 0.125,
    func.data$Home.Goals == 2 ~ 0.25,
    func.data$Home.Goals == 1 ~ 0.5,
    func.data$Home.Goals == 0 ~ 1
  ))

## OAA

func.data <- func.data %>% 
  mutate(OAA = case_when(
    func.data$Away.Goals > 3.5 ~ 1,
    func.data$Away.Goals == 3 ~ 0.875,
    func.data$Away.Goals == 2 ~ 0.75,
    func.data$Away.Goals == 1 ~ 0.50,
    func.data$Away.Goals == 0 ~ 0
  ))

## ODH

func.data <- func.data %>% 
  mutate(ODH = case_when(
    func.data$Away.Goals > 3.5 ~ 0,
    func.data$Away.Goals == 3 ~ 0.125,
    func.data$Away.Goals == 2 ~ 0.25,
    func.data$Away.Goals == 1 ~ 0.5,
    func.data$Away.Goals == 0 ~ 1
  ))

func.data$DAH <- NA
func.data$DAA <- NA
func.data$DDH <- NA
func.data$DDA <- NA
func.data$EAH <- NA
func.data$EAA <- NA
func.data$EDH <- NA
func.data$EDA <- NA
func.data$PAH <- NA
func.data$PAA <- NA
func.data$PDH <- NA
func.data$PDA <- NA
func.data$CAH <- NA
func.data$CAA <- NA
func.data$CDH <- NA
func.data$CDA <- NA

func.data$tot.goals <- func.data$Home.Goals + func.data$Away.Goals

## Standardising xG

for(i in 1:nrow(func.data)){
  if(func.data$Home.Expected.Goals[i] > 0.30){
    func.data[i, 'Home.Expected.Goals'] <- func.data$Home.Expected.Goals[i]-0.15
  }else{
    func.data[i, 'Home.Expected.Goals'] <- func.data$Home.Expected.Goals[i]/2
  }
}

for(i in 1:nrow(func.data)){
  if(func.data$Away.Expected.Goals[i] > 0.16){
    func.data[i, 'Away.Expected.Goals'] <- func.data$Away.Expected.Goals[i]+0.15
  }else{
    func.data[i, 'Away.Expected.Goals'] <- func.data$Away.Expected.Goals[i]*2
  }
}

