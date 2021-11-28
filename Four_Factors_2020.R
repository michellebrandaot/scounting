#####################
## four factors table
#####################

library(tidyverse)
library(rvest)
library(jsonlite)
library(tidyverse)
library(janitor)
library(gt)
library(httr)
library(purrr)

headers = c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.gleague.nba.com/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)


url_four_factors <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Four+Factors&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Showcase&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="


res1 <- GET(url = url_four_factors, add_headers(.headers = headers))
json_resp1 <- fromJSON(content(res1, "text"))
four_factorsT <- data.frame(json_resp1$resultSets$rowSet)

colnames(four_factorsT) <-
  json_resp1[["resultSets"]][["headers"]][[1]]
four_factorsT$Outcome <- "TOT"
#-------------------------

url_four_factors1 <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Four+Factors&Month=0&OpponentTeamID=0&Outcome=W&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Showcase&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="


res1W <-
  GET(url = url_four_factors1, add_headers(.headers = headers))
json_resp1W <- fromJSON(content(res1W, "text"))
four_factorsW <- data.frame(json_resp1W$resultSets$rowSet)

colnames(four_factorsW) <-
  json_resp1W[["resultSets"]][["headers"]][[1]]
four_factorsW$Outcome <- "WIN"
#-----------
url_four_factors2 <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Four+Factors&Month=0&OpponentTeamID=0&Outcome=L&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2021-22&SeasonSegment=&SeasonType=Showcase&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="


res1L <-
  GET(url = url_four_factors2, add_headers(.headers = headers))
json_resp1L <- fromJSON(content(res1L, "text"))
four_factorsL <- data.frame(json_resp1L$resultSets$rowSet)

colnames(four_factorsL) <-
  json_resp1L[["resultSets"]][["headers"]][[1]]
four_factorsL$Outcome <- "LOSSES"

four_factors2021 <- rbind(four_factorsT, four_factorsW, four_factorsL)

four_factors2021 <-
  four_factors2021 %>% select(
    TEAM_NAME,
    EFG_PCT,
    FTA_RATE,
    TM_TOV_PCT,
    OREB_PCT,
    OPP_EFG_PCT,
    OPP_FTA_RATE,
    OPP_TOV_PCT,
    OPP_OREB_PCT,
    EFG_PCT_RANK,
    FTA_RATE_RANK,
    TM_TOV_PCT_RANK,
    OREB_PCT_RANK,
    OPP_EFG_PCT_RANK,
    OPP_FTA_RATE_RANK,
    OPP_TOV_PCT_RANK,
    OPP_OREB_PCT_RANK,
    Outcome
  )


dat3 <-  four_factors2021 %>%
  select(-c(TEAM_NAME, Outcome)) %>% # this removes the alpha column if all your character columns need converted to numeric
  mutate_if(is.character, as.numeric)

Four_Factors2021  <-
  cbind(dat3, four_factors2021$TEAM_NAME, four_factors2021$Outcome)

Four_Factors2021 <-
  Four_Factors2021 %>% rename(TEAM_NAME = "four_factors2021$TEAM_NAME", Outcome =
                                "four_factors2021$Outcome")  %>%
  mutate(
    EFG_PCT = (EFG_PCT * 100),
    OREB_PCT = (OREB_PCT * 100),
    TM_TOV_PCT = (TM_TOV_PCT * 100),
    OPP_EFG_PCT = (OPP_EFG_PCT * 100),
    OPP_TOV_PCT = (OPP_TOV_PCT * 100),
    OPP_OREB_PCT = (OPP_OREB_PCT * 100),
    FTA_RATE = (FTA_RATE * 100),
    OPP_FTA_RATE = (OPP_FTA_RATE * 100)
  ) %>%
  select(
    TEAM_NAME,
    EFG_PCT,
    FTA_RATE,
    TM_TOV_PCT,
    OREB_PCT,
    OPP_EFG_PCT,
    OPP_FTA_RATE,
    OPP_TOV_PCT,
    OPP_OREB_PCT,
    EFG_PCT_RANK,
    FTA_RATE_RANK,
    TM_TOV_PCT_RANK,
    OREB_PCT_RANK,
    OPP_EFG_PCT_RANK,
    OPP_FTA_RATE_RANK,
    OPP_TOV_PCT_RANK,
    OPP_OREB_PCT_RANK,
    Outcome
  )


write.csv(Four_Factors2021,'data/','four_factors2021.csv'))

