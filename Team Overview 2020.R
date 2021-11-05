# 1. Team overview - 2021

library(tidyverse)
library(rvest)
library(jsonlite)
library(tidyverse)
library(janitor)
library(gt)
library(httr)
library(devtools)
library(purrr)

#Set header connections
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


#------------------------------------------------------------

url <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
res <- GET(url = url, add_headers(.headers = headers))

json_resp <- fromJSON(content(res, "text"))
dfT <- data.frame(json_resp$resultSets$rowSet)

colnames(dfT) <- json_resp[["resultSets"]][["headers"]][[1]]
dfT$Outcome <- "TOT"
#-------------------------
urlw <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=W&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
resW <- GET(url = urlw, add_headers(.headers = headers))
json_respW <- fromJSON(content(resW, "text"))
dfW <- data.frame(json_respW$resultSets$rowSet)

colnames(dfW) <- json_respW[["resultSets"]][["headers"]][[1]]
dfW$Outcome <- "WIN"

#-------------------
urlL <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=L&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
resL <- GET(url = urlL, add_headers(.headers = headers))
json_respL <- fromJSON(content(resL, "text"))
dfL <- data.frame(json_respL$resultSets$rowSet)

colnames(dfL) <- json_respL[["resultSets"]][["headers"]][[1]]
dfL$Outcome <- "LOSSES"



## cleaning dataset
df <- dfT %>% select(-c(TEAM_ID, CFPARAMS, GP_RANK, CFID))

### Winning

Team <-
  dfT %>% select(TEAM_NAME, GP, W, L, W_PCT, PACE, PACE_RANK, PACE_PER40)


write.csv(Team,paste0('dataa/','Team.csv'))