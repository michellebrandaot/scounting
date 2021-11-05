### -- Stats traditional -----------

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


tradit <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
res3 <- GET(url = tradit, add_headers(.headers = headers))
json_resp3 <- fromJSON(content(res3, "text"))
traditional_statsT <- data.frame(json_resp3$resultSets$rowSet)
colnames(traditional_statsT) <-
  json_resp3[["resultSets"]][["headers"]][[1]]
traditional_statsT$Outcome <- "TOT"
# -------------------------------
tradit2 <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=W&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
res3W <- GET(url = tradit2, add_headers(.headers = headers))
json_resp3W <- fromJSON(content(res3W, "text"))
traditional_statsW <- data.frame(json_resp3W$resultSets$rowSet)
colnames(traditional_statsW) <-
  json_resp3W[["resultSets"]][["headers"]][[1]]
colnames(traditional_statsW)
traditional_statsW$Outcome <- "WIN"
#-----------------------------------
tradit3 <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=L&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
res3L <- GET(url = tradit3, add_headers(.headers = headers))
json_resp3L <- fromJSON(content(res3L, "text"))
traditional_statsL <- data.frame(json_resp3L$resultSets$rowSet)
colnames(traditional_statsL) <-
  json_resp3L[["resultSets"]][["headers"]][[1]]
colnames(traditional_statsL)
traditional_statsL$Outcome <- "LOSSES"
#--------------------------------------
traditional_stats <-
  rbind(traditional_statsT, traditional_statsL, traditional_statsW)
traditional_stats <-
  traditional_stats %>% select(
    TEAM_NAME,
    FG_PCT,
    FG_PCT_RANK,
    FG3_PCT,
    FG3_PCT_RANK,
    FT_PCT,
    FT_PCT_RANK,
    BLK,
    BLK_RANK,
    STL,
    STL_RANK,
    Outcome
  )
#View(traditional_stats)


dat3 <-  traditional_stats %>%
  select(-c(TEAM_NAME, Outcome)) %>% # this removes the alpha column if all your character columns need converted to numeric
  mutate_if(is.character, as.numeric)

traditional_stats  <- cbind(dat3, traditional_stats$TEAM_NAME, traditional_stats$Outcome)

traditional_stats <- cbind(dat3, traditional_stats$TEAM_NAME)
traditional_stats <- traditional_stats %>% rename(TEAM_NAME = "traditional_stats$TEAM_NAME",
                                                  Outcome = "traditional_stats$Outcome") %>%  mutate(
                                                    FG_PCT = (FG_PCT * 100),
                                                    FG3_PCT = (FG3_PCT * 100),
                                                    FT_PCT =(FT_PCT * 100)) %> select(TEAM_NAME,
                                                                                      FG_PCT,FG_PCT_RANK, FG3_PCT, FG3_PCT_RANK,FT_PCT, FT_PCT_RANK,BLK,BLK_RANK,STL,STL_RANK,Outcome)

write_csv(traditional_stats,paste0('dataa/','traditional_stats.csv'))
