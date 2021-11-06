### team_shooting_style 2020 #

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

team_shooting_stylet <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Scoring&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
res4t <-
  GET(url = team_shooting_stylet, add_headers(.headers = headers))
json_resp4t <- fromJSON(content(res4t, "text"))
team_shooting_styleT <- data.frame(json_resp4t$resultSets$rowSet)
colnames(team_shooting_styleT) <-
  json_resp4t[["resultSets"]][["headers"]][[1]]
colnames(team_shooting_styleT)
team_shooting_styleT$Outcome <- "TOT"
#------------------------------------

team_shooting_styleW <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Scoring&Month=0&OpponentTeamID=0&Outcome=W&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
res4w <-
  GET(url = team_shooting_styleW, add_headers(.headers = headers))
json_resp4w <- fromJSON(content(res4w, "text"))
team_shooting_styleW <- data.frame(json_resp4w$resultSets$rowSet)
colnames(team_shooting_styleW) <-
  json_resp4w[["resultSets"]][["headers"]][[1]]
colnames(team_shooting_styleW)
team_shooting_styleW$Outcome <- "WIN"

#-----------------
team_shooting_styleL <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Scoring&Month=0&OpponentTeamID=0&Outcome=L&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
res4l <-
  GET(url = team_shooting_styleL, add_headers(.headers = headers))
json_resp4l <- fromJSON(content(res4l, "text"))
team_shooting_styleL <- data.frame(json_resp4l$resultSets$rowSet)
colnames(team_shooting_styleL) <-
  json_resp4l[["resultSets"]][["headers"]][[1]]
colnames(team_shooting_styleL)
team_shooting_styleL$Outcome <- "LOSSES"

#---------------------------------------------
team_shooting_style <-
  rbind(team_shooting_styleL,
        team_shooting_styleW,
        team_shooting_styleT)
#View(team_shooting_style)

team_shooting_style <-
  team_shooting_style %>% select(
    TEAM_NAME,
    PCT_FGA_2PT,
    PCT_FGA_2PT_RANK,
    PCT_FGA_3PT,
    PCT_FGA_3PT_RANK,
    PCT_PTS_2PT,
    PCT_PTS_2PT_RANK,
    PCT_PTS_2PT_MR,
    PCT_PTS_2PT_MR_RANK,
    PCT_PTS_3PT,
    PCT_PTS_3PT_RANK,
    PCT_PTS_FB,
    PCT_PTS_FB_RANK,
    PCT_PTS_PAINT,
    PCT_PTS_PAINT_RANK,
    PCT_AST_3PM,
    PCT_AST_3PM_RANK,
    Outcome
  )


shooting_dat2 <-  team_shooting_style %>%
  select(-c(TEAM_NAME, Outcome)) %>% # this removes the alpha column if all your character columns need converted to numeric
  mutate_if(is.character, as.numeric)

team_shooting_style  <-
  cbind(shooting_dat2,
        team_shooting_style$TEAM_NAME,
        team_shooting_style$Outcome)
team_shooting_style <-
  team_shooting_style %>% rename(TEAM_NAME = "team_shooting_style$TEAM_NAME",
                                 Outcome = "team_shooting_style$Outcome") %>% mutate(PCT_FGA_2PT = (PCT_FGA_2PT *
                                                                                                      100))

write_csv(team_shooting_style,paste0('dataa/','team_shooting_style.csv'))
