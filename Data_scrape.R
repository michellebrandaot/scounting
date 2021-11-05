# G-league scouting

library(tidyverse)
library(rvest)
library(jsonlite)
library(tidyverse)
library(janitor)
library(gt)
library(httr)
library(purrr)

## scouting 
# 1. Team overview
# 2. Efficiency_table
# 3. Four Factors table
# 4. Traditional Stats table 
# 5. Team Shooting style table
# 6. Team_shooting_style_defense
# 7. Player Shooting Style
# 8. Lineups



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


# headers <- c(
#   `Host` = 'stats.nba.com',
#   `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_5) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/45.0.2454.101 Safari/537.36',
#   `Accept` = 'application/json, text/plain, */*',
#   `Accept-Language` = 'en-US,en;q=0.5',
#   `Accept-Encoding` = 'gzip, deflate, br',
#   `x-nba-stats-origin` = 'stats',
#   `x-nba-stats-token` = 'true',
#   `Connection` = 'keep-alive',
#   `Origin` = "http://stats.nba.com",
#   `Referer` = 'https://www.nba.com/',
#   `Pragma` = 'no-cache',
#   `Cache-Control` = 'no-cache'
# )

# url <- "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
# resp <- url %>% .nba_headers()
# df_list <- purrr::map(1:length(resp$resultSets$name), function(x) {
#   / <- resp$resultSets$rowSet[[x]] %>% data.frame(stringsAsFactors = F) %>%
#     as_tibble()
#   json_names <- resp$resultSets$headers[[x]]
#   colnames(data) <- json_names
#   return(data)
# })
# names(df_list) <- resp$resultSets$name
# return(df_list)

#------------------------------------------------------------

url <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
res <- GET(url = url, add_headers(.headers = headers))

json_resp <- fromJSON(content(res, "text"))
dfT <- data.frame(json_resp$resultSets$rowSet)
? fromJSON
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

table_1 <- Team %>%
  gt(rowname_col = "TEAM_NAME") %>%
  tab_header(title = md("**SCOUTING REPORT**"),
             subtitle = "Regular Season only") %>%   tab_stubhead(label = "Team") %>%
  cols_label(
    GP = "Games Played",
    W = "Wins",
    L = "Losses",
    W_PCT = "Win Percentage",
    PACE = "Pace",
    PACE_RANK = md("*Rank*")
  ) %>%
  text_transform(
    locations = cells_body(columns = PACE_RANK),
    fn = function(x) {
      team <- word(x,-1)
      paste0("(", x, ")")
    }
  ) %>% cols_align(align = "center",
                   columns = everything()) %>%f
  tab_style(
    style = cell_text(color = "blue",
                      size = px(10)),
    locations = cells_body(columns = PACE_RANK)
  ) %>%
  tab_style(style = cell_text(size = px(1)),
            locations = cells_column_labels(columns = PACE_RANK))
)


write.csv(Team,paste0('dataa/',"Team.csv")

#------------------------------------------------

## Efficiency
df <- rbind(dfT, dfW, dfL)
Efficiency_table <-
  df %>% select(
    TEAM_NAME,
    OFF_RATING,
    OFF_RATING_RANK,
    DEF_RATING,
    DEF_RATING_RANK,
    NET_RATING,
    NET_RATING_RANK,
    Outcome
  )

### TEAM EFFICIENCY ##


table_team_efficiency <- Efficiency_table %>%
  gt(rowname_col = "TEAM_NAME") %>%
  tab_header(title = md("**Team Efficiency**"),
             subtitle = "Regular Season only") %>%
  tab_stubhead(label = "Team") %>%
  cols_label(
    OFF_RATING = "Offensive Rating",
    DEF_RATING = "Defensive Rating",
    NET_RATING = "Net Rating",
    OFF_RATING_RANK = md("*Rank*"),
    DEF_RATING_RANK = md("*Rank*"),
    NET_RATING_RANK = md("*Rank*")
  ) %>%
  text_transform(
    locations = cells_body(columns = c(
      OFF_RATING_RANK, DEF_RATING_RANK, NET_RATING_RANK
    )),
    fn = function(x) {
      team <- word(x,-1)
      paste0("(", x, ")")
    }
  ) %>%
  tab_spanner(label = md("**Offense**"),
              columns = c(OFF_RATING, OFF_RATING_RANK)) %>%
  tab_spanner(label =  md("**Defense**"),
              columns = c(DEF_RATING, DEF_RATING_RANK)) %>%
  tab_spanner(
    label =  md("**Net Rating**"),
    columns = c(NET_RATING, NET_RATING_RANK)
  ) %>%
  cols_align(align = "center",
             columns = everything()) %>%
  cols_align(align = "left",
             columns = c(OFF_RATING_RANK, DEF_RATING_RANK))  %>%
  tab_style(style = cell_text(color = "blue",
                              size = px(10)),
            locations = cells_body(columns = c(
              OFF_RATING_RANK, DEF_RATING_RANK, NET_RATING_RANK
            ))) %>%
  tab_style(style = cell_text(size = px(10)),
            locations = cells_body(columns = c(
              OFF_RATING_RANK, DEF_RATING_RANK, NET_RATING_RANK
            ))) %>%
  tab_style(style = cell_text(size = px(1)),
            locations = cells_column_labels(columns = c(
              OFF_RATING_RANK, DEF_RATING_RANK, NET_RATING_RANK
            )))



write.csv(Efficiency_table,paste0('dataa/',"Efficiency_table.csv")

#####################
## four factors table
#####################

url_four_factors <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Four+Factors&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="


res1 <- GET(url = url_four_factors, add_headers(.headers = headers))
json_resp1 <- fromJSON(content(res1, "text"))
four_factorsT <- data.frame(json_resp1$resultSets$rowSet)

colnames(four_factorsT) <-
  json_resp1[["resultSets"]][["headers"]][[1]]
four_factorsT$Outcome <- "TOT"

#-------------------------

url_four_factors1 <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Four+Factors&Month=0&OpponentTeamID=0&Outcome=W&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="


res1W <- GET(url = url_four_factors1, add_headers(.headers = headers))
json_resp1W <- fromJSON(content(res1W, "text"))
four_factorsW <- data.frame(json_resp1W$resultSets$rowSet)

colnames(four_factorsW) <-
  json_resp1W[["resultSets"]][["headers"]][[1]]
four_factorsW$Outcome <- "WIN"

#-----------
url_four_factors2 <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Four+Factors&Month=0&OpponentTeamID=0&Outcome=L&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="


res1L <- GET(url = url_four_factors2, add_headers(.headers = headers))
json_resp1L <- fromJSON(content(res1L, "text"))
four_factorsL <- data.frame(json_resp1L$resultSets$rowSet)

colnames(four_factorsL) <-
  json_resp1L[["resultSets"]][["headers"]][[1]]
four_factorsL$Outcome <- "LOSSES"

four_factors <- rbind(four_factorsT, four_factorsW, four_factorsL)

four_factors <-
  four_factors %>% select(
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


dat3 <-  four_factors %>%
  select(-c(TEAM_NAME, Outcome)) %>% # this removes the alpha column if all your character columns need converted to numeric
  mutate_if(is.character, as.numeric)

four_factors  <-
  cbind(dat3, four_factors$TEAM_NAME, four_factors$Outcome)

four_factors <-
  four_factors %>% rename(TEAM_NAME = "four_factors$TEAM_NAME", Outcome =
                            "four_factors$Outcome")  %>%
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

write.csv(four_factors,paste0('dataa/',"four_factors.csv")

## tabela  four factors

four_factors_table <-
  four_factors %>%  gt(rowname_col = "TEAM_NAME") %>%
  tab_header(title = md("**Four Factors**")) %>% tab_stubhead(label = "Team") %>%
  cols_label(
    EFG_PCT = "EFG%",
    TM_TOV_PCT = "TO%",
    OREB_PCT = "OREB%",
    EFG_PCT_RANK = md("*Rank*"),
    TM_TOV_PCT_RANK = md("*Rank*"),
    FTA_RATE = "FTA Rate",
    FTA_RATE_RANK = md("*Rank*"),
    OREB_PCT_RANK = md("*Rank*"),
    OPP_EFG_PCT = "OPP EFG%",
    OPP_EFG_PCT_RANK = md("*Rank*"),
    OPP_FTA_RATE = "OPP FTA Rate",
    OPP_FTA_RATE_RANK =  md("*Rank*"),
    OPP_TOV_PCT = "OPP TO%",
    OPP_TOV_PCT_RANK =  md("*Rank*"),
    OPP_OREB_PCT = "OPP OREB%",
    OPP_OREB_PCT_RANK = md("*Rank*")
  ) %>%
  text_transform(
    locations = cells_body(
      columns = c(
        EFG_PCT_RANK,
        TM_TOV_PCT_RANK,
        FTA_RATE_RANK,
        OREB_PCT_RANK,
        OPP_EFG_PCT_RANK,
        OPP_FTA_RATE_RANK,
        OPP_TOV_PCT_RANK,
        OPP_OREB_PCT_RANK
      )
    ),
    fn = function(x) {
      team <- word(x,-1)
      paste0("(", x, ")")
    }
  ) %>%
  tab_spanner(
    label = md("**Offense**"),
    columns = c(
      EFG_PCT,
      EFG_PCT_RANK,
      TM_TOV_PCT,
      TM_TOV_PCT_RANK,
      OREB_PCT,
      OREB_PCT_RANK,
      FTA_RATE,
      FTA_RATE_RANK
    )
  ) %>%
  tab_spanner(
    label =  md("**Defense**"),
    columns = c(
      OPP_EFG_PCT,
      OPP_EFG_PCT_RANK,
      OPP_TOV_PCT,
      OPP_TOV_PCT_RANK,
      OPP_OREB_PCT,
      OPP_OREB_PCT_RANK,
      OPP_FTA_RATE,
      OPP_FTA_RATE_RANK
    )
  ) %>%
  tab_style(style = cell_text(color = "blue",
                              size = px(10)),
            locations = cells_body(
              columns = c(
                EFG_PCT_RANK,
                TM_TOV_PCT_RANK,
                FTA_RATE_RANK,
                OREB_PCT_RANK,
                OPP_EFG_PCT_RANK,
                OPP_FTA_RATE_RANK,
                OPP_TOV_PCT_RANK,
                OPP_OREB_PCT_RANK
              )
            )) %>%
  tab_style(style = cell_text(size = px(1)),
            locations = cells_column_labels(
              columns = c(
                OPP_EFG_PCT_RANK,
                EFG_PCT_RANK,
                TM_TOV_PCT_RANK,
                OREB_PCT_RANK,
                FTA_RATE_RANK,
                OPP_TOV_PCT_RANK,
                OPP_OREB_PCT_RANK,
                OPP_FTA_RATE_RANK
              )
            ))


### -- Stats traditional -----------

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
View(traditional_stats)


dat3 <-  traditional_stats %>%
  select(-c(TEAM_NAME, Outcome)) %>% # this removes the alpha column if all your character columns need converted to numeric
  mutate_if(is.character, as.numeric)

traditional_stats  <-
  cbind(dat3, traditional_stats$TEAM_NAME, traditional_stats$Outcome)

traditional_stats  <- cbind(dat3, traditional_stats$TEAM_NAME)
traditional_stats <-
  traditional_stats %>% rename(TEAM_NAME = "traditional_stats$TEAM_NAME",
                               Outcome = "traditional_stats$Outcome") %>%  mutate(
                                 FG_PCT = (FG_PCT * 100),
                                 FG3_PCT = (FG3_PCT *
                                              100),
                                 FT_PCT =
                                   (FT_PCT * 100)
                               ) %>%
  select(
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



colnames(team_shooting_style)


write_csv(traditional_stats,paste0('dataa/',"traditional_stats.csv")

## table traditional stats

traditional_stats_table <-
  traditional_stats %>%  filter(TEAM_NAME == "Erie BayHawks") %>%
  gt(rowname_col = "TEAM_NAME") %>%
  tab_header(title = md("**Traditional Stats**"),
             subtitle = "Regular Season only |
    (average per game)") %>%
  tab_stubhead(label = "Team") %>%
  cols_label(
    TEAM_NAME = "",
    FG3_PCT = "3PT% ",
    FG3_PCT_RANK = md("*Rank*"),
    FG_PCT = "FG%",
    FG_PCT_RANK = md("*Rank*"),
    FT_PCT = "FT%",
    FT_PCT_RANK = md("*Rank*"),
    BLK = "BLK",
    BLK_RANK = md("*Rank*"),
    STL = "STL",
    STL_RANK = md("*Rank*"),
  ) %>%
  text_transform(
    locations = cells_body(columns = c(
      c(FG3_PCT_RANK, FG_PCT_RANK, FT_PCT_RANK, BLK_RANK, STL_RANK)
    )),
    fn = function(x) {
      team <- word(x,-1)
      paste0("(", x, ")")
    }
  )  %>%
  tab_style(style = cell_text(color = "blue",
                              size = px(10)),
            locations = cells_body(
              columns = c(FG3_PCT_RANK, FG_PCT_RANK, FT_PCT_RANK, BLK_RANK, STL_RANK)
            )) %>%
  tab_style(style = cell_text(size = px(1)),
            locations = cells_column_labels(
              columns = c(FG3_PCT_RANK, FG_PCT_RANK, FT_PCT_RANK, BLK_RANK, STL_RANK)
            )) %>%
  cols_align(align = "center",
             columns = everything())


### shooting ##-------------------------------------------

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


dat2 <-  team_shooting_style %>%
  select(-c(TEAM_NAME, Outcome)) %>% # this removes the alpha column if all your character columns need converted to numeric
  mutate_if(is.character, as.numeric)

team_shooting_style  <-
  cbind(dat2,
        team_shooting_style$TEAM_NAME,
        team_shooting_style$Outcome)
team_shooting_style <-
  team_shooting_style %>% rename(TEAM_NAME = "team_shooting_style$TEAM_NAME",
                                 Outcome = "team_shooting_style$Outcome") %>% mutate(PCT_FGA_2PT = (PCT_FGA_2PT *
                                                                                                      100))

write_csv(team_shooting_style,paste0('dataa/',"team_shooting_style.csv")

Table_shooting <- team_shooting_style  %>%
  gt(rowname_col = "TEAM_NAME") %>%
  tab_header(title = md("**Shooting Style**"),
             subtitle = "Regular Season only |
    (average per game)") %>% tab_stubhead(label = "Team") %>%
  cols_label(
    PCT_FGA_3PT = "%FGA 3P",
    PCT_FGA_3PT_RANK = md("*Rank*"),
    PCT_FGA_2PT = "%FGA 2PT",
    PCT_FGA_2PT_RANK = md("*Rank*"),
    PCT_PTS_2PT = "%PTS 2P",
    PCT_PTS_2PT_RANK = md("*Rank*"),
    PCT_PTS_2PT_MR = "%PTS 2P Midrange",
    PCT_PTS_2PT_MR_RANK = md("*Rank*"),
    PCT_PTS_3PT = "%PTS 3P",
    PCT_PTS_3PT_RANK = md("*Rank*"),
    PCT_PTS_PAINT = "%PTS PAINT",
    PCT_PTS_PAINT_RANK =  md("*Rank*"),
    PCT_PTS_FB = "%PTS FB",
    PCT_PTS_FB_RANK = md("*Rank*"),
    PCT_AST_3PM = "3PM Assisted",
    PCT_AST_3PM_RANK =  md("*Rank*")
  ) %>%
  text_transform(
    locations = cells_body(
      columns = c(
        PCT_FGA_3PT_RANK,
        PCT_FGA_2PT_RANK,
        PCT_FGA_3PT_RANK,
        PCT_PTS_2PT_RANK,
        PCT_PTS_2PT_MR_RANK,
        PCT_PTS_3PT_RANK,
        PCT_PTS_PAINT_RANK,
        PCT_PTS_FB_RANK,
        PCT_AST_3PM_RANK
      )
    ),
    fn = function(x) {
      team <- word(x,-1)
      paste0("(", x, ")")
    }
  )  %>%
  tab_spanner(
    label = md("**Shots Attemps**"),
    columns = c(PCT_FGA_3PT, PCT_FGA_3PT_RANK, PCT_FGA_2PT, PCT_FGA_2PT_RANK)
  ) %>%
  tab_spanner(
    label = md("**Percentage of Total Points**"),
    columns = c(
      PCT_PTS_2PT,
      PCT_PTS_2PT_RANK,
      PCT_PTS_2PT_MR,
      PCT_PTS_2PT_MR_RANK,
      PCT_PTS_3PT,
      PCT_PTS_3PT_RANK,
      PCT_PTS_PAINT,
      PCT_PTS_PAINT_RANK,
      PCT_PTS_FB,
      PCT_PTS_FB_RANK
    )
  ) %>%
  tab_spanner(
    label = md("**Shots Assisted**"),
    columns = c(PCT_AST_3PM, PCT_AST_3PM_RANK)
  ) %>%
  tab_style(style = cell_text(color = "blue",
                              size = px(10)),
            locations = cells_body(
              columns = c(
                PCT_FGA_3PT_RANK,
                PCT_FGA_2PT_RANK,
                PCT_FGA_3PT_RANK,
                PCT_PTS_2PT_RANK,
                PCT_PTS_2PT_MR_RANK,
                PCT_PTS_3PT_RANK,
                PCT_PTS_PAINT_RANK,
                PCT_PTS_FB_RANK,
                PCT_AST_3PM_RANK
              )
            )) %>%
  tab_style(style = cell_text(size = px(1)),
            locations = cells_column_labels(
              columns = c(
                PCT_FGA_3PT_RANK,
                PCT_FGA_2PT_RANK,
                PCT_FGA_3PT_RANK,
                PCT_PTS_2PT_RANK,
                PCT_PTS_2PT_MR_RANK,
                PCT_PTS_3PT_RANK,
                PCT_PTS_PAINT_RANK,
                PCT_PTS_FB_RANK,
                PCT_AST_3PM_RANK
              )
            )) %>%
  cols_align(align = "center",
             columns = everything())

# ----------------------------------------


## shooting style defense
team_shooting_style_defense <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Opponent&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
res5 <-
  GET(url = team_shooting_style_defense, add_headers(.headers = headers))
json_resp5 <- fromJSON(content(res5, "text"))
team_shooting_style_defense <-
  data.frame(json_resp5$resultSets$rowSet)
colnames(team_shooting_style_defense) <-
  json_resp5[["resultSets"]][["headers"]][[1]]
colnames(team_shooting_style_defense)

team_shooting_style_defense <-
  team_shooting_style_defense %>% select(
    TEAM_NAME,
    OPP_FGM,
    OPP_FGM_RANK,
    OPP_FGA,
    OPP_FGA_RANK,
    OPP_FG_PCT,
    OPP_FG_PCT_RANK,
    OPP_FG3M,
    OPP_FG3M_RANK,
    OPP_FG3A,
    OPP_FG3A_RANK,
    OPP_FG3_PCT,
    OPP_FG3_PCT_RANK
  )


team_shooting_style_defense1 <-
  "https://stats.gleague.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=0&LeagueID=20&Location=&MeasureType=Defense&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="
res6 <-
  GET(url = team_shooting_style_defense1, add_headers(.headers = headers))
json_resp6 <- fromJSON(content(res6, "text"))
team_shooting_style_defense1 <-
  data.frame(json_resp6$resultSets$rowSet)
colnames(team_shooting_style_defense1) <-
  json_resp6[["resultSets"]][["headers"]][[1]]
colnames(team_shooting_style_defense1)

team_shooting_style_defense2 <-
  team_shooting_style_defense1 %>% select(
    TEAM_NAME,
    OPP_PTS_FB,
    OPP_PTS_FB_RANK,
    OPP_PTS_PAINT,
    OPP_PTS_PAINT_RANK,
    OPP_PTS_OFF_TOV,
    OPP_PTS_OFF_TOV_RANK
  )


team_shooting_style_defense_final <-
  inner_join(team_shooting_style_defense,
             team_shooting_style_defense2,
             by = 'TEAM_NAME')
colnames(team_shooting_style_defense_final)


write_csv(team_shooting_style_defense_final,paste0('dataa/',
          "team_shooting_style_defense_final.csv")
## table shooting defense

Table_shooting <- team_shooting_style_defense_final  %>%
  gt(rowname_col = "TEAM_NAME") %>%
  tab_header(
    title = md("**Shooting Denfese Style**"),
    subtitle = "Regular Season only |
    (average per game)"
  ) %>% tab_stubhead(label = "Team") %>%
  cols_label(
    OPP_FGM = "FGM",
    OPP_FGM_RANK = md("*Rank*"),
    OPP_FGA = "FGA",
    OPP_FGA_RANK =  md("*Rank*"),
    OPP_FG_PCT = "FG%",
    OPP_FG_PCT_RANK = md("*Rank*"),
    OPP_FG3M = "3FGM",
    OPP_FG3M_RANK = md("*Rank*"),
    OPP_FG3A = "3FGA",
    OPP_FG3A_RANK = md("*Rank*"),
    OPP_FG3_PCT = "3FG%",
    OPP_FG3_PCT_RANK = md("*Rank*"),
    OPP_PTS_FB = "FB",
    OPP_PTS_FB_RANK = md("*Rank*"),
    OPP_PTS_PAINT = "PAINT PTS",
    OPP_PTS_PAINT_RANK = md("*Rank*"),
    OPP_PTS_OFF_TOV = "PTS of TOV",
    OPP_PTS_OFF_TOV_RANK = md("*Rank*")
  ) %>%
  text_transform(
    locations = cells_body(
      columns = c (
        OPP_FGM_RANK,
        OPP_FGA_RANK,
        OPP_FG_PCT_RANK,
        OPP_FG3M_RANK,
        OPP_FG3A_RANK,
        OPP_FG3_PCT_RANK,
        OPP_PTS_FB_RANK,
        OPP_PTS_PAINT_RANK,
        OPP_PTS_OFF_TOV_RANK
      )
    ),
    fn = function(x) {
      team <- word(x,-1)
      paste0("(", x, ")")
    }
  )  %>%
  tab_spanner(
    label = md("**Field Goal**"),
    columns = c(
      OPP_FGM ,
      OPP_FGM_RANK,
      OPP_FGA,
      OPP_FGA_RANK,
      OPP_FG_PCT,
      OPP_FG_PCT_RANK
    )
  ) %>%
  tab_spanner(
    label = md("**3PT**"),
    columns = c(
      OPP_FG3M,
      OPP_FG3M_RANK,
      OPP_FG3A,
      OPP_FG3A_RANK,
      OPP_FG3_PCT,
      OPP_FG3_PCT_RANK
    )
  ) %>%
  tab_spanner(label = md("**FB**"),
              columns = c(OPP_PTS_FB, OPP_PTS_FB_RANK)) %>%
  tab_spanner(
    label = md("**Paint PTS**"),
    columns = c(OPP_PTS_PAINT, OPP_PTS_PAINT_RANK)
  ) %>%
  tab_spanner(
    label = md("**PTS OFF TOV**"),
    columns = c(OPP_PTS_OFF_TOV, OPP_PTS_OFF_TOV_RANK)
  ) %>%
  tab_style(style = cell_text(color = "blue",
                              size = px(10)),
            locations = cells_body(
              columns = c(
                OPP_FGM_RANK,
                OPP_FGA_RANK,
                OPP_FG_PCT_RANK,
                OPP_FG3M_RANK,
                OPP_FG3A_RANK,
                OPP_FG3_PCT_RANK,
                OPP_PTS_FB_RANK,
                OPP_PTS_PAINT_RANK,
                OPP_PTS_OFF_TOV_RANK
              )
            )) %>%
  tab_style(style = cell_text(size = px(1)),
            locations = cells_column_labels(
              columns = c(
                OPP_FGM_RANK,
                OPP_FGA_RANK,
                OPP_FG_PCT_RANK,
                OPP_FG3M_RANK,
                OPP_FG3A_RANK,
                OPP_FG3_PCT_RANK,
                OPP_PTS_FB_RANK,
                OPP_PTS_PAINT_RANK,
                OPP_PTS_OFF_TOV_RANK
              )
            )) %>%
  cols_align(align = "center",
             columns = everything())



#### PLAYERS ##### ---------------------------------------------------------------------------------------------------

player_shooting_style <-
  "https://stats.gleague.nba.com/stats/leaguedashplayershotlocations?College=&Conference=&Country=&DateFrom=&DateTo=&DistanceRange=By+Zone&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=20&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&VsConference=&VsDivision=&Weight="
res7 <-
  GET(url = player_shooting_style, add_headers(.headers = headers))
json_resp7 <- fromJSON(content(res7, "text"))
player_shooting_style <- data.frame(json_resp7$resultSets$rowSet)


colnames(player_shooting_style) <-
  json_resp7[["resultSets"]][["headers"]][[4]][[2]]

#player points
player_shooting_style_points <-
  "https://stats.gleague.nba.com/players/traditional/?sort=PTS&dir=-1&Season=2020-21&SeasonType=Regular%20Season"
res10 <-
  GET(url = player_shooting_style_points, add_headers(.headers = headers))
json_resp10 <- fromJSON(content(res10, "text"))
player_shooting_style <- data.frame(json_resp8$resultSets$rowSet)


colnames(player_shooting_style) <-
  json_resp7[["resultSets"]][["headers"]][[4]][[2]]




colnames(player_shooting_style)
view(player_shooting_style)


colnames(player_shooting_style) <- c(
  "PLAYER_ID",
  "PLAYER_NAME",
  "TEAM_ID",
  "TEAM_ABBREVIATION",
  "AGE",
  "NICKNAME",
  "Restricted_Area_FGM",
  "Restricted_Area_FGA",
  "Restricted_Area_FG_PCT",
  "In_The_Paint_Non_RA_FGM",
  "In_The_Paint_Non_RA_FGA",
  "In_The_Paint_Non_RA_FG_PCT",
  "Mid_Range_FGM",
  "Mid_Range_FGA",
  "Mid_Range_FG_PCT",
  "Left_Corner_3_FGM",
  "Left_Corner_3_FGA",
  "Left_Corner_3_FG_PCT",
  "Right_Corner_3_FGM",
  "Right_Corner_3_FGA",
  "Right_Corner_3_FG_PCT",
  "Above_the_Break_3_FGM",
  "Above_the_Break_3_FGA",
  "Above_the_Break_3_FG_PCT",
  "Backcourt_3_FGM",
  "Backcourt_3_FGA",
  "Backcourt_3_FG_PCT"
)


player_shooting_style <-
  player_shooting_style %>% select(
    "PLAYER_NAME",
    "TEAM_ABBREVIATION",
    "Restricted_Area_FGM",
    "Restricted_Area_FGA",
    "Restricted_Area_FG_PCT",
    "In_The_Paint_Non_RA_FGM",
    "In_The_Paint_Non_RA_FGA",
    "In_The_Paint_Non_RA_FG_PCT",
    "Mid_Range_FGM",
    "Mid_Range_FGA",
    "Mid_Range_FG_PCT",
    "Left_Corner_3_FGM",
    "Left_Corner_3_FGA",
    "Left_Corner_3_FG_PCT",
    "Right_Corner_3_FGM",
    "Right_Corner_3_FGA",
    "Right_Corner_3_FG_PCT",
    "Above_the_Break_3_FGM",
    "Above_the_Break_3_FGA",
    "Above_the_Break_3_FG_PCT",
    "Backcourt_3_FGM",
    "Backcourt_3_FGA",
    "Backcourt_3_FG_PCT"
  )

write.csv(player_shooting_style,paste0('dataa/',"player_shooting_style.csv")

player_shooting_style  %>% arrange(TEAM_ABBREVIATION, PLAYER_NAME) %>%
  gt(rowname_col = "PLAYER_NAME") %>%
  tab_header(
    title = md("**Player Shooting  % for each Location**"),
    subtitle = "Regular Season only |
    (average per game)"
  ) %>% tab_stubhead(label = "Player") %>%
  cols_label(
    TEAM_ABBREVIATION = "Team",
    Restricted_Area_FGM = "FGM",
    Restricted_Area_FGA = "FGA",
    Restricted_Area_FG_PCT = "FG%",
    In_The_Paint_Non_RA_FGM = "FGM",
    In_The_Paint_Non_RA_FGA = "FGA",
    In_The_Paint_Non_RA_FG_PCT = "FG%",
    Mid_Range_FGM = "FGM",
    Mid_Range_FGA = "FGA",
    Mid_Range_FG_PCT = "FG%",
    Left_Corner_3_FGM = "FGM",
    Left_Corner_3_FGA = "FGA",
    Left_Corner_3_FG_PCT = "FG%",
    Right_Corner_3_FGM = "FGM",
    Right_Corner_3_FGA = "FGA",
    Right_Corner_3_FG_PCT = "FG%",
    Above_the_Break_3_FGM = "FGM",
    Above_the_Break_3_FGA = "FGA",
    Above_the_Break_3_FG_PCT = "FG%",
    Backcourt_3_FGM = "FGM",
    Backcourt_3_FGA = "FGA",
    Backcourt_3_FG_PCT = "FG%"
  ) %>%
  tab_spanner(
    label = md("**Restricted_Area**"),
    columns = c(
      Restricted_Area_FGM,
      Restricted_Area_FGA,
      Restricted_Area_FG_PCT
    )
  ) %>%
  tab_spanner(
    label = md("**In The Paint**"),
    columns = c(
      In_The_Paint_Non_RA_FGM,
      In_The_Paint_Non_RA_FGA,
      In_The_Paint_Non_RA_FG_PCT
    )
  )  %>%
  tab_spanner(
    label = md("**Mid-Range**"),
    columns = c(Mid_Range_FGM, Mid_Range_FGA, Mid_Range_FG_PCT)
  ) %>%
  tab_spanner(
    label = md("**Left Corner 3P**"),
    columns = c(Left_Corner_3_FGM, Left_Corner_3_FGA, Left_Corner_3_FG_PCT)
  ) %>%
  tab_spanner(
    label = md("**Right Corner 3P**"),
    columns = c(
      Right_Corner_3_FGM,
      Right_Corner_3_FGA,
      Right_Corner_3_FG_PCT
    )
  ) %>%
  tab_spanner(
    label = md("**Above the Break**"),
    columns = c(
      Above_the_Break_3_FGM,
      Above_the_Break_3_FGA,
      Above_the_Break_3_FG_PCT
    )
  ) %>%
  tab_spanner(
    label = md("**Backcourt**"),
    columns = c(Backcourt_3_FGM, Backcourt_3_FGA, Backcourt_3_FG_PCT)
  ) %>%
  tab_style(
    style = cell_fill(color = "#d5f4e6"),
    locations = cells_body(columns = Right_Corner_3_FG_PCT,
                           rows = Right_Corner_3_FG_PCT >= .33)
  ) %>%
  tab_style(
    style = cell_fill(color = "#d5f4e6"),
    locations = cells_body(
      columns = ,
      Above_the_Break_3_FG_PCT,
      rows = Above_the_Break_3_FG_PCT >= .33
    )
  ) %>%
  tab_style(
    style = cell_fill(color = "#d5f4e6"),
    locations = cells_body(columns = Left_Corner_3_FG_PCT,
                           rows = Left_Corner_3_FG_PCT >= .33)
  ) %>%
  tab_style(
    style = cell_fill(color = "#fefbd8"),
    locations = cells_body(columns = Right_Corner_3_FG_PCT,
                           rows = Right_Corner_3_FG_PCT <= .33)
  ) %>%
  tab_style(
    style = cell_fill(color = "#f7cac9"),
    locations = cells_body(columns = Right_Corner_3_FG_PCT,
                           rows = Right_Corner_3_FG_PCT <= .25)
  ) %>%
  tab_style(
    style = cell_fill(color = "#fefbd8"),
    locations = cells_body(columns = Left_Corner_3_FG_PCT,
                           rows = Left_Corner_3_FG_PCT <= .32)
  ) %>%
  tab_style(
    style = cell_fill(color = "#f7cac9"),
    locations = cells_body(columns = Left_Corner_3_FG_PCT,
                           rows = Left_Corner_3_FG_PCT <= .25)
  ) %>%
  tab_style(
    style = cell_fill(color = "#fefbd8"),
    locations = cells_body(columns = Above_the_Break_3_FG_PCT,
                           rows = Above_the_Break_3_FG_PCT <= .32)
  ) %>%
  tab_style(
    style = cell_fill(color = "#f7cac9"),
    locations = cells_body(columns = Above_the_Break_3_FG_PCT,
                           rows = Above_the_Break_3_FG_PCT <= .25)
  ) %>%
  tab_style(
    style = cell_fill(color = "#d5f4e6"),
    locations = cells_body(columns = Restricted_Area_FG_PCT,
                           rows = Restricted_Area_FG_PCT >= .5)
  ) %>%
  tab_style(
    style = cell_fill(color = "#d5f4e6"),
    locations = cells_body(columns = In_The_Paint_Non_RA_FG_PCT,
                           rows = In_The_Paint_Non_RA_FG_PCT >= .5)
  ) %>%
  tab_style(
    style = cell_fill(color = "#d5f4e6"),
    locations = cells_body(columns = Mid_Range_FG_PCT,
                           rows = Mid_Range_FG_PCT >= .5)
  ) %>%
  tab_style(
    style = cell_fill(color = "#fefbd8"),
    locations = cells_body(columns = Restricted_Area_FG_PCT,
                           rows = Restricted_Area_FG_PCT <= .49)
  ) %>%
  tab_style(
    style = cell_fill(color = "#fefbd8"),
    locations = cells_body(columns = In_The_Paint_Non_RA_FG_PCT,
                           rows = In_The_Paint_Non_RA_FG_PCT < .50)
  ) %>%
  tab_style(
    style = cell_fill(color = "#fefbd8"),
    locations = cells_body(columns = Mid_Range_FG_PCT,
                           rows = Mid_Range_FG_PCT <= .49)
  ) %>%
  tab_style(
    style = cell_fill(color = "#f7cac9"),
    locations = cells_body(columns = Restricted_Area_FG_PCT,
                           rows = Restricted_Area_FG_PCT <= .35)
  ) %>%
  tab_style(
    style = cell_fill(color = "#f7cac9"),
    locations = cells_body(columns = In_The_Paint_Non_RA_FG_PCT,
                           rows = In_The_Paint_Non_RA_FG_PCT <= .35)
  ) %>%
  tab_style(
    style = cell_fill(color = "#f7cac9"),
    locations = cells_body(columns = Mid_Range_FG_PCT,
                           rows = Mid_Range_FG_PCT <= .35)
  ) %>%
  tab_options(
    summary_row.background.color = "red",
    grand_summary_row.background.color = "red",
    row_group.background.color = "#EAF2F8",
    heading.background.color = "#EAF2F8",
    column_labels.background.color = "#EAF2F8",
    stub.background.color = "#EAF2F8",
    table.font.color = "#323232",
    table_body.hlines.color = "black",
    table_body.border.top.color = "red",
    heading.border.bottom.color = "#989898",
    row_group.border.top.color = "#989898",
    row_group.border.bottom.style = "none",
    stub.border.style = "dashed",
    stub.border.color = "#989898",
    stub.border.width = "2px",
    summary_row.border.color = "red",
    table.font.name = "Sans-serif",
    heading.subtitle.font.size = "bold",
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    stub.font.weight = "bold",
    table.background.color = "#F2F3F4",
    table.width = "50%"
  )



### lineups ---------------------------------
team_lineups <-
  "https://stats.gleague.nba.com/stats/leaguedashlineups?Conference=&DateFrom=&DateTo=&Division=&GameID=&GameSegment=&GroupQuantity=3&LastNGames=0&LeagueID=20&Location=&MeasureType=Four+Factors&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&TeamID=0&VsConference=&VsDivision="
res8 <- GET(url = team_lineups, add_headers(.headers = headers))

json_resp8 <- fromJSON(content(res8, "text"))
team_lineups <- data.frame(json_resp8$resultSets$rowSet)


colnames(team_lineups) <-
  json_resp8[["resultSets"]][["headers"]][[1]]
#View(team_lineups)
colnames(team_lineups)
team_lineups <-
  team_lineups %>% select(-c(
    GROUP_ID,
    GROUP_ID,
    TEAM_ID,
    GROUP_SET,
    W_PCT,
    W_PCT_RANK,
    GP_RANK,
    W_RANK,
    L_RANK
  ))
team_lineups <-
  team_lineups %>% select(
    GROUP_NAME,
    TEAM_ABBREVIATION,
    GP,
    W,
    L,
    MIN,
    MIN_RANK,
    EFG_PCT,
    EFG_PCT_RANK,
    FTA_RATE,
    FTA_RATE_RANK,
    TM_TOV_PCT,
    TM_TOV_PCT_RANK,
    OREB_PCT,
    OREB_PCT_RANK,
    OPP_OREB_PCT,
    OPP_OREB_PCT_RANK
  )

team_lineups$EFG_PCT <- as.numeric(team_lineups$EFG_PCT)
team_lineups$TM_TOV_PCT <- as.numeric(team_lineups$TM_TOV_PCT)
team_lineups$OREB_PCT <- as.numeric(team_lineups$OREB_PCT)
team_lineups$FTA_RATE <- as.numeric(team_lineups$FTA_RATE)
team_lineups$OPP_OREB_PCT <- as.numeric(team_lineups$OPP_OREB_PCT)
team_lineups$MIN_RANK <- as.numeric(team_lineups$MIN_RANK)
team_lineups$MIN <- as.numeric(team_lineups$MIN)

write.csv(team_lineups,paste0('dataa/',"team_lineups.csv")
dim(team_lineups)
team_lineups %>% filter(MIN > 5)
team_lineups %>%  gt(rowname_col = "GROUP_NAME") %>%
  tab_header(title = md("**3- Man Lineups - Four Factors**")) %>%
  cols_label(
    TEAM_ABBREVIATION	 = "Team",
    EFG_PCT = "EFG%",
    EFG_PCT_RANK = md("*Rank*"),
    TM_TOV_PCT = "TO%",
    TM_TOV_PCT_RANK = md("*Rank*"),
    FTA_RATE = "FTA Rate",
    FTA_RATE_RANK = md("*Rank*"),
    OREB_PCT = "OREB",
    OREB_PCT_RANK = md("*Rank*"),
    OPP_OREB_PCT = "OPP OREB%",
    OPP_OREB_PCT_RANK = md("*Rank*"),
    GP = "Games Played",
    W = "Wins",
    L = "Losses",
    MIN = "Minutes",
    MIN_RANK = md("*Rank*")
  ) %>%
  text_transform(
    locations = cells_body(
      columns = c(
        EFG_PCT_RANK,
        TM_TOV_PCT_RANK,
        FTA_RATE_RANK,
        OREB_PCT_RANK,
        OPP_OREB_PCT_RANK,
        MIN_RANK
      )
    ),
    fn = function(x) {
      team <- word(x, -1)
      paste0("(", x, ")")
    }
  ) %>%
  tab_spanner(
    label = md("**Offense**"),
    columns = c(
      EFG_PCT,
      EFG_PCT_RANK,
      TM_TOV_PCT,
      TM_TOV_PCT_RANK,
      OREB_PCT,
      OREB_PCT_RANK,
      FTA_RATE,
      FTA_RATE_RANK
    )
  ) %>%
  tab_spanner(
    label =  md("**Defense**"),
    columns = c(OPP_OREB_PCT,
                OPP_OREB_PCT_RANK,)
  ) %>%
  tab_style(style = cell_text(color = "blue",
                              size = px(10)),
            locations = cells_body(
              columns = c(
                EFG_PCT_RANK,
                TM_TOV_PCT_RANK,
                FTA_RATE_RANK,
                OREB_PCT_RANK,
                OPP_OREB_PCT_RANK,
                MIN_RANK
              )
            )) %>%
  tab_style(style = cell_text(size = px(1)),
            locations = cells_column_labels(
              columns = c(
                EFG_PCT_RANK,
                TM_TOV_PCT_RANK,
                OREB_PCT_RANK,
                FTA_RATE_RANK,
                OPP_OREB_PCT_RANK,
                MIN_RANK
              )
            )) %>% fmt_percent(
              columns = c(EFG_PCT,
                          TM_TOV_PCT,
                          OREB_PCT,
                          FTA_RATE,
                          OPP_OREB_PCT),
              decimals = 1
            ) %>% cols_align(align = "center",
                             columns = everything())


team_lineups %>% arrange(MIN_RANK) %>% slice_head(n = 10)

team_lineups %>% arrange(TM_TOV_PCT) %>% slice_head(n = 10)
