library(readr)
library(dplyr)
library(arrow)

headers <- c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'stats.nba.com',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

pareto_cols <- c("season_year", "player_id", "player_name", 
                 "team_name", "game_id", "game_date", "matchup", "min", 
                 "fgm", "fga", "fg3m", "fg3a", "ftm", "fta", 
                 "oreb", "dreb", "reb", "ast", "tov", "stl", "blk", "pf", "pts", "plusminus")

date_of_games <- Sys.Date() - 1

url_date <- format(as.Date(date_of_games), "%m/%d/%Y")

url <- paste0("https://stats.nba.com/stats/playergamelogs?DateFrom=", 
              url_date, 
              "&DateTo=",
              url_date, 
              "&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=Totals&Period=0&PlusMinus=N&Rank=N&Season=2024-25&SeasonSegment=&SeasonType=&ShotClockRange=&VsConference=&VsDivision=")

res <- httr::GET(url = url, httr::add_headers(.headers=headers))
json_resp <- jsonlite::fromJSON(httr::content(res, "text"))

new_logs <- data.frame(json_resp$resultSets$rowSet, stringsAsFactors = FALSE)

if(nrow(new_logs) == 0) {

  print(paste0("Exiting...no games on: ", date_of_games))

  nba_push <- FALSE

} else {

  colnames(new_logs) <- tolower(unlist(json_resp$resultSets$headers))
  new_logs <- new_logs |> 
    mutate(season_year = 2025) |> 
    mutate(player_id = as.numeric(player_id)) |> 
    mutate(across(c("player_id", "min", "fgm", "fga", "fg3m", "fg3a", "ftm", "fta", 
    "oreb", "dreb", "reb", "ast", "tov", "stl", "blk", "pf", "pts", "plusminus"), as.numeric)) |>
    select(all_of(pareto_cols)) |> 
    mutate(game_date = as.Date(game_date)) |> 
    mutate(fg2m = fgm - fg3m) |> 
    mutate(fg2a = fga - fg3a) |> 
    mutate(ts = round(pts/(2 * (fga + 0.44 * fta)), 3)) |> 
    mutate(code_name = paste0(player_name, " | ", player_id)) |> 
    mutate(type_season = "Regular Season")

  arrow::read_parquet("~/pareto-app/data/nba/nba_logs_2024-25_REG.parquet") |> 
    bind_rows(new_logs) |> 
    distinct() |>
    arrow::write_parquet("~/pareto-app/data/nba/nba_logs_2024-25_REG.parquet")
  
  arrow::read_parquet("~/pareto-app/pareto/nba_logs.parquet") |> 
    bind_rows(new_logs) |> 
    distinct() |>
    mutate(min = round(min, 1)) |> 
    arrow::write_parquet("~/pareto-app/pareto/nba_logs.parquet")

  arrow::write_parquet(new_logs, 
    paste0("~/pareto-app/data/nba/daily/logs_", 
           date_of_games,
            ".parquet"))
  
  saveRDS(date_of_games, "~/pareto-app/pareto/version_date.Rds")
            
  nba_push <- TRUE
}

          
  
  
  
          


