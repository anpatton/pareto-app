source("~pareto-app/get_nba_gamelogs.R")
#source("~pareto-app/get_wnba_gamelogs.R")
#source("~pareto-app/get_nfl_gamelogs.R")

wbna_push <- FALSE
nfl_push <- FALSE


if(any(c(nba_push, wnba_push, nfl_push))) {
   
  source("push_app.R")

} else {

  print("No updates. Exiting.")

}