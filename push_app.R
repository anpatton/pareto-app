rsconnect::setAccountInfo(
  name = "apanalytics",
  token = Sys.getenv("SHINY_TOKEN"),
  secret = Sys.getenv("SHINY_SECRET")
)

rsconnect::deployApp("~/pareto-app/pareto",
                     appId =  2235501,
                     forceUpdate = TRUE)