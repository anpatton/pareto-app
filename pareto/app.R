library(knitr)
library(kableExtra)
library(shinythemes)
library(shinycssloaders)
library(DT)
library(colorRamps)
library(grDevices)
library(tidyverse)
library(colorspace)
library(ggrepel)
library(lubridate)
library(DescTools)
library(rPref)
library(shinyjs)
library(markdown)

version_date <- readRDS("version_date.Rds")

logs <- arrow::read_parquet("nba_logs.parquet") |> 
  mutate(min = round(min, 2)) |> 
  mutate(idx = 1:n()) |> 
  drop_na(all_of(c(
    "pts", "fgm", "fga", "fg3m", "fg3a",
    "fg2m", "fg2a", "ftm", "fta", "ts", "reb", "oreb",
    "dreb", "ast", "stl", "tov", "blk", "pf", "min"
  )))

last_team <- logs |> 
  group_by(player_id, season_year) |> 
  arrange(game_date) |> 
  mutate(idx = 1:n()) |> 
  filter(idx == max(idx)) |> 
  select(player_id, season_year, team_name)

all_stats_nba <- c(
  "pts", "plusminus", "fgm", "fga", "fg3m", "fg3a",
  "fg2m", "fg2a", "ftm", "fta", "ts", "reb", "oreb",
  "dreb", "ast", "stl", "tov", "blk", "pf", "min"
)

all_players_box <- unique(logs$code_name)

#load("logs_nfl.rds")

#logs_nfl <- logs_nfl %>%
#  filter(is.na(season) == FALSE)

#modern_nfl_cols <- c("passing_air_yards", "passing_yards_after_catch", 
#                    "receiving_air_yards", "receiving_yards_after_catch")

#basic_nfl_cols <- c(
#  "player_id", "full_name", "recent_team", "position", "season",
#  "week", "season_type"
#)

#receiving_cols <- c(
#  "receptions", "targets", "receiving_yards", "receiving_tds",
#  "receiving_epa", "receiving_fumbles", "receiving_air_yards",
#  "receiving_yards_after_catch", "receiving_first_downs", "fantasy_points"
#)

#rushing_cols <- c(
#  "carries", "rushing_yards", "rushing_tds",
#  "rushing_epa", "rushing_fumbles", "rushing_first_downs", "fantasy_points"
#)

#passing_cols <- c(
#  "completions", "attempts", "passing_yards", "passing_tds",
#  "interceptions", "sacks", "sack_yards", "sack_fumbles",
#  "passing_air_yards", "passing_yards_after_catch", "passing_first_downs",
#  "passing_epa", "dakota", "carries",
#  "rushing_yards", "rushing_tds", "rushing_fumbles",
#  "rushing_first_downs", "rushing_epa", "fantasy_points"
#)

#all_stats_nfl <- unique(c(passing_cols, receiving_cols, rushing_cols))

#all_players_box_nfl <- unique(logs_nfl$code_name)

# last_team_nfl <- logs_nfl %>%
# group_by(player_id, season) %>%
# arrange(gameday) %>%
# mutate(idx = 1:n()) %>%
# filter(idx == max(idx)) %>%
# select(player_id, season, ree)

# all_players_season <- unique(season_logs$code_name)

#### SETUP APPLICATION ####
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: red;}"))
  ),
  navbarPage(
    "NBA + WNBA + NFL Pareto Exploration",
    tabPanel(
      "About Pareto",
      mainPanel(
        h2("Uniqueness and the Pareto Frontier"),
        includeMarkdown("explainer.md")
      )
    ),
    tabPanel(
      "NBA Single Game Box Scores",
      h4(paste0("Current as of: ", version_date),
        style = "color: #991D37"
      ),
      sidebarLayout(
        sidebarPanel(
          sliderInput(
            inputId = "bs_years",
            label = "What Years?",
            min = 1992,
            max = 2025,
            value = c(1992, 2025),
            step = 1,
            sep = ""
          ),
          selectInput(
            inputId = "bs_team",
            label = "What Team for Nodes?",
            choices = c("All", sort(unique(logs$team_name)))
          ),
          selectInput(
            inputId = "bs_season",
            label = "What Season Type?",
            choices = c("All", "Regular Season", "Playoffs")
          ),
          selectInput(
            inputId = "bs1",
            label = "Stat 1",
            choices = all_stats_nba,
            selected = "pts"
          ),
          radioButtons(
            inputId = "bs1_type",
            label = "High or Low?",
            choices = c("High", "Low")
          ),
          selectInput(
            inputId = "bs2",
            label = "Stat 2",
            choices = all_stats_nba,
            selected = "ast"
          ),
          radioButtons(
            inputId = "bs2_type",
            label = "High or Low?",
            choices = c("High", "Low")
          ),
          selectInput(
            inputId = "bsp1",
            label = "Highlight Player 1",
            choices = c(NA, all_players_box)
          ),
          selectInput(
            inputId = "bsp2",
            label = "Highlight Player 2",
            choices = c(NA, all_players_box)
          ),
          selectInput(
            inputId = "bsp3",
            label = "Highlight Player 3",
            choices = c(NA, all_players_box)
          ),
          actionButton(
            inputId = "make_pareto_box",
            label = "Make Chart"
          )
        ),
        mainPanel(
          plotOutput("pareto_chart_box") |> withSpinner(color = "#44a8f3"),
          br(),
          DT::dataTableOutput("pareto_box_table") |> withSpinner(color = "#44a8f3")
        )
      )
    )
  )
)#,
#    tabPanel(
#      "NFL Single Game Box Scores",
#      sidebarLayout(
#        sidebarPanel(
#          sliderInput(
#            inputId = "bs_years_nfl",
#            label = "What Years?",
#            min = 1999,
#            max = 2021,
#            value = c(1999, 2021),
#            step = 1,
#            sep = ""
#          ),
#          selectInput(
#            inputId = "bs_team_nfl",
#            label = "What Team for Nodes?",
#            choices = c("All", sort(unique(logs_nfl$recent_team)))
#          ),
#          selectInput(
#            inputId = "bs_season_nfl",
#            label = "What Season Type?",
#            choices = c("Regular Season" = "REG"),
#            selected = "Regular Season"
#          ),
#          selectInput(
#            inputId = "bs_pos_nfl",
#            label = "What Positions?",
#            choices = c("QB", "WR-TE-RB-FB", "All"),
#           selected = "QB"
#          ),
#          selectInput(
#            inputId = "bs1_nfl",
#            label = "Stat 1",
#            choices = all_stats_nfl,
#            selected = "completions"
#          ),
#          radioButtons(
#            inputId = "bs1_type_nfl",
#            label = "High or Low?",
#            choices = c("High", "Low"),
#           selected = "High"
#          ),
#          selectInput(
#            inputId = "bs2_nfl",
#           label = "Stat 2",
#            choices = all_stats_nfl,
#            selected = "passing_air_yards"
#          ),
#          radioButtons(
#            inputId = "bs2_type_nfl",
#            label = "High or Low?",
#            choices = c("High", "Low"),
#            selected = "High"
#         ),
#          selectInput(
#           inputId = "bsp1_nfl",
#            label = "Highlight Player 1",
#           choices = c(NA, all_players_box_nfl)
#          ),
#          selectInput(
#            inputId = "bsp2_nfl",
#            label = "Highlight Player 2",
#            choices = c(NA, all_players_box_nfl)
#          ),
#          selectInput(
#            inputId = "bsp3_nfl",
#            label = "Highlight Player 3",
#           choices = c(NA, all_players_box_nfl)
#          ),
#          actionButton(
#            inputId = "make_pareto_box_nfl",
#            label = "Make Chart"
#          )
#        ),
#        mainPanel(
#          plotOutput("pareto_chart_box_nfl") %>% withSpinner(color = "#44a8f3"),
#          br(),
#          DT::dataTableOutput("pareto_box_table_nfl") %>% withSpinner(color = "#44a8f3")
#        )
#      )
#    )


#### RENDER APPLICATION ####
server <- function(input, output, session) {
  observeEvent(input$bs_years, {
    updateSelectInput(session, "bs_team",
      choices = c(
        "All",
        sort(pull(unique(select(filter(logs, season_year <= max(input$bs_years) & season_year >= min(input$bs_years)), team_name))))
      )
    )
  })

#  observeEvent(input$bs_years_nfl, {
#    updateSelectInput(session, "bs_team_nfl",
#      choices = c(
#        "All",
#        sort(pull(unique(select(filter(logs_nfl, season <= max(input$bs_years_nfl) & season >= min(input$bs_years_nfl)), recent_team))))
#      )
#    )
#  })
  
#  #observeEvent(input$bs_pos_nfl, {
#  #  if (input$bs_pos_nfl == "All") {
#  #    cols_to_choose <- all_stats_nfl
# 
#      stat1 <- "fantasy_points"
#      stat2 <- "carries"
#    } else if (input$bs_pos_nfl == "QB") {
#      cols_to_choose <- unique(c(passing_cols, rushing_cols))#
#
#      stat1 <- "completions"
#      stat2 <- "passing_yards_after_catch"
#    } else {
#      cols_to_choose <- unique(c(receiving_cols, rushing_cols))#
#
#      stat1 <- "targets"
#      stat2 <- "receptions"
#    }
#
#    updateSelectInput(session, "bs1_nfl",
#      choices = cols_to_choose,
#      selected = stat1
#    )#
#
#    updateSelectInput(session, "bs2_nfl",
#      choices = cols_to_choose,
#      selected = stat2
#    )
#  })


  ########### -------- NBA -------- ############

  # pareto_box_labels <- eventReactive(input$make_pareto_box, {
  #   lab1 <- box_label_data %>%
  #     filter(box == input$bs1) %>%
  #     select(label) %>%
  #     pull()
  #
  #   lab2 <- box_label_data %>%
  #     filter(box == input$bs2) %>%
  #     select(label) %>%
  #     pull()
  #
  #   return(list(
  #     "lab1" = lab1,
  #     "lab2" = lab2
  #   ))
  # })

  pareto_box_data <- eventReactive(input$make_pareto_box, {
    if (input$bs_season != "All") {
      data_to_pareto <- logs |>
        filter(type_season == input$bs_season)
    } else {
      data_to_pareto <- logs
    }

    var1 <- sym(input$bs1)
    var2 <- sym(input$bs2)

    if (var1 == var2) {
      showNotification("Please pick two different box score stats.",
        type = "error",
        duration = 5
      )

      return(NULL)
    }


    if (input$bs1 == "plusminus" | input$bs2 == "plusminus") {
      data_to_pareto <- na.omit(data_to_pareto)
    } # else {

    # data_to_pareto <- data_to_pareto

    # }

    if (input$bs_team != "All") {
      data_to_pareto <- data_to_pareto |>
        filter(team_name == input$bs_team)
    }

    data_to_pareto <- data_to_pareto |>
      filter(season_year >= min(input$bs_years)) |>
      filter(season_year <= max(input$bs_years))

    if (input$bs1_type == "High" & input$bs2_type == "High") {
      res <- psel(data_to_pareto, high(eval(var1)) * high(eval(var2)))
    } else if (input$bs1_type == "High" & input$bs2_type == "Low") {
      res <- psel(data_to_pareto, high(eval(var1)) * low(eval(var2)))
    } else if (input$bs1_type == "Low" & input$bs2_type == "High") {
      res <- psel(data_to_pareto, low(eval(var1)) * high(eval(var2)))
    } else if (input$bs1_type == "Low" & input$bs2_type == "Low") {
      res <- psel(data_to_pareto, low(eval(var1)) * low(eval(var2)))
    }

    res <- res |>
      mutate(label = paste0(
        player_name, " (", !!var1, " ",
        # pareto_box_labels()[["lab1"]],
        toupper(input$bs1),
        " | ", !!var2, " ",
        # pareto_box_labels()[["lab2"]], ")"
        toupper(input$bs2)
      )) |>
      group_by(!!var1, !!var2) |>
      top_n(n = 1, wt = -idx)

    return(res)
  })

  output$pareto_chart_box <- renderPlot({
    validate(
      need(is.null(pareto_box_data()) == FALSE, "Please make a valid selection.")
    )

    check <- input$make_pareto_box

    isolate({
      if (input$bs_season == "Regular Season") {
        data_to_pareto <- logs |>
          filter(type_season == "Regular Season")

        season_lab <- "Regular Season "
      } else if (input$bs_season == "Playoffs") {
        data_to_pareto <- logs |>
          filter(type_season == "Playoffs")

        season_lab <- "Playoffs "
      } else if (input$bs_season == "All") {
        data_to_pareto <- logs
        season_lab <- ""
      }

      plot_df <- data_to_pareto |>
        filter(!idx %in% pareto_box_data()$idx) |>
        sample_n(2000, replace = TRUE) |>
        filter(season_year >= min(input$bs_years)) |>
        filter(season_year <= max(input$bs_years))

      players_in <- c(input$bsp1, input$bsp3, input$bsp2) |>
        na.omit()

      highlight_df <- data_to_pareto |>
        filter(code_name %in% players_in) |>
        filter(season_year >= min(input$bs_years)) |>
        filter(season_year <= max(input$bs_years))

      if (input$bs1 == "plusminus" | input$bs2 == "plusminus") {
        caption_lab <- "+/- since 2000 | @anpatt7.bsky.social | https://apanalytics.shinyapps.io/pareto/"
      } else {
        caption_lab <- "@anpatt7.bsky.social | https://apanalytics.shinyapps.io/pareto/"
      }

      if (input$bs_team == "All") {
        subtitle_lab <- "Nodes from All Teams"
      } else {
        subtitle_lab <- paste0("Nodes from ", input$bs_team)
      }

      if (min(input$bs_years) != max(input$bs_years)) {
        title_lab <- paste0(season_lab, "Pareto Box Score Frontier: ", min(input$bs_years), "-", max(input$bs_years), " Seasons")
      } else {
        title_lab <- paste0(season_lab, "Pareto Box Score Frontier: ", min(input$bs_years), " Season")
      }


      if (length(unique(pareto_box_data()$type_season)) > 1) {
        ggplot(pareto_box_data(), aes_string(x = input$bs1, y = input$bs2)) +
          guides(color = guide_legend(override.aes = list(alpha = 1))) +
          geom_point(data = plot_df, aes_string(x = input$bs1, y = input$bs2), shape = 19, alpha = 0.20, color = "gray", size = 4) +
          geom_point(data = highlight_df, aes_string(x = input$bs1, y = input$bs2, color = "code_name"), shape = 19, alpha = 0.40, size = 4) +
          geom_step(direction = "vh") +
          geom_point(size = 5, fill = "black", aes(shape = type_season)) +
          scale_shape_manual(values = c(17, 15), name = "Season") +
          geom_text_repel(aes(label = label),
            size = 4, box.padding = 0.5, nudge_y = 1
          ) +
          scale_color_manual(name = "", values = c("#006BB6", "#ED174C", "#BB9754")) +
          labs(
            x = toupper(input$bs1),
            y = toupper(input$bs2),
            title = title_lab,
            subtitle = subtitle_lab,
            caption = caption_lab
          ) +
          theme_bw(base_size = 18) 
        
      } else {
        ggplot(pareto_box_data(), aes_string(x = input$bs1, y = input$bs2)) +
          guides(color = guide_legend(override.aes = list(alpha = 1))) +
          geom_point(data = plot_df, aes_string(x = input$bs1, y = input$bs2), shape = 19, alpha = 0.20, color = "gray", size = 4) +
          geom_point(data = highlight_df, aes_string(x = input$bs1, y = input$bs2, color = "code_name"), shape = 19, alpha = 0.40, size = 4) +
          geom_step(direction = "vh") +
          geom_point(size = 5, fill = "black", shape = 17) +
          geom_text_repel(aes(label = label),
            size = 4, box.padding = 0.5, nudge_y = 1
          ) +
          scale_color_manual(name = "", values = c("#006BB6", "#ED174C", "#BB9754")) +
          labs(
            x = toupper(input$bs1),
            y = toupper(input$bs2),
            title = title_lab,
            subtitle = subtitle_lab,
            caption = caption_lab
          ) +
          theme_bw(base_size = 18) 
      }
    })
  })

  output$pareto_box_table <- DT::renderDataTable({
    validate(
      need(is.null(pareto_box_data()) == FALSE, "Please make a valid selection.")
    )

    tab_data <- pareto_box_data() |>
      arrange(game_date) |>
      select(-c(player_id, idx, code_name, label, team_name, season_full)) |>
      rename(
        Date = game_date,
        Player = player_name,
        Game = matchup,
        Season = season_year,
        Type = type_season
      ) |>
      mutate(Game = paste0("<a href='", "https://nba.com/game/", game_id, "'>", Game, "</a>")) |>
      select(-game_id)

    DT::datatable(tab_data,
      rownames = FALSE,
      caption = "Pareto Frontier Nodes",
      escape = FALSE,
      options = list(
        pageLength = 10,
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      ),
      selection = "none"
    )
  })
}

shinyApp(ui = ui, server = server)
