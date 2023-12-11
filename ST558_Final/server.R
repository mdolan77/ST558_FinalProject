# Author: Michael Dolan
# Date: 12/12/2023
# Purpose of Program: Final Project (Server file)

# Read in necessary packages
library(httr)
library(jsonlite)
library(shiny)
library(tidyverse)
library(DT)

# The below function was created in Project 2 to pull a dataset of NBA game stats
# for a specified player. It takes in arguments for the player's first and last name,
# seasons you want to look at, the stats you want to look at, and whether you want
# just postseason vs. regular season games.

player_game_stats <- function(first_name=NULL, last_name=NULL,
                              season=NULL, stats=everything(),
                              postseason=NULL, ...){
  
  # Return Error message if First and Last name not specified
  if (is.null(first_name) | is.null(last_name))
  {message("Error: First and last name of an NBA player must be specified.")}
  else {
    
    # Find the player's PlayerID to be used later
    player_search_url <- paste("https://www.balldontlie.io/api/v1/players/?search",
                               paste(first_name, last_name, sep = "+"), sep = "=")
    player_search <- GET(url=player_search_url)
    player_info <- fromJSON(rawToChar(player_search$content))
    playerID <- player_info$data[1,1]
    
    # Use the PlayerID to return the players stats for
    # every game in a season or several seasons
    stats_url <- paste0("https://www.balldontlie.io/api/v1/stats/?per_page=100",
                        if_else(is.null(season), "",
                                paste0("&seasons[]=",
                                       paste(season, collapse="&seasons[]=")
                                )),
                        paste0("&player_ids[]=", playerID),
                        ifelse(is.null(postseason), "",
                               paste0("&postseason=", postseason))
    ) 
    player_stats_api <- GET(url = stats_url)
    player_content <-
      fromJSON(rawToChar(player_stats_api$content))
    player_stats1 <- player_content$data
    
    # This API only allows 100 rows to be returned at a time,
    # so if there are more than 100, a loop is initialized.
    # The loop pulls the data for each page after the first,
    # and stores all pages into a list.
    total_pages <- player_content$meta$total_pages
    if (total_pages > 1) {
      page_list <- list()
      page_list[[1]] <- player_stats1
      for (i in 2:total_pages) {
        stats_url <- paste0("https://www.balldontlie.io/api/v1/stats/?per_page=100",
                            paste0("&page=", i),
                            if_else(is.null(season), "",
                                    paste0("&seasons[]=",
                                           paste(season, collapse="&seasons[]=")
                                    )),
                            paste0("&player_ids[]=", playerID),
                            ifelse(is.null(postseason), "",
                                   paste0("&postseason=", postseason))
        ) 
        player_stats_api <- GET(url = stats_url)
        player_content <-
          fromJSON(rawToChar(player_stats_api$content))
        page_list[[i]] <- player_content$data
      }
      
      # Each dataset in the list is combine vertically
      player_stats <- bind_rows(page_list)
    }
    
    # If there is only one page,
    # that page is stored as player_stats
    else{player_stats <- player_stats1}
    
    # Create a cleaner date variable
    player_stats$date <-
      substr(player_stats$game$date, 1, 10)
    
    #Create visible season variable and convert it to factor
    player_stats$season <- as.factor(player_stats$game$season)
    
    # Create a Home/Away game variable
    player_stats$home_away <-
      as.factor(if_else(player_stats$team$id == player_stats$game$home_team_id, "Home", "Away"))
    
    # Create a Win/Loss game variable
    player_stats$win_loss <-
      as.factor(if_else((player_stats$home_away == "Home" &
                 player_stats$game$home_team_score > 
                 player_stats$game$visitor_team_score) | 
                (player_stats$home_away == "Away" &
                   player_stats$game$visitor_team_score > 
                   player_stats$game$home_team_score),
              "Win", "Loss"))
    
    # Create visible score variables
    player_stats$team_score <- if_else(
      player_stats$home_away == "Home",
      player_stats$game$home_team_score,
      player_stats$game$visitor_team_score)
    player_stats$opponent_score <- if_else(
      player_stats$home_away == "Home",
      player_stats$game$visitor_team_score,
      player_stats$game$home_team_score)
    
    # Create a visible Post-Season variable
    player_stats$post_season <- as.factor(player_stats$game$postseason)
    
    # Create a visible player_team variable
    player_stats$player_team <- as.factor(player_stats$team$full_name)
    
    # Create opponent_team_id variable
    player_stats$opponent_team_id <-
      if_else(player_stats$home_away=="Home",
              player_stats$game$visitor_team_id,
              player_stats$game$home_team_id)
    
    # Create an opponent_team variable
    # Note: The original dataset only provides team ID
    # for the opposing team.
    # Instead of pulling a team dataset with another
    # API URL and merging the two datasets,
    # I decided to create a codebook of teams
    # since there are only 30 that need hardcoding.
    player_stats$opponent_team <-
      as.factor(
      if_else(player_stats$opponent_team_id==1, "Atlanta Hawks",
      if_else(player_stats$opponent_team_id==2,"Boston Celtics",
      if_else(player_stats$opponent_team_id==3,"Brooklyn Nets",
      if_else(player_stats$opponent_team_id==4,"Charlotte Hornets",
      if_else(player_stats$opponent_team_id==5,"Chicago Bulls",
      if_else(player_stats$opponent_team_id==6,"Cleveland Cavaliers",
      if_else(player_stats$opponent_team_id==7,"Dallas Mavericks",
      if_else(player_stats$opponent_team_id==8,"Denver Nuggets",
      if_else(player_stats$opponent_team_id==9,"Detroit Pistons",
      if_else(player_stats$opponent_team_id==10,"Golden State Warriors",
      if_else(player_stats$opponent_team_id==11,"Houston Rockets",
      if_else(player_stats$opponent_team_id==12,"Indiana Pacers",
      if_else(player_stats$opponent_team_id==13,"LA Clippers",
      if_else(player_stats$opponent_team_id==14,"Los Angeles Lakers",
      if_else(player_stats$opponent_team_id==15,"Memphis Grizzlies",
      if_else(player_stats$opponent_team_id==16,"Miami Heat",
      if_else(player_stats$opponent_team_id==17,"Milwaukee Bucks",
      if_else(player_stats$opponent_team_id==18,"Minnesota Timberwolves",
      if_else(player_stats$opponent_team_id==19,"New Orleans Pelicans",
      if_else(player_stats$opponent_team_id==20,"New York Knicks",
      if_else(player_stats$opponent_team_id==21,"Oklahoma City Thunder",
      if_else(player_stats$opponent_team_id==22,"Orlando Magic",
      if_else(player_stats$opponent_team_id==23,"Philadelphia 76ers",
      if_else(player_stats$opponent_team_id==24,"Phoenix Suns",
      if_else(player_stats$opponent_team_id==25,"Portland Trail Blazers",
      if_else(player_stats$opponent_team_id==26,"Sacramento Kings",
      if_else(player_stats$opponent_team_id==27,"San Antonio Spurs",
      if_else(player_stats$opponent_team_id==28,"Toronto Raptors",
      if_else(player_stats$opponent_team_id==29,"Utah Jazz",
      if_else(player_stats$opponent_team_id==30,"Washington Wizards", "ERROR"
      )))))))))))))))))))))))))))))))
    
    # Adjust percentage stats to fix outliers input as whole numbers
    player_stats$fg_pct <- if_else(player_stats$fg_pct>1, 
                                   player_stats$fg_pct/100,
                                   player_stats$fg_pct)
    
    player_stats$ft_pct <- if_else(player_stats$ft_pct>1, 
                                   player_stats$ft_pct/100,
                                   player_stats$ft_pct)
    
    player_stats$fg3_pct <- if_else(player_stats$fg3_pct>1, 
                                    player_stats$fg3_pct/100,
                                    player_stats$fg3_pct)
    
    # Sort games by date
    player_stats_sorted <- arrange(player_stats, date)
    
    # Select important stats and reorder columns
    player_stats_clean <- player_stats_sorted %>%
      select(date, min, pts, reb, ast, stl, blk, fgm, fga,
             fg_pct, fg3m, fg3a, fg3_pct, ftm, fta, ft_pct,
             oreb, dreb, turnover, pf, home_away, win_loss,
             player_team, team_score, opponent_team,
             opponent_score, post_season, season)
    
    # Allow user to select variables and order,
    # but always keeping the date for reference
    player_stats_final <- player_stats_clean %>%
      select(date, all_of(stats))
    
    # Print final player stats as a tibble
    as_tibble(player_stats_final)
  }
}
#End of Function


# Define server logic
shinyServer(function(input, output, session) {
  
  observeEvent(input$create_data, {
    if(input$season_option == "select_seasons"){
    Player_Data <- player_game_stats(first_name = input$first_name,
                                     last_name = input$last_name,
                                     season = c(input$seasons[1]:input$seasons[2]))
    }else{Player_Data <- player_game_stats(first_name = input$first_name,
                                           last_name = input$last_name)}
    output$data_table <- renderDataTable({datatable(Player_Data)})
  })
  
  # Update offensive rebound max so it does not exceed rebounds for GLM prediction input
  observe({if("reb" %in% input$glm_vars){
      updateNumericInput(session, "glm_oreb", max = input$glm_reb)}
    })
  
  # Update offensive rebound max so it does not exceed rebounds for random forest prediction input
  observe({if("reb" %in% input$rf_vars){
    updateNumericInput(session, "rf_oreb", max = input$rf_reb)}
  })
  
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')

    })

})
