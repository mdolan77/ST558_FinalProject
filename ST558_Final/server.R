# Author: Michael Dolan
# Date: 12/12/2023
# Purpose of Program: Final Project (Server file)

# Read in necessary packages
library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(DT)
library(caret)
library(randomForest)
library(glmnet)

# The below function was created in Project 2 to pull a dataset of NBA game stats
# for a specified player. It takes in arguments for the player's first and last name,
# seasons you want to look at, the stats you want to look at, and whether you want
# just postseason vs. regular season games.

player_game_stats <- function(first_name=NULL, last_name=NULL,
                              season=NULL, stats=everything(),
                              postseason=NULL, ...){
  
  # Return Error message if First and Last name not specified
  if (is.null(first_name) | is.null(last_name))
  {stop("Error: First and last name of an NBA player must be specified.")}
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
    }else if(total_pages==0){stop("Error: Check name and/or season inputs")}
    
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
    player_stats$post_season <- as.factor(if_else(player_stats$game$postseason == TRUE, "Playoffs",
                                                  "Non-Playoffs"))
    
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
    
    # Function to convert "min:sec" to numeric
    convert_to_numeric <- function(time_string) {
      parts <- strsplit(time_string, ":")[[1]]
      
      if (length(parts) == 1) {
        # If there's no colon, assume it's just the number of minutes
        total_minutes <- as.numeric(parts[1])
      } else {
        # If there's a colon, calculate total minutes
        minutes <- as.numeric(parts[1])
        seconds <- as.numeric(parts[2])
        total_minutes <- minutes + seconds / 60
      }
      
      return(total_minutes)
    }
    player_stats$min <- round(as.numeric(sapply(player_stats$min, convert_to_numeric)), 2)
    
    # Filter out DNPs
    player_stats_filtered <- player_stats %>% filter(!is.na(min) & min != 0)
    
    # Sort games by date
    player_stats_sorted <- arrange(player_stats_filtered, date)
    
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

  
  # Create data frame for player with above function, or display error message
  # if function does not execute properly.
  Player_Data <- eventReactive(input$create_data, {
    tryCatch({
        if(input$season_option == "select_seasons"){
              player_game_stats(first_name = input$first_name,
                                     last_name = input$last_name,
                                     season = c(input$seasons[1]:input$seasons[2]))
        }else{player_game_stats(first_name = input$first_name,
                                           last_name = input$last_name)}
    },
    error = function(e){data.frame(Error = "Check name and/or season inputs")})
    })
  observeEvent(input$create_data, {output$data_table <- renderDataTable({datatable(Player_Data())})})
  
    
    output$Plot <- renderPlot({
      
      # Generate bar plot
      if(input$plot_type == "bar" && input$group_option_bar == 1){
        ggplot(Player_Data(), aes_string(x=input$x_var_bar, y=input$y_var_bar)) +
          stat_summary(fun = input$stat_type_bar, geom = "bar", position = "dodge") +
          labs(title = "Bar Plot", x = input$x_var_bar, y=input$y_var_bar) +
          facet_wrap(~ get(input$group_var_bar))
      }else if(input$plot_type == "bar"){
        ggplot(Player_Data(), aes_string(x=input$x_var_bar, y=input$y_var_bar)) +
          stat_summary(fun = input$stat_type_bar, geom = "bar", position = "dodge") +
          labs(title = "Bar Plot", x = input$x_var_bar, y=input$y_var_bar)
      
      # Generate histogram
      }else if(input$plot_type == "hist" && input$group_option_hist == 1){
        x <- Player_Data()[,input$var_hist]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        ggplot(Player_Data(), aes_string(x=input$var_hist)) +
          geom_histogram(breaks=bins) +
          labs(title = "Histogram", x=input$var_hist)
          facet_wrap(~ get(input$group_var_hist))
      }else if(input$plot_type == "hist"){
        x <- Player_Data()[,input$var_hist]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        ggplot(Player_Data(), aes_string(x=input$var_hist)) +
          geom_histogram(breaks=bins) +
          labs(title = "Histogram", x=input$var_hist)
      
      #Generate box plot
      }else if(input$plot_type == "box" && input$group_option_box == 1){
        ggplot(Player_Data(), aes_string(x=input$x_var_box, y=input$y_var_box)) +
          geom_boxplot() +
          labs(title = "Box and Whisker Plot", x = input$x_var_box, y=input$y_var_box) +
          facet_wrap(~ get(input$group_var_box))
      }else if(input$plot_type == "box"){
        ggplot(Player_Data(), aes_string(x=input$x_var_box, y=input$y_var_box)) +
          geom_boxplot() +
          labs(title = "Box and Whisker Plot", x = input$x_var_box, y=input$y_var_box)
      
      # Generate scatter plot
      }else if(input$plot_type == "scatter" && input$group_option_scatter == 1){
        ggplot(Player_Data(), aes_string(x=input$x_var_scatter, y=input$y_var_scatter)) + 
          geom_point(position="jitter") + 
          labs(title = "Scatter Plot", x = input$x_var_scatter, y=input$y_var_scatter) + 
          facet_wrap(~ get(input$group_var_scatter))
      }else if(input$plot_type == "scatter"){
        ggplot(Player_Data(), aes_string(x=input$x_var_scatter, y=input$y_var_scatter)) + 
          geom_point(position="jitter") + 
          labs(title = "Scatter Plot", x = input$x_var_scatter, y=input$y_var_scatter)
      }
    })
      
      summary_tab <- reactive({
        
        # Generate Mean/Median Summary Tables for bar plot
        if(input$plot_type == "bar" && input$group_option_bar == 1 &&
           input$stat_type_bar == "mean"){
          Player_Data() %>% group_by(!!sym(input$x_var_bar), !!sym(input$group_var_bar)) %>%
          summarize(Mean = round(mean(!!sym(input$y_var_bar), na.rm = TRUE), 3),
                    SD = round(sd(!!sym(input$y_var_bar), na.rm = TRUE), 3))
        }else if(input$plot_type == "bar" && input$stat_type_bar == "mean"){
          Player_Data() %>% group_by(!!sym(input$x_var_bar)) %>%
          summarize(Mean = round(mean(!!sym(input$y_var_bar), na.rm = TRUE), 3),
                    SD = round(sd(!!sym(input$y_var_bar), na.rm = TRUE), 3))
        }else if(input$plot_type == "bar" && input$group_option_bar == 1 &&
                 input$stat_type_bar == "median"){
          Player_Data() %>% group_by(!!sym(input$x_var_bar), !!sym(input$group_var_bar)) %>%
          summarize(Q1 = round(quantile(!!sym(input$y_var_bar), na.rm = TRUE, prob=.25), 3),
                    Median = round(median(!!sym(input$y_var_bar), na.rm = TRUE), 3),
                    Q3 = round(quantile(!!sym(input$y_var_bar), na.rm = TRUE, prob=.75), 3))
        }else if(input$plot_type == "bar" && input$stat_type_bar == "median"){
          Player_Data() %>% group_by(!!sym(input$x_var_bar)) %>%
          summarize(Q1 = round(quantile(!!sym(input$y_var_bar), na.rm = TRUE, prob=.25), 3),
                    Median = round(median(!!sym(input$y_var_bar), na.rm = TRUE), 3),
                    Q3 = round(quantile(!!sym(input$y_var_bar), na.rm = TRUE, prob=.75), 3))
        
        # Generate Mean/Median Summary Tables for histogram
        }else if(input$plot_type == "hist" && input$group_option_hist == 1 &&
                 input$stat_type_hist == "mean"){
          Player_Data() %>% group_by(!!sym(input$group_var_hist)) %>%
          summarize(Mean = round(mean(!!sym(input$var_hist), na.rm = TRUE), 3),
                    SD = round(sd(!!sym(input$var_hist), na.rm = TRUE), 3))
        }else if(input$plot_type == "hist" && input$stat_type_hist == "mean"){
          Player_Data() %>%
          summarize(Mean = round(mean(!!sym(input$var_hist), na.rm = TRUE), 3),
                    SD = round(sd(!!sym(input$var_hist), na.rm = TRUE), 3))
        }else if(input$plot_type == "hist" && input$group_option_hist == 1 &&
                 input$stat_type_hist == "median"){
          Player_Data() %>% group_by(!!sym(input$group_var_hist)) %>%
          summarize(Q1 = round(quantile(!!sym(input$var_hist), na.rm = TRUE, prob=.25), 3),
                    Median = round(median(!!sym(input$var_hist), na.rm = TRUE), 3),
                    Q3 = round(quantile(!!sym(input$var_hist), na.rm = TRUE, prob=.75), 3))
        }else if(input$plot_type == "hist" && input$stat_type_hist == "median"){
          Player_Data() %>%
          summarize(Q1 = round(quantile(!!sym(input$var_hist), na.rm = TRUE, prob=.25), 3),
                    Median = round(median(!!sym(input$var_hist), na.rm = TRUE), 3),
                    Q3 = round(quantile(!!sym(input$var_hist), na.rm = TRUE, prob=.75), 3))
        
        # Generate Mean/Median Summary Tables for box plot
        }else if(input$plot_type == "box" && input$group_option_box == 1 &&
                 input$stat_type_box == "mean"){
          Player_Data() %>% group_by(!!sym(input$x_var_box), !!sym(input$group_var_box)) %>%
          summarize(Mean = round(mean(!!sym(input$y_var_box), na.rm = TRUE), 3),
                    SD = round(sd(!!sym(input$y_var_box), na.rm = TRUE), 3))
        }else if(input$plot_type == "box" && input$stat_type_box == "mean"){
          Player_Data() %>% group_by(!!sym(input$x_var_box)) %>%
          summarize(Mean = round(mean(!!sym(input$y_var_box), na.rm = TRUE), 3),
                    SD = round(sd(!!sym(input$y_var_box), na.rm = TRUE), 3))
        }else if(input$plot_type == "box" && input$group_option_box == 1 &&
                 input$stat_type_box == "median"){
          Player_Data() %>% group_by(!!sym(input$x_var_box), !!sym(input$group_var_box)) %>%
          summarize(Q1 = round(quantile(!!sym(input$y_var_box), na.rm = TRUE, prob=.25), 3),
                    Median = round(median(!!sym(input$y_var_box), na.rm = TRUE), 3),
                    Q3 = round(quantile(!!sym(input$y_var_box), na.rm = TRUE, prob=.75), 3))
        }else if(input$plot_type == "box" && input$stat_type_box == "median"){
          Player_Data() %>% group_by(!!sym(input$x_var_box)) %>%
          summarize(Q1 = round(quantile(!!sym(input$y_var_box), na.rm = TRUE, prob=.25), 3),
                    Median = round(median(!!sym(input$y_var_box), na.rm = TRUE), 3),
                    Q3 = round(quantile(!!sym(input$y_var_box), na.rm = TRUE, prob=.75), 3))
          
        # Generate Mean/Median Summary Tables for scatter plot
        }else if(input$plot_type == "scatter" && input$group_option_scatter == 1 &&
                 input$stat_type_scatter == "mean"){
          Player_Data() %>% group_by(!!sym(input$group_var_scatter)) %>%
          summarize(Mean_X = round(mean(!!sym(input$x_var_scatter), na.rm = TRUE), 3),
                    SD_X = round(sd(!!sym(input$x_var_scatter), na.rm = TRUE), 3),
                    Mean_Y = round(mean(!!sym(input$y_var_scatter), na.rm = TRUE), 3),
                    SD_Y = round(sd(!!sym(input$y_var_scatter), na.rm = TRUE), 3))
        }else if(input$plot_type == "scatter" && input$stat_type_scatter == "mean"){
          Player_Data() %>%
          summarize(Mean_X = round(mean(!!sym(input$x_var_scatter), na.rm = TRUE), 3),
                    SD_X = round(sd(!!sym(input$x_var_scatter), na.rm = TRUE), 3),
                    Mean_Y = round(mean(!!sym(input$y_var_scatter), na.rm = TRUE), 3),
                    SD_Y = round(sd(!!sym(input$y_var_scatter), na.rm = TRUE), 3))
        }else if(input$plot_type == "scatter" && input$group_option_scatter == 1 &&
                 input$stat_type_scatter == "median"){
          Player_Data() %>% group_by(!!sym(input$group_var_scatter)) %>%
          summarize(Q1_X = round(quantile(!!sym(input$x_var_scatter), na.rm = TRUE, prob=.25), 3),
                    Median_X = round(median(!!sym(input$x_var_scatter), na.rm = TRUE), 3),
                    Q3_X = round(quantile(!!sym(input$x_var_scatter), na.rm = TRUE, prob=.75), 3),
                    Q1_Y = round(quantile(!!sym(input$y_var_scatter), na.rm = TRUE, prob=.25), 3),
                    Median_Y = round(median(!!sym(input$y_var_scatter), na.rm = TRUE), 3),
                    Q3_Y = round(quantile(!!sym(input$y_var_scatter), na.rm = TRUE, prob=.75), 3))
        }else if(input$plot_type == "scatter" && input$stat_type_scatter == "median"){
          Player_Data() %>%
          summarize(Q1_X = round(quantile(!!sym(input$x_var_scatter), na.rm = TRUE, prob=.25), 3),
                    Median_X = round(median(!!sym(input$x_var_scatter), na.rm = TRUE), 3),
                    Q3_X = round(quantile(!!sym(input$x_var_scatter), na.rm = TRUE, prob=.75), 3),
                    Q1_Y = round(quantile(!!sym(input$y_var_scatter), na.rm = TRUE, prob=.25), 3),
                    Median_Y = round(median(!!sym(input$y_var_scatter), na.rm = TRUE), 3),
                    Q3_Y = round(quantile(!!sym(input$y_var_scatter), na.rm = TRUE, prob=.75), 3))
        }
      })
    
    # Output summary table
    output$Summary_table <- renderDT({datatable(summary_tab())
      })
    
    # Reactive expression for GLM predictors
    predictors_GLM <- reactive({
      req(input$glm_vars)
      return(input$glm_vars)
    })
    
    # Reactive expression for random forest predictors
    predictors_rf <- reactive({
      req(input$rf_vars)
      return(input$rf_vars)
    })
    
    # Training the models
    trained_models <- eventReactive(input$fit_models, {
      withProgress(message = 'Training models...', detail = 'This may take some time...', value = 0, {
        for (i in 1:15) {
          incProgress(1/15)
          Sys.sleep(0.1)
        }
      
      # Create formulas to be input into the train function
      formula_GLM <- as.formula(paste("win_loss ~", paste(predictors_GLM(), collapse = "+")))
      formula_rf <- as.formula(paste("win_loss ~", paste(predictors_rf(), collapse = "+")))
      
      # Remove rows with missing values
      Player_Data_no_na <- na.omit(Player_Data())
      
      # Create training and test data
      index <- createDataPartition(Player_Data_no_na$win_loss, p=input$training_set/100, list = FALSE)
      train_data <- Player_Data_no_na[index, ]
      test_data <- Player_Data_no_na[-index, ]
      
      # Create cross-validation parameters
      ctrl <- trainControl(
        method = "cv",
        number = input$cv)
      
      ntree_rf <- input$ntree
      
      # Train GLM Model
      withProgress(message = 'Training Random Forest model...', value = 0, {
       GLM_Model <- train(
        formula_GLM,
        data = train_data,
        method = "glm",
        family = binomial,
        trControl = ctrl
       )
       Sys.sleep(0.5)
       incProgress(100)
      })
      
      # Train Random Forest Model
      withProgress(message = 'Training Random Forest model...', value = 0, {
       RF_Model <- train(
        formula_rf,
        data = train_data,
        method = "rf",
        trControl = ctrl,
        tuneGrid = data.frame(mtry=input$mtry),
        ntree = ntree_rf
       )
       Sys.sleep(0.5)
       incProgress(100)
      })
      
      # Return list including models and data sets
      return(list(RF_Model = RF_Model, GLM_Model = GLM_Model, train_data = train_data, test_data = test_data))
      })
    })
    
    # Output results of the models
    output$modelSummaries <- renderPrint({
      models <- trained_models()
      
      cat("\n\nGLM Model Summary:\n")
      print(summary(models$GLM_Model))
      
      cat("Random Forest Model Summary:\n")
      print(summary(models$RF_Model))
      
      cat("\n\nGLM Model Results (Training Set):\n")
      print(models$GLM_Model$results)
      
      cat("Random Forest Model Results (Training Set):\n")
      print(models$RF_Model$results)
      
      cat("\n\nGLM Model Results (Test Set):\n")
      print(confusionMatrix(data = models$test_data$win_loss,
                            reference=predict(models$GLM_Model,
                                              newdata = models$test_data)))
      
      cat("\n\nRandom Forest Model Results (Test Set):\n")
      print(confusionMatrix(data = models$test_data$win_loss,
                            reference=predict(models$RF_Model,
                                              newdata = models$test_data)))
      
    })
    
    # Update mtry max based on how many variables are selected in random forest
    selected_vars_rf <- reactive({input$rf_vars})
    observe({updateNumericInput(session, "mtry", max = length(selected_vars_rf()))})
    
    # Saved variables used in GLM
    selected_vars_glm <- reactive({input$glm_vars})
    
    # Update offensive rebound max so it does not exceed rebounds for GLM prediction input
    observe({if("reb" %in% input$glm_vars){
      updateNumericInput(session, "glm_oreb", max = input$glm_reb)}
    })
    
    # Update offensive rebound max so it does not exceed rebounds for random forest prediction input
    observe({if("reb" %in% input$rf_vars){
      updateNumericInput(session, "rf_oreb", max = input$rf_reb)}
    })
    
    # Function to generate predictions
    generatePrediction <- function(selected_vars, num_input, predict_model) {
      
      # Create a data frame with selected variables and numeric inputs
      new_data <- as.data.frame(setNames(num_input, selected_vars), stringsAsFactors = FALSE)
      
      prediction <- predict(predict_model, newdata = new_data)
      
      return(prediction)
    }
    
    # Generate predictions and output table with predicted outcomes
    observeEvent(input$predict, {
      input_vars_rf <- lapply(selected_vars_rf(), function(var) {input[[paste0("rf_", var)]]})
      input_vars_glm <- lapply(selected_vars_glm(), function(var) {input[[paste0("glm_", var)]]})
      prediction_rf <- generatePrediction(selected_vars_rf(), input_vars_rf, trained_models()$RF_Model)
      prediction_glm <- generatePrediction(selected_vars_glm(), input_vars_glm, trained_models()$GLM_Model)
      
      pred_table <- reactive({data.frame("GLM Prediction"=prediction_glm,
                                         "Random Forest Prediction"=prediction_rf)})
      output$prediction_table <- renderDT(datatable(pred_table(),
                                          colnames = c("GLM Prediction",
                                                       "Random Forest Prediction")))
    })
})
