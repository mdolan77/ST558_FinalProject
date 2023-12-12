# Author: Michael Dolan
# Date: 12/12/2023
# Purpose of Program: Final Project (UI file)

#Read in necessary packages
library(shiny)
library(tidyverse)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("NBA Player Data"),
    
    # Create Tabset Panel
    tabsetPanel(
    
    #Create the About tab
    tabPanel("About"
      
    ),
    
    # Tab that generates the dataset to be used throughout the app
    tabPanel("Data Creation",
      sidebarLayout(
      sidebarPanel(
        h4("Type the first and last name of an NBA player"),
        h5("Note: Include any suffixes like Jr. or Sr. in the Last Name box"),
        
        # Inputs (first name, last name, and seasons) to be used in API function
        textInput("first_name", "First Name"),
        textInput("last_name", "Last Name"),
        br(),
        
        h4("Seasons"),
        
        # Give the option to use all seasons for that player or only select certain seasons
        radioButtons("season_option", "Select an option", choices = c("All Seasons"="all_seasons", "Select Seasons"="select_seasons")),
        
        # Slider input for selecting seasons, with some instructions
        conditionalPanel(condition = "input.season_option == 'select_seasons'",
        h4("Select a range of seasons"),
        h5("Note: Seasons are represented by the year they began (e.g. 2023 indicates the 2023-2024 season)"),
        
        sliderInput("seasons", "Season range", min=1949, max=2023, value = c(1949, 2023), sep="")
        ),
        actionButton("create_data", "Create Data"),
        h5("Note: Make sure that the player's name is be spelled correctly and the range includes seasons that the player played in. Otherwise it will result in an error message.")
      ),
      mainPanel(dataTableOutput("data_table"))
      )),
    
    # Tab that creates customizable plots and data summaries
    tabPanel("Data Exploration",
    
    # Sidebar to customize the data, plot, and table
    sidebarLayout(
      sidebarPanel(
        h4("Customize the plot/table"),
        selectInput("plot_type", "Plot Type",
                    choices = c("Bar Plot" = "bar", "Histogram"="hist",
                                "Box and Whisker Plot"="box", "Scatter Plot"="scatter")),
        
        # Conditional panel for bar plot options
        conditionalPanel(condition = "input.plot_type == 'bar'",
        selectInput("x_var_bar", "X-Axis Variable",
                    choices = c("Home/Away"="home_away", "Win/Loss"="win_loss",
                                "Player Team"="player_team", "Opposing Team"="opponent_team",
                                Season="season", "Playoffs/Non-Playoffs"="post_season")),
        selectInput("y_var_bar", "Y-Axis Variable",
                    choices = c(Points = "pts", Rebounds="reb", Assists="ast", Steals="stl",
                                Blocks = "blk", "Field Goals Made"="fgm", "Field Goal Attempts"="fga",
                                "Field Goal Percentage"="fg_pct", "3-Pointers Made"="fg3m",
                                "3-Point Attempts"="fg3a", "3-Point Percentage"="fg3_pct",
                                "Free Throws Made"="ftm", "Free Throw Attempts"="fta",
                                "Free Throw Percentage"="ft_pct", "Offensive Rebounds"="oreb",
                                "Defensive Rebounds"="dreb", Turnovers="turnover",
                                "Personal Fouls"="pf", "Team Score"="team_score",
                                "Opposing Team Score"="opponent_score")),
        
        # Allow option for faceting of bar plot
        checkboxInput("group_option_bar", "Include Grouping Variable"),
        conditionalPanel(condition = "input.group_option_bar",
        h5("Note: Grouping variable and X-Axis Variable cannot be the same for the table to generate."),
        selectInput("group_var_bar", "Grouping Variable",
                    choices = c("Home/Away"="home_away", "Win/Loss"="win_loss",
                                "Player Team"="player_team", "Opposing Team"="opponent_team",
                                Season="season", "Playoffs/Non-Playoffs"="post_season"))),
        
        # Customize bar plot and table summaries
        selectInput("stat_type_bar", "Summary Type (Plot and Table)",
                    choices = c("Value-Based Summaries (Mean, SD)"="mean",
                                "Rank-Based Summaries (Median, IQR)"="median"))
        ),
        
        # Conditional panel for histogram options
        conditionalPanel(condition = "input.plot_type == 'hist'",
        sliderInput("bins",
                    "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30),
        selectInput("var_hist", "Variable",
                    choices = c(Points = "pts", Rebounds="reb", Assists="ast", Steals="stl",
                                Blocks = "blk", "Field Goals Made"="fgm", "Field Goal Attempts"="fga",
                                "Field Goal Percentage"="fg_pct", "3-Pointers Made"="fg3m",
                                "3-Point Attempts"="fg3a", "3-Point Percentage"="fg3_pct",
                                "Free Throws Made"="ftm", "Free Throw Attempts"="fta",
                                "Free Throw Percentage"="ft_pct", "Offensive Rebounds"="oreb",
                                "Defensive Rebounds"="dreb", Turnovers="turnover",
                                "Personal Fouls"="pf", "Team Score"="team_score",
                                "Opposing Team Score"="opponent_score")),
        
        # Allow option for faceting of histogram
        checkboxInput("group_option_hist", "Include Grouping Variable"),
        conditionalPanel(condition = "input.group_option_hist",
        selectInput("group_var_hist", "Grouping Variable",
                    choices = c("Home/Away"="home_away", "Win/Loss"="win_loss",
                                "Player Team"="player_team", "Opposing Team"="opponent_team",
                                Season="season", "Playoffs/Non-Playoffs"="post_season"))),
        
        # Customize table summaries
        selectInput("stat_type_hist", "Summary Type (Table Only)",
                    choices = c("Value-Based Summaries (Mean, SD)"="mean",
                                "Rank-Based Summaries (Median, IQR)"="median"))
        ),
        
        # Conditional panel for histogram options
        conditionalPanel(condition = "input.plot_type == 'box'",
        selectInput("x_var_box", "X-Axis Variable",
                    choices = c("Home/Away"="home_away", "Win/Loss"="win_loss",
                                "Player Team"="player_team", "Opposing Team"="opponent_team",
                                Season="season", "Playoffs/Non-Playoffs"="post_season")),
        selectInput("y_var_box", "Y-Axis Variable",
                    choices = c(Points = "pts", Rebounds="reb", Assists="ast", Steals="stl",
                                Blocks = "blk", "Field Goals Made"="fgm", "Field Goal Attempts"="fga",
                                "Field Goal Percentage"="fg_pct", "3-Pointers Made"="fg3m",
                                "3-Point Attempts"="fg3a", "3-Point Percentage"="fg3_pct",
                                "Free Throws Made"="ftm", "Free Throw Attempts"="fta",
                                "Free Throw Percentage"="ft_pct", "Offensive Rebounds"="oreb",
                                "Defensive Rebounds"="dreb", Turnovers="turnover",
                                "Personal Fouls"="pf", "Team Score"="team_score",
                                "Opposing Team Score"="opponent_score")),
        
        # Allow option for faceting of box plot
        checkboxInput("group_option_box", "Include Grouping Variable"),
        conditionalPanel(condition = "input.group_option_box",
        h5("Note: Grouping variable and X-Axis Variable cannot be the same for the table to generate."),
        selectInput("group_var_box", "Grouping Variable",
                    choices = c("Home/Away"="home_away", "Win/Loss"="win_loss",
                                "Player Team"="player_team", "Opposing Team"="opponent_team",
                                Season="season", "Playoffs/Non-Playoffs"="post_season"))),
        
        # Customize table summaries
        selectInput("stat_type_box", "Summary Type (Table Only)",
                    choices = c("Value-Based Summaries (Mean, SD)"="mean",
                                "Rank-Based Summaries (Median, IQR)"="median"))
        ),
        
        # Conditional panel for scatter plot options
        conditionalPanel(condition = "input.plot_type == 'scatter'",
        selectInput("x_var_scatter", "X-Axis Variable",
                    choices = c(Points = "pts", Rebounds="reb", Assists="ast", Steals="stl",
                                Blocks = "blk", "Field Goals Made"="fgm", "Field Goal Attempts"="fga",
                                "Field Goal Percentage"="fg_pct", "3-Pointers Made"="fg3m",
                                "3-Point Attempts"="fg3a", "3-Point Percentage"="fg3_pct",
                                "Free Throws Made"="ftm", "Free Throw Attempts"="fta",
                                "Free Throw Percentage"="ft_pct", "Offensive Rebounds"="oreb",
                                "Defensive Rebounds"="dreb", Turnovers="turnover",
                                "Personal Fouls"="pf", "Team Score"="team_score",
                                "Opposing Team Score"="opponent_score")),
        selectInput("y_var_scatter", "Y-Axis Variable",
                    choices = c(Points = "pts", Rebounds="reb", Assists="ast", Steals="stl",
                                Blocks = "blk", "Field Goals Made"="fgm", "Field Goal Attempts"="fga",
                                "Field Goal Percentage"="fg_pct", "3-Pointers Made"="fg3m",
                                "3-Point Attempts"="fg3a", "3-Point Percentage"="fg3_pct",
                                "Free Throws Made"="ftm", "Free Throw Attempts"="fta",
                                "Free Throw Percentage"="ft_pct", "Offensive Rebounds"="oreb",
                                "Defensive Rebounds"="dreb", Turnovers="turnover",
                                "Personal Fouls"="pf", "Team Score"="team_score",
                                "Opposing Team Score"="opponent_score")),
        
        # Allow option for faceting of scatter plot
        checkboxInput("group_option_scatter", "Include Grouping Variable"),
        conditionalPanel(condition = "input.group_option_scatter",
        selectInput("group_var_scatter", "Grouping Variable",
                    choices = c("Home/Away"="home_away", "Win/Loss"="win_loss",
                                "Player Team"="player_team", "Opposing Team"="opponent_team",
                                Season="season", "Playoffs/Non-Playoffs"="post_season"))),
        
        # Customize table summaries
        selectInput("stat_type_scatter", "Summary Type (Table Only)",
                    choices = c("Value-Based Summaries (Mean, SD)"="mean",
                                "Rank-Based Summaries (Median, IQR)"="median"))
        )
        ),

        # Show a plot and table
        mainPanel(
            plotOutput("Plot"), dataTableOutput("Summary_table")
        )
      )
    ),
    
    # Tab for modeling
    tabPanel("Modeling",
     tabsetPanel(
      
      # Subtab that describes the two models (GLM and random forest)
      tabPanel("Modeling Info",
      withMathJax(),
      h5("Here is an equation: $$E=mc^2$$"),
      h4("Here is an equation: $$E=mc^2$$")
        
      ),
      
      # Subtab that fits the two models
      tabPanel("Model Fitting",
      sidebarLayout(
      sidebarPanel(
      
      #Slider input to specify the percent of the data to train the model on
      h3("Select the percentage of the data to train the model"),
      h5("(The remaining percent will be used to test the model)"),
      sliderInput("training_set", "Training Data Percentage", 75, min=1, max=99, post = "%"),
      br(),
      
      # Slider input to determine the number of folds for cross-validation (max of 15)
      h3("Select the cross-validation settings"),
      h5("(Used for both models)"),
      sliderInput("cv", "Number of Cross-Validations Folds", 5, min = 1, max = 15),
      br(),
      
      # Checkbox for variables to use in the GLM model
      h3("Generalized Linear Model Options"),
      checkboxGroupInput("glm_vars", "Variables", inline = TRUE,
                         choices = c(Points = "pts", Rebounds="reb", Assists="ast", Steals="stl",
                                Blocks = "blk", "Field Goals Made"="fgm",
                                "Field Goal Percentage"="fg_pct", "3-Pointers Made"="fg3m",
                                "3-Point Percentage"="fg3_pct", "Free Throws Made"="ftm",
                                "Free Throw Percentage"="ft_pct", "Offensive Rebounds"="oreb",
                                Turnovers="turnover", "Personal Fouls"="pf", "Home/Away"="home_away",
                                "Playoffs/Non-Playoffs"="post_season")
                         ),
      br(),
      
      # Checkbox for variables to use in the random forest model
      h3("Random Forest Model Options"),
      checkboxGroupInput("rf_vars", "Variables", inline = TRUE,
                         choices = c(Points = "pts", Rebounds="reb", Assists="ast", Steals="stl",
                                Blocks = "blk", "Field Goals Made"="fgm",
                                "Field Goal Percentage"="fg_pct", "3-Pointers Made"="fg3m",
                                "3-Point Percentage"="fg3_pct", "Free Throws Made"="ftm",
                                "Free Throw Percentage"="ft_pct", "Offensive Rebounds"="oreb",
                                Turnovers="turnover", "Personal Fouls"="pf", "Home/Away"="home_away",
                                "Playoffs/Non-Playoffs"="post_season")
                         ),
      
      # Specify tuning parameters for the random forest model
      h4("Input tuning parameters for the random forest model"),
      numericInput("mtry", "Maximum Features", 3, min=2, max=15),
      numericInput("ntree", "Number of Trees", 200, min = 50, max = 500),
      br(),
      
      # Button to create models
      actionButton("fit_models", "Create Models"),
      ),
      mainPanel()
      )),
      
      # Subtab for prediction
      tabPanel("Prediction",
      sidebarLayout(
      sidebarPanel(
      h3("Input values for the below variables to get a prediction for each model"),
      br(),
      
      # Conditionally show predictor inputs based on variables selected in GLM
      h4("Generalized Linear Model Variables"),
      conditionalPanel(condition = "input.glm_vars.indexOf('pts') !== -1",
                       numericInput("glm_pts", "Points", 0, min=0, max=100)),
      conditionalPanel(condition = "input.glm_vars.indexOf('reb') !== -1",
                       numericInput("glm_reb", "Rebounds", 0, min=0, max=55)),
      conditionalPanel(condition = "input.glm_vars.indexOf('ast') !== -1",
                       numericInput("glm_ast", "Assists", 0, min=0, max=30)),
      conditionalPanel(condition = "input.glm_vars.indexOf('stl') !== -1",
                       numericInput("glm_stl", "Steals", 0, min=0, max=15)),
      conditionalPanel(condition = "input.glm_vars.indexOf('blk') !== -1",
                       numericInput("glm_blk", "Blocks", 0, min=0, max=20)),
      conditionalPanel(condition = "input.glm_vars.indexOf('fgm') !== -1",
                       numericInput("glm_fgm", "Field Goals Made", 0, min=0, max=40)),
      conditionalPanel(condition = "input.glm_vars.indexOf('fg_pct') !== -1",
                       numericInput("glm_fg_pct", "Field Goal Percentage", 0, min=0, max=100)),
      conditionalPanel(condition = "input.glm_vars.indexOf('fg3m') !== -1",
                       numericInput("glm_fg3m", "3-Pointers Made", 0, min=0, max=20)),
      conditionalPanel(condition = "input.glm_vars.indexOf('fg3_pct') !== -1",
                       numericInput("glm_fg3_pct", "3-Point Percentage", 0, min=0, max=100)),
      conditionalPanel(condition = "input.glm_vars.indexOf('ftm') !== -1",
                       numericInput("glm_ftm", "Free Throws Made", 0, min=0, max=30)),
      conditionalPanel(condition = "input.glm_vars.indexOf('ft_pct') !== -1",
                       numericInput("glm_ft_pct", "Free Throw Percentage", 0, min=0, max=100)),
      conditionalPanel(condition = "input.glm_vars.indexOf('oreb') !== -1",
                       numericInput("glm_oreb", "Offensive Rebounds", 0, min=0, max=25)),
      conditionalPanel(condition = "input.glm_vars.indexOf('turnover') !== -1",
                       numericInput("glm_turnover", "Turnovers", 0, min=0, max=20)),
      conditionalPanel(condition = "input.glm_vars.indexOf('pf') !== -1",
                       numericInput("glm_pf", "Personal Fouls", 0, min=0, max=6)),
      conditionalPanel(condition = "input.glm_vars.indexOf('home_away') !== -1",
                       selectInput("glm_home_away", "Home/Away", choices=c("Home", "Away"))),
      conditionalPanel(condition = "input.glm_vars.indexOf('post_season') !== -1",
                       selectInput("glm_post_season", "Playoffs/Non-Playoffs",
                                   choices=c("Playoffs"="TRUE", "Non-Playoffs"="FALSE"))),
      br(),
      
      # Conditionally show predictor inputs based on variables selected in random forest
      h4("Random Forest Model Variables"),
      conditionalPanel(condition = "input.rf_vars.indexOf('pts') !== -1",
                       numericInput("rf_pts", "Points", 0, min=0, max=100)),
      conditionalPanel(condition = "input.rf_vars.indexOf('reb') !== -1",
                       numericInput("rf_reb", "Rebounds", 0, min=0, max=55)),
      conditionalPanel(condition = "input.rf_vars.indexOf('ast') !== -1",
                       numericInput("rf_ast", "Assists", 0, min=0, max=30)),
      conditionalPanel(condition = "input.rf_vars.indexOf('stl') !== -1",
                       numericInput("rf_stl", "Steals", 0, min=0, max=15)),
      conditionalPanel(condition = "input.rf_vars.indexOf('blk') !== -1",
                       numericInput("rf_blk", "Blocks", 0, min=0, max=20)),
      conditionalPanel(condition = "input.rf_vars.indexOf('fgm') !== -1",
                       numericInput("rf_fgm", "Field Goals Made", 0, min=0, max=40)),
      conditionalPanel(condition = "input.rf_vars.indexOf('fg_pct') !== -1",
                       numericInput("rf_fg_pct", "Field Goal Percentage", 0, min=0, max=100)),
      conditionalPanel(condition = "input.rf_vars.indexOf('fg3m') !== -1",
                       numericInput("rf_fg3m", "3-Pointers Made", 0, min=0, max=20)),
      conditionalPanel(condition = "input.rf_vars.indexOf('fg3_pct') !== -1",
                       numericInput("rf_fg3_pct", "3-Point Percentage", 0, min=0, max=100)),
      conditionalPanel(condition = "input.rf_vars.indexOf('ftm') !== -1",
                       numericInput("rf_ftm", "Free Throws Made", 0, min=0, max=30)),
      conditionalPanel(condition = "input.rf_vars.indexOf('ft_pct') !== -1",
                       numericInput("rf_ft_pct", "Free Throw Percentage", 0, min=0, max=100)),
      conditionalPanel(condition = "input.rf_vars.indexOf('oreb') !== -1",
                       numericInput("rf_oreb", "Offensive Rebounds", 0, min=0, max=25)),
      conditionalPanel(condition = "input.rf_vars.indexOf('turnover') !== -1",
                       numericInput("rf_turnover", "Turnovers", 0, min=0, max=20)),
      conditionalPanel(condition = "input.rf_vars.indexOf('pf') !== -1",
                       numericInput("rf_pf", "Personal Fouls", 0, min=0, max=6)),
      conditionalPanel(condition = "input.rf_vars.indexOf('home_away') !== -1",
                       selectInput("rf_home_away", "Home/Away", choices=c("Home", "Away"))),
      conditionalPanel(condition = "input.rf_vars.indexOf('post_season') !== -1",
                       selectInput("rf_post_season", "Playoffs/Non-Playoffs",
                                   choices=c("Playoffs"="TRUE", "Non-Playoffs"="FALSE"))),
      ),
      mainPanel()
      ))
     )
    )
    )
))
