# Author: Michael Dolan
# Date: 12/12/2023
# Purpose of Program: Final Project (UI file)

# Read in necessary packages
library(shiny)
library(tidyverse)
library(httr)
library(jsonlite)
library(DT)
library(caret)
library(randomForest)
library(glmnet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("NBA Player Data"),
    
    # Create Tabset Panel
    tabsetPanel(
    
    #Create the About tab
    tabPanel("About",
      h3("Purpose of App"),
      h4("The purpose of this app is the allow the user to pull the box score game stats of any NBA player for any particular season or group of seasons, as well as create plots and numerical summaries of various statistics. The user can also take that data and fit it to two models (generalized linear model and random forest model) that will try to predict the outcome of the game based on the individual player's stats. Although one player cannot single-handedly win a game, these models attempt to show how much impact the player's stat line has on the outcome based on the accuracy of the resulting model."),
      br(),
      h3("Data Source"),
      h4("This app facilitates the user to pull data from the ",
         a(href="https://app.balldontlie.io/", target="_blank", "balldontlie"),
         "API. This database provides access to statistics from the NBA, including individual game stats for players from 1946 to present. This is a free API that does not require an API key to access."),
      img(src = "cryingjordan.jpeg", width = "200px", height = "200px"),
      br(),
      br(),
      h3("How to Use the App"),
      h4("Data Exploration Tab (Data Creation)"),
      h5("First, navigate to the data exploration tab, where you can type in the first and last name of any NBA player. You can also select a specific group of seasons for that player, or pull all seasons that the player played. Then click create data. This is the data that will be referenced throught the rest of the app, and it can be reviewed in the data table to the right."),
      br(),
      h4("Data Exploration Tab (Plots and Summary Tables)"),
      h5("This is where you can create plots and summary tables of the data you just created. You can select from four types of plots (bar plot, histogram, box and whisker plot, or scatter plot), as well as the corresponding x and y axis variables (if applicable to the graph). There is also an option to select a grouping variable, which will create multiple faceted plots broken out by the selected categorical variable. Below the plot, there is a data table that will summarize your selected variables with either value-based summaries (mean and standard deviation) or rank-based summaries (1st quartile, median, and 3rd quartile)."),
      br(),
      h4("Modeling Tab (Modeling Info)"),
      h5("This tab gives an overview of the two models used on the following tab, as well as some benefits and drawbacks to each approach."),
      br(),
      h4("Modeling Tab (Model Fitting)"),
      h5("On this tab, you can fit two models (GLM and random forest) that will attempt to predict the outcome of a game, as well as cutomize the options that go into creating these models. You can adjust the training data percentage, which will determine what percentage of the data is used to train the model and what percentage of the data is used to test the model (the remaining portion that is not used to train). You can also adjust the number of folds used for cross-validation, which variables are used in the model, and some parameters that are specific to the random forest model (maximum features and number of trees). The model can then be fit, which produces summaries of the models on the right, as well as the accuracy metric of the models based on the training data and a confusion matrix based on the test data that also includes the accuracy metric."),
      br(),
      h4("Modeling Tab (Prediction)"),
      h5("This tab allows the user to input values for each variable selected in both models and produce the predicted outcome of the game based on these inputs. The model must be fit first prior to using this tab.")
    ),
    
    tabPanel("Data Exploration",
    
    tabsetPanel(
             
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
        
        sliderInput("seasons", "Season range", min=1946, max=2023, value = c(1946, 2023), sep="")
        ),
        actionButton("create_data", "Create Data"),
        h5("Note: Make sure that the player's name is be spelled correctly and the range includes seasons that the player played in. Otherwise it will result in an error message.")
      ),
      mainPanel(dataTableOutput("data_table"))
      )),
    
    # Tab that creates customizable plots and data summaries
    tabPanel("Plots and Summary Tables",
    
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
    ))),
    
    # Tab for modeling
    tabPanel("Modeling",
     tabsetPanel(
      
      # Subtab that describes the two models (GLM and Random Forest)
      tabPanel("Modeling Info",
      withMathJax(),
      h3("Generalized Linear Model (GLM) - Logistic Regression"),
      h4("Model Overview"),
      h5("A logistic GLM is a type of statistical model used for binary classification problems where the outcome variable can be one of two responses, success or failure. It includes a linear combination of the independent variables weighted by their coefficients. It also includes a link function, and in logistic regression this is the logit function, where p is the probability of the event occurring. The logit function transforms the probability scale (which ranges from 0 to 1) to the log-odds scale (which ranges from negative to positive infinity)."),
      br(),
      h4("Model Equation:"),
      h5("$$\\log\\left(\\frac{p}{1-p}\\right) = \\beta_0 + \\beta_1 X_1 + \\beta_2 X_2 + \\ldots + \\beta_p X_p$$"),
      h4("Benefits"),
      h5("There are several benefits to generalized linear models. GLMs can be fairly effective at predicting binary outcomes, and the predicted probabilities can be used to classify observations into different categories. It also allows for hypothesis testing, providing a framework for assessing the significance of predictor variables."),
      br(),
      h4("Drawbacks"),
      h5("There are also some drawbacks to GLMs. Logistic regression assumes a linear relationship between the independent variables and the log-odds of the event occurring, but if this assumption is false, model performance may be compromised. This model can also can be susceptible to overfitting if the model is too complex relative to the amount of available data."),
      br(),
      h3("Random Forest Model"),
      h4("Model Overview"),
      h5("A Random Forest is an ensemble learning method that combines the predictions of multiple decision trees to improve overall accuracy and reduce overfitting."),
      br(),
      h4("Model Building"),
      h5("While Random Forests don't have a simple equation like linear models, their prediction process involves aggregating the predictions of individual decision trees. The output of a Random Forest model is typically based on the majority vote (classification) or average (regression) of the predictions from the constituent trees. Random Forest is composed of a collection of decision trees. Each decision tree is built using a random subset of the training data (bootstrap sampling) and a random subset of features at each split. The randomness in building individual trees helps to decorrelate them and improve the overall model's performance."),
      br(),
      h4("Benefits"),
      h5("There are many benefits to Random Forest models. They generally provide high accuracy and their ensemble nature helps to mitigate overfitting, making them more robust when dealing with complex datasets. They can also provide a measure of variable importance, which helps to identify which features contribute the most to the model's predictive performance, and can capture non-linear relationships in data."),
      br(),
      h4("Drawbacks"),
      h5("Random Forests also have some drawbacks. Their training can be computationally complex and consume a significant amount of memory, especially for large datasets and models with a high number of trees and features, which can lead to higher cost. Random Forests are also considered to be black-box models, and understanding the detailed decision-making process under the hood can be difficult. They can also be sensitive to noisy or irrelevant features in the dataset, which may affect model performance.")
        
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
      numericInput("mtry", "Maximum Features", 1, min=1, max=16),
      numericInput("ntree", "Number of Trees", 200, min = 50, max = 500),
      br(),
      
      # Button to create models
      actionButton("fit_models", "Create Models"),
      ),
      mainPanel(verbatimTextOutput("modelSummaries"))
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
      
      actionButton("predict", "Predict Win or Loss")
      ),
      mainPanel(dataTableOutput("prediction_table"))
      ))
     )
    )
    )
))
