# ST558_FinalProject

The purpose of this app is the allow the user to pull the box score game stats of any NBA player for any particular season or group of seasons, as well as create plots and numerical summaries of various statistics. The user can also take that data and fit it to two models (generalized linear model and random forest model) that will try to predict the outcome of the game based on the individual player's stats.

Packages needed to run app:
 - httr
 - jsonlite
 - shiny
 - tidyverse
 - DT
 - caret
 - randomForest
 - glmnet
  
  
Code to install necessary packages:
```r
install.packages("shiny")
install.packages("tidyverse")
install.packages("httr")
install.packages("jsonlite")
install.packages("DT")
install.packages("caret")
install.packages("randomForest")
install.packages("glmnet")
```
  
  
Code to run app:
```r
shiny::runGitHub(repo="ST558_FinalProject",
                 username="mdolan77",
                 subdir="ST558_Final")
```