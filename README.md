# 558 Final Project - Creating a Shiny App - CDC Respiratory Illness Dashboard   

*  Brief description of the app and its purpose.  
The purpose of this app is to introduce users to the basic ideas underlying exploratory data analysis, variable selection and model building, model testing and selection, and prediction of a response value given new input for predictor values. 

This app will allow users to explore up-to-date information about covid, influenza and pneumonia deaths using the CDC's API. Users will be able to generate numerical summaries and graphs, decide on splits for training and test sets, select variables for building their model, evaluate model performance on the training and test set, and then select a model to make new predictions. Lastly, users will be able to scroll through the dataset (subset by rows and columns if desired) and download the full or subsetted dataset as a .csv file.


*  A list of packages needed to run the app.  
    +  `shiny`  
    +  `shinydashboard`  
    +  `jsonlite`  
    +  `tidyverse`  
    +  `shinycssloaders`  
    +  `htmltools`  
    +  `DT`  
    +  `shinyjs` 
    +  `rpart`
    +  `randomForest`
    +  `caret`



*  A line of code that would install all the packages used (so we can easily grab that and run it prior to running your app).  

install.packages(c("shiny", "shinydashboard", "jsonlite", "tidyverse", "shinycssloaders", "htmltools", "DT", "shinyjs", "rpart", "randomForest", "caret"))


*  The shiny::runGitHub() code that we can copy and paste into RStudio to run your app.  

shiny::runGitHub("558_Final_Project", "kebreeze")
