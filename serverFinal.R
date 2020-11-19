#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Packages Needed

library(tidyverse)
library(readr)
library(caret)
library(DBI)
library(lubridate)
library(DT)
library(knitr)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(rmarkdown)
library(plotly)
library(dendextend)

# Read in data

results <- read.csv("finalResults")
stageData <- read.csv("finalStageData")

# Create top finish time data

finishTime <- results %>% filter(result.time_ranking == 1)
finishTime <- left_join(finishTime, stageData, by = "stageId")

x <- as.difftime(as.character(finishTime$result.time), format = "%H:%M.%S")
finishTime$time <-x
finishTime <- finishTime %>% select(stageId, athleteId, name, stageNum, year, departure_city, arrival_city, classification, distance, time, result.time)

# Server

shinyServer(function(input, output) {
  
  # Data table page
  
  output$redoStageData <- renderDataTable({
    var <- input$stageCheckbox
    stageDataSub <- stageData
    
    #Slider filters
    stageDataSub <- stageDataSub %>% filter(distance >= input$dataDistance[[1]] & distance <= input$dataDistance[[2]])
    
    #Select and text input filters
    if (input$dataYear != "All"){
      stageDataSub <- stageDataSub %>% filter(year == input$dataYear)
    }
    if (input$dataClass != "All"){
      stageDataSub <- stageDataSub %>% filter(classification == input$dataClass)
    }
    if (input$dataDeparture != ""){
      stageDataSub <- stageDataSub %>% filter(departure_city == input$dataDeparture)
    }
    if (input$dataArrival != ""){
      stageDataSub <- stageDataSub %>% filter(arrival_city == input$dataArrival)
    }
    
    #Select variables
    stageDataSub <- stageDataSub %>% select(var)
  })
  
  #Download Data Set
  
  output$saveStageData <- downloadHandler(
    filename = "stage-data.csv",
    content = function(file) {
      write.csv(stageData1(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  #View Results Data
  
  output$redoResultsData <- renderDataTable({
    var2 <- input$resultsCheckbox
    resultsDataSub <- results
    
    #Slider filters
    resultsDataSub <- resultsDataSub %>% filter(result.sprint >= input$dataSprint[[1]] & result.sprint <= input$dataSprint[[2]]) %>% filter(result.climber >= input$dataClimber[1] & result.climber <= input$dataClimber[2]) %>% filter(result.time_ranking >= input$dataRanking[1] & result.time_ranking <= input$dataRanking[2])
    
    #Select and text input filters
    if (input$dataCountry != ""){
      resultsDataSub <- resultsDataSub %>% filter(country_code == input$dataCountry)
    }
    
    if (input$dataName != ""){
      resultsDataSub <- resultsDataSub %>% filter(name == input$dataName)
    }
    
    if (input$dataStageNum != ""){
      resultsDataSub <- resultsDataSub %>% filter(stageId == input$dataStageNum)
    }
    
    #Select variables
    resultsDataSub <- resultsDataSub %>% select(var2)
  })
  
  #Download Results Data
  
  output$saveResultsData <- downloadHandler(
    filename = "results.csv",
    content = function(file) {
      write.csv(resultsData1(), file, row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  # Exploratory Data Analysis - Tables Page
  
  #Most stage wins table
  
  submitButton1 <- eventReactive(input$submit1, {
    value <- input$stageWins
    num <- input$printNum1
    
    #Stage wins by cyclist or country, group by name or country, filter for only the winner, sum by name/country, arrange in descending order
    if (value == "Cyclist"){
      stageWinsTab <- results %>% group_by(name) %>% filter(result.time_ranking == 1) %>% summarise(totalWins = sum(result.time_ranking)) %>% arrange(desc(totalWins)) %>% head(num)
    } else {
      stageWinsTab <- results %>% group_by(country_code) %>% filter(result.time_ranking == 1) %>% summarise(totalWins = sum(result.time_ranking)) %>% arrange(desc(totalWins)) %>% head(num) 
    }
    stageWinsTab
  })
  
  #output stage wins table
  output$stageWinsTable <- renderDataTable({
    submitButton1()
  })
  
  #Most points table
  submitButton2 <- eventReactive(input$submit2, {
    type <- input$pointsType
    class <- input$totalPoints
    
    #Create table for either most sprint or climber points. then by either cyclist or country. Same method for calculating points as used in stage wins table
    if (type == "Sprint"){
      if (class == "Cyclist"){
        tab2 <- results %>% group_by(name) %>% summarise(totalPoints = sum(result.sprint)) %>% arrange(desc(totalPoints)) %>% head(input$printNum2)
      } else {
        tab2 <- results %>% group_by(country_code) %>% summarise(totalPoints = sum(result.sprint)) %>% arrange(desc(totalPoints)) %>% head(input$printNum2)
      }
    } else {
      if (class == "Cyclist"){
        tab2 <- results %>% group_by(name) %>% summarise(totalPoints = sum(result.climber)) %>% arrange(desc(totalPoints)) %>% head(input$printNum2)
      } else {
        tab2 <- results %>% group_by(country_code) %>% summarise(totalPoints = sum(result.climber)) %>% arrange(desc(totalPoints)) %>% head(input$printNum2)
      }
    }
    
    tab2
  })
  
  #output most points table
  output$mostPointsTab <- renderDataTable({
    submitButton2()
  })
  
  # Stage classification by distance table
  
  submitButton5 <- eventReactive(input$submit5, {
    tableData <- stageData
    tableData$dist <- cut(tableData$distance, breaks = input$distSlider)
    
    #Create a contingency table. Split by year if selected
    if (input$byYear == TRUE){
      tab3 <- table(tableData$dist, tableData$classification, tableData$year)
    } else if (input$byYear == FALSE){
      tab3 <- table(tableData$dist, tableData$classification)
    }
    tab3
  })
  
  output$contingencyTab <- renderPrint(
    submitButton5()
  )
  
  # Distance Histogram
  
  updateButton1 <- eventReactive(input$update1, {
    numBins <- input$bins
  })
  
  hist1 <- reactive({
    g <- ggplot(data = stageData, aes(x = distance))
    
    #Create histogram. Add a fill by variable if selected
    if (input$split == "No Split"){
      plot1 <- g + geom_histogram()
    } else if (input$split == "Classification"){
      plot1 <- g + geom_histogram(aes(fill = as.factor(classification)), position = "dodge", bins = updateButton1()) + geom_density(aes(y = ..count..), alpha = 0.4)
    } else if (input$split == "Year"){
      plot1 <- g + geom_histogram(aes(fill = as.factor(year)), position = "dodge", bins = updateButton1()) + geom_density(aes(y = ..count..), alpha = 0.4)
    }
    
    plot1 + labs(x = "Distance", y = "Count", title = "Distance Histogram") + scale_fill_discrete(name = input$split)
  })
  
  #Output histogram
  output$plot1 <- renderPlot({
    print(hist1())
  })
  
  #Download histogram
  
  output$saveHist <- downloadHandler(
    filename = "histogram.png",
    content = function(file) {
      ggsave(file, plot = hist1())
    },
    contentType = "image/png"
  )
  
  # Time by Distance Plot - this plot has two options, interactive or non interactive
  
  #non interactive plot - use ggplot
  pointplot <- reactive({
    g <- ggplot(finishTime, aes(x = distance, y = as.numeric(time)))
    
    #determine if a group by variable was chosen and plot accordingly
    if (input$split2 == "No Split"){
      plot2 <- g + geom_point()
    } else if (input$split2 == "Classification"){
      plot2 <- g + geom_point(aes(colour = as.factor(classification)))
    } else if (input$split2 == "Year"){
      plot2 <- g + geom_point(aes(colour = as.factor(year)))
    }
    
    #add labels, title, legend title
    plot2 + labs(x = "Distance", y = "Time (min)", title = "Winner's Finish Time by Distance") + scale_colour_discrete(name = input$split2)
  })
  
  #output non interactive plot
  output$staticplot <- renderPlot({
    print(pointplot())
  })
  
  #Interactive plot using plotly
  interactiveplot <- reactive({
    
    #determine if group by variable was selected. plot accordingly
    if (input$split2 == "No Split"){
      figure <- plot_ly(
        type = 'scatter',
        x = finishTime$distance,
        y = as.numeric(finishTime$time),
        
        #what to display when use hovers over points
        text = paste("<br>distance: ", finishTime$distance,
                     "<br>time(min): ", round(finishTime$time, 2)),
        hoverinfo = 'text',
        mode = 'markers'
      )
    } else if (input$split2 == "Classification"){
      figure <- plot_ly(
        type = 'scatter',
        x = finishTime$distance,
        y = as.numeric(finishTime$time),
        
        #what to display when user hovers over points
        text = paste("<br>distance: ", finishTime$distance,
                     "<br>time(min): ", round(finishTime$time, 2),
                     "<br>year: ", finishTime$year),
        hoverinfo = 'text',
        mode = 'markers',
        
        #add differrent colors for different years
        transforms = list(
          list(
            type = 'groupby',
            groups = finishTime$year,
            styles = list(
              list(target = 2017, value = list(marker =list(color = 'blue'))),
              list(target = 2018, value = list(marker =list(color = 'green'))),
              list(target = 2019, value = list(marker =list(color = 'orange'))),
              list(target = 2020, value = list(marker =list(color = 'purple')))
            )
          )
        )
      ) %>% layout(showlegend = TRUE)
    } else if (input$split2 == "Year"){
      figure <- plot_ly(
        type = 'scatter',
        x = finishTime$distance,
        y = as.numeric(finishTime$time),
        
        #what to display when user hovers over points
        text = paste("<br>distance: ", finishTime$distance,
                     "<br>time(min): ", round(finishTime$time, 2),
                     "<br>classification: ", finishTime$classification),
        hoverinfo = 'text',
        mode = 'markers',
        
        #add different colors for different classifications
        transforms = list(
          list(
            type = 'groupby',
            groups = finishTime$classification,
            styles = list(
              list(target = "Flat", value = list(marker =list(color = 'blue'))),
              list(target = "Medium Mountain", value = list(marker =list(color = 'green'))),
              list(target = "High Mountain", value = list(marker =list(color = 'orange'))),
              list(target = "Individual Time Trial", value = list(marker =list(color = 'purple'))),
              list(target = "Team Time Trial", value = list(marker =list(color = 'pink')))
            )
          )
        )
      ) %>% layout(showlegend = TRUE)
    }
    
    figure
  })
  
  #output interactive plot
  output$plot2 <- renderPlotly({
    interactiveplot()
  })
  
  #save plot if non interactive plot was chosen
  output$savePlot <- downloadHandler(
    filename = "timebydistanceplot.png",
    content = function(file) {
      ggsave(file, plot = pointplot())
    },
    contentType = "image/png"
  )
  
  # Supervised Learning Models Page
  
  # Linear Model
  
  #MathJax output
  
  output$math <- renderUI({
    withMathJax("The stage outcome model is a linear model of the the form $$ Y = \\mathbf{X} \\beta + E_{ij}$$ where $$ E_{ij} \\sim N(0, \\sigma^2) $$ \\(Y\\) is a vector of observed variables, \\(\\mathbf{X}\\) is the design matrix, \\(\\beta\\) is a vector of linear coefficients, and \\(E_{ij}\\) is a vector of error terms.")
  })
  
  model1 <- eventReactive(input$submit3, {
    
    yearnum <- as.numeric(input$stageModelYear)
    stagenum <- as.numeric(input$stageModelStage)
    predictorVar <- input$stageModelVar
    
    #Join results and stage data
    modelData <- left_join(results, stageData, by = "stageId") %>% select(-result.time)
    modelData <- modelData %>% filter(is.na(result.time_ranking) == FALSE)
    
    #Create overall counter for stage number - want to filter out stages results that are equal to or greater than desired prediction stage (ie don't use stage 13 from 2020 for model and prediction of outcome of stage 17 in 2019)
    overallStageNum <- modelData$stageNum + (modelData$year - 2017)*21
    modelData$overallStageNum <- overallStageNum
    stageModelData <- modelData %>% filter(overallStageNum < (stagenum + (yearnum - 2017)*21))
    past.ranking_avg <- stageModelData %>% group_by(year, athleteId) %>% summarise(past.ranking_avg = mean(result.time_ranking))
    stageModelData <- left_join(stageModelData, past.ranking_avg, by = c("year", "athleteId"))
    for (i in 1:length(stageModelData$past.ranking_avg)){
      if (is.na(stageModelData$past.ranking_avg[i])){
        stageModelData$past.ranking_avg[i] <- max(stageModelData$past.ranking_avg)
      }
    }
    
    #Filter the data so that it only contains desired predictor variables and the response
    stageModelData <- stageModelData %>% select(result.time_ranking, name, all_of(predictorVar))
    
    #Split into training and testing sets
    
    trainIndex <- sample(1:nrow(stageModelData), size = nrow(stageModelData)*0.7)
    testIndex <- dplyr::setdiff(1:nrow(stageModelData), trainIndex)
    
    #Partition stageData into training and testing data
    train1 <- stageModelData[trainIndex,]
    test1 <- stageModelData[testIndex,]
    
    #Train the model
    trControl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
    
    lm <- train(result.time_ranking ~ ., data = train1, method = "lm", trControl=trControl)
    
    #the output of this reactiveEvent should be a list containing necessary objects for different outputs
    modelObjects <- list(lm = lm, stageModelData = stageModelData)
    modelObjects
    
  })
  
  output$stageModelTable <- renderDataTable({
    
    yearnum <- as.numeric(input$stageModelYear)
    stagenum <- as.numeric(input$stageModelStage)
    predictorVar <- input$stageModelVar
    
    lm <- model1()$lm
    
    #Join results and stage data
    modelData <- left_join(results, stageData, by = "stageId") %>% select(-result.time)
    modelData <- modelData %>% filter(is.na(result.time_ranking) == FALSE)
    
    #Create Prediction Data
    stagePredict <- modelData %>% filter(year == yearnum)
    
    #Create points variables for prediction, use average for TdF so far, not points awarded in desired prediction stage
    if(stagenum == 1){
      pointsData <- stagePredict %>% filter(stageNum == 1) %>% select(athleteId)
      zeros <- seq(from = 0, to = 0, length.out = length(pointsData$athleteId))
      pointsData$result.sprint <- zeros
      pointsData$result.climber <- zeros
      pointsData$result.climber_ranking <- zeros
      pointsData$result.sprint_ranking <- zeros
      pointsData$past.ranking_avg <- zeros
    } else {
      pointsData <- stagePredict %>% filter(stageNum < stagenum) %>% group_by(athleteId) %>% summarise(result.sprint = mean(result.sprint), result.climber = mean(result.climber), result.climber_ranking = mean(result.climber_ranking), result.sprint_ranking = mean(result.sprint_ranking), past.ranking_avg = mean(result.time_ranking)) 
    }
    
    #Filter for the correct stage and drop result.sprint and result.climber results. Add in averaged climber and sprint results
    stagePredict <- stagePredict %>% filter(stageNum == stagenum) %>% select(-result.climber, -result.sprint, -result.sprint_ranking, -result.climber_ranking)
    stagePredict <- left_join(stagePredict, pointsData, by = "athleteId")
    
    #Predict outcome of stage
    lmPredict <- predict(lm, newdata = stagePredict)
    pred <- data.frame(stagePredict$athleteId, lmPredict) %>% rename("athleteId" = stagePredict.athleteId)
    pred <- left_join(pred, stagePredict, by = "athleteId") %>% select(athleteId, name, lmPredict, result.time_ranking)
    pred$prediction <- rank(pred$lmPredict)
    pred <- select(pred, athleteId, name, prediction, result.time_ranking) %>% rename("actual" = result.time_ranking)
    
    #Get fit statistics
    lmFit <- summary(lm)
    post <- postResample(lmPredict, pred$actual)        
    
    #Print prediction table
    pred
  })
  
  #fit statistics for the linear model
  output$stageModelSum <- renderPrint({
    lm <- model1()$lm
    lm$results
  })
  
  #fit statistics for the prediction
  output$stageModelPredSum <- renderPrint({
    yearnum <- as.numeric(input$stageModelYear)
    stagenum <- as.numeric(input$stageModelStage)
    predictorVar <- input$stageModelVar
    
    lm <- model1()$lm
    
    #Join results and stage data
    modelData <- left_join(results, stageData, by = "stageId") %>% select(-result.time)
    modelData <- modelData %>% filter(is.na(result.time_ranking) == FALSE)
    
    #Create Prediction Data
    stagePredict <- modelData %>% filter(year == yearnum)
    
    #Create points variables for prediction, use average for TdF so far, not points awarded in desired prediction stage
    if(stagenum == 1){
      pointsData <- stagePredict %>% filter(stageNum == 1) %>% select(athleteId)
      zeros <- seq(from = 0, to = 0, length.out = length(pointsData$athleteId))
      pointsData$result.sprint <- zeros
      pointsData$result.climber <- zeros
      pointsData$result.climber_ranking <- zeros
      pointsData$result.sprint_ranking <- zeros
      pointsData$past.ranking_avg <- zeros
    } else {
      pointsData <- stagePredict %>% filter(stageNum < stagenum) %>% group_by(athleteId) %>% summarise(result.sprint = mean(result.sprint), result.climber = mean(result.climber), result.climber_ranking = mean(result.climber_ranking), result.sprint_ranking = mean(result.sprint_ranking), past.ranking_avg = mean(result.time_ranking)) 
    }
    
    #Filter for the correct stage and drop result.sprint and result.climber results. Add in averaged climber and sprint results
    stagePredict <- stagePredict %>% filter(stageNum == stagenum) %>% select(-result.climber, -result.sprint, -result.sprint_ranking, -result.climber_ranking)
    stagePredict <- left_join(stagePredict, pointsData, by = "athleteId")
    
    #Predict outcome of stage
    lmPredict <- predict(lm, newdata = stagePredict)
    pred <- data.frame(stagePredict$athleteId, lmPredict) %>% rename("athleteId" = stagePredict.athleteId)
    pred <- left_join(pred, stagePredict, by = "athleteId") %>% select(athleteId, name, lmPredict, result.time_ranking)
    pred$prediction <- rank(pred$lmPredict)
    pred <- select(pred, athleteId, name, prediction, result.time_ranking) %>% rename("actual" = result.time_ranking)
    
    #Get fit statistics
    post <- postResample(lmPredict, pred$actual)
    post
  })
  
  #Time prediction model
  
  userVar2 <- eventReactive(input$submit4, {
    userVar <- input$timeModelVar
  })
  
  #Create Model - output of this reactive function should be a list of the model, training, and test data sets
  
  model2 <- reactive({ 
    
    #User input predictor var
    userVar <- userVar2()
    
    #Time needs to be numeric
    finishTime$numTime <- as.numeric(finishTime$time)
    
    #Filter data for only selected variables and response variable
    finishTime2 <- finishTime %>% select(numTime, all_of(userVar))
    
    #Create training and testing sets
    
    trainIndex <- sample(1:nrow(finishTime2), size = nrow(finishTime)*0.8)
    testIndex <- dplyr::setdiff(1:nrow(finishTime2), trainIndex)
    
    #Partition stageData into training and testing data
    timeTrain <- finishTime2[trainIndex,]
    timeTest <- finishTime2[testIndex,]
    
    #Train model based on type specified
    
    if (input$modelType == "Tree"){
      trControl <- trainControl(method = "LOOCV", number = 3)
      
      model <- train(numTime ~ ., data = timeTrain, method = "rpart", trControl=trControl)
    } else if (input$modelType == "Boosted Tree") {
      model <- train(numTime ~ ., data = timeTrain, method = "gbm", trControl = trainControl(method = "repeatedcv", number = 10, repeats = 3), verbose = FALSE)
    }
    
    modelInfo <- list(model = model, timeTrain = timeTrain, timeTest = timeTest)
    modelInfo
  })
  
  #fit statistics for the model, predict on test data and get fit info
  output$timeStats <- renderPrint({
    timeTest <- model2()$timeTest
    
    predModel <- predict(model2()$model, newdata = timeTest)
    modelRes <- postResample(predModel, timeTest$numTime)
    
    print(modelRes)
    
  })
  
  #Print a table that shows the predictor variables that were shown along with the stage and year for the prediction
  
  output$predVar <- renderTable({
    stage <- input$timeModelStage
    yearnum <- input$timeModelYear
    userVar3 <- userVar2()
    
    predData <- finishTime %>% filter(stageNum == stage & year == yearnum)
    
    predVar <- predData %>% select(stageNum, year, all_of(userVar3))
    
    predVar
  })
  
  #Table of the prediction and actual times
  
  output$predTable <- renderTable({
    stage <- input$timeModelStage
    yearnum <- input$timeModelYear
    userVar2 <- userVar2()
    
    predData <- finishTime %>% filter(stageNum == stage & year == yearnum)
    
    #Predict finish time
    
    prediction <- predict(model2()$model, newdata = predData)
    
    #Prediction table to show user
    
    table <- cbind(predData, prediction)
    
    predTable <- table %>% select(stageNum, year, prediction, time) %>% rename("actual" = time)
    
    predTable
  })
  
  # Unsupervised Learning - Clustering
  
  #Clusterint Title
  
  #Change title depending on which data was selected
  output$clusterTitle <- renderUI({
    if (input$dataSelect == "Results") {
      title <- "Stage Results Hierarchical Cluster Dendrogram"
    } else if (input$dataSelect == "Time") {
      title <- "Winner's Time Hierarchical Cluster Dendrogram"
    }
    h3(title)
  })
  
  #Choose which variables to include in the clustering
  
  cluster <- eventReactive(input$submit6, {
    #Choose correct data type and variables
    dataType <- input$dataSelect
    dataType
  })
  
  #Do clustering and print a dendrogram
  
  output$clusterPlot <- renderPlot({
    dataType <- cluster()
    
    #get data for clustering based on user input
    if (dataType == "Results"){
      clusterVar <- input$clustStageVar
      clusterData <- left_join(results, stageData, by = "stageId") %>% select(all_of(clusterVar)) %>% na.omit()
    } else if (dataType == "Time"){
      clusterVar <- input$clustTimeVar
      clusterData <- finishTime
      clusterData$time <- as.numeric(clusterData$time)
      clusterData <- clusterData %>% select(all_of(clusterVar))
    }
    
    #Do hierarchical clustering
    hierClust <- hclust(dist(clusterData))
    #hierClust
    dendrogram <- plot(hierClust)
    dendrogram
  })
  
})