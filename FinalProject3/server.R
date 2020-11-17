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
    
    output$stageTable <- renderDataTable({
        var <- input$stageCheckbox
        allVar <- attributes(stageData)$names
        
        #Select columns based on input
        stageDataSub <- stageData[, allVar, drop = FALSE]
        
        #Filter the data based on input
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
        
        stageDataSub <- stageDataSub %>% filter(distance > input$dataDistance[1] & distance < input$dataDistance[2])
        
        stageDataSub <- stageData[, var, drop = FALSE]
        
        stageDataSub
    })
    
    #Download Data Set
    
    output$saveStageData <- downloadHandler(
        filename = "stage-data.csv",
        content = stageDataSub,
        contentType = "text/csv"
    )
    
    #View Results Data
    
    output$resultsTable <- renderDataTable({
        var2 <- input$resultsCheckbox
        
        #Filter data based on slider input
        resultsDataSub <- results %>% filter(result.sprint > input$dataSprint[[1]] & result.sprint < input$dataSprint[[2]]) %>% filter(result.climber > input$dataClimber[1] & result.climber < input$dataClimber[2]) %>% filter(result.time_ranking > input$dataRanking[1] & result.time_ranking < input$dataRanking[2])
        
        #Filter the data based on input
        if (input$dataName != ""){
            resultsDataSub <- resultsDataSub %>% filter(name == input$dataName)
        }
        if (input$dataCountry != ""){
            resultsDataSub <- resultsDataSub %>% filter(country_code == input$dataCountry)
        }
        
        #Select columns base on input
        resultsDataSub <- results[, var2, drop = FALSE]
        
        resultsDataSub
    })
    
    #Need Help With This
    #Download Results Data
    
    output$saveResultsData <- downloadHandler(
        filename = "results.csv",
        content = resultsDataSub,
        contentType = "text/csv"
    )
    
    # Exploratory Data Analysis - Tables Page
    
    submitButton1 <- eventReactive(input$submit1, {
        value <- input$stageWins
        num <- input$printNum1
        if (value == "Cyclist"){
            stageWinsTab <- results %>% group_by(name) %>% filter(result.time_ranking == 1) %>% summarise(totalWins = sum(result.time_ranking)) %>% arrange(desc(totalWins)) %>% head(num)
        } else {
            stageWinsTab <- results %>% group_by(country_code) %>% filter(result.time_ranking == 1) %>% summarise(totalWins = sum(result.time_ranking)) %>% arrange(desc(totalWins)) %>% head(num) 
        }
        stageWinsTab
    })
    
    output$stageWinsTable <- renderDataTable({
        submitButton1()
    })
    
    submitButton2 <- eventReactive(input$submit2, {
        type <- input$pointsType
        class <- input$totalPoints
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
    
    output$mostPointsTab <- renderDataTable({
        submitButton2()
    })
    
    # Stage classification by distance table
    
    submitButton5 <- eventReactive(input$submit5, {
        tableData <- stageData
        tableData$dist <- cut(tableData$distance, breaks = input$distSlider)
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
    
    output$plot1 <- renderPlot({
        g <- ggplot(data = stageData, aes(x = distance))
        
        if (input$split == "No Split"){
            plot1 <- g + geom_histogram()
        } else if (input$split == "Classification"){
            plot1 <- g + geom_histogram(aes(fill = as.factor(classification)), position = "dodge", bins = updateButton1()) + geom_density(aes(y = ..count..), alpha = 0.4)
        } else if (input$split == "Year"){
            plot1 <- g + geom_histogram(aes(fill = as.factor(year)), position = "dodge", bins = updateButton1()) + geom_density(aes(y = ..count..), alpha = 0.4)
        }
        
        plot1 + labs(x = "Distance", y = "Count", title = "Distance Histogram") + scale_fill_discrete(name = input$split)
    })
    
    # Time by Distance Plot
    
    updateButton2 <- eventReactive(input$update2, {
        type <- input$split2
    })
    
    output$plot2 <- renderPlot({
        g <- ggplot(finishTime, aes(x = distance, y = as.numeric(time)))
        
        if (input$split2 == "No Split"){
            plot2 <- g + geom_point()
        } else if (input$split2 == "Classification"){
            plot2 <- g + geom_point(aes(colour = as.factor(classification)))
        } else if (input$split2 == "Year"){
            plot2 <- g + geom_point(aes(colour = as.factor(year)))
        }
        
        plot2 + labs(x = "Distance", y = "Time (min)", title = "Winner's Finish Time by Distance") + scale_colour_discrete(name = input$split2)
    })
    
    # Supervised Learning Models Page
    
    # Linear Model
    
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
        
    })
    
    output$stageModelTable <- renderDataTable({
        
        yearnum <- as.numeric(input$stageModelYear)
        stagenum <- as.numeric(input$stageModelStage)
        predictorVar <- input$stageModelVar
        
        lm <- model1()
        
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
    
    #output$stageModelTable <- renderDataTable({
    #    model1()
    #})
    
    output$stageModelSum <- renderPrint({
        lm <- model1()
        lm$results
    })
    
    output$stageModelPredSum <- renderPrint({
        yearnum <- as.numeric(input$stageModelYear)
        stagenum <- as.numeric(input$stageModelStage)
        predictorVar <- input$stageModelVar
        
        lm <- model1()
        
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
    
    model2 <- eventReactive(input$submit4, {
        #User input predictor var
        userVar <- input$timeModelVar
        
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
        
        model
        
    })
    
    output$timeStats <- renderPrint({
        
        #Fit Statistics
        
        predModel <- predict(model2(), newdata = timeTest)
        modelRes <- postResample(predModel, timeTest$numTime)
        
        modelRes
        
    })
    
    output$predVar <- renderTable({
        stage <- input$timeModelStage
        yearnum <- input$timeModelYear
        userVar <- input$timeModelVar
        
        predData <- finishTime %>% filter(stageNum == stage & year == yearnum)
        
        predVar <- predData %>% select(stageNum, year, all_of(userVar))
        
        predVar
    })
    
    output$predTable <- renderTable({
        stage <- input$timeModelStage
        yearnum <- input$timeModelYear
        
        predData <- finishTime %>% filter(stageNum == stage & year == yearnum)
        
        prediction <- predict(model2(), newdata = predData)
        
        #Prediction table to show user
        
        table <- cbind(predData, prediction)
        
        predTable <- table %>% select(stageNum, year, prediction, result.time) %>% rename("actual" = result.time)
        
        predTable
    })
    
    # Unsupervised Learning - Clustering
    
    #Clusterint Title
    
    clusterName <- eventReactive(input$submit6, {
        
    })
    
    output$clusterTitle <- renderUI({
        if (input$dataSelect == "Results") {
            title <- "Stage Results Hierarchical Cluster Dendrogram"
        } else if (input$dataSelect == "Time") {
            title <- "Winner's Time Hierarchical Cluster Dendrogram"
        }
        h3(title)
    })
    
    cluster <- eventReactive(input$submit6, {
        #Choose correct data type and variables
        dataType <- input$dataSelect
        if (dataType == "Stage Results"){
            clusterVar <- input$clustStageVar
            clusterData <- left_join(results, stageData, by = "stageId") %>% select(all_of(clusterVar)) %>% na.omit()
        } else if (dataType == "Winner's Time"){
            clusterVar <- input$clustTimeVar
            clustData <- finishTime
            clustData$time <- as.numeric(clustData$time)
            clustData <- clustData %>% select(all_of(clusterVar))
        }
        
        #Do hierarchical clustering
        hierClust <- hclust(dist(clustData))
        plot(hierClust)
    })
    
    #Plot dendrogram
    
    output$clusterPlot <- renderPlot({
        cluster()
    })
    
})
