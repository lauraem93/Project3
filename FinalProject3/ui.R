#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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

# Fix incorrect data point

# Create top finish time data

finishTime <- results %>% filter(result.time_ranking == 1)
finishTime <- left_join(finishTime, stageData, by = "stageId")

x <- as.difftime(as.character(finishTime$result.time), format = "%H:%M.%S")
finishTime$time <-x
finishTime <- finishTime %>% select(stageId, athleteId, name, stageNum, year, departure_city, arrival_city, classification, distance, time, result.time)

#UI

# Dashboard pages
dashboardPage(
    dashboardHeader(title = "Menu"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Information", tabName = "info"),
            menuItem("Data", tabName = "data"),
            menuItem("Data Exploration - Tables", tabName = "eda"),
            menuItem("Data Exploration - Figures", tabName = "graphs"),
            menuItem("Modeling", tabName = "model"),
            menuItem("Unsupervised Learning", tabName = "cluster")
        )
    ),
    dashboardBody(
        tabItems(
            #Information tab content
            tabItem(
                #Information page, info about variables and app
                tabName = "info",
                fluidPage(
                    mainPanel(
                        h3("App Information"),
                        br(),
                        p("The purpose of this app is to explore data collected on the Tour de France stage results over the past four years. To learn more about the Tour de France and other cycling races, visit ", a(href = "https://cyclingnews.com/races/tour-de-france-2020/", "cyclingnews"), " or the ", a(href = "https://www.letour.fr/en/", "Tour de France website")),
                        p("This app contains different pages for exploratory data analysis, modeling, unsupervised learning, and the data set. These pages can be accessed through the tabs on the left."),
                        br(),
                        h3("Data Information - Variable Explanation"),
                        br(),
                        p(strong("stageId"), " - id number for the stage"),
                        p(strong("athleteId"), " - cyclist's id number"),
                        p(strong("name"), " - cyclist name"),
                        p(strong("country_code"), " - country code"),
                        p(strong("result.sprint"), " - sprint jersey points awarded to the cyclist for the stage"),
                        p(strong("result.sprint_ranking"), " - cyclist's sprint ranking for a single stage, based on the sprint points awarded for the stage"),
                        p(strong("result.time"), " - cyclist's stage finish time, the finish time for cyclists who did not win the stage is given with respect to the finish time of the athlete who finished ahead of him"),
                        p(strong("result.time_ranking"), " - cyclist's ranking for the stage"),
                        p(strong("result.climber"), " - points awarded for the climber's jersey for the stage"),
                        p(strong("result.climber_ranking"), " - cyclist's ranking for a single stage, based on climber points awarded for the stage"),
                        p(strong("stageNum"), " - the stage number (1-21)"),
                        p(strong("year"), " - year"),
                        p(strong("departure_city"), " - starting location of the race"),
                        p(strong("arrival_city"), " - location of race finish"),
                        p(strong("classification"), " - stage classification; possible values are flat, medium mountain, high mountain, individual time trial, and team time trial"),
                        p(strong("distance"), " - stage distance in km")
                    )
                )
            ),
            tabItem(
                tabName = "data",
                fluidPage(
                    titlePanel("Results and Stage Data Sets"),
                    sidebarLayout(
                        sidebarPanel(
                            h3("Stage Data"),
                            checkboxGroupInput(
                                inputId = "stageCheckbox",
                                label = "Stage Data Variables",
                                choices = c("stageId", "stageNum", "year", "departure_city", "arrival_city", "classification", "distance"),
                                selected = c("stageId","stageNum", "year", "departure_city", "arrival_city", "classification", "distance")
                            ),
                            selectInput(
                                inputId = "dataYear",
                                label = "Year",
                                choices = list("All", 2020, 2019, 2018, 2017)
                            ),
                            selectInput(
                                inputId = "dataClass",
                                label = "Classification",
                                choices = list("All", "Flat", "Medium Mountain", "High Mountain", "Individual Time Trial", "Team Time Trial"),
                                selected = "All"
                            ),
                            textInput(
                                inputId = "dataDeparture",
                                label = "Departure",
                                value = ""
                            ),
                            textInput(
                                inputId = "dataArrival",
                                label = "Arrival",
                                value = ""
                            ),
                            sliderInput(
                                inputId = "dataDistance",
                                label = "Distance Range - including endpoints",
                                min = 0,
                                max = 300,
                                value = c(0,300)
                            ),
                            h3("Results Data"),
                            checkboxGroupInput(
                                inputId = "resultsCheckbox",
                                label = "Results Data Variables",
                                choices = c("stageId", "athleteId", "name", "country_code", "result.sprint", "result.sprint_ranking", "result.time", "result.time_ranking", "result.climber", "result.climber_ranking"),
                                selected = c("stageId", "athleteId", "name", "country_code", "result.sprint", "result.sprint_ranking", "result.time", "result.time_ranking", "result.climber", "result.climber_ranking")
                            ),
                            textInput(
                                inputId = "dataName",
                                label = "Cyclist - Input Lastname, Firstname",
                                value = ""
                            ),
                            textInput(
                                inputId = "dataCountry",
                                label = "Cyclist Nationality - Input Country Code",
                                value = ""
                            ),
                            textInput(
                                inputId = "dataStageNum",
                                label = "Stage ID - Input in form sr:stage:number",
                                value = ""
                            ),
                            sliderInput(
                                inputId = "dataSprint",
                                label = "Sprinter's Jersey Points - including endpoints",
                                min = 0,
                                max = 70,
                                value = c(0, 70)
                            ),
                            sliderInput(
                                inputId = "dataClimber",
                                label = "Climber's Jersey Points - including endpoints",
                                min = 0,
                                max = 70,
                                value = c(0, 70)
                            ),
                            sliderInput(
                                inputId = "dataRanking",
                                label = "Stage Ranking - including endpoints",
                                min = 0,
                                max = 180,
                                value = c(0, 180)
                            )
                        ),
                        mainPanel(
                            #dataTableOutput("redoResultsData"),
                            h3("Stage Data"),
                            p("This data set includes information about each stage including stage number, year, arrival and departure cities, distance (in km), stage ID number, and classification. Variables can be selected and the data can be filtered using the selections in the side panel. The data set can be saved using the download button below the table."),
                            dataTableOutput("redoStageData"),
                            downloadButton(
                                outputId = "saveStageData",
                                label = "Save Stage Data"
                            ),
                            h3("Results Data"),
                            p("This data set includes information about the athlete results of each stage including athlete name, ID number, and nationality (country_code), points awarded for the sprint and climber jerseys and the stage ranking for these jerseys, and overall time and stage ranking. The time is given with respect to the athlete that finished one place ahead. Variables can be selected and the data can be filtered using the selections in the side panel. The data set can be saved using the download button below the table."),
                            dataTableOutput("redoResultsData"),
                            downloadButton(
                                outputId = "saveResultsData",
                                label = "Save Results Data"
                            )
                        )
                    )
                )
            ),
            tabItem(
                tabName = "eda",
                fluidRow(
                    titlePanel("Exploratory Data Analysis - Tables"),
                    sidebarLayout(
                        sidebarPanel(
                            h3("Most Stage Wins"),
                            radioButtons(
                                inputId = "stageWins",
                                label = "By Cyclist or Country",
                                choices = c("Cyclist", "Country")
                            ),
                            numericInput(
                                inputId = "printNum1",
                                label = "Number to Display",
                                value = 10
                            ),
                            actionButton(
                                inputId = "submit1",
                                label = "Submit"
                            ),
                            h3("Total Points Awarded"),
                            radioButtons(
                                inputId = "pointsType",
                                label = "Sprint or Climber",
                                choices = c("Sprint", "Climber")
                            ),
                            radioButtons(
                                inputId = "totalPoints",
                                label = "By Cyclist or Country",
                                choices = c("Cyclist", "Country")
                            ),
                            numericInput(
                                inputId = "printNum2",
                                label = "Number to Display",
                                value = 10
                            ),
                            actionButton(
                                inputId = "submit2",
                                label = "Go"
                            ),
                            h3("Stage Classification by Distance"),
                            sliderInput(
                                inputId = "distSlider",
                                label = "Number of Distance Breaks",
                                min = 0,
                                max = 25,
                                value = 5
                            ),
                            checkboxInput(
                                inputId = "byYear",
                                label = "Split table by year?"
                            ),
                            actionButton(
                                inputId = "submit5",
                                label = "Submit"
                            )
                        ),
                        mainPanel(
                            h3("Stage Wins Table"),
                            p("The table below shows the cyclist or country with the most stage wins over all four years of the Tour de France (2017-2020). Table options can be selected and submitted in the side panel."),
                            dataTableOutput("stageWinsTable"),
                            h3("Total Points Awarded"),
                            p("This table displays the most total points awarded over all four years for either the sprinter or climber jersey. Table options can be selected and submitted on the left."),
                            dataTableOutput("mostPointsTab"),
                            h3("Classification by Distance Table"),
                            p("The table below is a contingency table of stage distance by classification. The distance is split into different ranges. The number of breaks in distance can be changed using the slider in the side panel. The contingency table can also be viewed collectively over all four years or split into four tables by year."),
                            verbatimTextOutput("contingencyTab")
                        )
                    )
                )
            ),
            tabItem(
                tabName = "graphs",
                fluidRow(
                    titlePanel("Exploratory Data Analysis - Figures"),
                    sidebarLayout(
                        sidebarPanel(
                            h3("Distance Histogram"),
                            radioButtons(
                                inputId = "split",
                                label = "Split by Classification or Year",
                                choices = c("No Split", "Classification", "Year"),
                                selected = "No Split"
                            ),
                            sliderInput(
                                inputId = "bins",
                                label = "Number of Histogram Bins",
                                min = 1,
                                max = 40,
                                value = 12
                            ),
                            actionButton(
                                inputId = "update1",
                                label = "Update Bins"
                            ),
                            h3("Winner's Time by Distance"),
                            radioButtons(
                                inputId = "interactive",
                                label = "View Interactive Plot?",
                                choices = c("Yes", "No"),
                                selected = "No"
                            ),
                            radioButtons(
                                inputId = "split2",
                                label = "Split by Classification or Year",
                                choices = c("No Split", "Classification", "Year"),
                                selected = "No Split"
                            ),
                        ),
                        mainPanel(
                            h3("Distance Histogram"),
                            p("Below is a histogram of stage distance with a density curve. The data in the histogram can be grouped by either ", em("classification"), " or ", em("year."), " Number of bins used to create the histogram can be changed using the slider on the left. The histogram can be saved using the download button below."),
                            plotOutput("plot1"),
                            br(),
                            downloadButton(
                                outputId = "saveHist",
                                label = "Save Histogram"
                            ),
                            h3("Winner's Time by Distance"),
                            p("Below is a scatterplot of winner's time by distance. The data in the scatter plot can be grouped by either ", em("classification"), " or ", em("year."), " An interactive plot can be viewed by choosing the appropriate option. If interactive is not selected, the plot can be saved using the download button below."),
                            conditionalPanel(
                                condition = "input.interactive == 'Yes'",
                                plotlyOutput("plot2")
                            ),
                            conditionalPanel(
                                condition = "input.interactive == 'No'",
                                plotOutput("staticplot")
                            ),
                            br(),
                            conditionalPanel(
                                condition = "input.interactive == 'No'",
                                downloadButton(
                                    outputId = "savePlot",
                                    label = "Save Plot"
                                )
                            ),
                        )
                    )
                )
            ),
            tabItem(
                tabName = "model",
                fluidRow(
                    titlePanel("Modeling and Prediction"),
                    sidebarLayout(
                        sidebarPanel(
                            h3("Stage Placing Prediction"),
                            h4("Model Settings"),
                            checkboxGroupInput(
                                inputId = "stageModelVar",
                                label = "Variable Selection",
                                choices = c("country_code", "result.sprint", "result.sprint_ranking", "result.climber", "result.climber_ranking", "classification", "distance", "past.ranking_avg")
                            ),
                            h4("Prediction Settings"),
                            selectInput(
                                inputId = "stageModelYear",
                                label = "Year",
                                choices = c(2020, 2019, 2018, 2017)
                            ),
                            selectInput(
                                inputId = "stageModelStage",
                                label = "Stage Number",
                                choices = c(seq(from = 1, to = 21, by = 1))
                            ),
                            actionButton(
                                inputId = "submit3",
                                label = "Submit"
                            ),
                            h3("Winner's Time Prediction"),
                            h4("Model Settings"),
                            checkboxGroupInput(
                                inputId = "timeModelVar",
                                label = "Variable Selection",
                                choices = c("stageNum", "year", "departure_city", "arrival_city", "classification", "distance")
                            ),
                            h4("Prediction Settings"),
                            radioButtons(
                                inputId = "modelType",
                                label = "Model Type",
                                choices = c("Tree", "Boosted Tree")
                            ),
                            selectInput(
                                inputId = "timeModelYear",
                                label = "Year",
                                choices = c(2020, 2019, 2018, 2017)
                            ),
                            selectInput(
                                inputId = "timeModelStage",
                                label = "Stage Number",
                                choices = c(seq(from = 1, to = 21, by = 1))
                            ),
                            actionButton(
                                inputId = "submit4",
                                label = "Submit"
                            )
                        ),
                        mainPanel(
                            h3("Stage Results Prediction"),
                            p("The first model on this page is used to predict the outcome of a stage. Select variables to include in the model and choose the stage on which to make predictions with the selection options in the side panel. The model does take a couple of minutes to run and will not pop up instantaneously. The predictions table contains the predicted and actual finish rankings for each athlete who finished the stage. Fit statistics are also shown to use as a measure of how well the model fits the data and makes predictions."),
                            uiOutput("math"),
                            dataTableOutput("stageModelTable"),
                            br(),
                            p("The fit statistics for the linear model are shown below:"),
                            verbatimTextOutput("stageModelSum"),
                            p("The fit statistics for the prediction are shown below:"),
                            verbatimTextOutput("stageModelPredSum"),
                            br(),
                            h3("Winner's Finish Time Prediction"),
                            p("The second model on this page is either a tree or boosted tree model for the winner's time based on stage variables. Again, variables to use in the model and the stage on which to make predictions can be selected in the side panel on the left. Fit statistics for the model are displayed as a measure of model fit. A table of which variables were used in the model is shown, and a table of actual and predicted finish times is also shown. This model also takes a couple of minutes to run and does not display results instantaneously."),
                            h4("Fit Statistics"),
                            verbatimTextOutput("timeStats"),
                            h4("Selected Prediction Variable Values"),
                            tableOutput("predVar"),
                            h4("Prediction and Actual Times"),
                            p("Time is shown in minutes."),
                            tableOutput("predTable")
                        )
                    ),
                )
            ),
            tabItem(
                tabName = "cluster",
                fluidRow(
                    titlePanel("Unsupervised Learning - Clustering"),
                    sidebarLayout(
                        sidebarPanel(
                            h3("Data Set to Use"),
                            selectInput(
                                inputId = "dataSelect",
                                label = "Data",
                                choices = c("Stage Results" = "Results", "Winner's Time" = "Time"),
                                selected = "Stage Results"
                            ),
                            conditionalPanel(
                                condition = "input.dataSelect == 'Results'",
                                checkboxGroupInput(
                                    inputId = "clustStageVar",
                                    label = "Variable Selection",
                                    choices = c("result.time_ranking", "name", "country_code", "result.sprint", "result.sprint_ranking", "result.climber", "result.climber_ranking", "classification", "distance", "past.ranking_avg")
                                )
                            ),
                            conditionalPanel(
                                condition = "input.dataSelect == 'Time'",
                                checkboxGroupInput(
                                    inputId = "clustTimeVar",
                                    label = "Variable Selection",
                                    choices = c("time", "stageNum", "year", "departure_city", "arrival_city", "classification", "distance")
                                )
                            ),
                            actionButton(
                                inputId = "submit6",
                                label = "Submit"
                            )
                        ),
                        mainPanel(
                            uiOutput("clusterTitle"),
                            p("Clustering is an unsupervised learning method. Hierarchical clustering is used to look at this data. In hierarchical clustering, all observations are in their own cluster and the closest/most related clusters are joined until all data is in one single cluster. The dendrogram produced is a visual of the clustering. Horizontal bars join clusters and vertical bars give an idea of how much change there is between clusters, ie more closely related clusters will have a shorter vertical distance between horizontal bars than distantly related clusters. Here, hierarchical clustering can be done for either winner's time data or stage results data. After choosing the data set to use, the variables to use in the clustering can be selected."),
                            plotOutput("clusterPlot"),
                        )
                    )
                )
            )
        )
    )
)
