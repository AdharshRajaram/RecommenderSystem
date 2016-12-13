library(shiny)
library(Matrix)
library(recommenderlab)
shinyUI(fluidPage(
  titlePanel("Recommender Systems"),
  sidebarLayout(
    sidebarPanel(
      fileInput("trainF","Upload Your Historical Ratings Data:"),
      #uiOutput("ClearFeature"),
      fileInput("FeatureF","Upload Item Features(Required only for content-based)"),
      selectInput("Method","Type of Recommender System:",c("Auto","IBCF","UBCF"),selected = "Auto")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("EDA",textOutput("default0"),textOutput("Nusers"),textOutput("Nitems"),
                 textOutput("TRating"),plotOutput("RatingDist"),plotOutput("UserDist"),
                 plotOutput("AvgRating"),plotOutput("ROC"),plotOutput("Metric")),
        tabPanel("Singe User",textOutput("default1")),
        tabPanel("All Users",textOutput("default2"))
      )
    )
  )
))