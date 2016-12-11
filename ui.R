library(shiny)

shinyUI(fluidPage(
  titlePanel("Recommender Systems"),
  sidebarLayout(
    sidebarPanel(
      fileInput("trainD","Upload Your Historical Data:"),
      selectInput("Method","Type of Recommender System:",c("Auto","IBCF","UBCF","Content-Based"),selected = "Auto")
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
