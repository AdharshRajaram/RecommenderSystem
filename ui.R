library(shiny)
shinyUI(fluidPage(
  titlePanel("Recommender Systems"),
  fluidRow(
    column(3,
     wellPanel(
       uiOutput("LoadInput"),
       actionButton("Load",label="Load")
     ),  
     wellPanel(
       uiOutput("InputTrain"),  
       uiOutput("SaveUI"), 
       uiOutput("InfoSavedModel")
     )
    ),
    column(9,mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "EDA",
          uiOutput("TrainEDA")
          ),
          tabPanel(
           "Recommended Items",
            uiOutput("TrainRec")
          )
      )
    )
   )  
  )
))
