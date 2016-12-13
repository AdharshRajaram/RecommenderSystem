library(shiny)
library(Matrix)
library(recommenderlab)
Dtext<-"Nothing to show.Please Upload File!"
shinyServer(function(input,output,session){

  
Build<-reactiveValues(
  infile=NULL,
  infile2=NULL,
  trainData=as.data.frame(NULL),
  FeatureData=NULL
  )  
  
contents<-reactive({
  Build$infile<-input$trainF
  if(is.null(Build$infile))
    {
      return(NULL)
    }
  else TRUE
  })

observeEvent(contents(),{
  trainData<-read.csv(Build$infile$datapath)
  #trainData<-trainData[1:20000,]
  Build$Nuser<-length(unique(trainData[,1]))
  Build$NItem<-length(unique(trainData[,3]))
  Build$TR<-if(length(unique(trainData[,5]))>2){"Numeric"} else{"Binary"}
  UIMatrix <- as(trainData[,c(1,3,5)],"realRatingMatrix")
  Build$r<-UIMatrix
  rm(UIMatrix)
  Build$trainData<-trainData
})

Features<-reactive({
  Build$infile2<-input$FeatureF
  if(is.null(Build$infile2))
  {
    return(NULL)
  }
  else 
  {
    Build$FeatureData<-read.csv(Build$infile2$datapath)
  }
})

observeEvent(Features(),{
  updateSelectInput(session,"Method",label="Type of Recommender System:",c("Auto","IBCF","UBCF","Content-Based"),selected = "Auto")
})



######Panel EDA#######
output$default0<-renderText({
  if(is.null(contents())) Dtext 
  else NULL
  })

output$Nusers<-renderText({
  if(!is.null(contents())){
    paste("No. of Users:",Build$Nuser)
    }
  })

output$Nitems<-renderText({
  if(!is.null(contents())){
    paste("Number of Items:",Build$NItem)
    }
  })

output$TRating<-renderText({
  if(!is.null(contents())){
    paste("Rating Type:",Build$TR)
    }
  })

output$RatingDist<-renderPlot({
  if(!is.null(contents())){
    hist(getRatings(Build$r),
    main="Histogram of Ratings")
    }
  })

output$UserDist<-renderPlot({
  if(!is.null(contents())){
    hist(rowCounts(Build$r),breaks = 100,
    main="Distribution of Ratings & User")
    }
  })

output$AvgRating<-renderPlot({
  if(!is.null(contents())){
    hist(colMeans(Build$r),breaks = 100,
    main="Distribution of Average Ratings per User")
    }
  })

output$ROC<-renderPlot({
  if(!is.null(contents())){
    e<-evaluationScheme(Build$r,method="split",train=0.7,given=10)
    r1<-Recommender(getData(e,"train"),"UBCF")
    r2<-Recommender(getData(e,"train"),"IBCF")
    r3<-Recommender(getData(e,"train"),"POPULAR")
    r4<-Recommender(getData(e,"train"),"RANDOM")
    
    algorithms <- list(
     # "random items" = list(name="RANDOM", param=NULL),
     # "popular items" = list(name="POPULAR", param=NULL),
      "user-based CF" = list(name="UBCF", param=list(nn=10)),
     # "item-based CF" = list(name="IBCF", param=list(k=10))
      )
    results <- evaluate(e, algorithms, type = "topNList",
                         n=c(1, 3, 5, 10))
    plot(results, annotate=c(1,3), legend="bottomright")
    }
  })

output$Metric<-renderPlot({
  if(!is.null(contents())){
    "Metric"
    }
  })


    
    output$default1<-renderText({
      if(is.null(contents())) Dtext
      else {paste(Features())}
      })
    output$default2<-renderText({
      if(is.null(contents())) Dtext
      else NULL
      })

})