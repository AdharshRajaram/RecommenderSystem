library(shiny)
Dtext<-"Nothing to show.Please Upload File!"
shinyServer(function(input,output){

  isupload<-reactive(
    {
      if(is.null(input$trainD))
      {
       TRUE
      }
      else FALSE
    }
 )

######Panel EDA#######
output$default0<-renderText({
  if(isupload()) Dtext 
  else NULL
  })

output$Nusers<-renderText({
  if(!isupload()){
    "Number of Users:"
     }
  })

output$Nitems<-renderText({
  if(!isupload()){
    "Number of Items:"
  }
})

output$TRating<-renderText({
  if(!isupload()){
    "Rating Type:"
  }
})

output$RatingDist<-renderPlot({
  if(!isupload()){
    "RatingDist"
  }
})

output$UserDist<-renderPlot({
  if(!isupload()){
    "UserDist"
  }
})

output$AvgRating<-renderPlot({
  if(!isupload()){
    "AvgRating"
  }
})

output$ROC<-renderPlot({
  if(!isupload()){
    "ROC"
  }
})

output$Metric<-renderPlot({
  if(!isupload()){
    "Metric"
  }
})


    
    output$default1<-renderText({
      if(isupload()) Dtext
      else NULL
      })
    output$default2<-renderText({
      if(isupload()) Dtext
      else NULL
      })

})