library(shiny)
library(Matrix)
library(recommenderlab)
Dtext<-"Nothing to show.Please Upload File!"
options(shiny.maxRequestSize=25*1024^2)

shinyServer(function(input,output,session){
  
  Build<-reactiveValues(
    start=FALSE,
    infile=NULL,
    infile2=NULL,
    trainData=as.data.frame(NULL),
    FeatureData=NULL,
    complete=FALSE
  )  
  
  Test<-reactiveValues(
    Begin=FALSE,
    TopN=5,
    Finish=FALSE 
  )
  Status<-reactiveValues(
    IsTrain ="N"
  )
  SAVED<-reactiveValues(
   load=NULL,
   File=NULL
  )
  
  
  
output$InputTrain<-renderUI({
 if(!input$Load>0){
  tagList(
    fileInput("trainF",label = "Upload Your Historical Ratings Data:"),
    fileInput("FeatureF","Upload Item Features(Required only for content-based)"),
    fileInput("NewD","Upload Your New Data\\Test Data"),
    selectInput("Method","Type of Recommender System:",c("Auto","IBCF","UBCF","RANDOM","POPULAR","CONTENTBASED"),selected = "Auto"),
    actionButton("SubmitInput",label="Submit") 
  )
 }  
})  
  
output$TrainEDA<-renderUI({
if(!input$Load>0){  
  tagList(
    textOutput("default0"),textOutput("Nusers"),textOutput("Nitems"),
    textOutput("TRating"),plotOutput("RatingDist"),plotOutput("UserDist"),
    plotOutput("AvgRating"),plotOutput("ROC"),plotOutput("Metric")
  )
}
else if(!is.null(SAVED$load$start))  {
  tagList(
    textOutput("LNusers"),textOutput("LNitems"),
    textOutput("LTRating"),plotOutput("LRatingDist"),plotOutput("LUserDist"),
    plotOutput("LAvgRating"),plotOutput("LROC"),plotOutput("LMetric")
  )
}
})
  
output$TrainRec<-renderUI({
if(!input$Load>0){  
  tagList(
    textOutput("default2"),
    uiOutput("Parameters"),
    tableOutput("ResultTable")
  )
} 
else if(!is.null(SAVED$load$start))  {
    tagList(
      textOutput("Ldefault2"),
      uiOutput("LParameters"),
      tableOutput("LResultTable")
    )
  }  
})
  
output$LoadInput<-renderUI({
  SModel<-list.files(pattern = "*.RData")
  tagList(
    selectInput("LoadModel","Load Trained Models:",c("NO",SModel),selected = "NO")
    )
  
})

observeEvent(input$Load,{
  if(input$Load>0){ 
    if(input$LoadModel!="NO"){
      load(input$LoadModel)
      l<-reactiveValuesToList(Build)
      SAVED$load<-l
    }
  }
})


output$InfoSavedModel<-renderUI({
  if(!is.null(SAVED$load$start)){
    tagList(
      textOutput("ModelInfo"),
      fileInput("TestLoad","Upload Your New Data\\Test Data")
    )
  }
})
 
output$ModelInfo<-renderText({
  #paste("Model :",input$LoadModel,'\n',"Type :",SAVED$load$selectmod@method)
  paste("Type :",SAVED$load$selectmod@method)
})

observeEvent(input$TestLoad,{
  if(!is.null(input$TestLoad)){
    SAVED$File<-input$TestLoad  
    Test$TestData<-read.csv(SAVED$File$datapath)
    Test$Nuser<-unique(Test$TestData[,1])
    Test$NItem<-length(unique(Test$TestData[,3]))
    Test$r <- as(Test$TestData[,c(1,3,5)],"realRatingMatrix") 
    
    
    coltrain<-colnames(SAVED$load$r)
    coltest<-colnames(Test$r)
    if(length(setdiff(coltest,coltrain))>0){stop("Some Items in test data are were not in train data")}
    coltoadd<-setdiff(coltrain,coltest)
    coltoadd<-matrix(data=NA,nrow = nrow(Test$r),ncol =length(coltoadd),dimnames=list(NULL,coltoadd) )
    Test$r<-cbind(as(Test$r,"matrix"),coltoadd)
    Test$r<-as(Test$r,"realRatingMatrix")
    Test$Begin<-TRUE
  }
})

contents<-reactive({
    
    if(!is.null(input$trainF) && input$SubmitInput>0)
    {
      Build$infile<-input$trainF  
      Build$method<-isolate(input$Method)
      Build$start<-TRUE
    }
    else 
    {
      return(NULL)
    }
  })
  
  observeEvent(contents(),{
    trainData<-read.csv(Build$infile$datapath)
    #trainData<-trainData[1:10000,]
    Build$Nuser<-length(unique(trainData[,1]))
    Build$NItem<-length(unique(trainData[,3]))
    if(length(unique(trainData[,5]))>2){
      Build$TR="Numeric" 
      Build$GR=(max(trainData[,5]) + min(trainData[,5]))/2
    } 
    else{
      Build$TR="Binary"
      Build$GR=1
    }
    Build$trainData<-trainData
    if(Build$TR=="Numeric"){
      Build$r <- as(trainData[,c(1,3,5)],"realRatingMatrix")
    }
    else{
      Build$r <- as(trainData[,c(1,3,5)],"binaryRatingMatrix")    
    }
    
    
  })
  
  Features<-reactive({
    
    if(!is.null(input$FeatureF) && input$SubmitInput>0)
    {
      Build$infile2<-input$FeatureF
      Build$FeatureData<-read.csv(Build$infile2$datapath) 
    }
    else 
    {
      return(NULL)
    }
  })
  
  observeEvent(Features(),{
    updateSelectInput(session,"Method",label="Type of Recommender System:",c("Auto","IBCF","UBCF","RANDOM","POPULAR","CONTENTBASED"),selected = "Auto")
  })
  
  
  
  
  
  
  model<-reactive({
    Build$e<-evaluationScheme(Build$r,method="split",train=1,given=-200,goodRating=1)
    if(Build$method=="Auto"){
      Build$UBCF_mod<-Recommender(getData(Build$e,"train"),method="UBCF")
      Build$POPULAR_mod<-Recommender(getData(Build$e,"train"),method="POPULAR")
      Build$RANDOM_mod<-Recommender(getData(Build$e,"train"),method="RANDOM")
      Build$IBCF_mod<-Recommender(getData(Build$e,"train"),method="IBCF")
      if(!is.null(Features())){
        Build$CB_mod<-Recommender(getData(Build$e,"train"),method="CONTENTBASED",parameter = list(features=Build$FeatureData))
      }
      else{
        Build$CB_mod<-NULL
      }
      
    }
    else if(Build$method=="CONTENTBASED"){
      Build$selectmod<-Recommender(getData(Build$e,"train"),method="CONTENTBASED",parameter = list(features=Build$FeatureData))
    }
    else{
      Build$selectmod<-Recommender(getData(Build$e,"train"),method=Build$method)
      print(Build$method)
      print(Build$selectmod)
    }
  })
  
  
  ######Panel EDA#######
  output$default0<-renderText({
    if(is.null(contents())) Dtext 
    else if(Build$complete){ NULL}
    else "Training in Progress...."
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
    if(Build$start){
    
      hist(getRatings(Build$r),
           main="Histogram of Ratings")
      
    }
  })
  
  output$UserDist<-renderPlot({
    if(Build$start){
      hist(rowCounts(Build$r),breaks = 100,lables=TRUE,
           main="Distribution of Ratings & User")
    }
  })
  
  output$AvgRating<-renderPlot({
    if(Build$start){
     
      hist(colMeans(Build$r),breaks = 100,
           main="Distribution of Average Ratings")
      
    }
  })
  
  output$ROC<-renderPlot({
    if(Build$start){
      model()
      ntopL<-c(1,3,5,10,15,20)
      if(Build$TR=="Numeric"){
      if(Build$method=="Auto"){
        ror.ubcf<-lapply(ntopL,function(i){predict(Build$UBCF_mod,newdata=getData(Build$e,"known"),type="topNList",n=i)})
        ror.ibcf<-lapply(ntopL,function(i){predict(Build$IBCF_mod,newdata=getData(Build$e,"known"),type="topNList",n=i)})
        ror.popular<-lapply(ntopL,function(i){predict(Build$POPULAR_mod,newdata=getData(Build$e,"known"),type="topNList",n=i)})
        ror.random<-lapply(ntopL,function(i){predict(Build$RANDOM_mod,newdata=getData(Build$e,"known"),type="topNList",n=i)})
        
        eubcf<-lapply(ror.ubcf,function(i){calcPredictionAccuracy(i, getData(Build$e, "unknown"),given=-5,goodRating=Build$GR)})
        eibcf<-lapply(ror.ibcf,function(i){calcPredictionAccuracy(i, getData(Build$e, "unknown"),given=-5,goodRating=Build$GR)})
        epopular<-lapply(ror.popular,function(i){calcPredictionAccuracy(i, getData(Build$e, "unknown"),given=-5,goodRating=Build$GR)})
        erandom<-lapply(ror.random,function(i){calcPredictionAccuracy(i, getData(Build$e, "unknown"),given=-5,goodRating=Build$GR)})
        eubcf<-t(as.data.frame(eubcf))
        eibcf<-t(as.data.frame(eibcf))
        epopular<-t(as.data.frame(epopular))
        erandom<-t(as.data.frame(erandom))
        eubcf<-eubcf[,c(8,7)]
        eibcf<-eibcf[,c(8,7)]
        epopular<-epopular[,c(8,7)]
        erandom<-erandom[,c(8,7)]
        rownames(eubcf)<-c("1","3","5","10","15","20")
        rownames(epopular)<-c("1","3","5","10","15","20")
        rownames(erandom)<-c("1","3","5","10","15","20")
        rownames(eibcf)<-c("1","3","5","10","15","20")
        
        plot(eubcf,type="l",col="green",ylim=c(0,1),xlim=c(0,1))
        points(eubcf,pch=1,col="green")
        text(eubcf,rownames(eubcf),col="green",pos=3)
        par(new=TRUE)
        plot(eibcf,type="l",col="red",axes=FALSE)
        points(eibcf,pch=2,col="red")
        text(eibcf,rownames(eibcf),col="red",pos=3)
        par(new=TRUE)
        plot(epopular,type="l",col="brown",axes=FALSE)
        points(epopular,pch=3,col="brown")
        text(epopular,rownames(epopular),col="red",pos=3)
        par(new=TRUE)
        plot(erandom,type="l",col="blue",axes=FALSE)
        points(erandom,pch=4,col="blue")
        text(erandom,rownames(erandom),col="blue",pos=3)
        
        Build$eubcf<-eubcf
        Build$eibcf<-eibcf
        Build$epopular<-epopular
        Build$erandom<-erandom
        
        if(!is.null(Features())){
          ror.cb<-lapply(ntopL,function(i){predict(Build$CB_mod,newdata=getData(Build$e,"known"),type="topNList",n=i)})
          ecb<-lapply(ror.cb,function(i){calcPredictionAccuracy(i, getData(Build$e, "unknown"),given=-5,goodRating=Build$GR)})
          ecb<-t(as.data.frame(ecb))
          ecb<-ecb[,c(8,7)]
          rownames(ecb)<-c("1","3","5","10","15","20")
          par(new=TRUE)
          plot(ecb,type="l",col="black",axes=FALSE)
          points(ecb,pch=5,col="black")
          text(ecb,rownames(ecb),col="black",pos=3)
          legend("bottomright",legend = c("UBCF","IBCF","POPULAR","RANDOM","CONTENTBASED"),col = c("green","red","brown","blue","black"),pch = c(1,2,3,4,5))
          Build$ecb<-ecb
        }
        else{
          legend("bottomright",legend = c("UBCF","IBCF","POPULAR","RANDOM"),col = c("green","red","brown","blue"),pch = c(1,2,3,4))
          
        }
      }
      else {
        ror.selmod<-lapply(ntopL,function(i){predict(Build$selectmod,newdata=getData(Build$e,"known"),type="topNList",n=i)})
        eselmod<-lapply(ror.selmod,function(i){calcPredictionAccuracy(i, getData(Build$e, "known"),given=-200,goodRating=Build$GR)})
        eselmod<-t(as.data.frame(eselmod))
        eselmod<-eselmod[,c(8,7)]
        Build$eselmod<-eselmod
        rownames(eselmod)<-c("1","3","5","10","15","20")
        par(new=TRUE)
        plot(eselmod,type="l",col="green")#,ylim=c(0,1),xlim=c(0,1))
        points(eselmod,pch=3,col="green")
        text(eselmod,rownames(eselmod),col="green",pos=3)
        legend("bottomright",legend = Build$method,col = "green",pch = 3) 
        
      }
      }
    }
  })
  
  output$Metric<-renderPlot({
    if(Build$start){
      if(Build$TR=="Numeric"){
      if(Build$method=="Auto"){
        p.ubcf<-predict(Build$UBCF_mod,getData(Build$e,"known"),type="ratings")
        p.ibcf<-predict(Build$IBCF_mod,newdata=getData(Build$e,"known"),type="ratings")
        p.popular<-predict(Build$POPULAR_mod,newdata=getData(Build$e,"known"),type="ratings")
        p.random<-predict(Build$RANDOM_mod,newdata=getData(Build$e,"known"),type="ratings")
        
        error.ubcf<-calcPredictionAccuracy(p.ubcf, getData(Build$e, "unknown"),given=-5)
        error.ibcf<-calcPredictionAccuracy(p.ibcf, getData(Build$e, "unknown"),given=-5)
        error.popular<-calcPredictionAccuracy(p.popular, getData(Build$e, "unknown"),given=-5)
        error.random<-calcPredictionAccuracy(p.random, getData(Build$e, "unknown"),given=-5)
        
        error<-rbind(error.ubcf,error.ibcf,error.popular,error.random)
        if(!is.null(Features())){
          p.cb<-predict(Build$CB_mod,newdata=getData(Build$e,"known"),type="ratings")
          error.cb<-calcPredictionAccuracy(p.cb,getData(Build$e, "unknown"),given=-5)
          error<-rbind(error,error.cb)
          barplot(error,beside=TRUE,legend.text = c("UBCF","IBCF","POPULAR","RANDOM","CONTENTBASED"))
        }
        else{
          barplot(error,beside=TRUE,legend.text = c("UBCF","IBCF","POPULAR","RANDOM"))
        }
        Build$error<-error
        error<-rbind(error.ibcf,error.popular,error.random)
        merror<-error[,1]*0.3+error[,2]*0.35+error[,3]*.35
        ind_NA<-which(is.na(merror) || is.nan(merror) || is.infinite(merror) || is.null(merror))
        merror<-merror[-ind_NA]
        #print(merror)
        #print(error)
        minmod<-names(which(merror==min(merror)))
        
        Build$min<-minmod
        
        
        #if(minmod=="error.ubcf"){
          
        #}
        
      }
      else {
        p.selmod<-predict(Build$selectmod,getData(Build$e,"known"),type="ratings")
        error.selmod<-calcPredictionAccuracy(p.selmod, getData(Build$e, "unknown"))
        barplot(error.selmod,beside=TRUE,legend.text = Build$method,ylim = c(0,10))
        Build$error.selmod<-error.selmod
      }
      }
      Build$selectmod<-Build$IBCF_mod
      Build$complete<-TRUE
    }
    
  })
  
  
  output$SaveUI<-renderUI({
    if(Build$complete){
      tagList(
        textInput(inputId = "ModelName",label = "Save the trained Model.",placeholder = "Enter a Model name e.g. Movies_2016"),
        actionButton("Save",label ="Save")
      )
    }
  })
  
  observeEvent(input$Save,{
    if(input$Save>0){
      save(Build,file = paste0(input$ModelName,".RData"))
    }
  })
  
  ###AllUSERTAB#####
  
  output$default2<-renderText({
    if(is.null(contents())) Dtext
    else if(is.null(input$NewD) && Build$complete)"Please upload Test Data"
    else if(!Test$Begin)"Training in Progress...."
    else if(!Test$Finish)"Prediction in Progress...."
    else paste("Model Used to Recommend :",Build$selectmod@method)
  })
  
  
  output$Parameters<-renderUI({
    if(Test$Begin){
      tagList(
        fluidRow(  
          column(4,selectInput("UserID","Select User:",Test$Nuser)),
          column(8,selectInput("TopN","No. of Items to recommend:",seq(from=5,to=50,by=5),selected = "5"))
        )
      )
    }
  })    
  
  observeEvent(Build$complete,{
    if(!is.null(input$NewD) && Build$complete)
    {
      Test$infile3<-input$NewD
      Test$TestData<-read.csv(Test$infile3$datapath) 
      Test$Nuser<-unique(Test$TestData[,1])
      Test$NItem<-length(unique(Test$TestData[,3]))
      Test$r <- as(Test$TestData[,c(1,3,5)],"realRatingMatrix") 
      
      
      coltrain<-colnames(Build$r)
      coltest<-colnames(Test$r)
      if(length(setdiff(coltest,coltrain))>0){
        stop("Some Items in test data are were not in train data")
        }
      coltoadd<-setdiff(coltrain,coltest)
      coltoadd<-matrix(data=NA,nrow = nrow(Test$r),ncol =length(coltoadd),dimnames=list(NULL,coltoadd) )
      Test$r<-cbind(as(Test$r,"matrix"),coltoadd)
      Test$r<-as(Test$r,"realRatingMatrix")
      
      #print(nrow(Test$r))
      #print(str(Test$r))
      #print(str(Build$selectmod))
      
      Test$Begin<-TRUE
    }
  })
  
  
  
  
  output$ResultTable<-renderTable({
    if(!is.null(input$TopN)){
      
      Test$Finish<-FALSE
      Test$result<-predict(Build$selectmod,newdata=Test$r,type="topNList",n=input$TopN)
      #Test$result<-predict(Build$selectmod,newdata=getData(Build$e,"known"),type="topNList",n=input$TopN)
      
      
      RecItems<-as.character(Test$result@items[names(Test$result@items)==input$UserID][[1]])
      #index<-Build$trainData[,3]==RecItems
      index<-as.character()
     
      for(i in 1:length(RecItems)){
        index[i]<-grep(RecItems[i],Build$trainData[,3])[1]
      }
      ItemDescription<-Build$trainData[index,4]
      PreRating<-Test$result@ratings[names(Test$result@ratings)==input$UserID][[1]]
      Test$Finish<-TRUE
      FinalResult<-as.data.frame(cbind(RecItems,as.character(ItemDescription),PreRating))
      print(FinalResult)
      if(Build$selectmod@method!="POPULAR"){
      colnames(FinalResult)<-c("ItemID","Item Description","Predicted Rating for the User")
      }
      if(length(RecItems)==0){"No Recommended Items for this User"}
      else { FinalResult}
      print(FinalResult)
      
    }
  },bordered = TRUE,hover = TRUE,align = 'l')
  
  
  
  
  
  
  
  output$LNusers<-renderText({
    if(!is.null(SAVED$load$start)){
      paste("No. of Users:",SAVED$load$Nuser)
    }
  })
  
  output$LNitems<-renderText({
    if(!is.null(SAVED$load$start)){
      paste("Number of Items:",SAVED$load$NItem)
    }
  })
  
  output$LTRating<-renderText({
    if(!is.null(SAVED$load$start)){
      paste("Rating Type:",SAVED$load$TR)
    }
  })
  
  output$LRatingDist<-renderPlot({
    if(!is.null(SAVED$load$start)){
      hist(getRatings(SAVED$load$r),
           main="Histogram of Ratings")
    }
  })
  
  output$LUserDist<-renderPlot({
    if(!is.null(SAVED$load$start)){
      hist(rowCounts(SAVED$load$r),breaks = 100,
           main="Distribution of Ratings & User")
    }
  })
  
  output$LAvgRating<-renderPlot({
    if(!is.null(SAVED$load$start)){
      hist(colMeans(SAVED$load$r),breaks = 100,
           main="Distribution of Average Ratings")
    }
  })
  
  output$LROC<-renderPlot({
    if(!is.null(SAVED$load$start)){
      
      ntopL<-c(1,3,5,10,15,20)
      if(SAVED$load$method=="Auto"){
        plot(SAVED$load$eubcf,type="l",col="green",ylim=c(0,1),xlim=c(0,1))
        points(SAVED$load$eubcf,pch=1,col="green")
        text(SAVED$load$eubcf,rownames(SAVED$load$eubcf),col="green",pos=3)
        par(new=TRUE)
        plot(SAVED$load$eibcf,type="l",col="red",axes=FALSE)
        points(SAVED$load$eibcf,pch=2,col="red")
        text(SAVED$load$eibcf,rownames(SAVED$load$eibcf),col="red",pos=3)
        par(new=TRUE)
        plot(SAVED$load$epopular,type="l",col="brown",axes=FALSE)
        points(SAVED$load$epopular,pch=3,col="brown")
        text(SAVED$load$epopular,rownames(SAVED$load$epopular),col="red",pos=3)
        par(new=TRUE)
        plot(SAVED$load$erandom,type="l",col="blue",axes=FALSE)
        points(SAVED$load$erandom,pch=4,col="blue")
        text(SAVED$load$erandom,rownames(SAVED$load$erandom),col="blue",pos=3)
        
        
        if(!is.null(SAVED$load$FeaturesData)){
          par(new=TRUE)
          plot(SAVED$load$ecb,type="l",col="black",axes=FALSE)
          points(SAVED$load$ecb,pch=5,col="black")
          text(SAVED$load$ecb,rownames(SAVED$load$ecb),col="black",pos=3)
          legend("bottomright",legend = c("UBCF","IBCF","POPULAR","RANDOM","CONTENTBASED"),col = c("green","red","brown","blue","black"),pch = c(1,2,3,4,5))
          
        }
        else{
          legend("bottomright",legend = c("UBCF","IBCF","POPULAR","RANDOM"),col = c("green","red","brown","blue"),pch = c(1,2,3,4))
          
        }
      }
      else {

        par(new=TRUE)
        rownames(SAVED$load$eselmod)<-c("5","10","15","20","25","30")
        plot(SAVED$load$eselmod,type="l",col="green")
        points(SAVED$load$eselmod,pch=3,col="green")
        text(SAVED$load$eselmod,rownames(SAVED$load$eselmod),col="green",pos=3)
        legend("bottomright",legend = SAVED$load$method,col = "green",pch = 3) 
        
      }
    }
  })
  
  output$LMetric<-renderPlot({
    if(!is.null(SAVED$load$start)){
      if(SAVED$load$method=="Auto"){
        
        if(!is.null(SAVED$load$FeaturesData)){
          barplot(SAVED$load$error,beside=TRUE,legend.text = c("UBCF","IBCF","POPULAR","RANDOM","CONTENTBASED"))
        }
        else{
          barplot(SAVED$load$error,beside=TRUE,legend.text = c("UBCF","IBCF","POPULAR","RANDOM"))
        }
      }
      else {
        if(!is.null(SAVED$load$error.selmod)){
        barplot(SAVED$load$error.selmod,beside=TRUE,legend.text = Build$method,ylim = c(0,10))
        }
      }
      Test$complete<-TRUE
    }
  })
  
  
  
  
  output$Ldefault2<-renderText({
    if(is.null(SAVED$load$start)) Dtext
    else if(is.null(input$TestLoad))"Please upload Test Data"
    else if(!Test$Finish)"Prediction in Progress...."
    else paste("Model Used to Recommend :",SAVED$load$selectmod@method)
  })
  
  
  output$LParameters<-renderUI({
    if(Test$Begin){
      tagList(
        fluidRow(  
          column(4,selectInput("LUserID","Select User:",Test$Nuser)),
          column(8,selectInput("LTopN","No. of Items to recommend:",seq(from=5,to=50,by=5),selected = "5"))
        )
      )
    }
  }) 
  
  output$LResultTable<-renderTable({
    if(!is.null(input$LTopN)){
      
      Test$Finish<-FALSE
      Test$result<-predict(SAVED$load$selectmod,newdata=Test$r,type="topNList",n=input$LTopN)
      RecItems<-as.character(Test$result@items[names(Test$result@items)==input$LUserID][[1]])
      
      index<-as.character()
      for(i in 1:length(RecItems)){
        index[i]<-grep(RecItems[i],SAVED$load$trainData[,3])[1]
      }
      ItemDescription<-SAVED$load$trainData[index,4]
      PreRating<-Test$result@ratings[names(Test$result@ratings)==input$LUserID][[1]]
      Test$Finish<-TRUE
      FinalResult<-as.data.frame(cbind(RecItems,as.character(ItemDescription),PreRating))
      colnames(FinalResult)<-c("ItemID","Item Description","Predicted Rating for the User")
      if(length(RecItems)==0){"No Recommended Items for this User"}
      else { FinalResult}
      
    }
  },bordered = TRUE,hover = TRUE,align = 'l')
  
  
})










