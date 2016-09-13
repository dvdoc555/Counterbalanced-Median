library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)

Teams<-read.csv("Teams.csv")
Scoring<-read.csv("TotalScoring.csv")



counter.balanced.median<- function(x){
  len<-length(x)
  x<-sort(x)
  x<-data.frame(x)
  x$row<-row.names(x)
  med<-median(x$x)
  #test if the calculated median is part of the set, and if not
  #return the calculated median.
  
  if(identical(as.numeric(x[x$x==med,2]),numeric(0))) return(med)
  
  #find the range of the median
  
  Min<-min(as.numeric(x[x$x==med,2]))
  Max<-max(as.numeric(x[x$x==med,2]))
  #Where is the center of the medial range?
  if(abs(Min-(len+1)/2)>abs(Max-(len+1)/2)){
    #What is the number next to the median?
    temp1<-x[Max+1,1]
    #How far does that number go?
    temp2<-max(as.numeric(x[x$x==temp1,2]))
    #What is the counterbalence to the mean?
    temp3<-len+1-Min
    #Which is smaller?
    cb2<-min(c(temp2,temp3))
    #What is the counter balence to that number(cb2)?
    cb1<-len+1-cb2
  }else{
    temp1<-x[Min-1,1]
    temp2<-min(as.numeric(x[x$x==temp1,2]))
    temp3<-len+1-Max
    cb1<-max(c(temp2,temp3))
    cb2<-len+1-cb1
  }
  cb<-mean(x[cb1:cb2,1])
  cb
}


shinyServer(  
  function(input, output) { 
    output$Input1<-renderText(input$Input1)
    
    #SelTeams<-as.character(Teams[Teams$Year==1970,2])
    output$secondSelection <- renderUI({
      selectInput("Input2", "Team", 
              choices = as.character(Teams[Teams$Year==input$Input1,2]))
    })
   
    output$OriginalData=renderTable({
      Teams[Teams$Year==input$Input1&Teams$Team==input$Input2,2:10]
      
    })
    output$Input2<-renderText(input$Input2)
    
    
    output$mytable=renderTable({
      x<-Scoring[Scoring$Year==input$Input1&Scoring$Team==input$Input2,3]
      x<-as.integer(x)
      z<-(input$Input3)
      if(!z=="")z<-as.numeric(strsplit(z,",")[[1]])
      if(!is.na(z)&!z=="")x<-c(x,z)
      x[order(as.integer(x))]
      y<-Teams[Teams$Year==input$Input1&Teams$Team==input$Input2,2:10]
      y$Own<-mean(x)
      y$Own.CBM<-counter.balanced.median(x)
      y
      
    })
    output$mytable2=renderText({
      x<-Scoring[Scoring$Year==input$Input1&Scoring$Team==input$Input2,3]
      x<-as.integer(x)
      z<-(input$Input3)
      if(!z=="")z<-as.numeric(strsplit(z,",")[[1]])
      if(!is.na(z)&!z=="")x<-c(x,z)
      x[order(as.integer(x))]
      
    })
    
    output$OriginalPrediction=renderTable({
      Teams[Teams$Year==input$Input1&Teams$Team==input$Input2,c(11:15)]
      
    },digits=3)
    
    
    output$mytable3=renderTable({
      x<-Scoring[Scoring$Year==input$Input1&Scoring$Team==input$Input2,3]
      x<-as.integer(x)
      z<-(input$Input3)
      if(!z=="")z<-as.numeric(strsplit(z,",")[[1]])
      if(!is.na(z)&!z=="")x<-c(x,z)
      Opp<-Teams[Teams$Year==input$Input1&Teams$Team==input$Input2,5]
      Opp.CBM<-Teams[Teams$Year==input$Input1&Teams$Team==input$Input2,7]
      
      y<-Teams[Teams$Year==input$Input1&Teams$Team==input$Input2,c(11:15)]
      xM<-mean(x)
      xCBM<-counter.balanced.median(x)
      y$P1<-xM^1.87969970703125/(xM^1.87969970703125+Opp^1.87969970703125)
      y$P2<-xCBM^1.52496337890625/(xCBM^1.52496337890625+Opp.CBM^1.52496337890625)
      y$Dif<-xM-xCBM-Opp+Opp.CBM
      y$P3<-(xM^1.914306640625/(xM^1.914306640625+Opp^1.914306640625))*0.99871826171875-y$Dif*0.03375244140625
      y
    },digits=3)
    output$RPGHist<-renderPlot({
      x<-Scoring[Scoring$Year==input$Input1&Scoring$Team==input$Input2,3]
      x<-as.integer(x)
      z<-(input$Input3)
      if(!z=="")z<-as.numeric(strsplit(z,",")[[1]])
      if(!is.na(z)&!z=="")x<-c(x,z)
      x[order(as.integer(x))]
      hist(x,breaks=31,xlab="Runs",col="red")
    })
    }
)