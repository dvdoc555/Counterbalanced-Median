library(shiny)
library(shinydashboard)
library(dplyr)


shinyUI(pageWithSidebar(  
  headerPanel("Choices"),  
  sidebarPanel(    
    h5("Select a Team and a Year below"),
    selectInput(  "Input1", "Year:",
                  c(1970:2013)),
    uiOutput("secondSelection"),
    h6("Enter comma seperated numeric values below"),
    textInput(inputId="Input3",label = "Additional Data"),
    h6("The data will be added and the mean and the counter balanced median 
       (CBM) will be recalculated"),
    h3("Glossary"),
    h5("Team: Abbreviation for Team playing Major League Baseball"),
    h5("Own: Mean Runs per game for the team"),
    h5("Opp: Mean Runs allowed per game for the Team"),
    h5("Own.CBM: Counter Balanced Median Runs per game for the team"),
    h5("Opp.CBM: Counter Balanced Median Runs allowed per game for the team"),
    h5("G: Games Played by the team"),
    h5("W: Wins by the team"),
    h5("L: Losses by the team"),
       h5("WP: Winning Percentage - Wins/(Wins+Losses)"),
       h5("CBM: Counter Balenced Median"),
       h5("P1: Also known as the Pythogrian Baseball Theorem.  My exact 
       calculation is Own^X/(Own^X+Opp^X) and X equals 1.87969970703125"),
       h5("P2: Own.CBB^X/(Own.CBB^X+Opp.CBB^X) and X equals 1.52496337890625"),
       h5("Dif: Own-Own.CBM+Opp.CBM-Opp"),
       h5("P3: (Own^X/(Own^X+Opp^X)) *  0.99871826171875)-Dif*0.03375244140625
    and X equals 1.914306640625") 
       ),
    mainPanel(    
    h3('Original Data'),
    tableOutput('OriginalData'),
    h3('Results with Added Data'),
    tableOutput('mytable'),
    h3('Raw Data with Added Data'),
    h5('Rank ordered runs in each game for the team listed above with Data
       added in the text box to the right. The initial number should match
       G above.'),
    tableOutput('mytable2'),
    h3('Original Prediction'),
    tableOutput('OriginalPrediction'),
    h3('Results with Added Data'),
    tableOutput('mytable3'),
    plotOutput('RPGHist')
    )
       ))