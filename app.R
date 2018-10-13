library(shiny)
library(shinythemes)
library(png)
library(shinydashboard)
library(dplyr)
library(data.table)
library(shinyWidgets)
library(caret)
library(DT)
library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)
library(forecast)
library(prophet)

setwd("/Users/sush/Desktop/Purdue/Project")
#Read train dataset
train <- as.tibble(fread("train.csv", skip = 86672217, header = FALSE))
setnames(train, c("id","date", "store_nbr", "item_nbr", "unit_sales","onpromotion"))
train$date=as.Date(train$date)
#Read items
items = as.tibble(fread('items.csv'))
str(items)
#Read stores
stores = as.tibble(fread('stores.csv'))
str(stores)

###################################################################################

set.seed(1234)

######################################################################

# item number, store number, date, promotion
ui<-fluidPage(theme = shinytheme("darkly"),
              titlePanel(column(8, offset = , tags$img(src = "https://image.slidesharecdn.com/corporacionlafavorita1-171204004915/95/corporacion-la-favorita-1-638.jpg?cb=1512348601", width = 250))),
              column(8, offset = 2 , tags$hr(style="border: solid 1.5px black")),
              tags$head(
                tags$style(HTML("body{
                                background-image: url(https://a0.muscache.com/airbnb/guidebook/v1_grocery_store_hero@2x.jpg);
                                }"))),
  fluidRow(column(width = 4, offset = 1, 
                  wellPanel(top = 75, style="border: solid 2px green;background-color:black",
                            selectInput("item",
                                        unique(items[ ,1]),
                                        label = h2("Enter Item Number", 
                                                   style = "color:gold;font-family:Courier;text-align:center",
                                                   icon("number",class="fa-align-right fa-1x"))
                            ),
                            selectInput("store",
                                        unique(stores[ , 1]),
                                        label = h2("Enter Store Number", 
                                                   style = "color:gold;font-family:Courier;text-align:center",
                                                   icon("list-ol",class="fa-align-right fa-1x"))
                            ),
                            selectizeInput("onpromotion",
                                           h2("Is it on promotion?", 
                                              style = "color:gold;font-family:Courier;text-align:center",
                                              icon("list-ol",class="fa-align-right fa-1x")),
                                           c("Yes","No")),
                            
                            dateInput("date",
                                      value= '2017-08-16',
                                      min = '2017-08-16',
                                      max = '2017-08-31',
                                      label = h2("Input the Date", 
                                                 style = "color:gold;font-family:Courier;text-align:left",
                                                 icon("hourglass",class="fa-align-right fa-1x"))
                            )
                            
                  )),
           column(width = 6,
                  wellPanel(style="border: solid 2px green;background-color: black",
                            h2(textOutput("pred1"),
                               style = "display:inline-block;width:100%;text-align: center; font-size: 30px;background-color:gold"),
                            h3(textOutput("pred2"),
                               style = "color:gold;font-family:Courier;text-align:justified"),
                            h3(textOutput("pred3"),
                               style = "color:gold;font-family:Courier;text-align:justified"),
                            h3(textOutput("pred4"),
                               style = "color:gold;font-family:Courier;text-align:justified"),
                            h3(textOutput("pred5"),
                               style = "color:gold;font-family:Courier;text-align:justified"),
                            actionButton("go", "Predict the sales", 
                                         style="display:inline-block;width:100%;text-align: center; font-size: 30px;background-color:gold",
                                         icon = icon("exclamation-sign",lib="glyphicon")),
                            h2("The Predicted Sales are: ", style="color:gold"),
                            verbatimTextOutput("value"),
                            h1(textOutput("header"),
                               style = "color:gold;font-family:Courier;text-align:centre")
                            
                  ))))

#input <- as.data.frame(cbind(store = 44, item = 329362, date = "2017-08-05"))
head(train)
#input$store

server<-function(input, output) {
  data1<-reactive({
    ifelse(input$onpromotion=="Yes",1,0)})
  
  output$value <- renderText({
    if (input$go > 0){
#########################################################      
      total = train %>%
        filter(store_nbr == input$store) %>%
        filter(item_nbr == input$item) %>%
        arrange(desc(date)) %>%
        select(date,unit_sales) %>% head(60)
      
      colnames(total) = c("ds","y")
      
      if(input$onpromotion=="Yes"){
      if(nrow(total)>10){
      m <- prophet(total,changepoint.prior.scale = 0.1)
      
      future <- make_future_dataframe(m, periods = 15,freq = "day")
      
      forecast <- predict(m, future)
      predictions = tail(round(forecast$yhat),15)
      a=predictions[(day(as.Date(input$date))-15)]
      paste(a*1.5)
      
      
      
      }
      else{
        k=mean(train[train["item_nbr"]==input$item,"unit_sales"])
        paste(k*1.5)
        }
      } else {
        
        if(nrow(total)>10){
          m <- prophet(total,changepoint.prior.scale = 0.1)
          
          future <- make_future_dataframe(m, periods = 15,freq = "day")
          
          forecast <- predict(m, future)
          predictions = tail(round(forecast$yhat),15)
          a=predictions[(day(as.Date(input$date))-15)]
          paste(a)
          
          
          
        }
        else{
          k=mean(train[train["item_nbr"]==input$item,"unit_sales"])
          paste(k)
        }
      }
#########################################################      
      #pred <- predict(lmfit,
      #                newdata = data.frame(item_nbr=as.numeric(input$item),
      #                                     store_nbr=as.numeric(input$store),
      #                                     date=input$date,
      #                                     onpromotion=as.numeric(data1()),
      #                                     
      #                                     interval="predict"))
      #
      #a<-pred
      #paste(a[1])
    }}
  )
  output$pred1 <- renderText({  
    paste("Review your choices!")
  })
  output$pred2 <- renderText({
    paste("Item number you selected is ", input$item)
  })
  output$pred3 <- renderText({
    paste("You selected item number ", input$store)})
  output$pred4 <- renderText({
    paste("Is the item on promotion?", input$onpromotion)})
  output$pred5 <- renderText({
    paste("Date selected", input$date)})
  }
shinyApp(ui = ui, server=server)
