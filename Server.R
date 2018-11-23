library(shinydashboard)
library(caret)
library(ROCR)
set.seed(13) 

shinyServer(function(input,output){
  #========================================Output_of_Routewise_Analytics=================================  
  
  output$Routewise_Cancellation_Reasons <- renderTable({
    
    subset(Delayreasonrevised,Delayreasonrevised$AIRLINE==input$AIRLINE & Delayreasonrevised$ORIGIN_AIRPORT==input$ORIGIN_AIRPORT & Delayreasonrevised$DESTINATION_AIRPORT==input$DESTINATION_AIRPORT)
    
    #subset(Routewise_Cancellation_Reasons,Routewise_Cancellation_Reasons$ORIGIN_AIRPORT==input$ORIGIN_AIRPORT)
    
    #subset(Routewise_Cancellation_Reasons,Routewise_Cancellation_Reasons$DESTINATION_AIRPORT==input$DESTINATION_AIRPORT)
    
    
    
    
  })
  #==========================================output of Airport Analytics======================
  output$Airportwise_analytics <- renderTable({
    
    subset(Airport_Iatacode_Details,Airport_Iatacode_Details$STATE==input$STATE)
    
    
    
    
  })
  #output$Countrywise <- renderPlotly({
    #Airport_Iatacode_Details <- plot_ly(y=Airport_Iatacode_Details$AIRPORT, x=Airport_Iatacode_Details$CITY,color=Airport_Iatacode_Details$COUNTRY,type = "bar") 
    
    
  #})
  
  output$Airportwise <- renderPlotly({
    Airport_Iatacode_Details <- plot_ly(y=Airport_Iatacode_Details$CITY, x=Airport_Iatacode_Details$AIRPORT,color=Airport_Iatacode_Details$STATE,type = "scatter") 
    
    
  })
  
  
  
  
  #==========================================output_of_Airlinewise_Analytics==============================
  output$Airlines_Full_Details <- renderTable({ 
    subset(Delayreasonrevised, Delayreasonrevised$AIRLINE==input$Airlinename)
  })
  #====================================Output_of_Departure_Delay_Analytics=====================
  
  output$PlotofOnTime <- renderPlotly({
    plot_ly(Delayreasonrevised, x=~Delayreasonrevised$Year, y =~Delayreasonrevised$On_Time, color =~Delayreasonrevised$AIRLINE, type = "bar") 
    
  }) 
  
  output$plotmOfDelay <- renderPlotly({
    plot_ly(Delayreasonrevised, x=~Delayreasonrevised$AIRLINE, y =~Delayreasonrevised$Delay_Time, color =~Delayreasonrevised$Month, type = "bar") 
    
    
    # ggplotly(Delay_Reasons, aes(x = Delay_Reasons$AIRLINE, y = Delay_Reasons$Delay_Time, col = Delay_Reasons$Year)) +
    #  geom_point()
    
    # Make your plot interactive
    # ggplotly()
    
    
  })
  output$ plotmOfDelaybydayofweek <- renderPlotly({
    plot_ly(Delayreasonrevised, x=~Delayreasonrevised$DayOfWeek, y =~Delayreasonrevised$Delay_Time, color =~Delayreasonrevised$AIRLINE, type = "bar") 
  })
  
  
  #=====================================Cancellation_analysis=============
  output$Plotofcancel <- renderPlot({
    
    Delayreasonrevised$CANCELLED[Delayreasonrevised$Cancelled == 0] = 'No'
    Delayreasonrevised$CANCELLED[Delayreasonrevised$Cancelled == 1] = 'Yes'
    
    qplot(factor(Delayreasonrevised$CANCELLED), data= Delayreasonrevised, geom="bar", fill=factor(Delayreasonrevised$CANCELLED))
    
    
  }) 
  output$plotmOfcancellationreason <- renderPlot({
    
    Delayreasonrevised$CReason[Delayreasonrevised$CReason == 'A'] = 'Carrier'
    Delayreasonrevised$CReason[Delayreasonrevised$CReason == 'B'] = 'Weather'
    Delayreasonrevised$CReason[Delayreasonrevised$CReason == 'C'] = 'NAS'
    Delayreasonrevised$CReason[Delayreasonrevised$CReason == 'D'] = 'Security'
    
    Delayreasonrevised %>% filter(Delayreasonrevised$CReason != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubset = subset(Delayreasonrevised, CReason != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    ggplot(CancelledSubset,aes(Year,fill=CReason)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
  })
  output$plotmOfcancellationreasonwithairline <- renderPlot({
    
    Delayreasonrevised$CReason[Delayreasonrevised$CReason == 'A'] = 'Carrier'
    Delayreasonrevised$CReason[Delayreasonrevised$CReason == 'B'] = 'Weather'
    Delayreasonrevised$CReason[Delayreasonrevised$CReason == 'C'] = 'NAS'
    Delayreasonrevised$CReason[Delayreasonrevised$CReason == 'D'] = 'Security'
    
    Delayreasonrevised %>% filter(Delayreasonrevised$CReason != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubset = subset(Delayreasonrevised, CReason != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    ggplot(CancelledSubset,aes(AIRLINE,fill=CReason)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
  })
  
  #===========================Review_analytics===============
  output$wordcloud <- renderPlot({
    setwd("F:/MSC II  project final/M.sc sem 4 airline final project/Final Dataset")
    getwd()
    readLines("content.csv")
    str(readLines("content.csv"))
    text <- paste(readLines("content.csv"), collapse = " ")
    text
    text2 <- gsub(pattern = "\\W", replace =" ", text)
    text2
    text2 <- gsub(pattern = "\\d", replace =" ", text2)
    text2
    text2 <- tolower(text2)
    text2
    library(tm)
    #stopwords()
    #text2
    #removeWords(text2, stopwords())
    #text2 <- removeWords(text2, stopwords())
    #text2
    text2 <- gsub(pattern = "\\b[A-z]\\b{1}}", replace=" ", text2)
    text2
    text2 <- stripWhitespace(text2)
    text2
    library(stringr)
    library(wordcloud)
    library(plotly)
    text2
    textbag <- str_split(text2, pattern="\\s+")
    textbag
    class(textbag)
    textbag <- unlist(textbag)
    class(textbag)
    textbag
    str(textbag)
    getwd()
    readLines("positive-word.txt")
    poswords <- scan("positive-word.txt", what = 'character', comment.char = ";")
    str(poswords)
    negwords <- scan("negative-word.txt", what = 'character', comment.char = ";")
    str(negwords)
    match(textbag, poswords)
    match(textbag, negwords)
    ##posword <- c(pos, 'interesting')
    ##negwords <- c(neg, 'not interesting') 
    posscore <- sum(!is.na(match(textbag, poswords)))
    posscore
    negscore <- sum(!is.na(match(textbag, negwords)))
    negscore
    score <- sum(!is.na(match(textbag, poswords))) - sum(!is.na(match(textbag, negwords)))
    score
    mean(score)
    hist(score)
    sd(score)
    wordcloud(textbag)
    # wordcloud(textbag, min.freq = 4)
    wordcloud(textbag,min.freq = 4, random.order = FALSE, scale = c(3, 0.5), colors = rainbow(3))
    
    
    
    
  })
  
  output$sentimental<- renderPlot({
    
    Review_Analysis$Total[Review_Analysis$Total == 'positive'] = 'positive'
    Review_Analysis$Total[Review_Analysis$Total == 'negative'] = 'negative'
    
    qplot(factor(Review_Analysis$Total), data= Review_Analysis, geom="bar", fill=factor(Review_Analysis$Total))
    
    
  }) 
  output$sentimentalall <- renderPlotly({
    
    
    Review_Analysis$Total <- Review_Analysis$Total 
    
    
    
    
    Review_Analysis$Total[Review_Analysis$Total == 0] <- 'negative'
    Review_Analysis$Total[Review_Analysis$Total == 1] <- 'positive'
    
    p <- ggplot(Review_Analysis, aes(Total, fill = Review_Analysis$`Airline name`)) + 
      geom_bar(width=0.8, position="dodge",color="black")+
      labs(x = "Total", y = "Count",title = "Reviews", fill = "Airlines")+
      theme(legend.text = element_text(colour="blue", size=10, 
                                       face="bold"))
    print(p)
    ggplotly(p)
    
    
  })
  
  
  
  
  
  #=============================Recommended_Analytics===========
  #output$positivenegativegraph <- renderPlotly({
  #plot_ly(Review_Analysis, x=Review_Analysis$`Airline name`, y =Review_Analysis$year, color =Review_Analysis$Total, type = "scatter") 
  
  #}) 
  output$positivenegativegraph <- renderPlotly({
    p <- plot_ly( x = Review_Analysis$recommended, y = Review_Analysis$`Airline name`,color = Review_Analysis$Total, type = 'bar', name = 'Airsystemdelay')
    
    print(p)
    
  })
  #================which airline to choose========
  output$airlinechoose <- renderPlotly({
    DelayreasonrevisedDayOfWeek <- Delayreasonrevised$DayOfWeek
    Delayreasonrevised$Month <- Delayreasonrevised$Month
    
    # Define levels
    Delayreasonrevised$AIRLINE <- factor(Delayreasonrevised$AIRLINE)
    Delayreasonrevised$Year <- factor(Delayreasonrevised$Year)
    Delayreasonrevised$Month <- factor(Delayreasonrevised$Month)
    Delayreasonrevised$ORIGIN_AIRPORT <- factor(Delayreasonrevised$ORIGIN_AIRPORT)
    Delayreasonrevised$DESTINATION_AIRPORT <- factor(Delayreasonrevised$DESTINATION_AIRPORT)
    
    # Define numbers by actual days and months.
    
    
    Delayreasonrevised$Month[Delayreasonrevised$Month == 1] <- 'January'
    Delayreasonrevised$Month[Delayreasonrevised$Month == 2] <- 'February'
    Delayreasonrevised$Month[Delayreasonrevised$Month == 3] <- 'March'
    Delayreasonrevised$Month[Delayreasonrevised$Month == 4] <- 'April'
    Delayreasonrevised$Month[Delayreasonrevised$Month == 5] <- 'May'
    Delayreasonrevised$Month[Delayreasonrevised$Month == 6] <- 'June'
    Delayreasonrevised$Month[Delayreasonrevised$Month == 7] <-'July'
    Delayreasonrevised$Month[Delayreasonrevised$Month == 8] <- 'August'
    Delayreasonrevised$Month[Delayreasonrevised$Month == 9] <- 'September'
    Delayreasonrevised$Month[Delayreasonrevised$Month == 10] <- 'October'
    Delayreasonrevised$Month[Delayreasonrevised$Month == 11] <- 'November'
    Delayreasonrevised$Month[Delayreasonrevised$Month == 12] <- 'December'
    p <- ggplot(Delayreasonrevised, aes(Month, fill = Delayreasonrevised$AIRLINE)) + 
      geom_bar(width=0.8, position="dodge",color="black")+
      labs(x = "Month", y = "Count",title = "Flight Delay Counts by Airline Carriers(By Month)", fill = "Airlines")+
      theme(legend.text = element_text(colour="blue", size=10, 
                                       face="bold"))
    print(p)
    ggplotly(p)
    
    
  })
  
  output$airlinechooseweek <- renderPlotly({
    Delayreasonrevised$DayOfWeek <- Delayreasonrevised$DayOfWeek
    Delayreasonrevised$Month <- Delayreasonrevised$Month
    
    # Define levels
    Delayreasonrevised$AIRLINE <- factor(Delayreasonrevised$AIRLINE)
    Delayreasonrevised$Year <- factor(Delayreasonrevised$Year)
    Delayreasonrevised$Month <- factor(Delayreasonrevised$Month)
    Delayreasonrevised$ORIGIN_AIRPORT <- factor(Delayreasonrevised$ORIGIN_AIRPORT)
    Delayreasonrevised$DESTINATION_AIRPORT <- factor(Delayreasonrevised$DESTINATION_AIRPORT)
    
    
    # Define numbers by actual days and months.
    Delayreasonrevised$DayOfWeek[Delayreasonrevised$DayOfWeek == 1] <- 'Monday'
    Delayreasonrevised$DayOfWeek[Delayreasonrevised$DayOfWeek == 2] <- 'Tuesday'
    Delayreasonrevised$DayOfWeek[Delayreasonrevised$DayOfWeek == 3] <- 'Wednesday'
    Delayreasonrevised$DayOfWeek[Delayreasonrevised$DayOfWeek == 4] <- 'Thursday'
    Delayreasonrevised$DayOfWeek[Delayreasonrevised$DayOfWeek == 5] <- 'Friday'
    Delayreasonrevised$DayOfWeek[Delayreasonrevised$DayOfWeek == 6] <- 'Saturday'
    Delayreasonrevised$DayOfWeek[Delayreasonrevised$DayOfWeek == 7] <- 'Sunday'
    
    
    
    p <- ggplot(Delayreasonrevised, aes(DayOfWeek, fill = Delayreasonrevised$AIRLINE)) + 
      geom_bar(width=0.8, position="dodge",color="black")+
      labs(x = "DayOfWeek", y = "Count",title = "Flight Delay Counts by Airline Carriers(By DayOfWeek)", fill = "Airlines")+
      theme(legend.text = element_text(colour="blue", size=10, 
                                       face="bold"))
    print(p)
    ggplotly(p)
    
  })
  
  
  #=========================UA airline  frequent delay reason(delay analytics)========
  output$UAairlinechoose <- renderPlotly({
    
    UAdelayreason$Total[ UAdelayreason$Total == '1'] = 'Air system delay'
    UAdelayreason$Total[ UAdelayreason$Total == '2'] = 'Security delay'
    UAdelayreason$Total[ UAdelayreason$Total == '3'] = 'Airline delay'
    UAdelayreason$Total[ UAdelayreason$Total == '4'] = 'Late aircraft delay'
    UAdelayreason$Total[ UAdelayreason$Total == '5'] = 'Weather delay'
    UAdelayreason$Total[ UAdelayreason$Total == '6'] = 'Carrier delay'
    UAdelayreason$Total[ UAdelayreason$Total == '7'] = 'NAS delay'
    UAdelayreason$Total[ UAdelayreason$Total == '8'] = 'Baggage error'
    UAdelayreason$Total[ UAdelayreason$Total == '9'] = 'Airspace congestion'
    UAdelayreason$Total[ UAdelayreason$Total == '10'] = 'suddenly landing due to technical problem'
    
    UAdelayreason %>% filter(UAdelayreason$Total != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubsets = subset(UAdelayreason, Total != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    p <- ggplot(CancelledSubsets,aes(Month,fill=Total)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
    print(p)
    ggplotly(p)
  })
  
  #=========================AA airline  frequent delay reason(delay analytics)========
  output$AAairlinechoose <- renderPlotly({
    
    AAdelayreason$Total[ AAdelayreason$Total == '1'] = 'Air system delay'
    AAdelayreason$Total[ AAdelayreason$Total == '2'] = 'Security delay'
    AAdelayreason$Total[ AAdelayreason$Total == '3'] = 'Airline delay'
    AAdelayreason$Total[ AAdelayreason$Total == '4'] = 'Late aircraft delay'
    AAdelayreason$Total[ AAdelayreason$Total == '5'] = 'Weather delay'
    AAdelayreason$Total[ AAdelayreason$Total == '6'] = 'Carrier delay'
    AAdelayreason$Total[ AAdelayreason$Total == '7'] = 'NAS delay'
    AAdelayreason$Total[ AAdelayreason$Total == '8'] = 'Baggage error'
    AAdelayreason$Total[ AAdelayreason$Total == '9'] = 'Airspace congestion'
    AAdelayreason$Total[ AAdelayreason$Total == '10'] = 'suddenly landing due to technical problem'
    
    AAdelayreason %>% filter(AAdelayreason$Total != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubsets = subset(AAdelayreason, Total != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    p <- ggplot(CancelledSubsets,aes(Month,fill=Total)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
    print(p)
    ggplotly(p)
  })
  
  #=========================US airline  frequent delay reason(delay analytics)========
  output$USairlinechoose <- renderPlotly({
    
    USdelayreason$Total[ USdelayreason$Total == '1'] = 'Air system delay'
    USdelayreason$Total[ USdelayreason$Total == '2'] = 'Security delay'
    USdelayreason$Total[ USdelayreason$Total == '3'] = 'Airline delay'
    USdelayreason$Total[ USdelayreason$Total == '4'] = 'Late aircraft delay'
    USdelayreason$Total[ USdelayreason$Total == '5'] = 'Weather delay'
    USdelayreason$Total[ USdelayreason$Total == '6'] = 'Carrier delay'
    USdelayreason$Total[ USdelayreason$Total == '7'] = 'NAS delay'
    USdelayreason$Total[ USdelayreason$Total == '8'] = 'Baggage error'
    USdelayreason$Total[ USdelayreason$Total == '9'] = 'Airspace congestion'
    USdelayreason$Total[ USdelayreason$Total == '10'] = 'suddenly landing due to technical problem'
    
    USdelayreason %>% filter(USdelayreason$Total != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubsets = subset(USdelayreason, Total != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    p <- ggplot(CancelledSubsets,aes(Month,fill=Total)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
    print(p)
    ggplotly(p)
  })
  #=========================F9 airline  frequent delay reason(delay analytics)========
  output$F9airlinechoose <- renderPlotly({
    
    F9delayreason$Total[ F9delayreason$Total == '1'] = 'Air system delay'
    F9delayreason$Total[ F9delayreason$Total == '2'] = 'Security delay'
    F9delayreason$Total[ F9delayreason$Total == '3'] = 'Airline delay'
    F9delayreason$Total[ F9delayreason$Total == '4'] = 'Late aircraft delay'
    F9delayreason$Total[ F9delayreason$Total == '5'] = 'Weather delay'
    F9delayreason$Total[ F9delayreason$Total == '6'] = 'Carrier delay'
    F9delayreason$Total[ F9delayreason$Total == '7'] = 'NAS delay'
    F9delayreason$Total[ F9delayreason$Total == '8'] = 'Baggage error'
    F9delayreason$Total[ F9delayreason$Total == '9'] = 'Airspace congestion'
    F9delayreason$Total[ F9delayreason$Total == '10'] = 'suddenly landing due to technical problem'
    
    F9delayreason %>% filter(F9delayreason$Total != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubsets = subset(F9delayreason, Total != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    p <- ggplot(CancelledSubsets,aes(Month,fill=Total)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
    print(p)
    ggplotly(p)
  })
  
  #=========================B6 airline  frequent delay reason(delay analytics)========
  output$B6airlinechoose <- renderPlotly({
    
    B6delayreason$Total[ B6delayreason$Total == '1'] = 'Air system delay'
    B6delayreason$Total[ B6delayreason$Total == '2'] = 'Security delay'
    B6delayreason$Total[ B6delayreason$Total == '3'] = 'Airline delay'
    B6delayreason$Total[ B6delayreason$Total == '4'] = 'Late aircraft delay'
    B6delayreason$Total[ B6delayreason$Total == '5'] = 'Weather delay'
    B6delayreason$Total[ B6delayreason$Total == '6'] = 'Carrier delay'
    B6delayreason$Total[ B6delayreason$Total == '7'] = 'NAS delay'
    B6delayreason$Total[ B6delayreason$Total == '8'] = 'Baggage error'
    B6delayreason$Total[ B6delayreason$Total == '9'] = 'Airspace congestion'
    B6delayreason$Total[ B6delayreason$Total == '10'] = 'suddenly landing due to technical problem'
    
    B6delayreason %>% filter(B6delayreason$Total != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubsets = subset(B6delayreason, Total != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    p <- ggplot(CancelledSubsets,aes(Month,fill=Total)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
    print(p)
    ggplotly(p)
  })
  
  #=========================OO airline  frequent delay reason(delay analytics)========
  output$OOairlinechoose <- renderPlotly({
    
    OOdelayreason$Total[ OOdelayreason$Total == '1'] = 'Air system delay'
    OOdelayreason$Total[ OOdelayreason$Total == '2'] = 'Security delay'
    OOdelayreason$Total[ OOdelayreason$Total == '3'] = 'Airline delay'
    OOdelayreason$Total[ OOdelayreason$Total == '4'] = 'Late aircraft delay'
    OOdelayreason$Total[ OOdelayreason$Total == '5'] = 'Weather delay'
    OOdelayreason$Total[ OOdelayreason$Total == '6'] = 'Carrier delay'
    OOdelayreason$Total[ OOdelayreason$Total == '7'] = 'NAS delay'
    OOdelayreason$Total[ OOdelayreason$Total == '8'] = 'Baggage error'
    OOdelayreason$Total[ OOdelayreason$Total == '9'] = 'Airspace congestion'
    OOdelayreason$Total[ OOdelayreason$Total == '10'] = 'suddenly landing due to technical problem'
    
    OOdelayreason %>% filter(OOdelayreason$Total != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubsets = subset(OOdelayreason, Total != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    p <- ggplot(CancelledSubsets,aes(Month,fill=Total)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
    print(p)
    ggplotly(p)
  })
  #=========================AS airline  frequent delay reason(delay analytics)========
  output$ASairlinechoose <- renderPlotly({
    
    ASdelayreason$Total[ ASdelayreason$Total == '1'] = 'Air system delay'
    ASdelayreason$Total[ ASdelayreason$Total == '2'] = 'Security delay'
    ASdelayreason$Total[ ASdelayreason$Total == '3'] = 'Airline delay'
    ASdelayreason$Total[ ASdelayreason$Total == '4'] = 'Late aircraft delay'
    ASdelayreason$Total[ ASdelayreason$Total == '5'] = 'Weather delay'
    ASdelayreason$Total[ ASdelayreason$Total == '6'] = 'Carrier delay'
    ASdelayreason$Total[ ASdelayreason$Total == '7'] = 'NAS delay'
    ASdelayreason$Total[ ASdelayreason$Total == '8'] = 'Baggage error'
    ASdelayreason$Total[ ASdelayreason$Total == '9'] = 'Airspace congestion'
    ASdelayreason$Total[ ASdelayreason$Total == '10'] = 'suddenly landing due to technical problem'
    
    ASdelayreason %>% filter(ASdelayreason$Total != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubsets = subset(ASdelayreason, Total != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    p <- ggplot(CancelledSubsets,aes(Month,fill=Total)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
    print(p)
    ggplotly(p)
  })
  #=========================NK  airline  frequent delay reason(delay analytics)========
  output$NKairlinechoose <- renderPlotly({
    
    NKdelayreason$Total[ NKdelayreason$Total == '1'] = 'Air system delay'
    NKdelayreason$Total[ NKdelayreason$Total == '2'] = 'Security delay'
    NKdelayreason$Total[ NKdelayreason$Total == '3'] = 'Airline delay'
    NKdelayreason$Total[ NKdelayreason$Total == '4'] = 'Late aircraft delay'
    NKdelayreason$Total[ NKdelayreason$Total == '5'] = 'Weather delay'
    NKdelayreason$Total[ NKdelayreason$Total == '6'] = 'Carrier delay'
    NKdelayreason$Total[ NKdelayreason$Total == '7'] = 'NAS delay'
    NKdelayreason$Total[ NKdelayreason$Total == '8'] = 'Baggage error'
    NKdelayreason$Total[ NKdelayreason$Total == '9'] = 'Airspace congestion'
    NKdelayreason$Total[ NKdelayreason$Total == '10'] = 'suddenly landing due to technical problem'
    
    NKdelayreason %>% filter(NKdelayreason$Total != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubsets = subset(NKdelayreason, Total != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    p <- ggplot(CancelledSubsets,aes(Month,fill=Total)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
    print(p)
    ggplotly(p)
  })
  
  #=========================WN airline  frequent delay reason(delay analytics)========
  output$WNairlinechoose <- renderPlotly({
    
    WNdelayreason$Total[ WNdelayreason$Total == '1'] = 'Air system delay'
    WNdelayreason$Total[ WNdelayreason$Total == '2'] = 'Security delay'
    WNdelayreason$Total[ WNdelayreason$Total == '3'] = 'Airline delay'
    WNdelayreason$Total[ WNdelayreason$Total == '4'] = 'Late aircraft delay'
    WNdelayreason$Total[ WNdelayreason$Total == '5'] = 'Weather delay'
    WNdelayreason$Total[ WNdelayreason$Total == '6'] = 'Carrier delay'
    WNdelayreason$Total[ WNdelayreason$Total == '7'] = 'NAS delay'
    WNdelayreason$Total[ WNdelayreason$Total == '8'] = 'Baggage error'
    WNdelayreason$Total[ WNdelayreason$Total == '9'] = 'Airspace congestion'
    WNdelayreason$Total[ WNdelayreason$Total == '10'] = 'suddenly landing due to technical problem'
    
    WNdelayreason %>% filter(WNdelayreason$Total != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubsets = subset(WNdelayreason, Total != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    p <- ggplot(CancelledSubsets,aes(Month,fill=Total)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
    print(p)
    ggplotly(p)
  })
  #=========================DL airline  frequent delay reason(delay analytics)========
  output$DLairlinechoose <- renderPlotly({
    
    DLdelayreason$Total[ DLdelayreason$Total == '1'] = 'Air system delay'
    DLdelayreason$Total[ DLdelayreason$Total == '2'] = 'Security delay'
    DLdelayreason$Total[ DLdelayreason$Total == '3'] = 'Airline delay'
    DLdelayreason$Total[ DLdelayreason$Total == '4'] = 'Late aircraft delay'
    DLdelayreason$Total[ DLdelayreason$Total == '5'] = 'Weather delay'
    DLdelayreason$Total[ DLdelayreason$Total == '6'] = 'Carrier delay'
    DLdelayreason$Total[ DLdelayreason$Total == '7'] = 'NAS delay'
    DLdelayreason$Total[ DLdelayreason$Total == '8'] = 'Baggage error'
    DLdelayreason$Total[ DLdelayreason$Total == '9'] = 'Airspace congestion'
    DLdelayreason$Total[ DLdelayreason$Total == '10'] = 'suddenly landing due to technical problem'
    
    DLdelayreason %>% filter(DLdelayreason$Total != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubsets = subset(DLdelayreason, Total != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    p <- ggplot(CancelledSubsets,aes(Month,fill=Total)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
    print(p)
    ggplotly(p)
  })
  #=========================EV airline  frequent delay reason(delay analytics)========
  output$EVairlinechoose <- renderPlotly({
    
    EVdelayreason$Total[ EVdelayreason$Total == '1'] = 'Air system delay'
    EVdelayreason$Total[ EVdelayreason$Total == '2'] = 'Security delay'
    EVdelayreason$Total[ EVdelayreason$Total == '3'] = 'Airline delay'
    EVdelayreason$Total[ EVdelayreason$Total == '4'] = 'Late aircraft delay'
    EVdelayreason$Total[ EVdelayreason$Total == '5'] = 'Weather delay'
    EVdelayreason$Total[ EVdelayreason$Total == '6'] = 'Carrier delay'
    EVdelayreason$Total[ EVdelayreason$Total == '7'] = 'NAS delay'
    EVdelayreason$Total[ EVdelayreason$Total == '8'] = 'Baggage error'
    EVdelayreason$Total[ EVdelayreason$Total == '9'] = 'Airspace congestion'
    EVdelayreason$Total[ EVdelayreason$Total == '10'] = 'suddenly landing due to technical problem'
    
    EVdelayreason %>% filter(EVdelayreason$Total != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubsets = subset(EVdelayreason, Total != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    p <- ggplot(CancelledSubsets,aes(Month,fill=Total)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
    print(p)
    ggplotly(p)
  })
  #=========================HA airline  frequent delay reason(delay analytics)========
  output$HAairlinechoose <- renderPlotly({
    
    HAdelayreason$Total[ HAdelayreason$Total == '1'] = 'Air system delay'
    HAdelayreason$Total[ HAdelayreason$Total == '2'] = 'Security delay'
    HAdelayreason$Total[ HAdelayreason$Total == '3'] = 'Airline delay'
    HAdelayreason$Total[ HAdelayreason$Total == '4'] = 'Late aircraft delay'
    HAdelayreason$Total[ HAdelayreason$Total == '5'] = 'Weather delay'
    HAdelayreason$Total[ HAdelayreason$Total == '6'] = 'Carrier delay'
    HAdelayreason$Total[ HAdelayreason$Total == '7'] = 'NAS delay'
    HAdelayreason$Total[ HAdelayreason$Total == '8'] = 'Baggage error'
    HAdelayreason$Total[ HAdelayreason$Total == '9'] = 'Airspace congestion'
    HAdelayreason$Total[ HAdelayreason$Total == '10'] = 'suddenly landing due to technical problem'
    
    HAdelayreason %>% filter(HAdelayreason$Total != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubsets = subset(HAdelayreason, Total != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    p <- ggplot(CancelledSubsets,aes(Month,fill=Total)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
    print(p)
    ggplotly(p)
  })
  #=========================MQ airline  frequent delay reason(delay analytics)========
  output$MQairlinechoose <- renderPlotly({
    
    MQdelayreason$Total[ MQdelayreason$Total == '1'] = 'Air system delay'
    MQdelayreason$Total[ MQdelayreason$Total == '2'] = 'Security delay'
    MQdelayreason$Total[ MQdelayreason$Total == '3'] = 'Airline delay'
    MQdelayreason$Total[ MQdelayreason$Total == '4'] = 'Late aircraft delay'
    MQdelayreason$Total[ MQdelayreason$Total == '5'] = 'Weather delay'
    MQdelayreason$Total[ MQdelayreason$Total == '6'] = 'Carrier delay'
    MQdelayreason$Total[ MQdelayreason$Total == '7'] = 'NAS delay'
    MQdelayreason$Total[ MQdelayreason$Total == '8'] = 'Baggage error'
    MQdelayreason$Total[ MQdelayreason$Total == '9'] = 'Airspace congestion'
    MQdelayreason$Total[ MQdelayreason$Total == '10'] = 'suddenly landing due to technical problem'
    
    MQdelayreason %>% filter(MQdelayreason$Total != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubsets = subset(MQdelayreason, Total != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    p <- ggplot(CancelledSubsets,aes(Month,fill=Total)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
    print(p)
    ggplotly(p)
  })
  #=========================VX airline  frequent delay reason(delay analytics)========
  output$VXairlinechoose <- renderPlotly({
    
    VXdelayreason$Total[ VXdelayreason$Total == '1'] = 'Air system delay'
    VXdelayreason$Total[ VXdelayreason$Total == '2'] = 'Security delay'
    VXdelayreason$Total[ VXdelayreason$Total == '3'] = 'Airline delay'
    VXdelayreason$Total[ VXdelayreason$Total == '4'] = 'Late aircraft delay'
    VXdelayreason$Total[ VXdelayreason$Total == '5'] = 'Weather delay'
    VXdelayreason$Total[ VXdelayreason$Total == '6'] = 'Carrier delay'
    VXdelayreason$Total[ VXdelayreason$Total == '7'] = 'NAS delay'
    VXdelayreason$Total[ VXdelayreason$Total == '8'] = 'Baggage error'
    VXdelayreason$Total[ VXdelayreason$Total == '9'] = 'Airspace congestion'
    VXdelayreason$Total[ VXdelayreason$Total == '10'] = 'suddenly landing due to technical problem'
    
    VXdelayreason %>% filter(VXdelayreason$Total != 'N') 
    
    # Weather is the bigggest reason
    
    CancelledSubsets = subset(VXdelayreason, Total != 'N')
    #ggplot(diamonds, aes(color, fill=cut)) + geom_bar() + coord_flip()
    p <- ggplot(CancelledSubsets,aes(Month,fill=Total)) + geom_bar()
    ##3ggplot(CancelledSubset,aes(Delay_Reasons$Year,fill=Delay_Reasons$CANCELLATION_REASON)) + geom_bar()
    
    print(p)
    ggplotly(p)
  })
  #=====================================ontime vs,dayofweek==========
  
  output$ontimedayweek <- renderPlotly({
    Delay_Reasons$DayOfWeek <- Delay_Reasons$DayOfWeek
    Delay_Reasons$Month <- Delay_Reasons$Month
    
    # Define levels
    Delay_Reasons$AIRLINE <- factor(Delay_Reasons$AIRLINE)
    Delay_Reasons$Year <- factor(Delay_Reasons$Year)
    Delay_Reasons$Month <- factor(Delay_Reasons$Month)
    Delay_Reasons$ORIGIN_AIRPORT <- factor(Delay_Reasons$ORIGIN_AIRPORT)
    Delay_Reasons$DESTINATION_AIRPORT <- factor(Delay_Reasons$DESTINATION_AIRPORT)
    
    
    # Define numbers by actual days and months.
    Delay_Reasons$DayOfWeek[Delay_Reasons$DayOfWeek == 1] <- 'Monday'
    Delay_Reasons$DayOfWeek[Delay_Reasons$DayOfWeek == 2] <- 'Tuesday'
    Delay_Reasons$DayOfWeek[Delay_Reasons$DayOfWeek == 3] <- 'Wednesday'
    Delay_Reasons$DayOfWeek[Delay_Reasons$DayOfWeek == 4] <- 'Thursday'
    Delay_Reasons$DayOfWeek[Delay_Reasons$DayOfWeek == 5] <- 'Friday'
    Delay_Reasons$DayOfWeek[Delay_Reasons$DayOfWeek == 6] <- 'Saturday'
    Delay_Reasons$DayOfWeek[Delay_Reasons$DayOfWeek == 7] <- 'Sunday'
    
    
    
    p <- ggplot(Delay_Reasons, aes(DayOfWeek, fill = Delay_Reasons$On_Time)) + 
      geom_bar(width=0.8, position="dodge",color="black")+
      labs(x = "DayOfWeek", y = "Count",title = "Flight Delay Counts by Airline Carriers(By DayOfWeek)", fill = "Airlines")+
      theme(legend.text = element_text(colour="blue", size=10, 
                                       face="bold"))
    print(p)
    ggplotly(p)
    
    
    
  })
  
  #=================Time series===================
  output$Tm <- renderPlot({
    ap<- AirPassengers2
    
    ap
    
    AP= AirPassengers2$AirPassengers
    class(ap)
    plot(x=AirPassengers2$Time,y= AP,col=c("red"))
    
    #plot(Ap,xlab="Time", ylab = "Passenger numbers (1000's)",main="Air Passenger numbers from 1949 to 1961")
    
    #plot(AP) + labs(x ="Date", y = "Passenger numbers (1000's)", title="Air Passengers from 1949 to 1961") 
    
    #plot(acf(AP,plot=FALSE))+ labs(title="Correlogram of Air Passengers from 1949 to 1961") 
    
    
    
  })
  
})






# output$histogram <- renderPlot({
# hist(B$DEPARTURE_DELAY,breaks = input$bins)


#output$allairlinesnameswithiatacode1 <- renderTable({
#subset(allairlinesnameswithiatacode1,allairlinesnameswithiatacode1$AIRLINE==input$Airlinenames)
# })
# output$plot <- renderPlot({
#with(allairlinesnameswithiatacode1,boxplot(allairlinesnameswithiatacode1$AIRLINE==allairlinesnameswithiatacode1$IATA_CODE))
#})
# output$pdf <- renderPlot({

# })








