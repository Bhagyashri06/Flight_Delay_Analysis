

library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)

shinyUI(
  #================================================Dashboard_Designing========================= 
  dashboardPage(title="analytics dashboard",skin = "blue",
                
                dashboardHeader(title = "Airline Analytics"
                                
                ),
                
                #=============================================================Menu's_Of_Dashboard==========================================                
                dashboardSidebar(
                  br(),
                  br(),
                  br(),
                  
                  sidebarMenu(
                    
                    menuItem("Dashboard",icon = icon("home",lib = "glyphicon"),tabName = "Dashboard"),
                    menuItem("Airlinewise Analytics",icon = icon("plane",lib = "glyphicon"),tabName = "Airline_Analytics"),
                    menuItem("Routewise Analytics",icon = icon("road",lib = "glyphicon"),tabName = "Route_Analytics"),
                    menuItem("Airportwise Analytics", icon = icon("stats",lib ="glyphicon"),tabName = "Airportwise_Analytics"),
                    menuItem("Exploratory Data analysis ",icon = icon("ok",lib ="glyphicon"),tabName = "airlinechoose"),
                    menuItem("On and Delayed Time analysis", icon = icon("bullhorn",lib = "glyphicon"),tabName = "Departure_Delay"),
                    menuItem("Cancellation Analytics", icon = icon("globe",lib = "glyphicon"),tabName = "Cancellation_analysis"),
                    menuItem("Delayreason wise Analysis",icon = icon("flag",lib ="glyphicon"),tabName = "Delay_Analytics"),
                    menuItem("Time series analysis ",icon = icon("text-size",lib ="glyphicon"),tabName = "Timeseries"),
                    menuItem("Review Analytics", icon = icon("comment",lib ="glyphicon"),tabName = "Review_Analysis")
                    #menuItem("Recommended", icon = icon("hand-right",lib ="glyphicon"),tabName = "Recommended_Analytics"),
                    #menuItem("check onTime performance ",icon = icon("zoom-in",lib ="glyphicon"),tabName = "ontimeperformance")
                    
                    
                    
                  )
                ),
                #============================================Main_codind(KPI)===============================
                dashboardBody(
                  tabItems(
                    tabItem(tabName = "Dashboard",
                            h1("Information Dashboard"),
                            fluidRow(
                              infoBox(" Total Airlines",14,icon = icon("location-arrow"),color = "blue"),
                              infoBox("Total Delay Reasons",10,icon = icon("exclamation-circle"),color = "blue"),
                              infoBox("Total countries",1,icon = icon("globe"),color = "blue")
                            )
                            
                    ),
                    #=========================================This_Code_Is_For_TestingPDF(Not used)=========================                   
                    #tabItem(
                    #tabName = "Departure_Delay",
                    #fluidPage(
                    #titlePanel("Delay_Analytics"),
                    #sidebarLayout(
                    #sidebarPanel(
                    #selectInput("Airlinenames","select Airline_Name",choices=allairlinesnameswithiatacode1$AIRLINE,selectize = FALSE),
                    #selectInput("Airlinenames","Xlab",choices = allairlinesnameswithiatacode1$AIRLINE,selectize = FALSE ) ,
                    #selectInput("Airlinenames","Ylab",choices = allairlinesnameswithiatacode1$IATA_CODE,selectize = FALSE )
                    
                    # ),
                    #mainPanel(
                    #tabsetPanel(type="tab"
                    #tabPanel("Data",tableOutput("allairlinesnameswithiatacode1")),
                    #tabPanel("pdf",tags$iframe(style = "height:400px; width:100%;scrolling=yes", src="qlik.pdf"))
                    #tabPanel("plot",plotOutput("plot"))
                    #)
                    # )
                    # )
                    # )
                    
                    # ),
                    #================================================================================Airline_Analytics_only_simple_Data_will_Display_By_Airline_namewise=========================
                    tabItem(tabName = "Airline_Analytics",
                            h1("Analytics Dashboard"),
                            fluidPage(
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput("Airlinename","select Airline name",choices = Delayreasonrevised$AIRLINE )
                                ),
                                mainPanel(
                                  tabsetPanel(type="tab",
                                              tabPanel("Data",tableOutput("Airlines_Full_Details")))
                                )
                              )
                              
                              
                              
                              
                            )        
                    ),
                    #=========================================================================Routewise_Analytics_By_putting_orgairport_destairport_and_airlinename_(multiple selection)=================================
                    
                    tabItem(tabName = "Route_Analytics",
                            h1("Analytics Dashboard"),
                            fluidPage(
                              
                              
                              selectInput("AIRLINE", "AIRLINE", choices = c(Delayreasonrevised$AIRLINE),selected = ""),
                              
                              selectInput("ORIGIN_AIRPORT", "ORIGIN_AIRPORT", choices = c(Delayreasonrevised$ORIGIN_AIRPORT), selected = ""),
                              
                              
                              selectInput("DESTINATION_AIRPORT", "DESTINATION_AIRPORT", choices = c(Delayreasonrevised$DESTINATION_AIRPORT), selected = "")
                              #tableOutput("dataset")
                            ),
                            mainPanel(
                              tabsetPanel(type="tab",
                                          tabPanel("Data",tableOutput("Routewise_Cancellation_Reasons")))
                            )
                    ),
                    #======================================AirportWise_Analytics==================================
                    tabItem(tabName = "Airportwise_Analytics",
                            h1(" Airportwise Analytics Dashboard"),
                            fluidPage(
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput("STATE", "STATE", choices = c(Airport_Iatacode_Details$STATE),selected = "")
                                  
                                  
                                  
                                ),
                                mainPanel(
                                  tableOutput("Airportwise_analytics")
                                  
                                  
                                )),
                              
                              fluidRow(
                                box(title="Airport VS.City ",status = "primary",solidHeader = T,plotlyOutput("Airportwise"),width = "400px")
                                #box(title="country VS.States ",status = "primary",solidHeader = T,plotlyOutput("Countrywise"),width = "400px")
                              )
                            )),
                    
                    
                    #=======================================================Departure_Delay_Analytics_=================
                    tabItem(tabName = "Departure_Delay",
                            h1(" Departure  Delay Analytics Dashboard"),
                            
                            fluidRow(
                              
                              box(title="Airline Name VS. OnTime Airline(by year)",status="primary",solidHeader=T,plotlyOutput("PlotofOnTime"),width="100px"),
                              br(),
                              h4("Conclusion: From this graph Virgin America airline is continuous in ontime"),
                              br(),
                              box(title="Airline Name VS. Delayed Time(by month) ",status = "primary",solidHeader = T,plotlyOutput("plotmOfDelay"),width = "100px"),
                              br(),
                              h4("Conclusion: From this Graph virgin America airline is less delayed than other airlines"),
                              br(),
                              box(title="Airline Name VS. Delayed Time(by dayofweek) ",status = "primary",solidHeader = T,plotlyOutput("plotmOfDelaybydayofweek"),width = "100px"),
                              br(),
                              h4("Conclusion: From this graph saturday and sunday is having most of the delay.")
                              
                            )
                            
                            
                            
                            
                    ) ,
                    #===================================cancellation_delay========================
                    tabItem(tabName = "Cancellation_analysis",
                            h1(" Cancellation Reason Analytics Dashboard"),
                            
                            fluidRow(
                              
                              box(title="Cancelled or Not",status="primary",solidHeader=T,plotOutput("Plotofcancel"),width="100px")
                              
                              
                            ),
                            fluidRow(
                              box(title="Year VS.cancellation reasons ",status = "primary",solidHeader = T,plotOutput("plotmOfcancellationreason"),width = "100px")
                              
                            ),
                            fluidRow(
                              box(title="Airline Name VS.cancellation reasons ",status = "primary",solidHeader = T,plotOutput("plotmOfcancellationreasonwithairline"),width = "100px")
                              
                            )
                            
                            
                            
                            
                            
                    ) ,
                    
                    
                    
                    
                    #==============================================Review_analytics===============================
                    tabItem(tabName = "Review_Analysis",
                            h1(" Review Analytics Dashboard"),
                            tabsetPanel(type="tabs",tabPanel("wordcloud",plotOutput("wordcloud")),
                                        tabPanel("sentimental",plotOutput("sentimental")),
                                        tabPanel("All airlines reviews",
                                                 fluidRow(
                                                   box(title="Reviews of all airlines",status = "primary",solidHeader = T,plotlyOutput("sentimentalall"),width = "400px")
                                                   
                                                 ),
                                                 mainPanel(
                                                   
                                                   
                                                   
                                                 ))     
                                        
                                        
                            )
                            
                            
                            
                    ),
                    #===================================================Recommended-analytics===========
                    
                    tabItem(tabName = "Recommended_Analytics",
                            h1(" Recommended Analytics Dashboard"),
                            
                            
                            fluidRow(
                              
                              box(title="Airline VS.Reviews",status="primary",solidHeader=T,plotlyOutput("positivenegativegraph"),width="100px")
                              
                              
                            )
                            
                            
                            
                            
                    ) ,
                    
                    
                    
                    #====================================which airline to choose=============================
                    tabItem(tabName = "airlinechoose",
                            h1("which month and dayofweek choose to travel"),
                            
                            fluidRow(
                              box(title="which airline do most frequent delays in month",status = "primary",solidHeader = T,plotlyOutput("airlinechoose"),width = "400px"),
                              box(title="which airline do most frequent delays in weeks",status = "primary",solidHeader = T,plotlyOutput("airlinechooseweek"),width = "400px")
                            ),
                            mainPanel(
                              
                              
                              
                            ) ),
                    #================Delay analytics=======================
                    tabItem(tabName = "Delay_Analytics",
                            h1("which Delay reaons occurs most frequent for all airlines"),
                            tabsetPanel(type="tabs",tabPanel("UA and AA airline",
                                                             fluidRow(
                                                               box(title="United Airlines",status = "primary",solidHeader = T,plotlyOutput("UAairlinechoose"),width = "400px"),
                                                               box(title="American Airlines",status = "primary",solidHeader = T,plotlyOutput("AAairlinechoose"),width = "400px")
                                                             ),
                                                             mainPanel(
                                                               
                                                               
                                                               
                                                             )),
                                        tabPanel("US and Frontier airline",fluidRow(
                                          box(title="US Airways ",status = "primary",solidHeader = T,plotlyOutput("USairlinechoose"),width = "400px"),
                                          box(title="Frontier Airlines",status = "primary",solidHeader = T,plotlyOutput("F9airlinechoose"),width = "400px")
                                        ),
                                        mainPanel(
                                          
                                          
                                          
                                        )
                                        
                                        ),
                                        tabPanel("Jetblue and OO airline",
                                                 fluidRow(
                                                   box(title="Jetblue Airlines",status = "primary",solidHeader = T,plotlyOutput("B6airlinechoose"),width = "400px"),
                                                   box(title="Skywest Airline",status = "primary",solidHeader = T,plotlyOutput("OOairlinechoose"),width = "400px")
                                                 ),
                                                 mainPanel(
                                                   
                                                   
                                                   
                                                 )),
                                        tabPanel("AS and NK airline",fluidRow(
                                          box(title="Alaska Airlines  Airline",status = "primary",solidHeader = T,plotlyOutput("ASairlinechoose"),width = "400px"),
                                          box(title="Spirit Airline",status = "primary",solidHeader = T,plotlyOutput("NKairlinechoose"),width = "400px")
                                        ),
                                        mainPanel(
                                          
                                          
                                          
                                        )
                                        
                                        ),
                                        tabPanel("WN and DL airline",
                                                 fluidRow(
                                                   box(title="Southwest Airlines",status = "primary",solidHeader = T,plotlyOutput("WNairlinechoose"),width = "400px"),
                                                   box(title="Delta Airlines",status = "primary",solidHeader = T,plotlyOutput("DLairlinechoose"),width = "400px")
                                                 ),
                                                 mainPanel(
                                                   
                                                   
                                                   
                                                 )),
                                        tabPanel("EV and HA airline",fluidRow(
                                          box(title="Atlantic Southeast airlines ",status = "primary",solidHeader = T,plotlyOutput("EVairlinechoose"),width = "400px"),
                                          box(title="Hawaiian Airlines",status = "primary",solidHeader = T,plotlyOutput("HAairlinechoose"),width = "400px")
                                        ),
                                        mainPanel(
                                          
                                          
                                          
                                        )
                                        
                                        ),
                                        tabPanel("MQ and VX airline",fluidRow(
                                          box(title="American Eagle airlines ",status = "primary",solidHeader = T,plotlyOutput("MQairlinechoose"),width = "400px"),
                                          box(title="Virgin America airlines ",status = "primary",solidHeader = T,plotlyOutput("VXairlinechoose"),width = "400px")
                                        ),
                                        mainPanel(
                                          
                                        )
                                        
                                        ) )
                            
                            
                    ),
                    #==============================ontime performace check====================
                    tabItem(tabName = "ontimeperformance",
                            h1("OnTime Vs.DayOfWeek"),
                            
                            fluidRow(
                              box(title="OnTime Vs.DayOfWeek",status = "primary",solidHeader = T,plotlyOutput("ontimedayweek"),width = "400px"),
                              box(title="which airline do most frequent delays in weeks",status = "primary",solidHeader = T,plotlyOutput("arrivaltime"),width = "400px")
                            ),
                            mainPanel(
                              
                              
                              
                            ) ),
                    #===============================Time series==============
                    tabItem(tabName = "Timeseries",
                            h1("Time series analysis "),
                            
                            fluidRow(
                              
                              box(title="Time series by year",status="primary",solidHeader=T,plotOutput("Tm"),width="100px")
                              #box(title="Time series by month) ",status = "primary",solidHeader = T,plotOutput("Ty"),width = "100px")
                              #box(title="Airline Name VS. Delayed Time(by dayofweek) ",status = "primary",solidHeader = T,plotlyOutput("plotmOfDelaybydayofweek"),width = "100px")
                              
                              
                            )
                            
                            
                            
                            
                    ) 
                    
                    
                  ))
                
  ))



