#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    navbarPage("Coronavirus(COVID-19) Real-time Dynamics", theme = shinytheme("slate"),
             tabPanel("World Trend", fluid = TRUE, icon = icon("globe"),
                      titlePanel("Culmulative Confirmed Cases and Deaths"),
                      br(),
                      add_busy_spinner(spin = "circle", position = "bottom-left", 
                                       margins = c(250, 450), color = "#b8b3b3"),
                      fluidRow(
                          column(8,leafletOutput("map"),
                                 br(),
                                 br()
                                 ),
                          column(4, h3(htmlOutput("currentTime")),
                                 h2(htmlOutput("today_info")
                                    )
                                 )
                          ),
                      mainPanel(
                        htmlOutput("link6")
                        )
                      ),
             tabPanel("Detailed Situation", fluid = TRUE, icon = icon("table"),
                      titlePanel("Confirmed Cases and Deaths by Country/Region"),
                      mainPanel(
                        br(),
                        DTOutput("tbl"),
                        br(),
                        br(),
                        htmlOutput("link1")
                        )
             ),
             tabPanel("Daily Report", icon = icon("chart-line"),
                      titlePanel("Cumulative and New Confirmed Cases"),
                      htmlOutput("video"),
                      br(),
                      fluidRow(
                        column(4, selectInput(inputId = "country",
                                              label = "Select Countries:",
                                              choices = sort(unique(confirm$`Country/Region`)),
                                              selected = "US",
                                              multiple = TRUE))),
                      mainPanel(width = 12,
                                br(),
                                tabsetPanel(tabPanel("Confirmed", icon = icon("meh"),
                                                     fluidRow(column(6, 
                                                                     br(),
                                                                     h4("Cumulative Confirmed Cases"),
                                                                     plotlyOutput("confirmed_line")),
                                                              column(6, 
                                                                     br(),
                                                                     h4("Daily New Confirmed Cases"),
                                                                     plotlyOutput("confirmed_new")))),
                                            tabPanel("Death", icon = icon("frown"),
                                                     fluidRow(column(6, 
                                                                     br(),
                                                                     h4("Cumulative Deaths"),
                                                                     plotlyOutput("death_line")),
                                                              column(6,  
                                                                     br(),
                                                                     h4("Daily New Deaths"),
                                                                     plotlyOutput("death_new")))),
                                            tabPanel("Recovered", icon = icon("angellist"),
                                                     fluidRow(column(6, 
                                                                     br(),
                                                                     h4("Cumulative Recovered Cases"),
                                                                     plotlyOutput("recover_line")),
                                                              column(6, 
                                                                     br(),
                                                                     h4("Daily New Recovered Cases"),
                                                                     plotlyOutput("recover_new"))))
                                            ),
                                htmlOutput("link2")
                                )
                      ),
             
             tabPanel("U.S. Testing", icon = icon("flask"),#bolt flask
                      titlePanel("Daily Testing by State around U.S."),
                      mainPanel(offset = 4, width = 8,
                                br(),
                                tabsetPanel(tabPanel("Current U.S. Testing", icon = icon("ambulance"),
                                                     br(),
                                                     h4("Today's Testing Situation around U.S."),
                                                     h6("* Maximum Testing result is shown in the legend"),
                                                     br(),
                                                     leafletOutput("US_map")), 
                                            tabPanel("Daily State Testing", icon = icon("user-md"),
                                                     selectInput(inputId = "state",
                                                                 label = "Select a state:",
                                                                 choices = unique(state_map$state),
                                                                 selected = "CA", width = "20%"),
                                                     h4("Daily Testing Tracking by State (4PM Eastern)"),
                                                     plotlyOutput("state_line")
                                            )
                                ),
                                br(),
                                br(),
                                htmlOutput("link5")
                      )
             ),
             
             tabPanel("Specific Country", icon = icon("location-arrow"),
                      titlePanel("Confirmed Cases and Deaths by Province/State"),
                      h6("* Data may not be available in some States/Provinces"),
                      h6("* Maximum Confirmed Cases/Deaths are shown in the legend"),
                      br(),
                      fluidRow(column(8, 
                                      leafletOutput("pro_map"),                      
                                      br(),
                                      br(),
                                      htmlOutput("link7")
                                      ),
                               column(4,
                                      radioButtons(inputId = "province",
                                                   label = "Select a Country:",
                                                   choices = c("China","Canada","Australia"),
                                                   selected = "China")
                                      )
                               )
                      ),
             
             tabPanel("Breaking News", icon = icon("bolt"),
                      titlePanel("News & Live Updates"),
                      mainPanel(
                        h6("* Published time is local time"),
                        br(),
                        selectInput(inputId = "country2",
                                            label = "Select a Country to view news:",
                                            choices = c("United State" = "us", "Canada" = "ca", 
                                                        "Australia" = "au", "United Kingdom" = "gb"),
                                            selected = "us",
                                            multiple = FALSE),
                                htmlOutput("news_updates"),
                        br(),
                        br(),
                        htmlOutput("link3"),
                        width = 9)
                      ),
             
             navbarMenu("More Information", icon = icon("bullhorn"),
                        tabPanel("About", icon = icon("info-circle"),#bolt flask
                                 titlePanel("About this application"),
                                 mainPanel(
                                   width = 9,
                                   htmlOutput("ABOUT"),
                                   br(),
                                   br(),
                                   htmlOutput("link9")
                                 )
                        ),
                        "----",
                        tabPanel("General Q&A", icon = icon("question-circle"),
                                 titlePanel("Frequently Asked Questions and Answers"),
                                 fluidRow(
                                   column(2),
                                   column(6,
                                          img(src="coronavirus.jpg", height = 290, width = 540),
                                          h6("The 2019 Novel Coronavirus (2019-nCoV), 
                                              portrayed in an illustration created at the Centers for 
                                              Disease Control and Prevention. ALISSA ECKERT, MS; 
                                              DAN HIGGINS, MAM")
                                   )
                                 ),
                                 mainPanel(width = 9,
                                           br(),
                                           htmlOutput("QandA"),
                                           br(),
                                           br(),
                                           htmlOutput("link4"),
                                           htmlOutput("link8")
                                           )
                                 )
                        )
    )
  )
)



