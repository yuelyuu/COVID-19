
library(leaflet)
library(tidyverse)
library(rvest)
library(DT)
library(shinythemes)
library(plotly)
library(jsonlite)
library(httr)
library(lubridate)
library(htmltools)
library(htmlwidgets)
library(geojsonio)
library(furrr)
library(shiny)
library(shinybusy)
#options(encoding = "UTF-8")
#setwd('C:/Users/98455/Desktop/winter_SY/STA141B/Final Project')
source("helpers.R") 
world <- geojson_read("custom.geo.json", what = "sp")
list <- read_coro_data("https://www.worldometers.info/coronavirus/#countries", world)
data_Map <- list[[1]]
corona_data <- list[[2]]
summ <- list[[3]]

list2 <- read_daily_data()
confirm <- list2[[1]]
death <- list2[[2]]
recover <- list2[[3]]
state <- list2[[4]]
state_map <- list2[[5]]


list3 <- c("us", "ca", "au", "gb")
news <- list()
for(i in 1:length(list3)){
  temp <- read_news(list3[i])
  news[[list3[i]]] <- apply(temp, 1, row_new_html)
}

QA <- as.character(
  tags$body(
    div(
      tags$h4(
        tags$strong("Q: Why is the disease being called coronavirus disease 2019, COVID-19?")),
      tags$h5("A: On February 11, 2020 the World Health Organization announced an official name for the disease 
            that is causing the 2019 novel coronavirus outbreak, first identified in Wuhan China. The new name 
            of this disease is coronavirus disease 2019, abbreviated as COVID-19. In COVID-19, 'CO' stands for 
            'corona,' 'VI' for 'virus,' and 'D' for disease. Formerly, this disease was referred to 
            as \"2019 novel coronavirus\" or \"2019-nCoV\".")
    ),
    tags$br(),
    div(
      tags$h4(
        tags$strong("Q: How It Spreads?")),
      tags$h5("A: The virus that causes COVID-19 seems to be spreading easily and sustainably in the community (\"community spread\") in ",
              tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/prepare/transmission.html?CDC_AA_refVal=https%3A%2F%2Fwww.cdc.gov%2Fcoronavirus%2F2019-ncov%2Fabout%2Ftransmission.html",
                     tags$i(tags$strong("some affected geographic areas."))), 
              " Community spread means people have been infected with the virus in an area, including some who are not sure how or where they became infected.")
    ),
    tags$br(),
    div(
      tags$h4(
        tags$strong("Q: How can I protect myself?")),
      tags$h5("A: Visit the ",
              tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/prepare/prevention.html?CDC_AA_refVal=https%3A%2F%2Fwww.cdc.gov%2Fcoronavirus%2F2019-ncov%2Fabout%2Fprevention.html",
                     tags$i(tags$strong("COVID-19 Prevention and Treatment"))),
              " page to learn about how to protect yourself from respiratory illnesses, like COVID-19.")
    ),
    tags$br(),
    div(
      tags$h4(
        tags$strong("Q: What are the symptoms and complications that COVID-19 can cause?")),
      tags$h5("A: Current symptoms reported for patients with COVID-19 have included mild to severe respiratory illness with fever1, cough, and difficulty breathing. ",
              tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/symptoms.html?CDC_AA_refVal=https%3A%2F%2Fwww.cdc.gov%2Fcoronavirus%2F2019-ncov%2Fabout%2Fsymptoms.html",
                     tags$i(tags$strong("Read about COVID-19 Symptoms."))))
    )  
  )
         
)

linkone <- as.character(
  tags$h5(tags$a(href = "https://www.worldometers.info/coronavirus/#countries",
                 "Data Source: https://www.worldometers.info/coronavirus/#countries")
          )
  )

linktwo <- as.character(
  tags$h5(tags$a(href = "https://github.com/CSSEGISandData/COVID-19",
                 "Data Source: https://github.com/CSSEGISandData/COVID-19")
          )
  )


linkthree <- as.character(
  tags$h5(tags$a(href = "https://newsapi.org/s/us-news-api",
                 "Source: https://newsapi.org/s/us-news-api")
          )
  )

linkfour <- as.character(
  tags$h5(tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/faq.html#basics",
                 "CDC Source: https://www.cdc.gov/coronavirus/2019-ncov/faq.html#basics")
          )
  )


linkfive <- as.character(
  tags$h5(tags$a(href = "https://covidtracking.com/",
                 "Data Source: https://covidtracking.com/")
          )
  )

linksix <- as.character(
  tags$h5(tags$a(href = "https://www.worldometers.info/coronavirus/#countries",
                 "Data Source: https://www.worldometers.info/coronavirus/#countries")
  )
)

linkseven <- as.character(
  tags$h5(tags$a(href = "https://github.com/CSSEGISandData/COVID-19",
                 "Data Source: https://github.com/CSSEGISandData/COVID-19")
  )
)

linkeight <- as.character(
  tags$h5(tags$a(href = "https://news.wisc.edu/uw-madison-researchers-lead-efforts-to-understand-thwart-new-coronavirus/",
                 "Image Source: https://news.wisc.edu/uw-madison-researchers-lead-efforts-to-understand-thwart-new-coronavirus/")
  )
)

linknine <- as.character(
  tags$h5(tags$a(href = "https://en.wikipedia.org/wiki/Coronavirus_disease_2019",
                 "Wikipedia: https://en.wikipedia.org/wiki/Coronavirus_disease_2019")
  )
)

About <- as.character(
  tags$body(
    tags$br(),
    div(
      tags$h4(
        tags$strong("Motivation")),
      tags$h5("Coronavirus disease 2019 (COVID-19) is an infectious disease caused by 
              severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2). The virus 
              is thought to be natural and have an animal origin, through spillover 
              infection. It was first transmitted to humans in Wuhan, China, in November 
              or December 2019, and the primary source of infection became human-to-human 
              transmission by early January 2020. The earliest known infection occurred 
              on 17 November 2019 and has since spread globally, resulting in the 2019-20 
              coronavirus pandemic."),
      tags$h5("The aim of this App is to provide worldwide real-time information about new coronavirus outbreak, including ", 
              tags$i(tags$strong("World Trend")), "showing virus spreading; ", 
              tags$i(tags$strong("Detailed Situation")), "with deaths, recovered and active cases; ",
              tags$i(tags$strong("Daily Report")), " that allowed country comparisons; ",
              tags$i(tags$strong("Typical Country Data")), "that people may concern about; ",
              tags$i(tags$strong("Breaking News")), "reported about \"coronavirus\" in four countries; ", 
              tags$i(tags$strong("More Information")), "where you could find more about the virus and U.S. testing.")
    ),
    tags$br(),
    div(
      tags$h4(
        tags$strong("Code")),
      tags$h5("Code and input data used to generate this Shiny App are available on ",
              tags$a(href = "https://github.com/STA141B/final-project",
                     tags$i(tags$strong("Github."))))
    ),
    tags$br(),
    div(
      tags$h4(
        tags$strong("Feedback")),
      tags$h5("If you want to report any issue, please contact us: covidapphelp@gmail.com.")
    ) 
  )
  
)

change <- as.character(
  tags$h5("Click ",tags$a(href = "https://flo.uri.sh/story/250907/embed",
                 tags$i(tags$strong("here")))," to see the daily change by country"
  )
)




# iframe <- 
#   div(
#     iframe(width = "1000", height = "500",
#            url_link = "https://flo.uri.sh/story/250907/embed")
#   )
 # <div class="flourish-embed" data-src="story/250907" data-url="https://flo.uri.sh/story/250907/embed"><script src="https://public.flourish.studio/resources/embed.js"></script></div>
 # <iframe src='https://flo.uri.sh/story/250907/embed' frameborder='0' scrolling='no' style='width:100%;height:600px;'></iframe><div style='width:100%!;margin-top:4px!important;text-align:right!important;'><a class='flourish-credit' href='https://public.flourish.studio/story/250907/?utm_source=embed&utm_campaign=story/250907' target='_top' style='text-decoration:none!important'><img alt='Made with Flourish' src='https://public.flourish.studio/resources/made_with_flourish.svg' style='width:105px!important;height:16px!important;border:none!important;margin:0!important;'> </a></div>