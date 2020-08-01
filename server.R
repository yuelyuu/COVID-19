#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$time <- renderText({
    print(paste("Today", Sys.Date()))
  })
    output$map <- renderLeaflet({
      Draw_world_map(data_Map, corona_data)
    })

    output$pro_map <- renderLeaflet({
      countrymap(input$province)
    })

    output$today_info <- renderText({
      paste("Total Confirmed <br/>",
            as.character(tags$span(style = "color:red", summ[[2]])),
            "<br/><br/>Total Deaths <br/>",
            as.character(tags$span(style = "color:#808080", summ[[4]])),
            "<br/><br/>Total Recovered <br/>",
            as.character(tags$span(style = "color:#90EE90", summ[[6]])), "<br/><br/>")
    })
    
    output$currentTime <- renderText({
      invalidateLater(1000)
      paste(Sys.time(),"(UTC)")
    })
    
    output$news_updates <- renderText({
      #paste(news[input$country2], sep = "<br/>")
      news[input$country2] %>% unlist() %>% reduce(paste0)
    })
    
    output$QandA <- renderText({
      QA
    })
    
    output$ABOUT <- renderText({
      About
    })
    
    output$link1 <- renderText({
      linkone
    })
    
    output$link2 <- renderText({
      linktwo
    })
    
    output$link3 <- renderText({
      linkthree
    })
    
    output$link4 <- renderText({
      linkfour
    })
    
    output$link5 <- renderText({
      linkfive
    })
    
    output$link6 <- renderText({
      linksix
    })
    
    output$link7 <- renderText({
      linkseven
    })
    
    output$link8 <- renderText({
      linkeight
    })
    
    output$link9 <- renderText({
      linknine
    })
    
    output$video <- renderText({
      change
    })
    
    
    output$tbl <- renderDT({
      datatable(corona_data %>% arrange(desc(TotalCases)),
                colnames = c("Country/Other", "Total Cases", "New Cases",
                             "Total Deaths", "New Deaths", "Total Recovered",
                             "Active Cases", "Serious/Critical"),
                class = 'order-column',
                style = "bootstrap", fillContainer = FALSE, filter = 'top',
                options = list(pageLength = 10, autoWidth = F)) %>%
        formatStyle(columns = c(1,2,3,4,5,6,7,8,9), width='250px')
    })

    output$US_map <- renderLeaflet({
      USmap("https://covidtracking.com/api/states")
    })

    output$state_line <- renderPlotly({
        state %>%
          filter(state == input$state) %>%
          plot_ly(x = ~date, y = ~positive, type = 'bar', name = "positive") %>%
          add_trace(y = ~negative, type = 'bar', name = "negative") %>%
          add_trace(y = ~pending, type = 'bar', name = "pending") %>%
          layout(barmode = 'stack',
                 xaxis = list(title = ""),
                 yaxis = list(title = ''),
                 plot_bgcolor = 'rgb(220, 216, 216)', paper_bgcolor = 'rgb(220, 216, 216)',
                 legend = list(orientation='h', xanchor='center', x= 0.5)
                 )
    })

    output$confirmed_line <- renderPlotly({
        date_ct <- confirm %>%
          filter(`Country/Region` %in% input$country) %>%
          select(`Country/Region`, "1/22/20":ncol(confirm)) %>%
          group_by(`Country/Region`) %>%
          summarise_each(funs(sum)) %>%
          pivot_longer(-`Country/Region`, names_to = "date", values_to = "num") %>%
          mutate(date = mdy(date)) %>%
          plot_ly(x = ~date, y = ~num) %>%
          add_trace(color = ~`Country/Region`,
                    type = 'scatter',mode = 'lines+markers')  %>%
          layout(
            height =340, width = 540, margin = list(0,0,0,0),
            xaxis = list(title = ""), #Cumulative Number of Confirmed Cases
            yaxis = list(title = ""), #Cumulative Confirmed Cases
            plot_bgcolor = 'rgb(220, 216, 216)', paper_bgcolor = 'rgb(220, 216, 216)')
    })

    output$confirmed_new <- renderPlotly({
        date_ct <- confirm %>%
          filter(`Country/Region` %in% input$country) %>%
          select(`Country/Region`, "1/22/20":ncol(confirm)) %>%
          group_by(`Country/Region`) %>%
          summarise_each(funs(sum)) %>%
          pivot_longer(-`Country/Region`, names_to = "date", values_to = "num") %>%
          mutate(date = mdy(date)) %>%
          group_by(`Country/Region`) %>%
          mutate(new = num - lag(num)) %>%
          plot_ly(x = ~date, y = ~new) %>%
          add_trace(color = ~`Country/Region`,
                    type = 'scatter',mode = 'lines+markers')  %>%
          layout(
            height =340, width = 540, margin = list(0,0,0,0),
            xaxis = list(title = ""),#Cumulative Number of Deaths
            yaxis = list(title = ""), #New Deaths
            plot_bgcolor = 'rgb(220, 216, 216)', paper_bgcolor = 'rgb(220, 216, 216)')
    })

    output$death_line <- renderPlotly({
        date_ct <- death %>%
          filter(`Country/Region` %in% input$country) %>%
          select(`Country/Region`, "1/22/20":ncol(death)) %>%
          group_by(`Country/Region`) %>%
          summarise_each(funs(sum)) %>%
          pivot_longer(-`Country/Region`, names_to = "date", values_to = "num") %>%
          mutate(date = mdy(date)) %>%
          plot_ly(x = ~date, y = ~num) %>%
          add_trace(color = ~`Country/Region`,
                    type = 'scatter',mode = 'lines+markers')  %>%
          layout(
            height =340, width = 540, margin = list(0,0,0,0),
            xaxis = list(title = ""), 
            yaxis = list(title = ""),
            plot_bgcolor = 'rgb(220, 216, 216)', paper_bgcolor = 'rgb(220, 216, 216)')
    })

    output$death_new <- renderPlotly({
        date_ct <- death %>%
          filter(`Country/Region` %in% input$country) %>%
          select(`Country/Region`, "1/22/20":ncol(death)) %>%
          group_by(`Country/Region`) %>%
          summarise_each(funs(sum)) %>%
          pivot_longer(-`Country/Region`, names_to = "date", values_to = "num") %>%
          mutate(date = mdy(date)) %>%
          group_by(`Country/Region`) %>%
          mutate(new = num - lag(num)) %>%
          plot_ly(x = ~date, y = ~new) %>%
          add_trace(color = ~`Country/Region`,
                    type = 'scatter',mode = 'lines+markers')  %>%
          layout(
            height =340, width = 540, margin = list(0,0,0,0),
            xaxis = list(title = ""),
            yaxis = list(title = ""),
            plot_bgcolor = 'rgb(220, 216, 216)', paper_bgcolor = 'rgb(220, 216, 216)')
    })

    output$recover_line <- renderPlotly({
        date_ct <- recover %>%
          filter(`Country/Region` %in% input$country) %>%
          select(`Country/Region`, "1/22/20":ncol(recover)) %>%
          group_by(`Country/Region`) %>%
          summarise_each(funs(sum)) %>%
          pivot_longer(-`Country/Region`, names_to = "date", values_to = "num") %>%
          mutate(date = mdy(date)) %>%
          plot_ly(x = ~date, y = ~num) %>%
          add_trace(color = ~`Country/Region`,
                    type = 'scatter',mode = 'lines+markers')  %>%
          layout(
            height =340, width = 540, margin = list(0,0,0,0),
            xaxis = list(title = ""),
            yaxis = list(title = ""),
            plot_bgcolor = 'rgb(220, 216, 216)', paper_bgcolor = 'rgb(220, 216, 216)')
    })

    output$recover_new <- renderPlotly({
        date_ct <- recover %>%
          filter(`Country/Region` %in% input$country) %>%
          select(`Country/Region`, "1/22/20":ncol(recover)) %>%
          group_by(`Country/Region`) %>%
          summarise_each(funs(sum)) %>%
          pivot_longer(-`Country/Region`, names_to = "date", values_to = "num") %>%
          mutate(date = mdy(date)) %>%
          group_by(`Country/Region`) %>%
          mutate(new = num - lag(num)) %>%
          plot_ly(x = ~date, y = ~new) %>%
          add_trace(color = ~`Country/Region`,
                    type = 'scatter',mode = 'lines+markers')  %>%
          layout(
            height =340, width = 540, margin = list(0,0,0,0),
            xaxis = list(title = ""),
            yaxis = list(title = ""),
            plot_bgcolor = 'rgb(220, 216, 216)', paper_bgcolor = 'rgb(220, 216, 216)')
    })
})
