#
# Data Visualization Final Project
# Fall 2021
# Team 2
#   Jonathan Gragg
#   William Johnson
#   Douglas Wiley
#

# plotting
#library(ggplot2)
library(plotly)

# shiny
library(shiny)
library(shinydashboard)

#utilities
library(dplyr)
library(readr)
library(janitor)

# get 311 calls dataset
calls <- read_csv(url("https://raw.githubusercontent.com/DataSciencend/--Data-Viz-2021-Fall/master/Week01/311_Phone_Call_Log_Mod.csv?token=APZQY2I5JIHHHB2YDTQTR4TBWFBRG"))
calls <- clean_names(calls)
calls <- calls %>% rename(id=fid) 
calls <- calls %>% select(id, department, called_about, duration_seconds, call_date)
calls$call_date <- as.Date(calls$call_date)
calls <- calls %>% filter(!is.na(duration_seconds))


ui <- dashboardPage(
  
  header <- dashboardHeader(title='South Bend 311 Calls Dashboard'),
  
  # this disable doesn't seem to be working?
  sidebar <- dashboardSidebar(disable=TRUE),
  
  body <- dashboardBody(
    fluidRow(
      column(width=6, 
             box(width=NULL, 
                 height=NULL,
                 solidHeader=FALSE, 
                 status='primary',
                 title=uiOutput('pietitle'),
                 plotlyOutput("piechart")
             ),
             box(width=NULL,
                 DT::dataTableOutput("table")
             )
      ),
      column(width=6,
             box(width=NULL, 
                 height=NULL,
                 solidHeader=FALSE, 
                 status='primary',
                 title=uiOutput('timetitle'),
                 plotlyOutput("timechart")
             ),
             box(width=NULL, title = 'Calls Summary Mean:',
                 status='primary', textOutput('sumstatsmean')
             ),
             box(width=NULL, title = 'Calls Summary Median:',
                 status='primary', textOutput('sumstatsmedian')
             ),
             box(width=NULL, title = 'Calls Summary Standard Deviation:',
                 status='primary', textOutput('sumstatssd')
             )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # these values keep track of the drilldown state
  drills <- reactiveValues(
    department=NULL,
    called_about=NULL,
    show_top=NULL)
  
  summarystats <- reactiveValues(
    callsmean = NULL,
    callsmedian = NULL,
    callssd = NULL)
  # filter the data based on active drill-downs, used by the grid
  calls_detail <- reactive({
    
    # all departments
    if (!length(drills$department)) {
      summarystats$callsmean <- round(mean(calls$duration_seconds), 2)
      summarystats$callsmedian <- round(median(calls$duration_seconds), 2)
      summarystats$callssd <- round(sd(calls$duration_seconds),2)
      return(calls %>%  mutate(value=department))
    }
    
    
    # just the called_about's for the specific department
    calls <- filter(calls, department %in% drills$department)
    if (!length(drills$called_about)) {
      summarystats$callsmean <- round(mean(calls$duration_seconds), 2)
      summarystats$callsmedian <- round(median(calls$duration_seconds), 2)
      summarystats$callssd <- round(sd(calls$duration_seconds),2)
      return(calls %>%  mutate(value=called_about))
    }
    
    
    summarystats$callsmean <- round(mean(calls$duration_seconds), 2)
    summarystats$callsmedian <- round(median(calls$duration_seconds), 2)
    summarystats$callssd <- round(sd(calls$duration_seconds),2)
    calls <- filter(calls, called_about %in% drills$called_about)
    return(calls %>%  mutate(value=called_about))
  })
  
  # add a count variable, used by the barchart
  calls_summary <- reactive({
    
    # count and arrange descending
    calls_detail() %>% 
      count(value) %>% 
      arrange(n) %>% 
      filter(n > 100)
  })
  
  # add a count variable, used by the timechart
  calls_time <- reactive({
    
    # count and arrange descending
    calls_detail() %>% 
      group_by(call_date, department) %>% 
      summarise(total = n())
  })
  
  
  
  # bar chart of calls by 'current level of category'
  output$piechart <- renderPlotly({
    
    # horizontal bar chart shows number of calls for 
    # department or called_about
    plot_ly(
      calls_summary(), 
      values=~n, 
      labels=~value,
      type='pie', 
      source='piechart',
      textinfo = 'label+percent',
      textposition = 'inside',
      insidetextfont = list(color = '#FFFFFF'),
      insidetextorientation='radial',
      hoverinfo = 'text',
      marker = list(
        colors = colors,
        line = list(color = '#FFFFFF', width = 1)),
      showlegend=TRUE) %>% 
      config(displayModeBar=FALSE) %>% 
      layout(mode = 'hide')
    
  })
  
  output$timechart <- renderPlotly({
    plot_ly(
      calls_detail(), 
      x = ~call_date,
      type='histogram',
      color = ~department,
      source='timechart',
      hoverinfo = 'text',
      marker = list(
        colors = colors,
        line = list(color = '#FFFFFF', width = 1)),
      showlegend=TRUE) %>% 
      config(displayModeBar=FALSE) %>%
      layout(yaxis = list(title = 'Total'),
             xaxis = list(title = 'Call Date'), 
             barmode = 'stack')
  })
  
  output$table <- DT::renderDataTable(DT::datatable({
    calls_detail() %>% 
      select(id, department, called_about) %>% 
      rename(Id=id) %>% 
      rename(Department=department) %>% 
      rename(Topic=called_about)
  }))
  
  # control the state of the drilldown by clicking the bar graph
  observeEvent(event_data("plotly_click", source = "piechart"), {
    
    # get the event data
    pie_event_data <- event_data("plotly_click", source = "piechart")
    
    # no need to go further
    if (!length(pie_event_data)) {
      return()
    }
    
    # get the slice of the pie was clicked
    p <- calls_summary()[pie_event_data$pointNumber+1,]
    
    # if a department hasn't been set yet
    if (!length(drills$department)) {
      drills$department <- p$value
      # if called_about hasn't been set yet
    } else if (!length(drills$called_about)) {
      drills$called_about <- p$value
    } 
  })
  
  output$pietitle <- renderUI({
    
    if (is.null(drills$department) && is.null(drills$called_about)) {
      return <- paste('ALL CALLS')
    } else if (!is.null(drills$department) && is.null(drills$called_about)) {
      return <- tags$span(
        actionLink('lnkHome', 'ALL CALLS'),
        HTML('|'),
        drills$department)
    } else if (!is.null(drills$department) && !is.null(drills$called_about)) {
      tags$span(
        actionLink('lnkHome', 'ALL CALLS'),
        HTML('|'),
        actionLink('lnkDepartments', drills$department),
        HTML('|'),
        drills$called_about)
    }
  })
  
  output$timetitle <- renderUI({
    
    if (is.null(drills$department) && is.null(drills$called_about)) {
      return <- paste('TIME SERIES')
    } else if (!is.null(drills$department) && is.null(drills$called_about)) {
      return <- tags$span(
        actionLink('lnkHome', 'TIME SERIES'),
        HTML('|'),
        drills$department)
    } else if (!is.null(drills$department) && !is.null(drills$called_about)) {
      tags$span(
        actionLink('lnkHome', 'TIME SERIES'),
        HTML('|'),
        actionLink('lnkDepartments', drills$department),
        HTML('|'),
        drills$called_about)
    }
  })
  
  output$sumstatsmean <- renderText({
    return <- summarystats$callsmean
  })
  output$sumstatsmedian <- renderText({
    return <- summarystats$callsmedian
  })
  output$sumstatssd <- renderText({
    return <- summarystats$callssd
  })
  
  # event handlers
  observeEvent(
    input$lnkHome, {
      drills$department <- NULL
      drills$called_about <- NULL
      callsmean <- round(mean(calls$duration_seconds), 2)
      callsmedian <- round(median(calls$duration_seconds), 2)
      callssd <- round(sd(calls$duration_seconds),2)
    })
  
  observeEvent(
    input$lnkDepartments, {
      drills$called_about <- NULL
      callsmean <- round(mean(calls$duration_seconds), 2)
      callsmedian <- round(median(calls$duration_seconds), 2)
      callssd <- round(sd(calls$duration_seconds),2)
    })
}

shinyApp(ui, server)