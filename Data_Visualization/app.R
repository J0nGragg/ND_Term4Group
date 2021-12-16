#
# Data Visualization Final Project
# Fall 2021
# Team 2
#   Jonathan Gragg
#   William Johnson
#   Douglas Wiley
#

# shiny
library(shiny)
library(shinydashboard) # dashboardPage

# plotting
library(plotly)         # plot_ly

#utilities
library(dplyr)          # everything else
library(lubridate)      # seconds_to_period
library(readr)          # read_csv
library(janitor)        # clean_names

# get 311 calls dataset
calls <- read_csv('311_Phone_Call_Log_Mod.csv', show_col_types = FALSE)
calls <- clean_names(calls)
calls <- calls %>% rename(id=fid) 
calls <- calls %>% select(id, department, called_about, duration_seconds, duration_minutes, call_date)
calls$call_date <- as.Date(calls$call_date)
calls <- calls %>% filter(!is.na(duration_seconds))

# get weather data
weather <- read_csv('South_Bend_Weather.csv', show_col_types = FALSE)
weather <- weather %>% select(-STATION,-NAME)
weather <- clean_names(weather)
weather <- weather %>% rename(call_date = date)

# join calls and weather info
calls <- calls %>% inner_join(weather, by='call_date')

ui <- dashboardPage(
    
    header <- dashboardHeader(
        title='City of South Bend 311 Calls Dashboard',
        titleWidth='400px'),
    
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
                       status='primary',
                       DT::dataTableOutput("detailtable")
                   )
            ),
            column(
                width=6,
                box(width=NULL, 
                   height=NULL,
                   solidHeader=FALSE, 
                   status='primary',
                   title=uiOutput('timetitle'),
                   plotlyOutput("timechart")
                ),
                box(width=NULL, 
                   height=NULL,
                   status='primary', 
                   htmlOutput('callstats')
                ),
                box(width=NULL, 
                   height=NULL,
                   status='warning',
                   title=uiOutput('toptentitle'),
                   tableOutput('toptentable')
                )
            )
        )
    )
)

server <- function(input, output, session) {
    
    # these values keep track of the drill down state
    drills <- reactiveValues(
        department=NULL,
        called_about=NULL,
        show_top=NULL)
    
    # convert seconds to mm:ss format
    format_seconds <- function(s) {
        p <- seconds_to_period(s)
        return <- sprintf('%02d:%02d', minute(p), round(second(p)))
    }
    
    # filter the data based on active drill-downs, used by the grid
    call_detail <- reactive({
        
        # all departments
        if (!length(drills$department)) {
            return(calls %>%  mutate(value=department))
        }
        
        # just the called_about's for the specific department
        calls <- filter(calls, department %in% drills$department)
        if (!length(drills$called_about)) {
            return(calls %>%  mutate(value=called_about))
        }

        calls <- filter(calls, called_about %in% drills$called_about)
        return(calls %>%  mutate(value=called_about))
    })
    
    call_summary <- reactive({
        
        # count and arrange descending
        call_detail() %>% 
            count(value) %>% 
            arrange(n) %>% 
            filter(n > 100)
    })
    
    calls_temp <- reactive({
        
        # count and arrange descending
        call_detail() %>% 
            group_by(call_date) %>% summarise(temp = mean(tavg))
    })
    
    # generate the department/called_about pie chart
    output$piechart <- renderPlotly({
        
        plot_ly(
            call_summary(), 
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
            config(displayModeBar=FALSE) #%>% 
            #onRender("
            #    function(el, x) {
            #        Plotly.d3.select('.cursor-crosshair').style('cursor', 'pointer')
            #    }
            #")
    })
    
    # generate the temperature with call volume histogram
    output$timechart <- renderPlotly({
        
        plot_ly(
            call_detail(), 
            x = ~call_date,
            type='histogram',
            source='timechart',
            hoverinfo = 'text',
            name = 'Calls') %>% 
            add_trace(
                data = calls_temp(),
                x = ~call_date, 
                y = ~temp, 
                name = "Temperature", 
                yaxis = "y2", 
                mode = "lines+markers", 
                type = "scatter") %>%
            layout(
                yaxis = list(title = 'Volume'),
                yaxis2 = list(
                    overlaying = 'y',
                    side = 'right',
                    title = 'Avg Temperature (F)'),
                    xaxis = list(title = 'Month'), 
                    barmode = 'stack') %>% 
            config(displayModeBar=FALSE)
    })
    
    # creates the call detail data table
    output$detailtable <- DT::renderDataTable(
        
        DT::datatable({
            call_detail() %>% 
                mutate(Duration = format_seconds(duration_seconds)) %>%
                mutate(Id = paste0("<a href='' target='_blank'>", id, "</a>")) %>% 
                select(Id, call_date, department, called_about, Duration) %>% 
                rename(Date=call_date) %>% 
                rename(Department=department) %>% 
                rename(Topic=called_about)
        }, 
        escape = FALSE,
        option=list(columnDefs=list(list(targets=5, class="dt-right")))
        )
    )
    
    # renders the 'breadcrumb' style active pie title
    output$pietitle <- renderUI({
        
        if (is.null(drills$department) && is.null(drills$called_about)) {
            return <- paste('ALL CALLS')
        } else if (!is.null(drills$department) && is.null(drills$called_about)) {
            return <- tags$span(
                actionLink('lnkHome', 'ALL CALLS'),
                HTML('\U203A'),
                drills$department)
        } else if (!is.null(drills$department) && !is.null(drills$called_about)) {
            tags$span(
                actionLink('lnkHome', 'ALL CALLS'),
                HTML('\U203A'),
                actionLink('lnkDepartments', drills$department),
                HTML('\U203A'),
                drills$called_about)
        }
    })
    
    # creates the call volume histogram title
    output$timetitle <- renderUI({
        
        if (is.null(drills$department) && is.null(drills$called_about)) {
            return <- paste('Monthly Call Volume for All 311')
        } else if (!is.null(drills$department) && is.null(drills$called_about)) {
            return <- paste('Monthly Call Volume for ', drills$department)
        } else if (!is.null(drills$department) && !is.null(drills$called_about)) {
            paste('Monthly Call Volume for ', drills$called_about)
        }
    })
    
    # creates the call statistics widget
    output$callstats <- renderUI({
        
        # what has been clicked on
        pie_context <- 'All 311'
        
        # format text
        if (is.null(drills$department) && is.null(drills$called_about)) {
            pie_context <- 'All 311'
        } else if (!is.null(drills$department) && is.null(drills$called_about)) {
            pie_context <- drills$department
        } else if (!is.null(drills$department) && !is.null(drills$called_about)) {
            pie_context <- drills$called_about
        }
        
        return <- tags$div(
            tags$div(
                HTML(paste('Average Handle Time for ', pie_context)), 
                style='font-size: 18px; font-weight:500;'),
            tags$div(
                tags$span(
                    format_seconds(mean(call_detail()$duration_seconds)), 
                    style='font-size: 72px;'),
                tags$span(HTML('mm:ss')),
                style='text-align:center;'
            ),
            tags$div(
                HTML(paste('AHT for all 311 is', format_seconds(mean(calls$duration_seconds)))),
                style=paste(
                    'font-size: 12px; text-align: left; color: #ff8000; font-weight: bold; visibility:', 
                    ifelse(pie_context=='All 311', 'hidden;', 'visible;'))
            )
        )
    })
    
    # creates the top ten table
    output$toptentable <- renderTable(
        
        calls %>% 
            mutate(`Department/Topic` = paste(department, called_about, sep=': ')) %>% 
            group_by(`Department/Topic`) %>% 
            summarise(
                Calls = n(), 
                Hours = as.integer(sum(duration_seconds) / 360)) %>% 
            arrange(desc(Calls)) %>% 
            mutate(Calls = format(Calls, big.mark = ",", scientific = FALSE) ) %>% 
            mutate(Hours = format(Hours, big.mark = ",", scientific = FALSE) ) %>% 
            slice(1:10)
    )
    
    # creates the top ten title
    output$toptentitle <- renderUI({
        return <- '311 Top 10 by Call Volume'
    })
    
    # event handlers
    observeEvent(event_data("plotly_click", source = "piechart"), {
        
        # get the event data
        pie_event_data <- event_data("plotly_click", source = "piechart")
        
        # no need to go further
        if (!length(pie_event_data)) {
            return()
        }
        
        # get the slice of the pie was clicked
        p <- call_summary()[pie_event_data$pointNumber+1,]
        
        # if a department hasn't been set yet
        if (!length(drills$department)) {
            drills$department <- p$value
        # if called_about hasn't been set yet
        } else if (!length(drills$called_about)) {
            drills$called_about <- p$value
        } 
    })
    
    observeEvent(
        input$lnkHome, {
            drills$department <- NULL
            drills$called_about <- NULL
        })
    
    observeEvent(
        input$lnkDepartments, {
            drills$called_about <- NULL
        })
}

shinyApp(ui, server)