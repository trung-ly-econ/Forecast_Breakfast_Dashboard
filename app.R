if (!require("shiny")) install.packages("shiny")
if (!require("fredr")) install.packages("fredr")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("plotly")) install.packages("plotly")  
if (!require("waiter")) install.packages("waiter")  
if (!require("shinycssloaders")) install.packages("shinycssloaders")  
if (!require("DT")) install.packages("DT")  
if (!require("shinydashboard")) install.packages("shinydashboard")  

library(shiny)
library(fredr)
library(tidyverse)
library(plotly)
library(waiter)
library(shinycssloaders)
library(DT)
library(shinydashboard)


###############
fredr_set_key("43ab67b0f6c4a7e98a9ff8c70efe1702")

# FRED series IDs 
data_id_urate <- tibble(county = c("Carroll", "Haralson", "Douglas",
                                   "Paulding", "Heard", "Coweta"),
                        series = c("GACARR0URN", "GAHARA3URN", "GADOUG0URN",
                                   "GAPAUL3URN", "GAHEAR9URN", "GACOWE7URN")
)

data_id_gdp <- tibble(county = c("Carroll", "Haralson", "Douglas",
                                 "Paulding", "Heard", "Coweta"),
                      series = c("REALGDPALL13045", "REALGDPALL13143", "REALGDPALL13097",
                                 "REALGDPALL13223", "REALGDPALL13149", "REALGDPALL13077")
)

data_id <- rbind(data_id_gdp, data_id_urate)

county_list <- c("Carroll", "Haralson", "Douglas","Paulding", "Heard", "Coweta")


############################## UI ##############################
# Header #
ui_header <- dashboardHeader(title = HTML("West Georgia Economic Snapshot"),
                             disable = FALSE,
                             titleWidth  = 550
)

ui_header$children[[2]]$children[[2]] <- ui_header$children[[2]]$children[[1]]
ui_header$children[[2]]$children[[1]] <- tags$a(href='http://www.westga.edu',
                                                tags$img(src='logo_ccexpress.png'),
                                                target = '_blank',
                                                # height='67',width='228.6', 
                                                align = 'right'
)
###########################################
# Sidebar #
ui_sidebar <- dashboardSidebar(width = 300,
                               # allow dateRangeInput to fully show
                               tags$div(tags$style(HTML( ".dropdown-menu{z-index:10000 !important;}"))),
                               
                               sidebarMenu(id = "sidebar",
                                           menuItem("Individual County", tabName = "IndividualCounty"),
                                           menuItem("Compare Across Counties", tabName = "CompareAcrossCounties"),
                                           conditionalPanel(
                                               'input.sidebar == "IndividualCounty"',
                                               # Inputs:
                                               # select county
                                               selectInput(inputId = "selectCounty",
                                                           label = "Please select the county of interest",
                                                           choices = county_list,
                                                           # multiple = TRUE
                                               ), 
                                               
                                               # Select date range 
                                               dateRangeInput("date", 
                                                              strong("Please select the time period"),
                                                              start = "2020-01-01", end = "2020-12-31",
                                                              min = "1990-01-01", format = "mm-yyyy"),
                                               
                                               # Button to start app
                                               actionButton("showme", "Show me the economic snapshot!")
                                           )
                                           
                               ),
                               textOutput("res"),
                               br(),
                               p("Note: This dashboard is brought to you by the Department of Economics and the Center for Business and Economic Research (CBER) at the University of West Gerogia (UWG). For suggestions and comments, please send an email to cber@westga.edu. [probably add some legal disclaimer]"),
                               htmlOutput("sponsor_logo", align = "center") 
)

###########################################
# Main body #

ui_body <- dashboardBody(
    tabItems(
        tabItem(tabName = "IndividualCounty", 
                withSpinner(tagList(
                    # Outputs:
                    fluidRow(
                        box(
                            column(6,
                                   h4(paste0("Unemployment rate for the selected time period")),
                                   p("Data are available at the monthly frequency") ,
                                   dataTableOutput("data1")
                            )
                        ),
                        box(
                            column(6,
                                   h4(paste0("Real Gross Domestic Product for the selected time period")),
                                   p("Data are available at the annual frequency") ,
                                   dataTableOutput("data2")
                            )
                        )
                    ),
                    plotlyOutput("plot")
                )
                )
        ),
        tabItem(tabName = "CompareAcrossCounties",
                "Content to be added"
        )
    )
)


# UI
ui <- dashboardPage(ui_header, ui_sidebar, ui_body,
                    skin = "green"
)

############################## Server ############################## 

server <- function(input, output, session) {
    
    # randomly show sponsors' logos
    www_imgs <- paste0("sponsor",1:5,".JPG")
    
    random_image <- www_imgs[round(runif(1, min = 1, max = length(www_imgs)), digits = 0)]
    
    output$sponsor_logo = renderUI({
        tags$img(src=random_image, width="50%", height="50%")
    })
    
    # tell users which tab they've selected
    output$res <- renderText({
        paste("You've selected:", input$sidebar)
    })

    ############################    
    ### Single county output ###
    # adjust start date to first of month
    start_date <- eventReactive(input$showme, {
        paste0(format(input$date[1], "%Y"),"-",format(input$date[1], "%m"),"-01")
    })
    
    # adjust end date to first of month
    end_date <- eventReactive(input$showme, {
        paste0(format(input$date[2], "%Y"),"-",format(input$date[2], "%m"),"-01")
    })
    
    # obtain data: use selected county to pull series IDs, then use fredr to
    # get the data from FRED
    county_data <- eventReactive(input$showme, {
        id <- data_id$series[which(data_id$county %in% input$selectCounty)]
        map_dfr(id, fredr) %>% 
            mutate(Series = case_when(str_detect(series_id, "GDP") ~ "Real Gross Domestic Product",
                                      str_detect(series_id, "URN") ~ "Unemployment Rate")
            ) %>% 
            select(!c(realtime_start, realtime_end, series_id)) %>% 
            rename(Date = date, Value = value) %>% 
            mutate(County = input$selectCounty)
    })
    
    
    # print data using the datatable function with options for download
    output$data1 <- renderDataTable(
        datatable(
            county_data() %>% filter(str_detect(Series, "Unemployment")) %>% 
                filter(Date >= as.Date(start_date())) %>% 
                filter(Date <= as.Date(end_date())), 
            extensions = 'Buttons',
            options = list(dom = 'Brtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           pageLength = 12)
        )
    )
    
    output$data2 <- renderDataTable(
        datatable(
            county_data() %>% filter(str_detect(Series, "Gross Domestic Product")) %>% 
                filter(Date >= as.Date(start_date())) %>% 
                filter(Date <= as.Date(end_date())), 
            caption = "GDP data",
            extensions = 'Buttons',
            options = list(dom = 'Brtip',
                           buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                           pageLength = 12)
        )
    )
    
    # plot urate data
    output$plot <- renderPlotly({
        ggplotly(
            county_data() %>%
                filter(str_detect(Series, "Unemployment")) %>%
                filter(Date >= as.Date(start_date())) %>%
                filter(Date <= as.Date(end_date())) %>%
                ggplot(mapping = aes(x = Date, y = Value, group = County)) +
                geom_point() +
                geom_line() +
                labs(y = "Unemployment rate (percent)") +
                theme_minimal()
        )
    })
    
}

# run app
shinyApp(ui, server)

