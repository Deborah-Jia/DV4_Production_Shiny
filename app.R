library(shiny)
library(ggiraph)
library(shinythemes)
library(data.table)
library(DT)
library(lubridate)
library(shinydashboard)

source('helper_function/global.R')

# Nobel Prize UI -------------------------------------------------------
ui <- dashboardPage( 
    dashboardHeader(title = 'Nobel Winners'),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem('Plots', tabName = 'Plots' ,icon = icon('tint')),
            menuItem('Publication', tabName = 'Publication', icon= icon('balance-scale-left')),
            menuItem("Source code", icon = icon("file-code-o"), href = "https://github.com/Deborah-Jia/DV4_Production_Shiny")
        )
        ), 
    
    dashboardBody(
        tabItems(
            tabItem(tabName = "Plots",
                          fluidRow(
                              box(
                                       checkboxGroupInput("choose_category", label = h3("Checkbox Category"),
                                                          choices = c("Chemistry", "Economics", "Literature", "Medicine", "Peace","Physics"),
                                                          selected = c("Chemistry", "Economics", "Literature", "Medicine", "Peace","Physics")
                                       ),
                              
                                       dateRangeInput("range_dates", label = h3("Date range"), format = 'yyyy', end = '2016-12-31', start = '1907-01-01')),
                                       
                              box(
                              selectInput("select_country", label = h3("Select one country"),
                                                   choices = nobel$birth_country,
                                                   selected = nobel$birth_country
                                       ),
                        
                              )
                          ),
                          
                          fluidRow(                                       
                              box(width = 6,
                                       girafeOutput("p1_plot")),#, width = "100%", height = "700px"),
                                       box(width = 6,
                                       girafeOutput("p2_plot"))#, width = "100%", height = "700px")
                                       ),
                    fluidRow(box(width = 12,
                        h1(textOutput('chosen_country'))))

                          ),
                          
                 

        tabItem("Publication",
                fluidRow(
                    selectizeInput('pub_ticker', label = "Pick a winner", choices = NULL),
                              dateRangeInput('chosen_date',label = 'Date', format = 'yyyy', end = '2018-12-31', start = '1826-01-01')
                          ),
                fluidRow(
                              dataTableOutput('pub_data',height = "auto")
                          )
                 )
                 
)))


# Nobel Prize Server ---------------------------------------------------

library(shiny)

server <- shinyServer(function(input, output, session) {
    
    to_text <- reactive({
        country_medal_info(input$select_country)
    })
    output$p1_plot <- renderGirafe({
        girafe(ggobj = year_category_bar(lubridate::year(as.Date(input$range_dates[1],"%Y-%m-%d")),
                          lubridate::year(as.Date(input$range_dates[2],"%Y-%m-%d")), input$choose_category))
    })


    output$p2_plot <- renderGirafe({
        girafe(ggobj = country_line(input$select_country))
    })


    output$chosen_country <- renderText({
        to_text()
    })


    #all publications
    updateSelectizeInput(session, 'pub_ticker', choices = publication$laureate_name, selected = "stoddart, j", server =TRUE)
    
    # output$pub_ticker <- renderUI({
    #     selectInput('ticker', label = 'Selected Names', choices = publication$laureate_name, multiple = TRUE, selected = publication$laureate_name)
    # })

    my_reactive_df <- reactive({
        df<- name_year_table(input$pub_ticker, input$chosen_date[1], input$chosen_date[2])
        return(df)
    })
    output$pub_data <- DT::renderDataTable({
        my_reactive_df()
    })
    
    
})


# Run the application 
shinyApp(ui = ui, server = server)
