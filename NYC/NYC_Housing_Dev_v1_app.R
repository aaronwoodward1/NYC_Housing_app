#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(dplyr)
library(tidyverse)
library(lubridate)
library(shiny)

#StreetEasy_df$time <- as.Date(StreetEasy_df$time, 'ymd')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("NYC Boroughs and Neighborhoods"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          ui <- fluidPage(
#            selectInput("boro", "Borough", choices = unique(StreetEasy_df$Borough)),
            selectInput("boro", "Borough", choices = unique(master_df$Borough)),
            selectInput("neighborhood", "Neighborhood", choices = NULL),
            selectInput("transaction", "Sale or Rent", choices = c("Sale", "Rent")),
            #Conditional panel to filter out the property types based on transaction ("Sale" or "Rent")
            conditionalPanel(
              condition = "input.transaction == 'Sale'",
              selectInput("sale_selected", "Type of Property:",
                          choices = c("All properties", "Condo", "Co-Op", "Single-Family"),
                          selected = 1,
                          multiple = FALSE,
                          selectize = TRUE,
                          width = NULL,
                          size = NULL
              )),
            conditionalPanel(
              condition = "input.transaction == 'Rent'",
              selectInput("rent_selected", "Type of Rental:",
                          choices = c("All Rentals", "Studio", "1 Bedroom", "2 Bedrooms",
                                      "3+ Bedrooms"),
                          selected = 1,
                          multiple = FALSE,
                          selectize = TRUE,
                          width = NULL,
                          size = NULL
              )),
            conditionalPanel(
              condition = "input.transaction == 'Sale'",
              selectInput("sale_selected", "Select Sale metric:",
                          choices = c("Median Sale Price", 
                                      "Median Sale Ask Price", 
                                      "Number of Properties Sold", 
                                      "Sale Inventory", 
                                      "Days on Market"),
                          selected = 1,
                          multiple = FALSE,
                          selectize = TRUE,
                          width = NULL,
                          size = NULL
              )),
            conditionalPanel(
              condition = "input.transaction == 'Rent'",
              selectInput("rent_selected", "Select Rent Metric:",
                          choices = c("Median Asking Rent", 
                                      "Rental Inventory"),
                          selected = 1,
                          multiple = FALSE,
                          selectize = TRUE,
                          width = NULL,
                          size = NULL
              ))
          )
        ),
 
        # Show a plot of the generated distribution
           mainPanel(
             tableOutput("data_table")
             )
        )
)
 


  
# Define server logic required to draw a histogram
server <- function(input, output) {
  boro <- reactive({
    filter(master_df, Borough == input$boro)
#  filter(StreetEasy_df, Borough == input$boro)
  })

  observeEvent(boro(), {
    choices <- unique(boro()$areaName)
    updateSelectInput(inputId = "neighborhood", choices = choices) 
  })

#  observeEvent <- reactive({
#    updateSelectInput(inputId = "transaction")
#  })
  
#  observeEvent <- reactive({
#    choices <- unique(transaction()$)
#  })
  
#transaction_type <- reactive({
 #req(input$transaction)
 #filter(territory(), CUSTOMERNAME == input$customername)
# })

#observeEvent(transaction_type(), {
  
#  choices <- unique(customer()$ORDERNUMBER)
#  updateSelectInput(inputId = "ordernumber", choices = choices)
#})

output$data_table <- renderTable({
  req(input$neighborhood, input$transaction)
  boro() %>% 
    filter(areaName == input$neighborhood) 
    select(time, RentAskRentAll, SaleAskPriceAll)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
