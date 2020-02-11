library(shiny)
library(tidycensus)
library(leaflet)
library(mapview)
library(dplyr)
library(DT)


#get variables from csv files
states <- read.csv("states.csv")
variables1 <- read.csv("general_income_vars.csv")
variables1$name <- as.character(variables1$name)
census_api_key("4b8e206a26dcf3fb692e924c9b2dcf67d3ec030a")

#generate ui with input to change variables
ui <- fluidPage(titlePanel("Census Data Browser"),
                sidebarLayout(sidebarPanel(
                  selectInput(
                    "var1",
                    "Select a state:",
                    choices = c("All", as.character(states$NAME))
                  ),
                  selectInput(
                    "var2",
                    "Select a variable:",
                    variables1$desc
                  )
                ),
                mainPanel(leafletOutput("map"),
                          DT::dataTableOutput("table1"))))

#plot the resulting esimates for each tract onto the map of spokane county
server <- function(input, output, session) {
  
  #get the data for the state or county level  
  geom <- reactive({
    if(input$var1 == "All"){
      get_acs(geography = "state",
              variables = variables1[variables1$desc %in% input$var2, ]$name,
              geometry = TRUE,
              shift_geo = TRUE)
    }
    else{
      get_acs(geography = "county",
              state = input$var1,
              variables = variables1[variables1$desc %in% input$var2, ]$name,
              geometry = TRUE)
    }
  })
  
  output$map <- renderLeaflet({
    spok_shapes2 <- select(geom(), GEOID, NAME, geometry, estimate)
    m <- mapview(spok_shapes2, zcol = "estimate", layer.name = "Median Income", legend = TRUE)
    m@map
  })
  
  output$table1 <- DT::renderDataTable({
    
    #select only the data from the shape file
    pre1 <- geom() %>% data.frame() %>% select(GEOID, NAME, variable, estimate, moe)
    pre2 <- merge(pre1, variables1, by.x = "variable", by.y = "name", all.x = TRUE) %>% arrange(desc(estimate)) %>% mutate(rank = 1:length(estimate))
    pre3 <- select(pre2, rank, "Location" = NAME, "Occupation" = desc, "Median Income" = estimate, "MOE" = moe)
    
    DT::datatable(pre3,
                  rownames = FALSE,
                  extensions = list('Buttons'=NULL, 'Responsive'=TRUE),
                  options = list(
                    lengthMenu = list(c(25, 50, -1), c('25', '50', 'All')),
                    pageLength = 100,
                    dom = 'lfBrtip',
                    buttons = c('copy', 'csv', 'excel', 'pdf', 'print')), 
                  escape = FALSE
    ) %>% formatCurrency(c('Median Income', 'MOE'), "$")
  })
  
}

#run the application
shinyApp(ui = ui, server = server)
