# load packages
library(shiny)
library(tidyverse)
library(lubridate)
library(here)

# Cleans up error message that pops up on launch
# The error doesn't prevent to app from functioning. I suspect it's an odd timing thing
options(shiny.sanitize.errors = TRUE)

merged_data <- read_csv(here::here("data", "merged-data.csv"), 
                        col_types = cols(Illness = col_character()))

# Separating deficiency_site into classification and area
# Empty entries for areas become NA
# Inspection_date becomes functional date
 merged_data <- merged_data %>%
  mutate(area = case_when(
    area == "" ~ NA_character_,
    TRUE ~ area)) %>% 
  mutate(inspection_date = ymd(inspection_date))



# Create UI
ui <- fluidPage(
  
  # App title
  titlePanel("How Dirty is Your Cruise Ship?"),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panels for inputs
    sidebarPanel(
      # Drop-down for cruise line
      selectizeInput("cruiseline",
                     label = "Please select your cruise line",
                     choices = unique(merged_data$cruise_line)),
      br(),
      br(),
      # Drop-down for cruise ship
      # Conditional on selected cruise line. Dropdown generated in server
      uiOutput("cruiseship"),
      img(src = "how-dirty.png", height = 150, width = 225),
    ),
    
    
    
    # Main panel for output
    mainPanel(
      tabsetPanel(
        tabPanel("Score Over Time", plotOutput("plot")),
        tabPanel("Common Violation Areas", 
                 column(width = 6, tableOutput("classification")),
                 column(width = 6, tableOutput("vio_area"))),
        tabPanel("Outbreaks",
                 column(width = 12, align = "center",
                        helpText(p("If table is empty, there are no recorded outbreaks."),
                                 p("Years may be listed more than once if there are multiple outbreaks.")),
                        tableOutput("outbreak_table"))),
        tabPanel("Download", downloadButton("download", "Download Ship Data"))
        )
      )
    )
  )


# Create server
server <- function(input, output) {

# Create data outputs refer to  
  data <- reactive({
    
    merged_data %>% 
      filter(cruise_line == input$cruiseline) %>% 
      filter(cruise_ship == input$cship)
  })
  
# Create conditional dropdown to select cruise ship    
  output$cruiseship <- renderUI({
    ships <- merged_data %>% 
      filter(cruise_line == input$cruiseline) %>% 
      select(cruise_ship)
    
    selectizeInput("cship",
                   "Please select your cruise ship",
                   choices = ships)
  })
 
# Fortifying data so it can be graphed
  output$plot <- renderPlot({
    graph_data <- fortify(data())
    
    ggplot(data = graph_data, aes(x = inspection_date, y = inspection_score))+
      geom_point()+
      geom_line()+
      geom_hline(yintercept = 85, color = "red")+
      labs(title = "Inspection rating of your cruise ship over time",
           x = "Year",
           y = "Score",
           caption = "Minimum passing score of 85 is marked by red line")+
      theme_minimal()
  })
  
# Fortifying data to be used in a table
# NA areas removed
# Create table of violation areas
  output$vio_area <- renderTable({
    data <- fortify(data()) %>% 
      drop_na(area)
    
    suppressMessages(data %>% 
                       group_by(area) %>% 
                       summarize("Number of Violations" = n()) %>% 
                       slice_max(`Number of Violations`, n = 10, with_ties = FALSE) %>% 
                       rename(Area = area))
  },
  striped = TRUE)

# Creating table of violation classification
  output$classification <- renderTable({
    data <- fortify(data())
    
    suppressMessages(data %>%
                       group_by(classification) %>% 
                       summarize("Number of Violations" = n()) %>% 
                       slice_max(`Number of Violations`, n = 10, with_ties = FALSE) %>% 
                       rename(Classification = classification))
  },
  striped = TRUE)
  
# Creating outbreaks table
output$outbreak_table <- renderTable({
  data <- fortify(data())
  
  suppressMessages(data %>% 
                     group_by(Year) %>% 
                     summarize("Illness" = Illness) %>%
                     distinct() %>% 
                     drop_na(Illness))
    
  },
  digits = 0,
  striped = TRUE)

# Create download
output$download <- downloadHandler(
  filename = function() {
    paste(input$cship, "Report.csv", sep = " ")
  },
  content = function(file) {
    write.csv(data(), file, row.names = FALSE)
  }
)
}
# Run app
shinyApp(ui = ui, server = server)