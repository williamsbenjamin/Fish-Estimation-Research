library(shiny)
library(tidyverse)
library(googleCharts)
# Define UI for application that draws a histogram
ui <- fluidPage(
  googleChartsInit(),
   titlePanel("Record Linkage Cutpoint"),
   
   # Sidebar with a slider input for number of bins 
   # sidebarLayout(
   #    sidebarPanel(
   #      sliderInput("bins",
   #                  "Cutpoint:",
   #                  min = 5,
   #                  max = 20,
   #                  value = 15,
   #                  animate = TRUE)
   #    ),
   # 
   #    # Show a plot of the generated distribution
   #    mainPanel(
   #       plotOutput("distPlot", width = "100%", height = "500px")
   #    )
   # )
   plotOutput("distPlot", width = "100%", height = "500px"),
   fluidRow(
    shiny::column(8, offset = 4,
     sliderInput("bins",
                 "Cutpoint:",
                 min = 5,
                 max = 20,
                 value = 15,
                 animate = TRUE)
    )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     cut_gather %>% 
        filter(Stat != "ty2_est" #&
       #          Stat != "rel_bias" &
       #          Stat != "PSE"
       ) %>% 
       ggplot(aes(cutoff,value)) +
       geom_col() +
       geom_vline(xintercept = input$bins,
                  color = "red", 
                  size = 1.2) +
       facet_wrap(~Stat, nrow = 3, scales = "free") +
       ylab(NULL) 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

