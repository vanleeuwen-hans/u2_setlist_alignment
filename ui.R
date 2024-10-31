# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("U2 Setlist Alignment"),
  
  # tour selection and max shows input
  fluidRow(
    column(
      width = 5,
      selectInput("tour", "Select Tour:", 
                  choices = unique(u2data$tour),
                  selected = "U2 Vertigo Tour")
    ),
    column(
      width = 5,
      selectInput("max_shows", "Number of shows:", 
                  choices = c(10, 25, 50),
                  selected = 25)
    )
  ),
  
  # Show the plot output with loading spinner
  withSpinner(
    plotOutput("setlistPlot", width="1000px", height = "750px"),
    type = 8,  # You can choose different spinner types (1-8)
    size = 2  # Adjust the size
  )
)