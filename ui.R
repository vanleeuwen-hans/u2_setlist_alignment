# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("U2 Setlist Alignment"),
  
  # Short description and links
  HTML("<small><small>This U2 Setlist Alignment visualization was created as part of the Capstone Project for my Google Data Analytics certification. The use of the data was with kind permission from Matt @ <a href='https://www.u2gigs.com'>u2gigs.com</a>. The code for this app can be found in my GitHub repository at: <a href='https://github.com/vanleeuwen-hans/u2_setlist_alignment'>https://github.com/vanleeuwen-hans/u2_setlist_alignment</a>. Hans van Leeuwen - October 2024.</small></small></br></br>"),
  
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