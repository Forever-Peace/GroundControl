shinyUI(fluidPage(
  titlePanel("Player Matching Engine"),
  
  sidebarLayout(position = "right",
    sidebarPanel( 
      h1("Control Panel"), 
      uiOutput("playerControls"),
      checkboxInput("sortplayers",label=h5("Sort by Career Carries"),TRUE),
      uiOutput("player2Controls")
    ),
    mainPanel(
      plotOutput("cumcarries"),
      h4("Only players with 50+ carries since 2010 are included."),
      textOutput("textcarries1"),
      textOutput("textcarries2"),
      h5("Top 10 Matches:"),
      textOutput("top10matches")
    )
  )
))