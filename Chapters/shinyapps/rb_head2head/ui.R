shinyUI(fluidPage(
  titlePanel("Player Comparison Machine"),
  
  sidebarLayout(position = "right",
    sidebarPanel( 
      h1("Control Panel"),
      numericInput("samplenum", label = h3("Number of carries to sample"), min = 0, max = 500, value = 25, step = 1),
      uiOutput("playerControls1"), 
      uiOutput("playerControls2"),
      checkboxInput("sortplayers",label=h5("Sort by Career Carries"),TRUE),
      actionButton("recalc","Run Simulation")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Run Distributions", plotOutput("carrieskde")),
        tabPanel("Results", htmlOutput("resultstext"))
      ),
      h5("Select two players and a sample size for a head to head competition. The simulator find who went further with their carries (expressed in yards per carry). These competitions are simulated 10,000 times to estimate win probability for each player. Vertical lines are overall player means (i.e. the average YPC for 10,000 YPCs found for the given sample size).")
    )
  )
))