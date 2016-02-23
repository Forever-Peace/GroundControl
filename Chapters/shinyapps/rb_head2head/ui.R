shinyUI(fluidPage(
  titlePanel("Player Comparison Machine"),
  
  sidebarLayout(position = "right",
    sidebarPanel( 
      h1("Control Panel"),
      numericInput("samplenum", label = h3("Number of carries to sample"), min = 0, max = 500, value = 25, step = 1),
      uiOutput("playerControls1"), 
      uiOutput("playerControls2"),
      checkboxInput("sortplayers",label=h5("Sort by Career Carries"),TRUE),
      actionButton("recalc","Run Simulation"),
      conditionalPanel("$('#carrieskde').hasClass('recalculating')", 
                       tags$div('Loading ... ')
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Run Distributions", plotOutput("carrieskde")),
        tabPanel("Results", htmlOutput("resultstext"),tableOutput("fivenumtable"))
      ),
      h5("Select two players and a sample size for a head to head competition. The simulator finds who went further with their carries (expressed in yards per carry). To estimate the proportion that each player wins, we simulate the competition 10,000 times. Vertical lines are overall player means (i.e. the average YPC for the given sample size over all 10,000 competition simulations).")
    )
  )
))