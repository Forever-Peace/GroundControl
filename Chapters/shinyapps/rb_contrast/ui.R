shinyUI(fluidPage(
  titlePanel("Player Comparison Machine"),
  
  sidebarLayout(position = "right",
    sidebarPanel( 
      h1("Control Panel"), 
      selectInput("compchoice",label=h3("Comparison to plot"),choices=c("Years","Players"),selected="Players"),
      uiOutput("playerControls1"), 
      conditionalPanel(
        condition="input.compchoice=='Players'",
        uiOutput("playerControls2")
      ),
      checkboxInput("sortplayers",label=h5("Sort by Career Carries"),TRUE),
      conditionalPanel(
        condition="input.compchoice=='Years'",
        uiOutput("yearControls1"),
        uiOutput("yearControls2")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Run Distribution", plotOutput("carrieskde")),
        tabPanel("Cumulative Distribution", plotOutput("cumcarries"))
      ),
      textOutput("textcarries1"),
      textOutput("textcarries2")
    )
  )
))