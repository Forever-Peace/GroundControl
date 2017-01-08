shinyUI(fluidPage(
  titlePanel("Defensive Run Distributions Against vs Average"),
  
  sidebarLayout(position = "right",
    sidebarPanel( 
      h1("Control Panel"), 
      uiOutput("teamControls"),
      checkboxInput("plotallyears",label=h5("Plot 2010-2016 (combine all years)"),FALSE),
      conditionalPanel(
        condition="input.plotallyears==false",
        uiOutput("yearControls")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Run Distribution", plotOutput("carrieskde")),
        tabPanel("Cumulative Distribution", plotOutput("cumcarries"))
      ),
      h4("League average in blue, selected team in red."),
      textOutput("textcarries")
    )
  )
))