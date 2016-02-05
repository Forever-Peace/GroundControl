shinyUI(fluidPage(
  titlePanel("Player vs Team"),
  
  sidebarLayout(position = "right",
    sidebarPanel( 
      h1("Control Panel"), 
      uiOutput("playerControls"), 
      checkboxInput("sortplayers",label=h5("Sort by Career Carries"),TRUE),
      checkboxInput("plotallyears",label=h5("Plot Career (combine all years)"),TRUE),
      conditionalPanel(
        condition="input.plotallyears==false",
        uiOutput("yearControls")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Run Distribution", plotOutput("carrieskde")),
        tabPanel("Cumulative Distribution", plotOutput("cumcarries")),
        tabPanel("Run Share", plotOutput("runshare"))
      ),
      h5("The selected player is compard to all teammates that made rushing attempts in the same games."),
      textOutput("textcarries1"),
      textOutput("textcarries2"),
      h5(""),
      h5("Other teammates included:"),
      textOutput("playercomparisons")
    )
  )
))