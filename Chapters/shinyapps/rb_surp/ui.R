shinyUI(fluidPage(
  titlePanel("Player Game Scores"),
  
  sidebarLayout(position = "right",
    sidebarPanel( 
      h1("Control Panel"), 
      uiOutput("playerControls"),
      checkboxInput("sortplayers",label=h5("Sort by Career Carries"),TRUE),
      checkboxInput("markyears",label=h5("Mark games from particular year"),FALSE),
      conditionalPanel(
        condition="input.markyears==true",
        uiOutput("yearControls")
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Game Surprisal", plotOutput("gamesurp")),
        tabPanel("Game Score (marginal Surprisal)", plotOutput("surpmarg"))
      ),
      h4("League average in blue, selected player in red.")
    )
  )
))
