shinyUI(fluidPage(
  titlePanel("Player Run Distributions vs Average"),
  
  sidebarLayout(position = "right",
    sidebarPanel( 
      h1("Control Panel"), 
      #selectInput("playertoplot",label=h3("Player"),choices=levels(unique(rb_app$full_name))),
      uiOutput("playerControls"),
      checkboxInput("sortplayers",label=h5("Sort by Career Carries"),FALSE),
      checkboxInput("plotallyears",label=h5("Plot Career (combine all years)"),FALSE),
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
      h4("League average in blue, selected player in red."),
      textOutput("textcarries")
    )
  )
))