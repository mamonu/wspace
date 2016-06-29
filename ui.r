

shinyUI(pageWithSidebar(
  headerPanel('wordspace'),
  sidebarPanel(
    textInput("text", "Word:", "method"),textInput("text2", "Word:", "methodology"),
    numericInput('clusters', 'nearest neighbours: ', 15,
                 min = 1, max = 20)
  ),
  mainPanel(
    plotOutput('plot1'),plotOutput('plot2'), verbatimTextOutput('info')
  )
))

