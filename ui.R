
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rCharts)

shinyUI(fluidPage(

  # Application title
  titlePanel("AB Test - Bonferroni Corrected Multiple Chi-Square Test"),
 

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("alpha",
                  "Significance Level of AB test",
                  min = 0.01,
                  max = 0.1,
                  value = 0.05, step=0.01)
    ),

    # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(
            tabPanel("Summary", tableOutput("TestResultTable"), 
                            textOutput('SiginificanceText')),
            tabPanel("Rejection Region",showOutput('ChiSquareDist',lib='polycharts')),
            tabPanel("Group Response Distribution",showOutput('DistributionGraph',lib='nvd3'))
        )
  
    )
  )
 
))
