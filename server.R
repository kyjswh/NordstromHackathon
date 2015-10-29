
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(rCharts)
source('global.R')

shinyServer(function(input, output) {
  
  #This is the raw input data
  input_data <- read.csv('testData.csv',header=T)
  
  #Do some data manipulation on raw data, using global functions

  GroupSummaryData <- getSummaryData(input_data)
  TestResult <- getABTestResult(GroupSummaryData)
  #TestResult <- reactive({
        #alpha = input$alpha
       # getABTestResult(GroupSummaryData, alpha)
      #  })
  
  #Main Visualization Parts
  #TestResultDisplay <- TestResult
  #names(TestResultDisplay) <- c('Test', 'Chi-Square', 'Degree of Freedom', 'pvalue','Mean Proportion Difference', 'Significant?')
  
  output$TestResultTable <- renderTable({
      TestResult$significant <- ifelse(TestResult$corrected_pvalue < input$alpha, 'Yes','No')
      names(TestResult) <- c('Test', 'Chi-Square', 'Degree of Freedom', 'pvalue','Mean Proportion Difference', 'Significant?')
      return(TestResult)
  }
  )
 
  output$DistributionGraph <- renderChart2({
      barChart <- nPlot(Count~user_group,group = 'Response',
                       data = GroupSummaryData, type = 'multiBarChart')
 
      barChart$addParams(width = 600, height = 500, dom = 'chart1',
                   title = "Count of Response in each test group", xlab = 'Test Group')
      return(barChart)  
    })
  
  #Second Graph to Display The rejection Region
  output$ChiSquareDist <- renderChart2({
      TestResult$significant <- ifelse(TestResult$corrected_pvalue < input$alpha, 'Yes','No')
      #The siginificant tests
      sig_group <- subset(TestResult, significant == 'Yes')
    
      if(nrow(sig_group) > 0){
      #The best performance test group
        max_diff <- max(sig_group$mean_diff)
        best_group <- subset(sig_group, mean_diff == max_diff)
      }
      else{
          max_diff <- max(TestResult$mean_diff)
          best_group <- subset(TestResult, mean_diff == max_diff) 
      }
      
        df <- best_group$df[1]
        diff <- best_group$mean_diff[1]
        chisq_stat <- qchisq(best_group$corrected_pvalue[1],df,lower.tail=FALSE)
      
        chisq_dt <- data.frame(x = seq(0,10,0.1))
        chisq_dt$y <- dchisq(chisq_dt$x, df)
      
        p1 <- rPlot(x = 'x', y = 'y', data = chisq_dt[-1,], type = 'line',size = list(const = 3),color=list(const='Blue'))
      
        addLine <-data.frame(x=chisq_stat, y=seq(0,1))
        p1$layer(y~x,data=chisq_dt[chisq_dt$x > qchisq(input$alpha,df,lower.tail=F),], type = 'area',color=list(const='red'),size = list(const = 3))
         p1$layer(y~x, data=addLine, type = 'line', color=list(const='Green'),size = list(const = 3))
         p1$guides(x = list(title = "Chi Square", ticks = seq(1,10,1)))
         p1$guides(y = list(title = "Density"))
        p1$addParams(width = 600, height = 500, dom = 'chart2',
                   title = "Rejection Region of Chi-Square Distriution")
        return(p1)  
  
  })
  
  #output whether significance
  output$SiginificanceText <- renderText({
        TestResult$significant <- ifelse(TestResult$corrected_pvalue < input$alpha, 'Yes','No')
        
        #The siginificant tests
        sig_group <- subset(TestResult, significant == 'Yes')
        
        #Best Group
        if(nrow(sig_group) > 0){
            best_group <- subset(sig_group, mean_diff == max(sig_group$mean_diff))
        }
        else{
            best_group <- subset(TestResult, mean_diff == max(TestResult$mean_diff))
        }
        
        if(nrow(sig_group) > 0){
            paste("The best variant under alpha=", input$alpha, " is: ", strsplit(as.character(best_group$TestName[1]),' VS ')[[1]][2],sep='')
        }
        else{
            paste("The test is not significant under significance level:", input$alpha)
        }
      
  }
 )
})
