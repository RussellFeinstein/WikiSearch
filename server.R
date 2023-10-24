# Example: Shiny app that search Wikipedia web pages
# File: server.R 
library(shiny)
library(tm)
library(stringi)
# library(proxy)
source("WikiSearch.R")

shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    # Progress Bar
    withProgress({
      setProgress(message='Mining Wikipedia...')
      result <- SearchWiki(input$select)
    })
    wordcloud(words = colnames(result),
              freq=colSums(result),
              max.words=50, random.order=FALSE,
              scale=c(4,.9), colors=brewer.pal(6,"Dark2")
    )
    
    # plot(result, labels = input$select, sub = "",main="Wikipedia Search")
  })
})