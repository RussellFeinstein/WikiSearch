# Example: Shiny app that searches Wikipedia web pages
# File: WikiSearch.R

# Wikipedia Search
library(tm)
library(stringi)
library(WikipediR)
library(wordcloud)
# library(proxy)

SearchWiki <- function (titles) {
  # wiki.URL <- "https://en.wikipedia.org/wiki/"
  # articles <- lapply(titles,function(i) stri_flatten(readLines(stri_paste(wiki.URL,i)), col = " "))
  
  articles <- lapply(titles,function(i) page_content("en","wikipedia", page_name = i,as_wikitext=TRUE)$parse$wikitext)
  
  docs <- Corpus(VectorSource(articles)) # Get Web Pages' Corpus
  remove(articles)
  
  # Text analysis - Preprocessing 
  transform.words <- content_transformer(function(x, from, to) gsub(from, to, x))
  temp <- tm_map(docs, transform.words, "’", "'")
  temp <- tm_map(temp, transform.words, "<.+?>", " ")
  temp <- tm_map(temp, transform.words, "\t", " ")
  temp <- tm_map(temp, content_transformer(tolower)) # Conversion to Lowercase
  # temp <- tm_map(temp, PlainTextDocument)
  temp <- tm_map(temp, stripWhitespace)
  temp <- tm_map(temp, removeWords, stopwords("english"))
  temp <- tm_map(temp, removePunctuation)
  temp <- tm_map(temp, stemDocument, language = "english") # Perform Stemming
  remove(docs)
  
  # Create Dtm 
  dtm <- DocumentTermMatrix(temp)
  dtm <- removeSparseTerms(dtm, 0.4)
  # dtm$dimnames$Docs <- titles
  # docsdissim <- dist(as.matrix(dtm), method = "euclidean") # Distance Measure
  # h <- hclust(as.dist(docsdissim), method = "ward.D2") # Group Results
  
  # wordcloud(words = colnames(as.matrix(dtm)),
  #           freq=colSums(as.matrix((dtm))),
  #           max.words=50, random.order=FALSE
  #           )
  return(as.matrix(dtm))
  
}