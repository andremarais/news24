deps = c("XML", "RCurl", "SnowballC","tm","wordcloud", "topicmodels", "RWeka");
for (dep in deps){
  if (dep %in% installed.packages()[,"Package"] == FALSE){
    install.packages(dep);
  }
}
# Packages for drawing website data
require(XML)
require(RCurl)
# Packages for text mining
require(SnowballC)
require(tm)
require(wordcloud)
require(topicmodels)
require(RWeka)


news24 <- httpGET("http://www.news24.com/",curl = getCurlHandle())

read.date <- Sys.Date() 

link.locations <- gregexpr(format(read.date, "%Y%m%d"), news24, ignore.case = T)

links <- array()
for (i in 1:length(link.locations[[1]])){
  links[i] <- substring(news24, link.locations[[1]][i]-150,link.locations[[1]][i]+10)
  
}

SA.articles <- links[grepl("SouthAfrica", links, ignore.case = T)]

article.link <- array()
article <- array()
for (i in 1:length(SA.articles)){
article.link[i] <- substring(SA.articles[i], gregexpr("www", SA.articles[i], ignore.case = T)[[1]][1], gregexpr(format(read.date, "%Y%m%d"), SA.articles[i], ignore.case = T)[[1]][1]+7)
}

article.link <- unique(article.link)

#article names
article.name <- as.character()
for (i in 1:length(article.link)) {
  article.name[i] <- gsub("-", " ", substring(article.link[i],max(gregexpr("/", article.link[i])[[1]])+1, nchar(article.link[i])-9))
  
}


# removes all the Live articles. still need to figure that shit out
article.link <- article.link[!grepl("live", article.link, ignore.case = T)]

# downloads all the articles
for (i in 1:length(article.link)) {
  article[i] <- httpGET(paste("http://", article.link[i], sep = ""),curl = getCurlHandle())
}



#cleans up all the articles
article.body <- array()
for (i in 1:length(article.link)){
  
  #finds the article body
  article.body[i] <- substring(article[i], gregexpr("article-body", article[i], ignore.case =T), gregexpr("</article>", article[i], ignore.case =T))
  
  #finds all the HTML parts and removes them
  article.body[i] <- strsplit(article.body[[i]], "<")
  a <- article.body[i][[1]][!grepl(">", substring(article.body[i][[1]], nchar(article.body[i][[1]]), nchar(article.body[i][[1]])))]
  a <- a[!grepl("javascript",a)]
  a <- substring(a, regexpr(">", a)+1, nchar(a))
  a <- gsub("\"", "", a)
  a <- gsub("\n", "", a)
  article.body[i] <- paste(a, collapse = "")
  
  #removes the location where article was written
  article.body[i] <- substring(article.body[i], min(gregexpr("-", article.body[i])[[1]]) + 2, nchar(article.body[i]))

}




# text analysis starts here
article.tm <- Corpus(VectorSource(article.body))

tdm <- TermDocumentMatrix(article.tm)

article.tm <- tm_map(article.tm, removeWords, stopwords("en"))
article.tm <- tm_map(article.tm, removeWords, c("&lsquo;", "&rsquo;", "&ldquo;", "&ldquo;", "&ndash", "said"))
article.tm <- tm_map(article.tm, removePunctuation)
article.tm <- tm_map(article.tm, stripWhitespace)
article.tm <- tm_map(article.tm, removePunctuation)
article.tm <- tm_map(article.tm, removeNumbers)

tdm <- TermDocumentMatrix(article.tm)
tdm <- as.matrix(tdm)  

j <- 2

v <- sort(tdm[,j], decreasing = T)
article.name[j]
wordcloud(names(v),v,c(4,.2), min.freq = 2, 100)

# v <- sort(rowSums(m),decreasing=TRUE)
# head(v)
# wordcloud(names(v),v,c(4,.2),2,200)
# wordcloud(names(v),v,c(4,.2),2,100)

