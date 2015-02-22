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

grepl("live", article.link, ignore.case = T)
# removes all the Live articles. still need to figure that shit out
article.link <- article.link[!grepl("live", article.link, ignore.case = T)]

for (i in 1:length(article.link)) {
  article[i] <- httpGET(paste("http://", article.link[i], sep = ""),curl = getCurlHandle())
}




article.body <- array()

for (i in 1:length(article.link)){
article.body[i] <- substring(article[i], gregexpr("article-body", article[i], ignore.case =T), gregexpr("</article>", article[i], ignore.case =T))
article.body[i] <- strsplit(article.body[[i]], "<")
a <- article.body[i][[1]][!grepl(">", substring(article.body[i][[1]], nchar(article.body[i][[1]]), nchar(article.body[i][[1]])))]
a <- a[!grepl("javascript",a)]
a <- substring(a, regexpr(">", a)+1, nchar(a))
a <- gsub("\"", "", a)
a <- gsub("\n", "", a)
article.body[i] <- paste(a, collapse = "")
}

#comments <- tm_map(comments , removeWords, c("please","airport","bial","will","can"))

article.tm <- Corpus(VectorSource(article.body))
article.tm <- tm_map(article.tm, removeWords, c("the", "and", "that"))
article.tm <- tm_map(article.tm, stripWhitespace)
article.tm <- tm_map(article.tm, removePunctuation)
article.tm <- tm_map(article.tm, removeNumbers)




wordcloud(article.tm)
# 
# comments <- Corpus(DirSource(dataPath,pattern=".csv"),list(reader=readPlain))
# #'
# comments <- tm_map(comments , stripWhitespace)
# #'
# comments <- tm_map(comments, content_transformer(tolower))
# #'
# comments <- tm_map(comments , removePunctuation)
# #'
# comments <- tm_map(comments ,removeNumbers)
# #'
# comments <- tm_map(comments , removeWords, stopwords("english"))
# comments <- tm_map(comments , removeWords, c("please","airport","bial","will","can"))
# #'
# comments <- tm_map(comments,stemDocument)
# #'
# comments <- tm_map(comments , removeWords, stopwords("english"))
# comments <- tm_map(comments , removeWords, c("please","airport","bial","will","can"))
# #'
# tdm <- TermDocumentMatrix(comments,control=list(weighting=weightTfIdf))
# #'
# tdm <- TermDocumentMatrix(comments)
# m <- as.matrix(tdm)








