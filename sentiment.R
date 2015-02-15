require(XML)
require(RCurl)

news24 <- httpGET("http://www.news24.com/",curl = getCurlHandle())





link.locations <- gregexpr(format(Sys.Date()-1, "%Y%m%d"), news24, ignore.case = T)

links <- array()
for (i in 1:length(link.locations[[1]])){
  links[i] <- substring(news24, link.locations[[1]][i]-150,link.locations[[1]][i]+10)
  
}

SA.articles <- links[grepl("SouthAfrica", links, ignore.case = T)]

article.link <- array()
article <- array()
for (i in 1:length(SA.articles)){
article.link[i] <- substring(SA.articles[i], gregexpr("www", SA.articles[i], ignore.case = T)[[1]][1], gregexpr(format(Sys.Date()-1, "%Y%m%d"), SA.articles[i], ignore.case = T)[[1]][1]+7)
article[i] <- httpGET(article.link[i],curl = getCurlHandle())
}



