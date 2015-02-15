require(XML)
require(RCurl)

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
paste("https://", article.link[i], sep = "")

article[i] <- httpGET(paste("http://", article.link[i], sep = ""),curl = getCurlHandle())
}


i <- 4
nchar(article[i])
article.body <- array()
article.body[i] <- substring(article[i], gregexpr("article-body", article[i], ignore.case =T), gregexpr("</article>", article[i], ignore.case =T))
article.body[i] <- substring(article.body[i], min(gregexpr("<p>", article.body[i])[[1]])+3, max(gregexpr("</p>", article.body[i])[[1]])-1)

# teh great cleanup


article.body[i] <- sub("</a>", " ", article.body[i])

for (i in 1:length(gregexpr("<a",article.body[i]  )[[1]])) {
  a.s <- regexpr("<a", article.body[i])[[1]][1]
  a.e <- gregexpr(">", substring(article.body[i], regexpr("<a", article.body[i])[[1]][1], nchar(article.body[i])))[[1]][1]
  replace.this <- substring(article.body[i], a.s, a.s +a.e-1)
  article.body[i] <- sub(replace.this, " ",article.body[i] )
}
  
  









