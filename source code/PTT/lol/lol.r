library("XML")
library("httr")


data <- list()
for( i in 3000:3500){
  url <- paste('bbs/LoL/index', i, '.html', sep='')
  html <- content(GET("https://www.ptt.cc/", path = url,set_cookies(over18=1)
),as = 'parsed')
  url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
for(j in 1:19){
  data <- rbind(data, url.list[[j]])
}
}

  
  post <- list()
  puller <- list()
  
