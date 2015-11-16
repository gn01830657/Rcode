for(i in 1:length(data)){
 html <- content(GET('https://www.ptt.cc', path = data[[i]],set_cookies(over18=1)), as = 'parsed')
  post <- c(post,xpathApply(html, "//div[@id='main-content']", xmlValue))  
  post
  puller <- c(puller,xpathApply(html,"//div[@class='push']/span[@class='f3 hl push-userid']",xmlValue))
}
