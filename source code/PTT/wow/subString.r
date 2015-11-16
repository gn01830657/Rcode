puller <- unlist(puller)
post <- unlist(post)

  for(i in puller){post <- gsub(i, "", post)}
  
  library("tm")
  library("tmcn")
  
  post <- removePunctuation(post)
  post <- removeNumbers(post)
  

  post <- gsub("\n", "", post)
  post <- gsub("¡÷", "", post)
  post <- gsub("±À", "", post)
  post <- gsub("¼N", "", post)
  
 library("jiebaR")
 kill = worker(user = "C:/Program Files/R/R-3.2.1/library/jiebaRD/dict/WOW.txt")
 killed <- kill <= post
 killed <- data.frame(killed)
 
 
 
 R_corpus <- Corpus(DataframeSource(killed))
 tdm <- TermDocumentMatrix(R_corpus, control = list(wordLengths = c(3, Inf)))
 
 library("wordcloud")
 
 m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
wordcloud(d$word, d$freq, min.freq = 3, random.order = F, ordered.colors = F, 
    colors = rainbow(length(row.names(m1))))