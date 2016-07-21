math <- hsb$math
plot(density(math))
math.sj <- density(math, bw = "SJ")
plot(math.sj)
# ç·šç?„ç?—ç´°
plot(math.sj, lwd = 2) # lwdè¶Šå¤§è¶Šç??
# ç·šç?„å?‹æ??
plot(math.sj, lty = 2) 
if (FALSE) {
  # ä»¥ä?‹æ?‡ä»¤?¯ä»¥ç•«?‡ºlty??„æ•¸å­—è?‡ç•«??–å?Œç?„ç?æ??
  showLty <- function(ltys, xoff = 0, ...) {
    stopifnot((n <- length(ltys)) >= 1)
    op <- par(mar = rep(.5,4)); on.exit(par(op))
    plot(0:1, 0:1, type = "n", axes = FALSE, ann = FALSE)
    y <- (n:1)/(n+1)
    clty <- as.character(ltys)
    mytext <- function(x, y, txt)
      text(x, y, txt, adj = c(0, -.3), cex = 0.8, ...)
    abline(h = y, lty = ltys, ...); mytext(xoff, y, clty)
    y <- y - 1/(3*(n+1))
    abline(h = y, lty = ltys, lwd = 2, ...)
    mytext(1/8+xoff, y, paste(clty," lwd = 2"))
  }
  showLty(1:6)
}
# ç·šç?„é?è‰²
plot(math.sj, col = "red")
# å°ç?šä?‹ä?‹ç?„é¢ç©è?—è‰²
polygon(math.sj, col = "red") # ?€™æ˜¯ä¸€?€‹ä?Žé?Žç¹ª??–å‡½?•¸
# æ¨™é??
plot(math.sj, main = "math")
# xè»¸æ?™é??
plot(math.sj, xlab = "math")
# è«‹å?žåˆ°consoleè¼¸å…¥`submit()`
