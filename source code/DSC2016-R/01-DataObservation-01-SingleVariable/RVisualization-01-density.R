math <- hsb$math
plot(density(math))
math.sj <- density(math, bw = "SJ")
plot(math.sj)
# 線�?��?�細
plot(math.sj, lwd = 2) # lwd越大越�??
# 線�?��?��??
plot(math.sj, lty = 2) 
if (FALSE) {
  # 以�?��?�令?��以畫?��lty??�數字�?�畫??��?��?��?��??
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
# 線�?��?�色
plot(math.sj, col = "red")
# 對�?��?��?��?�面積�?�色
polygon(math.sj, col = "red") # ?��是一?���?��?�繪??�函?��
# 標�??
plot(math.sj, main = "math")
# x軸�?��??
plot(math.sj, xlab = "math")
# 請�?�到console輸入`submit()`
