x <- tail(sunspot.year, 50) # ?��?��?��??��?50筆�?��?��?��??

# ?��?��?��布�??
plot(x) 
# 將�?��??��起�??
lines(x) # 低�?�繪??�函?��
# 調色
lines(x, col = "red")
# ??��??
lines(x, lwd = 2)
# ?��變�?��?��?��??
plot(x) # ??�新?��???
lines(x, lty = 3)
# 標�??
plot(x, main = "sunspot")
# ?��?��x 軸座�?
plot(x, xaxt = "n")
# y 軸座標更?��
plot(x, yaxt = "n") # ??�刪?��y軸座�?
axis(2, at = seq(10, 200, 10), labels = seq(10, 200, 10))
# 請�?�到console輸入`submit()`

