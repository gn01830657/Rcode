# ??�建立math?���?
math <- hsb$math
hist(math)

# ?��變title
hist(math, main = "Histogram of math!")

# ?��變x軸說???
hist(math, xlab = "Value of math")

# ?��變y軸說???
hist(math, ylab = "frequency")

# ?��變�?�色
hist(math, col = "blue")

# ??��??
legend("topright", "test")

# ?��變�?�割�?
hist(math, breaks = 2)
hist(math, breaks = 10)
hist(math, breaks = 20)
# 請�?�到console輸入`submit()`
