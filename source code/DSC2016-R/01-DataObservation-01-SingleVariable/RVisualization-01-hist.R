# ??ˆå»ºç«‹math?‰©ä»?
math <- hsb$math
hist(math)

# ?”¹è®Štitle
hist(math, main = "Histogram of math!")

# ?”¹è®Šxè»¸èªª???
hist(math, xlab = "Value of math")

# ?”¹è®Šyè»¸èªª???
hist(math, ylab = "frequency")

# ?”¹è®Šé?è‰²
hist(math, col = "blue")

# ??–æ??
legend("topright", "test")

# ?”¹è®Šå?‡å‰²é»?
hist(math, breaks = 2)
hist(math, breaks = 10)
hist(math, breaks = 20)
# è«‹å?žåˆ°consoleè¼¸å…¥`submit()`
