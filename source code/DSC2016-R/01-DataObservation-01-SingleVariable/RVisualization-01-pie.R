# ??�建立sex?���?
sex <- table(hsb$sex)

# ??�設???
pie(sex)

# ??��?��?��??
pie(sex, main = "Sex")

# ?��顏色
col <- rainbow(length(sex)) # ?��?��rainbow?��?��?��??�若干種不�?��?�色�?
col # ?��是RGB??��?
pie(sex, main = "Sex", col = col)

# ??��?��?��??
pct <- sex / sum(sex) * 100 # 計�?�百??��??
label <- paste0(names(sex), " ", pct, "%") # ?��??�說??��?��??
label # ??�學?��以�?��?�label, names(sex), pct ??? "%" ??��?��??
pie(sex, labels = label)

if (FALSE) {
  # 3D pie chart
  library(plotrix) # ??�學請自行�?��?�這個�?�件
  pie3D(sex, labels = label)
}

# 請�?�到console輸入`submit()`

