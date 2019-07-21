#載入套件
library(dplyr)
library(jiebaR)
library(jiebaRD)
library(wordcloud2)

#讀文檔
setwd("C:/Users/jeff6/Desktop/Github/alan/Week2-4")
data <- readChar("CUG.txt",720000)

str(data)


#斷詞工具

cutter <- worker(stop_word = "C:/Users/jeff6/Desktop/Github/alan/Week2-4/a.txt")
new_user_word(cutter, "老孫")


#斷詞，排序x取1~50
x <- segment(data,cutter)
x <- sort(table(x),decreasing = T)[1:50]
x <- data.frame(x)



#文字雲
wordcloud2(x,
           size = 2,
           fontFamily = "微软雅黑",
           color = "random-light",
           backgroundColor = "black",
           shape = "triangle")


#----------------------------------------------------------------------------底
#由大到小排序
head(x[order(x$Freq,decreasing = TRUE),])

#去除空格，把所有自連起來
data1 <- str_trim(data1)
data1 <- str_trim(data1, side = c("both", "left", "right"))

#取前50
x <- sort(table(cutter[data]),decreasing = T)[1:50]

#關鍵字
keys <- worker("keywords",topn=5)
vector_keywords(x,keys)

#---------------------------------------------------------------參考

