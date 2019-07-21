
library(rvest)
library(tm)
library(jiebaR)
library(Matrix)

##抓全部連結裡面的內文
content <- c()   
x <- c(7987:7988)
front <- "https://tw.hjwzw.com/Book/Read/1889,57"
for(i in 1:length(x)){    ##抓links裡面1到最後一個數值 長度的迴圈
  url <- paste0(front, x[i])  ##把連結的尾巴加上前面的網址存到url
  print(url)
  
  content[i] <- read_html(url) %>%    ##讀url內的文字資料丟到content
    html_nodes("#AllySite+ div") %>%   ##抓指定的資料
    html_text                         ##文字資料 
}

### 開始清洗
content <- as.list(content)    ##轉成list
x <- Corpus(VectorSource(content)) %>% # Corpus(VectorSource())的input是list
  tm_map(removePunctuation) %>%  ## 標點符號
  tm_map(removeNumbers) %>%     ##數字
  tm_map(function(word) { # Regular Expression 把英文&數字的內容拿掉
    gsub("[A-Za-z0-9]", "", word)    ##把 function(word) 裡面的"[A-Za-z0-9]" 換成 ""
  })


### 開始斷詞

mixseg = worker()
jieba_tokenizer = function(d){ # 寫function來處理斷詞
  unlist(segment(d[[1]], mixseg))
}
seg <- lapply(x, jieba_tokenizer) # 對每一個文本(在這裡是每一個網頁內文)執行斷詞函數
n <- length(seg) # n為文本數 之後會一直用到

### 開始做TDM (TermDocumentMatrix)

count_token = function(d){ # 寫function來把清單轉為dataframe
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token) # lapply對list中每一個文本做出來的斷詞們轉為dataframe

TDM = tokens[[1]] # 設置一個初始TDM來方便Merge
for(id in 2:n){ # 用迴圈把list裡的所有dataframe merge起來
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', 1:id) # 這裡不一樣是因為我不是讀本機檔 
}

TDM[is.na(TDM)] <- 0 # 缺漏值補 0
head(TDM)

### 做 TF-IDF

# apply(要做的部分, 1對Row 2對Col, 要執行什麼函數)
tf <- apply(as.matrix(TDM[,2:(n+1)]), 2, sum) # 等等在算詞頻時會用到
idfCal <- function(word_doc){ # idf 計算的函數 總文本數/出現該詞的文本數
  log2( n / nnzero(word_doc) ) 
}

idf <- apply(as.matrix(TDM[,2:(n+1)]), 1, idfCal) # 對每個 Row 算 idf (有幾Col/幾個不是0)
doc.tfidf <- TDM

# 把剛剛算的tf重複擺在矩陣的Row 有幾個詞擺幾個Row
tempY = matrix(rep(c(as.matrix(tf)), each = length(idf)), nrow = length(idf)) 
# 把剛剛算的idf擺在矩陣中，每個詞彙有自己獨立的idf
tempX = matrix(rep(c(as.matrix(idf)), each = length(tf)), ncol = length(tf), byrow = TRUE)
# 帥氣的把全部一起算 (每個欄位都算到 (該文本出現該詞彙的次數 / 該文本的詞彙數，也就是TF) * 已經算好的idf)
doc.tfidf[,2:(n+1)] <- (doc.tfidf[,2:(n+1)] / tempY) * tempX

stopLine = rowSums(doc.tfidf[,2:(n+1)]) # tfidf 依照Row加總
delID = which(stopLine == 0) # 找到第幾個詞的RowSum是0
head(doc.tfidf[delID,1]) # 這些就是氾濫字眼 (TF-IDF=0 也就是中央空調的部分log(1) = 0)

TDM = TDM[-delID,] # 不要氾濫字眼就拿掉
doc.tfidf = doc.tfidf[-delID,] # 拿掉

### 找重要的關鍵字

TopWords = data.frame()
for( id in 1:n ){
  dayMax = order(doc.tfidf[,id+1], decreasing = TRUE) # 找到每一個文本 按照tf-idf大小排序
  showResult = t(as.data.frame(doc.tfidf[dayMax[1:5],1])) # 轉置這個dataframe並且取出前5高的
  TopWords = rbind(TopWords, showResult) # 跟其他Dataframe合併
}

rownames(TopWords) = colnames(doc.tfidf)[2:(n+1)]
TopWords = droplevels(TopWords)
View(TopWords)

#----------------------------------------------------------------------------------


library("tm")
library("tmcn")
library("rJava")
library("Rwordseg")
library("SnowballC")
library("slam")
library("Matrix")
library("Rcpp")

# import data
docs = data.frame(
  c("立找洗字人草鞋墩庄"),
  c("北投保北投莊"),
  c("邰北府淡水縣正堂"),
  c("仝立找洗字人林"),
  c("邰北縣奎府"),
  c("立杜賣盡根絕田契字"),
  c("仝立合約開鑿圳路字"),
  c("立仝換斷田契字"),
  c("立典大租契字竹塹社"),
  c("邰灣布政使司"))
colnames(docs) <- c(1:ncol(docs))

# query
q = c("邰","北","府")
insertWords(q)
q.num = c(1,1,1)

# corpus to tdm
d.corpus <- Corpus(VectorSource(docs))
d.corpus <- tm_map(d.corpus, segmentCN, nature = TRUE)
d.corpus <- Corpus(VectorSource(d.corpus))
tdm <- TermDocumentMatrix(d.corpus, control = list(wordLengths = c(1,1)))
inspect(tdm)

# tf-idf computation
tf <- apply(tdm, 2, sum) # term frequency
idf <- function(word_doc){ log2( (length(word_doc)+1) / nnzero(word_doc) ) }
idf <- apply(tdm, 1, idf)
doc.tfidf <- as.matrix(tdm)
for(i in 1:nrow(tdm)){
  for(j in 1:ncol(tdm)){
    doc.tfidf[i,j] <- (doc.tfidf[i,j] / tf[j]) * idf[i]
  }
}

# get short doc matrix
all.term <- rownames(doc.tfidf)
loc <- which(all.term %in% q)
s.tdm <- doc.tfidf[loc,]

# result : cos similarity ranking
cos.sim <- function(x, y){ x%*%y / sqrt(x%*%x * y%*%y) }
doc.cos <- apply(s.tdm, 2, cos.sim, y = q.num)
doc.cos[order(doc.cos, decreasing = TRUE)]

