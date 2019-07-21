# 載入套件
library(jiebaR)
library(jiebaRD)
library(wordcloud2)

library(text2vec)
library(stringr)


# 讀文檔
setwd("C:/Users/jeff6/Desktop/Github/alan/Week2-4")
data <- readChar("CUG.txt",720000)
str(data)

# 斷詞工具
cutter <- worker(stop_word = "C:/Users/jeff6/Desktop/Github/alan/Week2-4/a.txt")
new_user_word(cutter, "老孫")


# 斷詞、轉成list格式
x <- segment(data,cutter)
tokens=list(x)
class(tokens)


# 建構詞庫（對全文分詞结果進行彙總）
it = itoken(tokens, progressbar = FALSE)
vocab = create_vocabulary(it)
# 刪除一個字的詞
vocab=vocab[which(nchar(vocab$term)>1),]
# 刪除出現小於10次的詞
vocab = prune_vocabulary(vocab, term_count_min = 10L)
# 查看最高频的詞
tail(vocab,20)
vectorizer = vocab_vectorizer(vocab)
# 考慮詞的前後10個詞
tcm = create_tcm(it, vectorizer,skip_grams_window = 10L)
# 設置詞向量是4维的
glove = GlobalVectors$new(word_vectors_size = 40, vocabulary = vocab, x_max = 10)
wv_main = glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01)
dim(wv_main)
wv_context = glove$components
dim(wv_context)
wv_main[1,1]
t(wv_context)[1,1]
word_vectors = wv_main + t(wv_context)

# 建構“唐僧+徒弟”向量
relation = word_vectors["師父", , drop = FALSE] +
  word_vectors["徒弟", , drop = FALSE]

# 看和唐僧、徒弟，相關性前十的詞 
cos_sim = sim2(x = word_vectors, y = relation, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 10)

# 文字雲

z <- sort(table(x),decreasing = T)[1:50]
z <- data.frame(z)#先轉成dataframe

wordcloud2(z,
           size = 2,
           fontFamily = "微软雅黑",
           color = "random-light",
           backgroundColor = "black",
           shape = "triangle")

