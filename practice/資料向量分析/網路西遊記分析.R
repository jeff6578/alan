#載入套件
library(jiebaR)
library(jiebaRD)
library(wordcloud2)

library(text2vec)
library(stringr)


#讀文檔
setwd("C:/Users/jeff6/Desktop/Github/alan/Week2-4")
data <- readChar("CUG.txt",720000)

str(data)

#开始分词
wk = worker()
seg_MK=wk[data]

#注意：这里要转成list格式
tokens=list(seg_MK)
class(tokens)
# 构造词库（对文中分词结果进行汇总）
it = itoken(tokens, progressbar = FALSE)
vocab = create_vocabulary(it)
#删除一个字的词
vocab=vocab[which(nchar(vocab$term)>1),]
#删除出现小于5次的词
vocab = prune_vocabulary(vocab, term_count_min = 5L)
#查看最高频的词
tail(vocab,20)
vectorizer = vocab_vectorizer(vocab)
# 考虑词的前后5个词
tcm = create_tcm(it, vectorizer,skip_grams_window = 5L)
#设置词向量是4维的
#glove算法：https://nlp.stanford.edu/projects/glove/
glove = GlobalVectors$new(word_vectors_size = 40, vocabulary = vocab, x_max = 10)
wv_main = glove$fit_transform(tcm, n_iter = 10, convergence_tol = 0.01)
dim(wv_main)
wv_context = glove$components
dim(wv_context)
wv_main[1,1]
t(wv_context)[1,1]
word_vectors = wv_main + t(wv_context)

#构造“唐僧+徒弟”向量
relation = word_vectors["唐僧", , drop = FALSE] +
  word_vectors["徒弟", , drop = FALSE]

#计算相关性，查看相关性最高的词 
cos_sim = sim2(x = word_vectors, y = relation, method = "cosine", norm = "l2")
head(sort(cos_sim[,1], decreasing = TRUE), 20)
