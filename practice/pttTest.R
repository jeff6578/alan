library(tmcn)
rm(list=ls(all.names = TRUE))
#library(rvest)
#install.packages("tmcn", 
#repos="http://R-Forge.R-project.org")
URL   = "https://www.ptt.cc/bbs/NTUcourse/index.html"
html  = read_table(URL)
title = html_nodes(html, "a")
href  = html_attr(title, "href")
data = data.frame(title = toUTF8(html_text(title)),
                href = href)

