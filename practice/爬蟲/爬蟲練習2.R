library(rvest)
library(magrittr)

htmlcontent <- read_html("https://www.mobile01.com/forumtopic.php?c=29")

titlePath =  ".subject-text a"
titles = htmlcontent %>% html_nodes(".subject-text > a")%>%html_text()


replyPath =  " html/body/div[6]/table/tbody/tr[1]/td[2]"
reply = htmlcontent %>% html_nodes("td.reply")%>%html_text()
reply

mytable = data.frame(Title=titles, Reply=reply)

mytable

