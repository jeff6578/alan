rm(list=ls())
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- "msleep_ggplot2.csv"
if (!file.exists(filename)) download(url,filename)
msleep <- read.csv("msleep_ggplot2.csv")
head(msleep)
library(dplyr)
sleepData <- select(msleep, name, sleep_total)
head(sleepData)
head(select(msleep, -name))
head(select(msleep, awake:bodywt))
head(select(msleep, starts_with("b")))
filter(msleep, sleep_total >=16)
filter(msleep, sleep_total >=16, bodywt <=1)
filter(msleep, order %in% c("Perissodactyla", "Primates"))
       
head(select(msleep, name, sleep_total))
msleep %>%
  select(name, sleep_total) %>%
  head

msleep %>% arrange(order) %>% head
msleep %>% arrange(sleep_total) %>% head

msleep %>%
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>%
  head

msleep %>% 
  select(name, order, sleep_total) %>%
  arrange(order, sleep_total) %>% 
  filter(sleep_total >= 16)
