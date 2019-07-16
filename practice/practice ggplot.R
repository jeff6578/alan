##單筆折線圖

library(ggplot2)
df = data.frame(c("D0.5", "D1", "D2"),
                c(4.2, 10, 29.5))
names(df) = c("dose", "len")
head(df)


ggplot(data= df, aes(x=dose, y=len, group=1)) +
  geom_line()+
  geom_point()

ggplot(data= df, aes(x=dose, y=len, group=1)) +
  geom_line(linetype = "dashed")+
  geom_point()

ggplot(data= df, aes(x=dose, y=len, group=1)) +
  geom_line(color="red")+
  geom_point()

#試試看其他效果

ggplot(data= df, aes(x=dose, y=len, group=1)) +
  geom_step()+
  geom_point()

ggplot(data= df, aes(x=dose, y=len, group=1)) +
  geom_path()+
  geom_point()

##老師教學時間

library(grid)

ggplot(data= df, aes(x=dose, y=len, group=1)) +
  geom_line(arrow = arrow())+
  geom_point()

myarrow=arrow(angle = 15,
              ends = "both",
              type = "closed")

-----------------------------------------------------------------
##折線圖- 多線資料

df2 = data.frame(supp=rep(c("VC","OJ"), each=3),
                 dose=rep(c("D0.5", "D1", "D2"),2),
                 len=c(6.8, 15, 33, 4.2, 10, 29.5))
head(df2)

ggplot(data = df2, aes(x=dose, y=len, group=supp))+
  geom_line(linetype="dashed",
            color="blue",size=1.2)+
  geom_point(color="red",size=3)            

-----------------------------------------------------------------
##長條圖資料(以統計或不須統計)

dat = data.frame(
  time = factor(c("Lunch","Dinner"),
  levels = c("Lunch","Dinner")),
  total_bill = c(12.89, 17.23))
  
ggplot(data = dat, aes(x=time,y=total_bill,fill=time))+
  geom_bar(stat="identity")
  
##長條圖資料(未統計)
  
raw = diamonds
head(raw)

ggplot(data=diamonds,aes(x=cut))+
  geom_bar(fill="lightblue",color="black")
  
#散佈圖

airquality
ggplot(data = airquality)+
  geom_point(aes(x=Temp,y=Ozone,color=Month))

##堆疊直方圖資訊(未統計)

diamonds

ggplot(diamonds,aes(clarity,fill=cut))+
  geom_bar(position="fill")

#盒形圖資料(未統計)

diamonds

ggplot(diamonds,aes(x=clarity,y=depth))+
  geom_boxplot()


##雙連數  連續VS連續
diamonds

ggplot(data = diamonds,aes(x=table,y=depth))+
  geom_point()







    