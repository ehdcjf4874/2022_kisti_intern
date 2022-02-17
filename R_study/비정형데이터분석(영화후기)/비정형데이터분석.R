#install.packages("KoNLP")
#install.packages("remotes") 
#remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
#install.packages("wordcloud")

setwd("C:/Users/ehdcj/Desktop/2022_kisti_intern/R_study/비정형데이터분석(영화후기)")
library(wordcloud)
library(KoNLP)

data1<- readLines("영화후기댓글.txt",encoding = "UTF-8")
Sys.setlocale("LC_ALL","Korean") # 언어 다시 한글로
data1
data2<-sapply(data1,extractNoun,USE.NAMES = F)
data2
data3<-unlist(data2)
data3 <-Filter(function(x) {nchar(x)>=2},data3)
data3
write(unlist(data3),"다음평점2.txt")
data4 <-read.table("다음평점2.txt")
data4
wordcount<-table(data4)
wordcount
head(sort(wordcount,decreasing = T),20)
library(RColorBrewer)
palete<-brewer.pal(9,"Set3")
wordcloud(names(wordcount),freq=wordcount,scale=c(4,1),rot.per = 0,
          random.order = F, random.color = T, colors=palete)
