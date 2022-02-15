setwd("C://R")
#install.packages("rvest")
#install.packages("httr")
library(rvest)
library(httr)
library(KoNLP)
url='https://search.daum.net/search?nil_suggest=btn&w=blog&DA=SBC&q=%EC%9A%B0%EB%8F%84%EB%A7%9B%EC%A7%91%EC%B6%94%EC%B2%9C'
response = GET(url)
response
htxt=read_html(response)
htxt
comments = html_nodes(htxt, 'div.cont_inner')
comments
data=html_text(comments)
data
write(data,"우도맛집추천.txt")

#데이터를 명사로 전처리 과정
data1<-readLines("우도맛집추천.txt")
data1
data2<-sapply(data1,extractNoun,USE.NAMES = F)
data2
data3<- unlist(data2)
data3
data3<-Filter(function(x){nchar(x)>=2},data3)
data3
write(unlist(data3),"우도 맛집 명사.txt")
data3

#gsub를 이용하여 불용어 텍스트를 제거 또는 변환
data3=gsub("[[:punct:]]","",data3) #특수기호를 공백으로
data3=gsub("[0-9]","",data3) #숫자를 공백으로
data3=gsub("[a-z]","",data3) #소문자 알파벳을 공백으로
data3=gsub("[A-Z]","",data3) #대문자 알파벳을 공백으로
data3=gsub("우도","",data3) 
data3=gsub("추천","",data3) 
data3=gsub("맛집","",data3) 
data3=data3[data3!='']
data3
wordcount=table(data3)
wordcount
sort(wordcount,decreasing=T)
library(RColorBrewer)
palete<-brewer.pal(9,"Set3")
wordcloud(names(wordcount),freq=wordcount,scale=c(4,1),rot.per=0,
          ramdom.order=F,random.color = T,colors=palete)


