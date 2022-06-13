library(rvest);search()
library(dplyr);search()
library(stringr)   # str_sub()      # 문자 자르기
library(readr)     # parse_number() #숫자만
library(data.table);search() # %like% 연산자 
library(ggplot2)
library(dygraphs) # 인터렉티브 시계열 그래프
library(xts) #시계열 데이터 생성
library(ggplot2)
library(tidyr)
library(plotrix)
library(readxl)
library(wordcloud)
library(RColorBrewer)
library(odbc)
library(rJava)
library(RJDBC)
library(DBI)
library(wordcloud2)

#미국

ame_Gyeongbokgung <- read.csv("amerika.csv",encoding = "UTF-8")

ame_chong <- read.csv("changduck.csv",encoding = "UTF-8")

ame_ducksu <- read.csv("Deoksugung.csv",encoding = "UTF-8")

ame_chongky <- read.csv("Changgyeonggung.csv",encoding = "UTF-8")

goo_ame<- data.frame(ame_Gyeongbokgung,ame_chong,ame_ducksu,ame_chongky)



new_goo<- goo_ame %>% select(2,3,5,8,11)

new_go <- new_goo[,c(2,1,3,4,5)]


#미국 검색빈도 파이 그래프

sum_new_go<- new_go %>% summarise(ky_sum = sum(Gyeongbokgung),
                                  ch_sum = sum(Changdeokgung),
                                  cy_sum = sum(Deoksugung),
                                  du_sum = sum(Changgyeonggung))


rownames(sum_new_go) = c("value")
colnames(sum_new_go) = c("value")

sum_new_go<- t(sum_new_go)

ame<- as.data.frame(sum_new_go)

ame<- ame %>%  mutate(gong = "ky_sum")


ame[2,2] <- "ch_sum"
ame[3,2] <- "cy_sum"
ame[4,2] <- "du_sum"


#일반 파이 그래프
pie(ame$value,labels = ame$gong)

#GGPLOt
ggplot(ame,aes(x="",y=value,fill=gong))+geom_bar(width = 1,stat = "identity")+coord_polar("y")+geom_text(aes(label=paste0(value)),position = position_stack(vjust = 0.5))+ ggtitle("2018~2022경복궁 검색 빈도 합계") +xlab(" ") + ylab("경복궁 검색 빈도")

#3D 파이
pie3D(ame$value,labels = ame$gong)

#인터렉티브  

new_go$date <- as.POSIXct(new_go$date) 

                             
Gyeongbokgung <- xts(new_go$Gyeongbokgung, order.by = new_go$date)
Changdeokgung <- xts(new_go$Changdeokgung, order.by = new_go$date)
Deoksugung <- xts(new_go$Deoksugung, order.by = new_go$date)
Changgyeonggung <- xts(new_go$Changgyeonggung, order.by = new_go$date)


my_ame_size<- cbind(Gyeongbokgung,Changdeokgung,Deoksugung,Changgyeonggung)

#미국 검색 비율
dygraph(my_ame_size) %>% dyRangeSelector()

#일본

ja_Gyeongbokgung <- read.csv("ja_kyoung.csv",encoding = "UTF-8")

ja_chong <- read.csv("ja_chang.csv",encoding = "UTF-8")

ja_ducksu <- read.csv("ja_ducksu.csv",encoding = "UTF-8")

ja_chongky <- read.csv("ja_changgyoung.csv",encoding = "UTF-8")

goo_ja<- data.frame(ja_Gyeongbokgung,ja_chong,ja_ducksu,ja_chongky)

goo_ja

new_ja<- goo_ja %>% select(2,3,5,8,11)

new_japan <- new_ja[,c(2,1,3,4,5)]

View(new_japan)

new_japan_name <- new_japan %>% rename(ja_kyoung=경복궁...일본.,
                                       ja_duck = 덕수궁...일본.,
                                       ja_chang = 창덕궁...일본.,
                                       ja_changkyung = 창경궁...일본.)

new_japan_name


new_japan_name$date <- as.POSIXct(new_japan_name$date) 




Gyeongbokgung <- xts(new_japan_name$ja_kyoung, order.by = new_go$date)
Changdeokgung <- xts(new_japan_name$ja_chang, order.by = new_go$date)
Deoksugung <- xts(new_japan_name$ja_duck, order.by = new_go$date)
Changgyeonggung <- xts(new_japan_name$ja_changkyung, order.by = new_go$date)


my_ja_size<- cbind(Gyeongbokgung,Changdeokgung,Deoksugung,Changgyeonggung)


#일본 검색 비율
dygraph(my_ja_size) %>% dyRangeSelector()


#중국

ca_Gyeongbokgung <- read.csv("ch_kyong.csv",encoding = "UTF-8")

ca_chong <- read.csv("ch_chong.csv",encoding = "UTF-8")

ca_ducksu <- read.csv("ch_duck.csv",encoding = "UTF-8")

ca_chongky <- read.csv("ch_chongkyoug.csv",encoding = "UTF-8")

goo_ca<- data.frame(ca_Gyeongbokgung,ca_chong,ca_ducksu,ca_chongky)

goo_ca

new_ca<- goo_ca %>% select(2,3,5,8,11)

new_china <- new_ca[,c(2,1,3,4,5)]

new_china

new_china_name <- new_china %>% rename(ca_kyoung=경복궁...대만.,
                                       ca_duck = 덕수궁...대만.,
                                       ca_chang = 창경궁...대만.,
                                       ca_changkyung = 창경궁...대만..1)


new_china_name$date <- as.POSIXct(new_china_name$date) 

Gyeongbokgung <- xts(new_china_name$ca_kyoung, order.by = new_go$date)
Changdeokgung <- xts(new_china_name$ca_chang, order.by = new_go$date)
Deoksugung <- xts(new_china_name$ca_duck, order.by = new_go$date)
Changgyeonggung <- xts(new_china_name$ca_changkyung, order.by = new_go$date)


my_ca_size<- cbind(Gyeongbokgung,Changdeokgung,Deoksugung,Changgyeonggung)


#중국  검색 비율
dygraph(my_ca_size) %>% dyRangeSelector()


#크롤링

#크롤링 빈도 워드 클라우드

jdbc_d <- JDBC(driverClass = "oracle.jdbc.OracleDriver",classPath = "C:/DATABASE/dbhomeXE/inventory/Scripts/ext/jlib/ojdbc8.jar")

# db 드라이브 생성 ----


dm_con1 <- dbConnect(jdbc_d,"jdbc:oracle:thin:@192.168.56.1:1521:XE",
                     "hr","hr")


#rs <- dbSendQuery(dm_con1, "CREATE TABLE samjo_word (title varchar2(20))")

#dbClearResult(rs)

my_kyoung=read_excel("naver_ky2.xlsx", sheet = 1)

class(my_kyoung)
dim(my_kyoung)

my_kyoung

title<-as.character(my_kyoung$title)
word<-strsplit(title, " ")

words<-unlist(word)

words


#한번만 사용해야 합니다. 테이블생성하고 데이터를 바로 넣는 작업이라
dbWriteTable(dm_con1, "SAMJO_WORD", words)

#데이터가 정상적으로 들어갔는지 확인 작업입니다.
dbGetQuery(dm_con1, "SELECT * FROM SAMJO_WORD")

#DB에서 할수 있지만 R에서도 전처리가 가능하다는걸 보여주기 위한 코드입니다.
#특수기호들을 지워보겠습니다.
temp1<- dbGetQuery(dm_con1, "SELECT X,REGEXP_REPLACE(X,'[[:punct:]]','') as words
           FROM SAMJO_WORD")

temp1$WORDS

tab1<-table(temp1$WORDS)
tab2<-sort(tab1, decreasing = T)

pal<-brewer.pal(8, "Accent")


wordcloud(names(tab2),
          freq = tab2,
          max.words = 50,
          random.order = F,
          rot.per = .1,
          scale = c(7,0.3),
          colors = pal
          )

#워드 클라우드 심화버전

tab1<-table(words)
tab2<-sort(tab1, decreasing = T)

tab2

#별모양
wordcloud2(data=tab2,color = "random-light", backgroundColor = "grey",fontFamily = '나눔바른고딕',shape='star')

#일반모양
wordcloud2(data=tab2,color = "random-light", backgroundColor = "grey",fontFamily = '나눔바른고딕')

#이미지를 이용한 워드 클라우드
wordcloud2(data=tab2, figPath = "bird.png", size = 1)

#글자형식으로 만들기
letterCloud(data=tab2,word="P",wordSize=1,fontFamily='나눔바른고딕')


#도라에몽의 뭐든지 워드 클라우드 해드려요 펑션~~~~

y4<- my_mpg$year


doraemong<-function(x){
  #크롤링 바로하는 코드 만들어야함
  #이거는 도저히 못하겠다. 파이썬꺼 끌어와야하는데
  
  
  #캐릭터 타입으로 변환
  title<-as.character(x)
  
  #띄어 쓰기 기준으로 자르기
  word<-strsplit(title, " ")
  
  #리스트를 벡터로 만ㄷ르어 줍니다.
  words<-unlist(word)
  
  #특문 문자를 지웁니다.
  cm_words<- gsub("[[:punct:]]", "", words)
  
  #빈도 수를 조사합니다.
  tab1<-table(cm_words)
  #정렬합니다.
  tab2<-sort(tab1, decreasing = T)
  
  pal<-brewer.pal(8, "Accent")
  
  
  tab3<- wordcloud(names(tab2),
            freq = tab2,
            max.words = 50,
            random.order = F,
            rot.per = .1,
            scale = c(7,0.3),
            colors = pal
  )
  return(tab3)
}

doraemong(my_kyoung)

#도라에몽의 뭐든지 워드 클라우드2

doraemong2<-function(x){
  #크롤링 바로하는 코드 만들어야함
  #이거는 도저히 못하겠다. 파이썬꺼 끌어와야하는데
  
  
  #캐릭터 타입으로 변환
  title<-as.character(x)
  
  #띄어 쓰기 기준으로 자르기
  word<-strsplit(title, " ")
  
  #리스트를 벡터로 만ㄷ르어 줍니다.
  words<-unlist(word)
  
  #특문 문자를 지웁니다.
  cm_words<- gsub("[[:punct:]]", "", words)
  
  #빈도 수를 조사합니다.
  tab1<-table(cm_words)
  #정렬합니다.
  tab2<-sort(tab1, decreasing = T)
  
  pal<-brewer.pal(8, "Accent")
  
  tab3<- wordcloud2(data=tab2,color = "random-light", backgroundColor = "grey",fontFamily = '나눔바른고딕',shape='star')
  
  return(tab3)
}

doraemong2(my_kyoung)

#파이썬 코드 가져와서 끌어다 쓰기 #실패!!!!!!!!!!!!!!!
library(reticulate)
install.packages("gridExtra")
py_run_file("doraemong.py")


UsSA_map<- ggChoropleth(data=my_data, # 지도에 표현할 데이터
                        map=USA_map,  # 지도데이터
                        aes(fill=search_rate,
                            map_id=state),
                        title="USA search Rate(Palace)",
                        col="darkblue",
                        interactive=T)
