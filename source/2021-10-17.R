# Data Frame에서 조건에 맞는 데이터만 추출해 보자
# emp.csv 읽어오기
# 데이터를 읽는 방법
# 1. 경로맞추기
  # 1. 현재 작업디렉터리 셋팅
  # 2. 불러올 파일의 전체 경로 데이터 불러오기

getwd()
setwd("C:/Users/TJ/Downloads/-R-20211016-main/-R-20211016-main/data/Part-I")
df<-read.csv("emp.csv",header = T)
test = str_extract(df$name,'신.*?')
!is.na(test)
df[!is.na(test),]
#
grep('신.*?',df$name,perl=T)
df[grep('신.*?',df$name,perl=T),]

str(df)
df
# pay가 200 이상인 데이터를 추출해 보자
df['pay'] >= 200
df[df['pay'] >= 200,]

# vector
v <- c(1:3)
v2<-c(T,T,F)
# 짝수만 출력
v;v2
v[v2]

# data frame
df<-data.frame(x=c(1:3),y=c(11:13))
df > 2
df[df['x'] > 2]

# data Frame에서 조건에 맞는 데이터 추출하는 방법
# 1 subset
subset(df, df['pay']>200)
subset(df, pay>200) # -->*****

# 2 true false 를 이용
df[df['pay']>200,]

# 다중조건 
subset(df, x>=3)

# no 103번이상이고 pay가 300이상인 데이터
# no >= 103 & pay >= 300
subset(df, no>=103 & pay >= 300)
# no 103번이상이거나 pay가 300이상인 데이터
subset(df, no>=103 | pay >= 300)
# pay가평균 이상인 항목들
pay.mean = apply(df['pay'], 2, mean)
pay.mean = mean(df$pay)
pay.mean

subset(df, pay>=pay.mean)

# 두개의 데이터 프레임을 합치기( 두개의 dataset)
height<-data.frame(id=c(1,2),h=c(180,175))
weigth<-data.frame(id=c(1,2),w=c(80,75))
height;weigth
merge(height,weigth,by = 'id')

# list 자료구조
v1<- c(1:5)
v1[1]
l1<-list(1,2,3,4,5)
l1[[1]]

v2<-unlist(l1)

l2<-list(name=c('홍길동','이순신'), age = c(100,150))
l2[['name']]
l2$name
df$no
class(l2)
class(df)
l2$name[2]
length(l2)

a<-list(c(1:5))
b<-list(6:10)
lapply(c(a,b), max)
sapply(c(a,b), max)

# list 안에 list가 있는... 다차원 리스트
multi_list =  list(a=list(1,2,3),
      b=list(10,20,30),
      c=list(100,200,300)
)
multi_list$c[[2]]

v1<-c(1:5)
v2<-c(11:15)
cbind(v1,v2)
rbind(v1,v2)
cbind(a=1,b=1:5)
rbind(a=1,b=1:5)
#cbind(x=0,y=rbind(a=1,b=1:5))
class(multi_list)
multi_list
multi_list$a
do.call(cbind,multi_list)
do.call(rbind,multi_list)

#문자열 처리
.libPaths()
.libPaths("D:/lib")
install.packages("stringr")
library(stringr)
str_extract("12a2b45c홍길동",'[a-z]')
str_extract_all("12a2bb45ccc홍길동",'[a-z]{2,5}')
str_extract_all("12a2bb45ccc홍길동",'[가-힣]{3}')

str_extract("홍길동",'^홍.*?')

  
# 파일 읽기(csv) - 결측치 처리 하기
df<- read.csv("student4.txt",header = T)
df
df<- read.csv("student4.txt",header = T,na.strings = '-')
df  
# 기술통계량
summary(df)
titanic <- read.csv("https://url.kr/lg81du", header = T)
str(titanic)
# 빈도수를 구하는 함수
table(titanic$class)
head(titanic)
tail(titanic)

# 10 ~ 15 라인만 출력
titanic[10:15,]
cbind(x=c(1:5),y=c(10:14))
barplot(cbind(x=c(1:5),y=c(10:14)))

data<-table(titanic$sex)
barplot(data)


man<-c(100,150,200)
women<-c(50,80,78)
cbind(man,women)
barplot(cbind(man,women))

# 성별대비 클래스
table(titanic$sex,titanic$class)
barplot(table(titanic$sex,titanic$class))
table(titanic$class,titanic$sex)
barplot(table(titanic$class,titanic$sex))

#성별에 따른 생존율을 구해보세요
colnames(titanic)[4]<-'gender'
data<-table(titanic$survived, titanic$gender)
data[1]/sum(data[1:2])
data[2]/sum(data[1:2])
data[3]/sum(data[3:4])
data[4]/sum(data[3:4])


#객실 등급에 따른 생존여부 교차분석
# barplot으로 시각화 하기
data<-titanic[,c(5,2)]
head(data)
data.talbe<-table(data)
barplot(data.talbe)

table(titanic$class,titanic$survived)
