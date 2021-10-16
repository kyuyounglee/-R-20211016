# vector
  # 생성
v1<-c(1,2,3,4,5)
c(1:5)
v2<-seq(1,10,2)
rep(1:5,2)
rep(1:5,each=2)
  # 사용
v1[3] # 읽기
v1[3] <- 100  #쓰기
v1[2:4]
v1[c(1,3,5)]
v1[-c(1,3,5)]
# matric
  # 생성
matrix(c(1:10))
matrix(c(1:10),nrow = 2)
m<-matrix(c(1:10),nrow = 2,byrow = T)
m2<-cbind(v1,v2)
m3<-rbind(v1,v2)
  # 사용
m[1,3]
m[1,4]
m[2,3]
m[2,4]
m[,3:4]
m[,c(3,5)]
# array
  # 생성
a<-array(1:8,c(2,2,2))
  # 사용
a[,,1]
a[2,2,1]
a[2,2,2]
a[2,2,]
a[,1,]
# data.Frame
# 데이터 타입이 서로 다르게 설계
  # 생성
data.frame(data1 = c(1:3),data2 = c(11:13))
id<-c(1,2,3)
name<-c('a','b','c')
data.frame(id,name)
data.frame(num = id,alpa = name)
data.frame(m)
data.frame(m2)
df<-data.frame(m3)
colnames(df)<-c('a','b','c','e','d')
class(df)

# 외부의 파일을 읽어서 data frame 형태로 만들기
filePath = 'C:/Users/TJ/Downloads/-R-20211016-main/-R-20211016-main/data/Part-I'
# 소스의 파일 경로를 변경
setwd(filePath)
#현재 소스파일의 경로
getwd()
df<- read.table("emp.txt",header = 1)
df[2,'급여']
df[2,3]

# csv 파일 읽기
df<-read.csv("emp.csv",header = T)
df
# 헤더정보를 변경
colnames(df)
colnames(df)<-c("사번","이름","급여")
df

#컬럼단위로 데이터를 읽을때
df['사번']
df[1]
df$사번

class(df['사번'])
class(df[1])
class(df$사번)

# 1. 파일에서 데이터를 읽는다
# 2. 데이터 구조는 data.frame 형태
# str 함수를 이용해서 데이터 구조를 파악
str(df)

seq(2, 12, 2.5)

df<-data.frame(x=c(1:5), y=seq(2,12,2.5))
str(df)


df<-read.csv("emp.csv",header = T)
df
# 헤더정보를 변경
colnames(df)
colnames(df)<-c("사번","이름","급여")
df
# row 의 수
nrow(df)
# col 의 수
ncol(df)
colnames(df)
names(df)

#이순신 유관순 강감찬 의 이름만 출력
df[c(2,3,4),2]

mean(df$급여)
mean(df[,3])
mean(df[,'급여'])
#mean(df['급여'])
apply(df['급여'],2,mean)

df[,c(1,3)]
