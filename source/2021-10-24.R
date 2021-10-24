name = c('유관순','홍길동','이순신','신사임당')
gender = c('F','M','M','F')
price = c(50,65,45,75)

# cbind
df = cbind(name,gender)
df = cbind(df, price)
str(df)
# data.frame
df = data.frame(name,gender,price)
str(df)

df['result_number'] =  ifelse(df$price >= 75, 1, 0)
df

table(df['result'])
hist(df$result_number)

# 연습문제 2
EMP = c('2014홍길동220','2002이순신300','2010유관순260')

.libPaths('c:/Rlib')
.libPaths()
install.packages("stringr")
library(stringr)
name = str_extract(EMP,'[가-힣]{3}')
year = str_extract(EMP,'[0-9]{4}')

# str_extract(EMP,'[0-9]{3}') 앞에서부터 시작해서 숫자로 되어 있는
# 값들중에 3자리

# str_extract(EMP,'[0-9]{3}$') 뒤에부터 시작해서 숫자로 되어 있는
# 값들중에 3자리
pay = as.numeric( str_extract(EMP,'[0-9]{3}$') )
df= data.frame(year,name,pay)
df
str(df)
summary(df)
mean(df$pay)
df[df$pay >= mean(df$pay),][,c('name','pay')]

# review
EMP = c('2021남궁옥분500','2014이산520','2014홍길동220','2002이순신300','2010유관순260')
# 1 앞에서 숫자 4자리 분류
year = str_extract(EMP,'[0-9]{4}')
pay = as.numeric( str_extract(EMP,'[0-9]{3}$') )
name = str_extract(EMP,'[가-힣]+')

df = data.frame(year,name,pay)
index = df$pay >= mean(df$pay)
df = df[index,]
df = df[,c('name','pay')]

# 출력형식 이름 => 급여
for (row in 1:length(df)) {
  cat(df[row,'name'],'=>',df[row,'pay'],'\n')
}

# 연습문제 4
.libPaths()
install.packages("RSADBE") # 압축파일
library(RSADBE)           # 풀기
data("Bug_Metrics_Software")  # read.csv 와 같이 제공하고있는 dataset 가져오기
Bug_Metrics_Software

str(Bug_Metrics_Software)  # 데이터 형식 확인  --> list구조

# 리스트 구조의 데이터를 확인
Bug_Metrics_Software[,,'Before']  #Bug_Metrics_Software[,,1]
Bug_Metrics_Software[,,'After']   #Bug_Metrics_Software[,,2]

# 리스트에 있는 자료구조를 변수에 저장
before = Bug_Metrics_Software[,,1]
after = Bug_Metrics_Software[,,2]
# 데이터 형식 확인 -- table
str(before)
head(before)
row.names(before)
colnames(before)
before['Lucene',]

rowSums(before)
colMeans(before)
summary(before)

# data frame >   list  > talbe

# 페키지 다운받는 방법
# install.packages()
# library()   -- 함수를 사용할수 있음..(특정 명령어)
# data() --> 특정 dataset을 사용


data("iris")
str(iris)
par(mfrow = c(1,2))
# 꽃받침의 너비를 시각화
hist(iris$Sepal.Width, col = 'green',
     ylim = c(0,40), xlim = c(2.0,4.5)
     )
hist(iris$Sepal.Width, col = 'green',
     xlim = c(2.0,4.5),
     freq = F
)
lines(density(iris$Sepal.Width),col='red')
table(iris$Sepal.Width)

par(mfrow = c(1,1))
hist(iris$Sepal.Width, col = 'green',
     xlim = c(2.0,4.5),
     freq = F
)
lines(density(iris$Sepal.Width),col='red')
x = seq(2.0,4.5,0.1)
curve(dnorm(x,mean = mean(iris$Sepal.Width)
            ,sd = sd(iris$Sepal.Width)),
      color = 'blue',add=T
     )
table(iris$Sepal.Width)
plot(table(iris$Sepal.Width))
# x 와 y의 값을 지정해서 분포의 형태를 볼때 
plot(iris$Sepal.Length, iris$Sepal.Width)
par(new=T)
plot(1:100,type='l',col='red',axes=F,ann=F,lwd=3)
text(70,80,"대각선",col="blue")


# 중첩자료 시각화  잘못된 데이터가 있는지 파악
# 하는게 쉬움...

x = c(1,2,3,sample(1:10,3))
y = rep(2,6)
hist(x)
plot(x,y)

#교차테이블로 df
df = as.data.frame(table(x,y))
str(df)
df

plot(x,y
     ,pch =21,bg = 'green'
     ,cex = 2*df$Freq,
     col='blue')

install.packages("UsingR")
library(UsingR)
data("Galton")
str(Galton)
table(Galton$child, Galton$parent)
data("galton")
galton
df = as.data.frame(table(galton$child, galton$parent))

# plot을 이용해서 산점도로 시각화를 할때
#  중복데이터들은 하나의 점으로만 표현하기에
#  반드시 교차분석을 통해 중복여부를 확인하고
# 중복이 있으면 시각화에 반영한다.
# cex = 적당한값*df$freq

x =  as.numeric( galton$child )
y =  as.numeric( galton$parent )
plot(x,y,pch=21,cex = 0.2*df$Freq
     ,bg='green',col='blue')


str(iris)

iris$Sepal.Length
iris$Sepal.Width
plot(iris$Sepal.Length,iris$Petal.Length)
plot(iris)

# 6장 데이터 조작
install.packages(c("dplyr",'hflights'))
library(dplyr)
library(hflights)

head(hflights)
hflights %>% head()

# iris 데이터중에서 petal.length >=5.0 데이터중에서 6개만
head(subset(iris, iris$Petal.Length >= 5.0))

# subset에 iris데이터보내고, 받아서 조건을 걸었고
# head를 이용해서 출력

iris %>% subset(iris$Petal.Length >= 5.0) %>% head()

# filter  조건에 맞는 데이터를 추출   subset 유사
head(filter(hflights,Month == 1 & DayofMonth == 2 ))
hflights %>% fileter(Month == 1 & DayofMonth == 2) %>% head()

# hflights  1 월과 2월의 데이터중에서 DepTime 시간이 1500보다 큰
# 데이터의 갯수는 ?

hflights %>% filter((Month == 1 | Month == 2) & DepTime >= 1500) %>% length()
length(filter(hflights,(Month == 1 | Month == 2) & DepTime >= 1500))

# Month DepTime 컬럼정보만 추출
hflights[,c('Month','DepTime')]
select(hflights, Month, DepTime)

hflights %>% 
  select(Month, DepTime) %>% 
  filter((Month == 1 | Month == 2) & DepTime >= 1500) %>% 
  tail()

# 컬럼을 추가
mutate(hflights, gain = ArrDelay - DepDelay)

s = group_by(iris, Species)
head(s)


name = c('a','a','b','c','b','c')
data = sample(1:100,6)
freq = c(1:6)

df = data.frame(name,data,freq)
df
summarise(group_by(df,name), sum(data) )

# iris
iris %>% group_by(Species) %>% summarise(mean(Sepal.Length),mean(Sepal.Width ))

# 마스터(기준) 데이터셋을 선택 또는 판단하고
# 마스터라는 의미는 데이터가 많거나 추출하려는 속성의 특징을 
# 가장 많이 포함하고 있는 데이터 셋이다

#  inner 또는 left 를 사용하는 left 에서  왼쪽에 오는 거는 마스터이다

install.packages("reshape2")
library(reshape2)

setwd("d:/data")
getwd()

df = read.csv("Part-II/data.csv")
df %>%  head()
str(df)
dcast(df,Customer_ID~Date,sum)


# 날자별 구매수량 
df1 =  df %>% group_by(Date) %>% 
  summarise(sum(Buy)) %>%
  as.data.frame()
  
barplot(v)  
v = c(1:10)
matrix(1:10,nrow = 2)
barplot(matrix(1:10,nrow = 2))

df1 = as.matrix(df1)

barplot(df1)

barplot(df1[,'sum(Buy)'])
v = df1[,'sum(Buy)']

names(v) = df1[,'Date']
v
barplot(v,col = rainbow(20))

getwd()
bmi =  read.csv('Part-I/bmi.csv')
str(bmi)
plot(bmi)

bmi %>% head()

barplot(table(bmi[,'label']))
t =  table(bmi[,'label'],bmi[,'height'])
barplot(t,beside = T,col = rainbow(3))
head(t)
plot(t['fat',], type = 'o', col='red')
par(new=T)
plot(t['normal',], type = 'o', col='green')
par(new=T)
plot(t['thin',], type = 'o', col='yellow')

plot(t['fat',],type = 'o')

plot(cars)

# 시각화, 데이터가져와서 가공
