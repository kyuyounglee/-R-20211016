getwd()
setwd("D:/data/Part-III")
data = read.csv("descriptive.csv", header = T)
head(data)
dim(data)
#기술통계량
str(data)
summary(data)

summary(data$gender)
table(data$gender)


# 1. boolean 타입  조건문을 쓰면 조건에해당하는 row의 true false 
# 2. index 조건을 사용하는데 그 조건은 which에 넣으면.. 해당 인덱스를 반환

data = data[data$gender == 1 | data$gender == 2, ]
table(data$gender)

barplot(table(data$gender), ylim = c(0,250),col = rainbow(2))

prop.table(table(data$gender))

data$level
summary(data$level)

plot(data$cost)

data = data[data$cost > 0,]

plot(data$cost[data$cost<50])

x =  data$cost[data$cost<50]
sort(x, decreasing=T)

summary(x)[2]
summary(x)[3]
summary(x)[4]

quantile(x, 1/4,na.rm = T)
quantile(x, 2/4,na.rm = T)
quantile(x, 3/4,na.rm = T)
quantile(x, 4/4,na.rm = T)

length(x)
max(table(x))
summary(x)
# na제거
summary(data)

data = subset(data, !is.na(data$resident))
data = subset(data, !is.na(data$gender))
data = subset(data, !is.na(data$age))
data = subset(data, !is.na(data$level))
data = subset(data, !is.na(data$cost))
data = subset(data, !is.na(data$type))
data = subset(data, !is.na(data$survey))
data = subset(data, !is.na(data$pass))
summary(data)
str(data)

max(table(data$cost))

x =  table(data$cost)
max(x)
x.m =  rbind(x)
str(x.m)

which(x.m[1,] == 11)


install.packages("moments")
library(moments)

#왜도
x = subset(data$cost, data$cost < 10)

skewness(x)
kurtosis(x)

hist(x)
hist(x,freq = F)
lines(density(x),col='blue')
x1 = seq(0,8,0.1)
curve(dnorm(x1, mean(x),sd(x)),col='red',add = T)


# regisdent 1 특별시 2~4 광역시  5 시구군
table(data$resident)
head(data)

data$resident2[data$resident == 1] = '특별시'
data$resident2[data$resident >=2 & data$resident <=4] = '광역시'
data$resident2[data$resident == 5] = '시구군'

data$gender2[data$gender == 1] = '남자'
data$gender2[data$gender == 2] = '여자'

data$age2[data$age <= 45] = '중년층'
data$age2[data$age > 45 & data$age < 60] = '장년층'
data$age2[data$age => 60] = '노년층'

head(data)

data$level2[data$level == 1] = '고졸'
data$level2[data$level == 2] = '대졸'
data$level2[data$level == 3] = '대학원졸'

data$pass2[data$pass == 1] = '합격'
data$pass2[data$pass == 2] = '실패'

head(data)

prop.table(table(data$resident2))

library(MASS)
data("Animals")
head(Animals)

summary( Animals$brain )
mean(Animals$brain)
median(Animals$brain)
sd(Animals$brain)
var(Animals$brain)
min(Animals$brain)
max(Animals$brain)

# 교차분석 카이제곱 검정
data = read.csv('cleanDescriptive.csv',header=T)
head(data)

data$level2
data$pass2

result = data.frame(Level = data$level2, Pass =data$pass2 )
head(result)
# 기존방법(base 패키지)
table(result)
prop.table(table(result))

# 패키지를 이용한 방법
install.packages("gmodels")
library(gmodels)   # CrossTable

install.packages("ggplot2")
library(ggplot2)

diamonds$color[1:5]
diamonds$cut[1:5]

CrossTable(x = diamonds$color, y = diamonds$cut)

nrow( subset(diamonds, diamonds$cut == 'Fair' & diamonds$color == 'D') )
0.024 +0.098 +0.223 +0.237 +0.418 +0.126 


x = data$level2
y = data$pass2

CrossTable(x,y,chisq = TRUE)

# 14 요인분석과 상관관계 분석

s1 = c(sample(1:6,6),sample(1:6,4))
s2 = c(sample(1:6,6),sample(1:6,4))
s3 = c(sample(1:6,6),sample(1:6,4))
s4 = c(sample(1:6,6),sample(1:6,4))
s5 = c(sample(1:6,6),sample(1:6,4))
s6 = c(sample(1:6,6),sample(1:6,4))
subject = data.frame(s1,s2,s3,s4,s5,s6)
# 주성분 분석.. 결과중에 Proportion of Variance 비율이 대략 80% 이상인 컬럼을
# 주성분으로 본다다
pc = prcomp(subject)
summary(pc)
plot(pc)

# 2단계 고유값으로 요인수 분석
en = eigen( cor(subject) )
str(en)
plot(en$values, type = 'o')
