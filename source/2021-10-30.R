.libPaths("C:/Rlib")
.libPaths()

install.packages("dplyr")
library(dplyr)
data("iris")
head(iris)
iris %>% head()
filter(iris,iris$Petal.Length >=1.5)

#연습문제 6장 1번
df = iris %>% filter(iris$Petal.Length >=1.5)


#연습문제 6장 2번
colnames(df)
df =  df %>% select(Sepal.Width,Petal.Width,Species) 
head(df)

#연습문제 6장 3번
df = df %>% mutate(diff = Sepal.Width - Petal.Width)
head(df)

#연습문제 6장 4번
df %>% group_by(Species) %>% summarise(sw=mean(Sepal.Width), pw=mean(Petal.Width ))

#연습문제 6장 5번
install.packages("reshape2")
library(reshape2)
iris %>% head()
summary(iris)
# 넓은 형식을 긴 형식으로 변경
vertical_df =  iris %>% melt(id=c('Species'))
head(vertical_df)

# 긴형식을 넓은형식으로 변경
vertical_df %>% dcast(Species ~ variable, mean)

###########################################################################

# 6장의 184 page
read.csv("D:\\data\\Part-II\\data.csv")  # "\n"  "\t"  "\b"    " \"abc\" "
df = read.csv("D:/data/Part-II/data.csv")
head(df)

df %>% dcast(Date ~ Customer_ID,sum)
df %>% dcast(Customer_ID ~ Date,sum)

df = read.csv("D:/data/Part-II/wide.csv")
df %>% head()
colnames(df) = c('Customer_ID','day1','day2','day3','day4','day5','day6','day7','sum')
df %>% head()

# 긴형식(wide) - 좁은형식(narrow)
wide = df[c(1:5),]
wide

wide %>% melt(id ='Customer_ID')


# dcast(데이터, 컬럼시작 ~ 컬럼끝, 집계함수) 
# df %>% dcast(Customer_ID ~ Date,sum)

# melt(데이터, id = 대표컬럼명)
# melt(wide, id ='Customer_ID')

# 적용 iris에
head(iris)
iris %>% melt(id='Species')

iris %>% melt(id='Species') %>% dcast(Species ~ variable )


data("smiths")
head(smiths)

long = smiths %>% melt(id = 1)
long

long %>% dcast(subject~...)


data("iris")
head(iris)
iris %>% melt(id='Species') %>% head()

iris %>% melt(id='Species')  %>% dcast(Species~...) %>% head()


data("airquality")
airquality %>% head()

names(airquality)
colnames(airquality)
toupper(names(airquality))
tolower(names(airquality))

names(airquality) = airquality %>% names() %>% toupper()
airquality

air_melt = airquality %>% melt(id=5:6,na.rm = TRUE)
air_melt %>% head()

names(air_melt) = air_melt %>% names() %>% toupper()
air_melt %>% head()

air_melt %>% acast(DAY ~ MONTH ~VARIABLE)

######## 7장  
# 기본
df =  read.csv("D:\\data\\Part-II\\dataset.csv",header = T)
head(df)
str(df)
summary(df)
View(df)

x =  df$resident
df$price

plot(df$price,panel.first = grid(10, 8),)

df[1:10,1:2]
df[c(2,4:6,3,1)]

# 결측치 제거 결측치가 있는 항목에대해서만
summary(df)
sum(na.omit(df$price))
sum(df$price,na.rm = T)

# 결측치를 포함한 row 전체를삭제
summary(df[!is.na(df$price),])

# 결측치를 대처  0
df$resident = ifelse(is.na(df$resident) ,0 ,df$resident )
summary(df)


df$job = ifelse(is.na(df$job),0,df$job)
summary(df)
str(df)

df$age = ifelse(is.na(df$age),0,df$age)
df$position = ifelse(is.na(df$position),0,df$position)
df$price = ifelse(is.na(df$price),0,df$price)

# 결측치 처리 완료(처리방법 대처 -> 0)
summary(df)


plot(df$gender)
table(df$gender)
hist(df$gender)
barplot(table(df$gender))

# 이상치 처리를 기본기능인 subset   
# dplry  filter

temp1 =  subset(df,df$gender ==1 | df$gender == 2)
hist(temp1$gender)

temp2 =  filter(df, df$gender ==1 | df$gender == 2)
hist(temp2$gender)
# 범주형 데이터 outlier 제거(이상치,극단치)
df =  df %>% filter(df$gender ==1 | df$gender == 2)

# 연속형 데이터 outlier 제거
plot(df$price)

temp3 =  df %>% filter(df$price>=2 & df$price<=8)
length(temp3$price)
nrow(temp3)

stem(temp3$price)
hist(temp3$price)


# boxplot -> outlier을 찾을 목적으로 시각화
summary(df$age)
boxplot(df$age)

boxplot(df$price)
summary(df$price)

boxplot(df$price)$stats

df = df %>% filter(df$price >=2.1 & df$price <= 7.9)
plot(df$price)
boxplot(df$price)

names(df)
df['regident2'] = 0
names(df)
df$regident2[df$resident == 1] = '1.서울특별시'
df$regident2[df$resident == 2] = '2.인천광역시'
df$regident2[df$resident == 3] = '3.대전광역시'
df$regident2[df$resident == 4] = '4.대구광역시'
df$regident2[df$resident == 5] = '5.시구군'
head(df)


# df[df$resident != 0,]
# df %>% subset(df$resident != 0)
df = df %>% filter(df$resident != 0)
head(df)

df$job2[df$job == 1] = '공무원'
df$job2[df$job == 2] = '회사원'
df$job2[df$job == 3] = '개인사업'
head(df)

df = df %>% filter(!is.na(df$job2))
summary(df)  
str(df)  

summary(df$age)
# 27 54
# 0 ~ 27 : 청년
# 27 ~ 54 : 중년
# 54 ~  장년

df$age2[df$age<=27] = '청년층'
df$age2[df$age>27 & df$age<54] = '중년층'
df$age2[df$age>=54] = '장년층'
head(df)  

df$gender2[df$gender == 1] = '남자'
df$gender2[df$gender == 2] = '여자'

table(df$position)
df$position2[df$position == 1] = '1급'
df$position2[df$position == 2] = '2급'
df$position2[df$position == 3] = '3급'
df$position2[df$position == 4] = '4급'
df$position2[df$position == 5] = '5급'

# 역코딩
table(df$survey)
# 1  ->5
# 5  ->1
df$csurvey = 6-df$survey
head(df)  
  

# 전처리... NA 제거하고  이상치 제거
# 가독성을 위해서 코딩변경 컬럼추가(한글로)
# 척도변경 : 연속형 데이터인 나이를  범주형으로 컬럼추가(청년 중년 장년)
# 역코딩 : 만족도 점수가 높으면 만족도가 높은것으로 컬럼 추가

# 컬럼(Feature 피처)  / 인사이트 도출을 위해 컬럼간의 관계를 파악
head(df)

df = df %>% filter(!is.na(df$position2))

# 범주형 vs 범주형
regident_gender = table(df$regident2, df$gender2)
barplot(regident_gender,beside = T,col = rainbow(5),
        legend = row.names(regident_gender),
        horiz = T
        )
 

head(df)

# 밀도 그래프를 쉽게 그리기위한 패키지 다운로드
install.packages("lattice")
library(lattice)
densityplot(~age, data = df, groups = regident2,
            plot.point = T, auto.key = T)


# 범주형 vs 범주형
# 교차분석  talbe( 범주형1, 범주형2)
# bar  his

# 범주형 vs 연속형(연속형 vs 범주형)
# 밀도함수  densityplot( )
# ~컬럼 --> 연속형데이터
# groups = 범주형컬럼


# 연속형 vs 범주형 vs 범주형
densityplot(~price|factor(gender2)
            ,data = df
            ,groups = position2
            ,plot.point = T, auto.key = T)



# 연속형 vs 연속형 vs 범주형
# 성별에따른 나이와 구매비용

# xyplot  점으로 표시
xyplot(price~age|factor(gender2), 
       data = df)


# 응용

head(iris)
str(iris)
table(iris$Petal.Width )

# 연속형 vs 범주형
densityplot(~Sepal.Length, 
            data = iris, groups = Species,
            ,plot.point = T, auto.key = T)


# 연속형 vs 연속형
xyplot(Sepal.Length ~ Sepal.Width,
       data = iris)

#연속형 vs 연속형 vs 범주형
xyplot(Sepal.Length ~ Sepal.Width|factor(Species),
       data = iris)


user_data = read.csv("D:\\data\\Part-II\\user_data.csv",
                     header = T)
head(user_data)  
table(user_data$house_type)

#새로운 컬럼을 만든다..
user_data$house_type2 =  ifelse(user_data$house_type ==1 | user_data$house_type ==1, 0,1)
head(user_data)


pay_data = read.csv("D:\\data\\Part-II\\pay_data.csv",
                     header = T)
head(pay_data,10)
table(pay_data$user_id)

table(pay_data$product_type)

# 상품유형에 따른 구매금액과 합계  dcast
product_price =  dcast(pay_data , user_id ~product_type ,sum, na.rm=T)
table(product_price$user_id)
# 219page