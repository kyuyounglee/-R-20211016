# 농산물 가격 데이터와 기상데이터 분석 가능한 상태로 만듦
# 농산물 가격 데이터 시각화 및 연관성 분석

# 상관분석  지역별 돼지고기 가격의 현황
.libPaths('C:/Rlib')


product_data = 'https://url.kr/97jw1e'  # product.csv
code_data = 'http://asq.kr/Z2wVOPH'  # code.csv

library1 = c('plyr','ggplot2','stringr')
unlist(lapply(library1,require,character.only = TRUE))

install.packages("zoo")
library(zoo)
install.packages("corrplot")
library(corrplot)
install.packages("RColorBrewer")
library(RColorBrewer)

#패키지 명                설명
#plyr         데이터 핸들링을 하기 위한 라이브러리
#ggplot2      시각화 기능 라이브러리
#stringr      문자열 핸들링을 하기 위한 라이브러리
#zoo          문자형 데이터를 데이트 형식으로 변홖하기 위한               #라이브러리
#corrplot     상관분석을 위한 라이브러리
#RcolorBrewer 색상 처리 기능 라이브러리

product = read.csv(product_data,header=T)
code = read.csv(code_data,header = T)

colnames(product) = c('date','category','item','region','mart','price')

# code 품목코드만 추출
# subset
category =  subset(code, code$구분코드설명 == '품목코드')
colnames(category) = c('code','exp','item','name')

# 분석대상이 돼지고기  produt에서 돼지고기 데이터만 추출
pig_item = category[category$name == '돼지고기',]['item']
#product[product$item == pig_item[1,1],]
#product[which(product$item == pig_item[1,1]),]
total.pig = product[which(product$item == pig_item[1,1]),]
head(total.pig)

# 지역별로 코드데이터를 가져온다
region =  subset(code, code$구분코드설명 == '지역코드')
head(region)
colnames(region) = c('code','exp','region','name')

# 지역코드와 전체돼지고지 가걱에대 한 데이터를 지역변수를 기준으로 하나의 
#데이터로 만든다

day.pig =  merge(total.pig, region, by='region')
date1 =  ddply(day.pig, .(date), summarise, name = name, region=region,price=price)
# 리스트로 재 편성성
date2 = ddply(date1, .(date,name),summarise, mean.price = mean(price))

total.pig.mean =  dlply(date2, .(name))
str(total.pig.mean)

# day.pig 일별로 정렬한후 , 지역별로 돼지고기의 평균가격을 구한다
# 위에서 구한 데이터를 지역별로 나누어서 저장한다

# ddply() 데이터프레임 을 분리해서 함수를 적용시킨후 다시 DF로 반환

# 예제
x = data.frame( Date = c('2021-01-01','2021-01-02','2021-01-02','2021-01-01',
                     '2021-01-02' ),
            Category = factor(c('First','First','Second','First','Second')),
            Frequency = c(10,15,5,2,14)
            )
ddply(x, .(Date,Category), summarise, Sum_F = sum(Frequency))
#####################################################################

# 각 지역별 데이터의 크기를 확인
for(i in 1: length(names(total.pig.mean))){
  cat(names(total.pig.mean)[i],"의 데이터는 : ",nrow(total.pig.mean[[i]]),'\n')
}

# 데이터전처리
# 데이터의 길이가 맞지 않는 지역을 제거하여 새로운 데이터셋을 생성
# 순천, 안동, 용인,의정부, 창원,춘천,포항
day.pig = day.pig[!day.pig$name %in% c('순천', '안동', '용인','의정부',
                             '창원','춘천','포항'), ]

#  이름, 지역, 날자별로 평균가격을 구한다.
pig.region.daily.mean =  ddply(day.pig, .(name,region, date), summarise, 
                               mean.price = mean(price))

pig.region.daily.mean$month =  str_sub(pig.region.daily.mean$date,1,7)
# 1. 지역별 월별 돼지고기 평균 값
# dplyr
pig.region.monthly.mean  = pig.region.daily.mean %>% 
            ddply(.(name,region, month), summarise, mean.price = mean(mean.price))


# 상관분석 및 데이터 시각화
#월별 돼지고기 가격변화를 시각화
pig.region.monthly.mean$month =  as.Date( as.yearmon( pig.region.monthly.mean$month,'%Y-%m') )

pig.region.monthly.mean %>% 
  ggplot(aes(x=month,y=mean.price, col = name,group=name)) + geom_line() +
  theme_bw() + geom_point(size=6,shape=20,alpha=0.5) + ylab("돼지고기 가격") +
  xlab("년 월")


# 연간 돼지고기 평균가격 시각화
pig.region.daily.mean$year =  str_sub(pig.region.daily.mean$date,1,4)

pig.region.yearly.mean  = pig.region.daily.mean %>% 
  ddply(.(name,region, year), summarise, mean.price = mean(mean.price))

pig.region.yearly.mean %>% 
  ggplot(aes(x=year,y=mean.price, col = name,group=name)) + geom_line() +
  theme_bw() + geom_point(size=6,shape=20,alpha=0.5) + ylab("돼지고기 가격") +
  xlab("년")


# 2011 ~ 2013년 까지의 돼지고기 연평균 가격의 변화를 막대 그래프로 시각화
pig.region.yearly.mean %>% 
  ggplot(aes(x=name,y=mean.price,fill = factor(year))) + theme_bw() +
  geom_bar(stat = "identity", position ="dodge", col = 'white')+
  ylab("돼지고기 가격") + xlab("")

# 2011 ~ 2013 가격분포를 상자그림으로 시각화 일별
pig.region.monthly.mean %>% ggplot(aes(x=name, y=mean.price, fill=name)) +
  theme_bw() + geom_boxplot() + xlab("") + ylab("돼지고기 가격")


pig.region.monthly.mean$year = pig.region.monthly.mean$month %>%
  str_sub(1,4)
# 2011 ~ 2013 일평균 가격분포를 년도별로 세분화
pig.region.monthly.mean %>% ggplot(aes(x=name, y=mean.price, fill=name)) +
  theme_bw() + geom_boxplot() + xlab("") + ylab("돼지고기 가격") +
  facet_wrap(~year, scales = 'fixed')


# 상관관계 시각화
temp = dlply(pig.region.daily.mean, .(name), summarise, mean.price)
str(temp)
pig.region = data.frame(서울 = unlist(temp$서울),
           부산 = unlist(temp$부산),
           대구 = unlist(temp$대구),
           인천 = unlist(temp$인천),
           광주 = unlist(temp$광주),
           대전 = unlist(temp$대전),
           울산 = unlist(temp$울산),
           수원 = unlist(temp$수원),
           청주 = unlist(temp$청주),
           제주 = unlist(temp$제주),
           전주 = unlist(temp$전주)
           )
cor_pig = cor(pig.region)
theme(axis.text.x=element_text(size=9))
corrplot(cor_pig, method="ellipse", type="upper", order="hclust", 
         addCoef.col ="white", tl.srt=0, tl.col="black", tl.cex=0.7, 
         col=brewer.pal(n=8, name="PuOr"))


# 광주 대구 서울의 세 지역이 서로 상관계가 높은것을 볼수 있다
# 세지역의 시계열 데이터를 뽑아서 시각화
str(pig.region.monthly.mean)
pig.region.monthly.mean$month = 
  pig.region.monthly.mean$month %>% as.yearmon("%Y-%m") %>% as.Date()

# 광주 대구 서울의 데이터만 뽑는다.
# 공백을 제거한다
pig.region.monthly.mean$name = str_trim(pig.region.monthly.mean$name)
temp =  pig.region.monthly.mean[pig.region.monthly.mean$name  %in% c('광주','대구','서울'), ]

temp %>% ggplot(aes(x=month,y=mean.price,col=name,groupname)) +
  geom_line() + theme_classic() + geom_point(size=6,shape=20,alpha=0.5) + 
  ylab("돼지고기 가격") + xlab("")

write.csv(pig.region,'pig.region.csv', fileEncoding = 'UTF-8')
write.csv(pig.region.monthly.mean,'pig.region.monthly.mean.csv',
          fileEncoding = 'UTF-8')
getwd()


