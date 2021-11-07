# 농산물 가격 데이터와 기상데이터 분석 가능한 상태로 만듦
# 농산물 가격 데이터 시각화 및 연관성 분석

# 상관분석  지역별 돼지고기 가격의 현황

product_data = 'https://url.kr/97jw1e'  # product.csv
code_data = 'http://asq.kr/Z2wVOPH'  # code.csv

library1 = c('plyr','ggplot2','stringr','zoo','corrplot',
             'RcolorBrewer')
unlist(lapply(library1,require,character.only = TRUE))

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
total.pig.mean

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
                             '창원','춘천','포항'),]

#  이름, 지역, 날자별로 평균가격을 구한다.

pig.region.daily.mean =  ddply(day.pig, .(name,region, date), summarise, 
                               mean.price = mean(price))


