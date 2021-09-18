#올리브영에 가장 많이 입점한 브랜드와 카테고리 간의 상관관계 파악하기

## 카테고리별로 제품들 가져오기
### 카테고리 1. 스킨케어 2.메이크업, 3.바디케어, 4.헤어케어, 5.향수 디퓨저, 6.미용소품, 7.남성 용품 크롤링

#'1. 스킨케어' 제품들 크롤링하기, 파일 만들기
#brand<-NULL
#goods<-NULL
#price<-NULL
#crwal_func2<-function(x,url){
#for (i in 1:x){
  #url2<-paste(url,i,sep="")
  #htxt<-read_html(url2)
  #brand<<-append(brand,html_nodes(htxt,"a.goodsList span.tx_brand")%>%html_text())
  #goods<<-append(goods,html_nodes(htxt,"p.tx_name")%>%html_text())
  #price<<-append(price,html_nodes(htxt,"span.tx_cur span.tx_num")%>%html_text())
  # }
  #}
  #crwal_func2(51,"https://www.oliveyoung.co.kr/store/display/getMCategoryList.do?dispCatNo=100000100010001&fltDispCatNo=&prdSort=03&pageIdx=")
  #crwal_func2(24,"https://www.oliveyoung.co.kr/store/display/getMCategoryList.do?dispCatNo=100000100010002&fltDispCatNo=&prdSort=03&pageIdx=")
  #crwal_func2(25,"https://www.oliveyoung.co.kr/store/display/getMCategoryList.do?dispCatNo=100000100010006&fltDispCatNo=&prdSort=03&pageIdx=")
  #crwal_func2(11,"https://www.oliveyoung.co.kr/store/display/getMCategoryList.do?dispCatNo=100000100010007&fltDispCatNo=&prdSort=03&pageIdx=")
  #crwal_func2(14,"https://www.oliveyoung.co.kr/store/display/getMCategoryList.do?dispCatNo=100000100010004&fltDispCatNo=&prdSort=03&pageIdx=")
  
  #skin_care_goods<-data.frame(brand,goods,price)
  #head(skin_care_goods)
  #write.csv(skin_care_goods,"skin_care_goods.csv")
  
#분석 도중 데이터가 없데이트 돼서 일단 주석으로 바꾸고 쓴 파일을 읽어오는 방식으로 진행

skin_care_goods<-read.csv("skin_care_goods.csv")
skin_care_goods<-skin_care_goods[,c(2,3,4)]
head(skin_care_goods)

#같은 크롤링 방법으로 2.메이크업, 3.바디케어, 4.헤어케어, 5.향수 디퓨저, 6.미용소품, 7.남성 용품 크롤링
makeup_goods<-read.csv("makeup_goods.csv")
makeup_goods<-makeup_goods[,c(2,3,4)]
head(makeup_goods)

body_care_goods<-read.csv("body_care_goods.csv")
body_care_goods<-body_care_goods[,c(2,3,4)]

hair_care_goods<-read.csv("hair_care_goods.csv")
hair_care_goods<-hair_care_goods[,c(2,3,4)]

perfume_goods<-read.csv("perfume_goods.csv")
perfume_goods<-perfume_goods[,c(2,3,4)]

beauty_item<-read.csv("beauty_item.csv")
beauty_item<-beauty_item[,c(2,3,4)]

for_man_goods<-read.csv("for_man_goods.csv")
for_man_goods<-for_man_goods[,c(2,3,4)]

#brand 빈도수 알아보기
#스킨케어

install.packages("dplyr")
library(dplyr)

#brand 빈도수 알아보기
skin_care<-skin_care_goods%>%group_by(brand)%>%summarise(freq=n())
#빈도수를 기준으로 상위 10개의 브랜드 10개 뽑아보기
top_skin_care_brand<-skin_care%>%arrange(desc(freq))%>%head(.,10)

#메이크업
maekup<-makeup_goods%>%group_by(brand)%>%summarise(freq=n())
top_maekup_brand<-maekup%>%arrange(desc(freq))%>%head(.,10)

#바디케어
body_care<-body_care_goods%>%group_by(brand)%>%summarise(freq=n())
top_body_care_brand<-body_care%>%arrange(desc(freq))%>%head(.,10)

#헤어케어
hair_care<-hair_care_goods%>%group_by(brand)%>%summarise(freq=n())
top_hair_care_brand<-hair_care%>%arrange(desc(freq))%>%head(.,10)

#향수
perfume<-perfume_goods%>%group_by(brand)%>%summarise(freq=n())
top_perfume_brand<-perfume%>%arrange(desc(freq))%>%head(.,10)

#미용소품
beauty<-beauty_item%>%group_by(brand)%>%summarise(freq=n())
top_beauty_item_brand<-beauty%>%arrange(desc(freq))%>%head(.,10)

#남자
man<-for_man_goods%>%group_by(brand)%>%summarise(freq=n())
top_for_man_brand<-man%>%arrange(desc(freq))%>%head(.,10)

install.packages("tidyverse")
library(tidyverse)

#카테고리를 알려줄 새로운 변수 추가하기
skin_care_goods<-skin_care_goods%>%mutate(category="스킨케어")
makeup_goods<-makeup_goods%>%mutate(category="메이크업")
body_care_goods<-body_care_goods%>%mutate(category="바디케어")
hair_care_goods<-hair_care_goods%>%mutate(category="헤어케어")
perfume_goods<-perfume_goods%>%mutate(category="향수/디퓨저")
beauty_item<-beauty_item%>%mutate(category="미용소품")
for_man_goods<-for_man_goods%>%mutate(category="남성")

#카테고리별로 브랜드의 개수를 파악하기

#스킨케어 카테고리에 입점한 브랜드의 개수
skin_care_goods%>%group_by(brand)%>%summarise(num=n())

skin_brand_num<-length(skin_care_goods$brand)

#메이크업 카테고리에 입점한 브랜드의 개수
makeup_goods%>%group_by(brand)%>%summarise(num=n())
makeup_brand_num<-length(makeup_goods$brand)

#바디케어 카테고리에 입점한 브랜드의 개수
body_care_goods%>%group_by(brand)%>%summarise(num=n())

body_brand_num<-length(body_care_goods$brand)

#헤어케어 카테고리에 입점한 브랜드의 개수
hair_care_goods%>%group_by(brand)%>%summarise(num=n())

hair_brand_num<-length(hair_care_goods$brand)

#스킨과 메이크업 아이템에 입점한 브랜드의 개수
skin_makeup<-inner_join(skin_care_goods,makeup_goods,by="brand");head(skin_makeup)

skin_makeup_brand<-skin_makeup%>%group_by(brand)%>%summarise(num=n())
skin_makeup_brand_num<-length(skin_makeup_brand$brand)

#스킨과 바디케어 아이템에 입점한 브랜드의 개수
skin_body<-inner_join(skin_care_goods,body_care_goods,by="brand")
skin_body_brand<-skin_body%>%group_by(brand)%>%summarise(num=n())
skin_body_brand_num<-length(skin_body_brand$brand)

#스킨과 헤어케어 아이템에 입점한 브랜드의 개수
skin_hair<-inner_join(skin_care_goods,hair_care_goods,by="brand")
skin_hair_brand<-skin_hair%>%group_by(brand)%>%summarise(num=n())
skin_hair_brand_num<-length(skin_hair_brand$brand)

#메이크업과 바디케어 아이템에 입점한 브랜드의 개수
makeup_body<-inner_join(makeup_goods,body_care_goods,by="brand")
makeup_body_brand<-makeup_body%>%group_by(brand)%>%summarise(num=n())
makeup_body_brand_num<-length(makeup_body_brand$brand)

#메이크업과 헤어케어 아이템에 입점한 브랜드의 개수
makeup_hair<-inner_join(makeup_goods,hair_care_goods,by="brand")
makeup_hair_brand<-makeup_hair%>%group_by(brand)%>%summarise(num=n())
makeup_hair_brand_num<-length(makeup_hair_brand$brand)

#바디케어과 헤어케어 아이템에 입점한 브랜드의 개수
body_hair<-inner_join(body_care_goods,hair_care_goods,by="brand")
body_hair_brand<-body_hair%>%group_by(brand)%>%summarise(num=n())
body_hair_brand_num<-length(body_hair_brand$brand)

#크롤링한 모든 제품을 합쳐서 전체 제품을 담은 데이터프레임 만들기
total_product<-skin_care_goods%>%rbind(.,makeup_goods)%>%rbind(.,body_care_goods)%>%rbind(.,hair_care_goods)%>%rbind(.,perfume_goods)%>%rbind(.,beauty_item)%>%rbind(.,for_man_goods)

#브랜드별로 그룹핑 한 다음 빈도수를 확인하고 내림차순으로 정렬하기, 그 후 10개만 추출하기
top_total_product_brand<-total_product%>%group_by(brand)%>%summarise(freq=n())%>%arrange(desc(freq))%>%head(.,10)

#앞서 뽑은 가장 많이 나타나는 10개의 브랜드인 브랜드만 추출하고 brand열과 item열을 추출하기, 그 후 브랜드별로 정렬하고 중복되는 값 없애기
total_top_brand_item<-total_product%>%filter(brand %in% top_total_product_brand$brand)%>%select(brand,category)%>%arrange(brand)%>%unique()

#그래프를 그려 아이템와 많이 입점하고 있는 브랜드와 어떤 관계가 있는지 알아보기
install.packages("igraph")
library(igraph)

g<-graph.data.frame(total_top_brand_item,directed = F)

plot(g,layout=layout.fruchterman.reingold,vertex.size=7,edge.arrow.size=0.5,vertex.color="pink")

#네트워크 그래프는 16개의 노드와 26개의 링크로 연결이 되어 있음
# 방향의 의미가 중요치 않은 무방향 네트워크임

#무방향 이진 네트워크의 밀도 : k/{n(n-1)/2}

k<-26; n<-16
density<-k/{n*(n-1)/2}
density #네트워크 밀도

#중심성 분석

#install.packages("tidygraph")
#install.packages("ggraph")

#매개 중심성 계산하기

library(tidygraph)
library(ggraph)

total_top_brand_item %>% 
  as_tbl_graph() %>% 
  mutate(centrality_closeness()) %>%
  as_tibble

#다슈는 가장 많은 카테고리에 연결됨으로 인해 다른 노드들 간의
#네트워크 관계 형성에 있어서 중개자/ 매개자 역할을 가장 잘 수행