rm(list=ls())
library(geojsonio)
library(tmap)
library(sf)
library(spdep)
library(stringr)
library(plyr)
library(dplyr)
library(GISTools)
library(areal)
library('spatialreg')



###############경계 맞추기 ##############

seoul_data = geojson_read("수정한 서울시 파출소 경계.geojson",what="sp")
seoul_ext_sf = st_as_sf(seoul_data)



## 지구대별 신고건수를 case_type 별로 필터링할 필요가 있음
df_112 <- read.csv("지구대별 신고종류.csv",header = TRUE,fileEncoding = "UTF-8", encoding = 'CP949')

## 교통불편은 제거
df_112 <- df_112[!(df_112$case_type == "교통불편" ), ]

## df_112.gp represents 지구대별 신고 총 건수
df_112.gp <- df_112 %>% group_by(psname,name) %>%
  summarise(count = sum(count))
df_112_top = rename(df_112.gp,"NAME"="name")
df_112_top[(df_112_top$psname == "서울관악경찰서" & df_112_top$NAME =="신사지구대") ,'NAME'] = "신사지구대(관악)"
df_112_top[(df_112_top$psname == "서울서부경찰서" & df_112_top$NAME =="신사지구대") ,'NAME'] = "신사지구대(서부)"
df_112_top = df_112_top[order(df_112_top$NAME),]

seoul_ext_sf[(seoul_ext_sf$PSNAME == "서울서부경찰서" & seoul_ext_sf$NAME =="신사지구대")
             ,'NAME'] = "신사지구대(서부)"
seoul_ext_sf[(seoul_ext_sf$PSNAME == "서울관악경찰서" & seoul_ext_sf$NAME =="신사지구대")
             ,'NAME'] = "신사지구대(관악)"

df_112_pol = inner_join(seoul_ext_sf,df_112_top,"NAME")


gu <-st_read("seoul_gu.shp")
dong=st_read("seoul_dong.shp")


## 동이랑 1인가구수 조인
pop1 = read.csv("1인가구수.csv")
pop1 = (pop1 %>% filter(동 != "소계"))
pop1$동 = str_replace_all(pop1$동,"\\.","\\·")
pop1$X1인가구= str_replace(pop1$X1인가구,",","")
pop1$X1인가구 = as.numeric(pop1$X1인가구)

dong = dong[order(dong$ADM_DR_NM),]
pop1 = pop1[order(pop1$동),]
pop1 = pop1[-which(pop1$동=="항동"),]
pop1 = rename(pop1,"ADM_DR_NM"="동")

##관악구 신사동,  강남구 신사동 분리   1121068 관악구
pop1[(pop1$자치구 == "관악구" & pop1$ADM_DR_NM =="신사동") ,'ADM_DR_NM'] = "신사동(관악)"
pop1[(pop1$자치구 == "강남구" & pop1$ADM_DR_NM =="신사동") ,'ADM_DR_NM'] = "신사동(강남)"

dong[(dong$ADM_DR_CD == "1121068" & dong$ADM_DR_NM =="신사동") ,'ADM_DR_NM'] = "신사동(관악)"
dong[(dong$ADM_DR_CD == "1123051" & dong$ADM_DR_NM =="신사동") ,'ADM_DR_NM'] = "신사동(강남)"

pop1 = rename(pop1, 'pop'='X1인가구')
dong = inner_join(dong,pop1,"ADM_DR_NM")

# tid = target id, sid = source id
pol_dong = aw_interpolate(gu, tid = SIGUNGU_CD, source = dong, sid = ADM_DR_CD, 
                          weight = "sum", output = "tibble", 
                          intensive = "pop")

### 파출소 멀티폴리곤에서 폴리곤으로 변환후 보간법을 통해 동경계를 파출소 경계로 통일
## 파출소와 동 데이터 데이터 타입 확인
st_geometry_type(df_112_pol)
st_geometry_type(dong)

## 동 데이터를 폴리곤으로 맞추기
# df_112_pol_polygon = st_cast(df_112_pol, "MULTIPOLYGON") %>% st_cast("POLYGON")
df_112_pol_polygon = df_112_pol
df_112_pol_polygon = df_112_pol_polygon[order(df_112_pol_polygon$NAME),]
st_geometry_type(df_112_pol_polygon)

###용신지구대 전농2파출소 제외

# which(df_112_pol_polygon$NAME=="용신지구대")
# df_112_pol_polygon = df_112_pol_polygon[-169,]
# which(df_112_pol_polygon$NAME=="전농2파출소")
# df_112_pol_polygon = df_112_pol_polygon[-196,]


df_112_pol_polygon = st_transform(df_112_pol_polygon,5179)

pol_dong = aw_interpolate(df_112_pol_polygon, tid = NAME, 
                          source = dong, sid = ADM_DR_CD,
                          weight = "sum", output = "tibble", 
                          extensive = "pop")


#### 1인가구수 보간법 했을때 합계 맞았으므로 이상없음!!########

## 보간법으로 나온 파출소경계 1인가구수 테이블과 파출소 경계 테이블 조인
df_112_pol = df_112_pol[order(df_112_pol$NAME),] 
df_112_pol_polygon = df_112_pol_polygon[order(df_112_pol_polygon$NAME),]
pol_dong = pol_dong[order(pol_dong$NAME),]

df_112_pol$pop = pol_dong$pop
df_112_pol_polygon$pop = pol_dong$pop

## 확인
si = st_read('시경계/ctp_rvn.shp')
seoulsi = si[si$CTP_ENG_NM == 'Seoul',]
seoulsi = st_transform(seoulsi, 5179)
tm_shape(seoulsi) + tm_borders()


######## 나머지 데이터 맞추기 #########

# 1. 경찰관 수 데이터 추가하기
patrol_num <- read.csv("경찰관수.csv")
patrol_num[,'nat_pol_agency'] = NULL
colnames(patrol_num) = c('PSNAME', "NAME", "officer_num")
patrol_num[(patrol_num$PSNAME == "서울관악경찰서" & patrol_num$NAME =="신사지구대") ,'NAME'] = "신사지구대(관악)"
patrol_num[(patrol_num$PSNAME == "서울서부경찰서" & patrol_num$NAME =="신사지구대") ,'NAME'] = "신사지구대(서부)"
patrol_num = patrol_num[order(patrol_num$NAME),]

df_112_pol_polygon = inner_join(df_112_pol_polygon, patrol_num, by = 'NAME')
df_112_pol_polygon = subset(df_112_pol_polygon, select=-c(psname.x, psname.y))

dong_patrol_num = aw_interpolate(dong, tid = ADM_DR_CD, source = df_112_pol_polygon  , sid = NAME, 
                                 weight = "sum", output = "tibble", 
                                 extensive = "officer_num")

dong_table = inner_join(dong_patrol_num[,c('ADM_DR_CD',"officer_num")],dong,by ='ADM_DR_CD')

# 2. 상업용지 면적 추가
ground_use = st_read('토지이용도/ground_use.shp')
ground_use_industry <- ground_use[ground_use$UCB == '3130',]
seoul_ground = st_intersection(seoulsi, ground_use_industry)


## 공간보간법을 이용하여 상업용지 면적 추가
ground_use_industry$total_industry_area = st_area(ground_use_industry)
ground_use_industry$total_industry_area = as.numeric(ground_use_industry$total_industry_area)
ground_use_industry$id = c(1:4267)
ground_inter = aw_interpolate(dong, tid = ADM_DR_CD, source = ground_use_industry, sid = id, 
                              weight = "sum", output = "tibble", 
                              extensive = "total_industry_area")

ground_inter = ground_inter[,c('ADM_DR_CD', 'total_industry_area')]
ground_inter$total_industry_area = as.numeric(ground_inter$total_industry_area)
ground_inter[is.na(ground_inter$total_industry_area),2] = 0
dong_table = inner_join(dong_table, ground_inter, by='ADM_DR_CD')

###### 상업지구 보간법 합계 확인 \#############
### 확인 결과 : 서울시 경계로 intersection 했을경우 경계를 포함하기 때문에 조금 차이가 있는 것 같다.
sum(st_area(seoul_ground))
# 35223566 [m^2]
sum(ground_inter$total_industry_area)
# 35189809
##############################################



# 3. cctv 개수 추가
cctv_num = read.csv('서울시 cctv 설치현황.csv')
cctv_num = rename(cctv_num, 'y' = 'WGS84위도')
cctv_num = rename(cctv_num, 'x' = 'WGS84경도')
cctv_num_shp = st_as_sf(cctv_num, coords = c("x", "y"), crs = 4326)
cctv_num_shp = st_transform(cctv_num_shp, 5179)
cctv_num_shp$설치연월 = sapply(cctv_num_shp$설치연월, function (x) {as.numeric(substr(x,1,4))})


## 2021년도 데이터 생성
dong_cctv_num = st_intersection(dong, cctv_num_shp)

dong_cctv_num = dong_cctv_num %>% group_by(ADM_DR_CD) %>%
  summarise(total_cctv_num_21 = sum(카메라대수))

dong_cctv_num = st_drop_geometry(dong_cctv_num)
dong_table = left_join(dong_table,data.frame(dong_cctv_num),by='ADM_DR_CD')

### cctv가 없는 파출소 지역의 결측값 처리
dong_table[is.na(dong_table$total_cctv_num_21),'total_cctv_num_21'] = 0



# 4. 보안등 개수 추가

security_num = read.csv("5개시도_보안등현황.csv")
security_num = rename(security_num, 'y' = 'lat')
security_num = rename(security_num, 'x' = 'lon')
security_num = st_as_sf(security_num, coords = c("x", "y"), crs = 4326)
security_num = st_transform(security_num, 5179)
st_crs(cctv_num_shp)


## 2021년도 데이터 생성
dong_security_num = st_intersection(dong, security_num)
dong_security_num = dong_security_num %>% group_by(ADM_DR_CD) %>%
  summarise(total_security_num_21 = sum(X.securitylight_cnt))

dong_security_num = st_drop_geometry(dong_security_num)
dong_table = left_join(dong_table,dong_security_num,'ADM_DR_CD')


### 보안등이 없는 파출소 지역의 결측값 처리
dong_table[is.na(dong_table$total_security_num_21),'total_security_num_21'] = 0

## 총 보안등 수 확인
sum(dong_table$total_security_num_21)     # 177846
# nrow(st_intersection(seoulsi, security_num))     #1 76122

# 5. 숙박업소 개수 추가
accomodation_num = read.csv('숙박업소.csv', header=TRUE, fileEncoding = "CP949")
accomodation_num = accomodation_num[,c('인허가일자','폐업일자','좌표정보.X.','좌표정보.Y.')]
accomodation_num = rename(accomodation_num, 'x' = '좌표정보.X.')
accomodation_num = rename(accomodation_num, 'y' = '좌표정보.Y.')
accomodation_num = accomodation_num[-which(is.na(accomodation_num$x)),]
accomodation_num = st_as_sf(accomodation_num, coords = c("x", "y"), crs=2097)
accomodation_num = st_transform(accomodation_num, 5179)

## 인허가일자가 나타나지 않은 숙박업소는 2019년도로 맞춰줌
accomodation_num[is.na(accomodation_num$인허가일자),'인허가일자'] = 20190000

## 폐업일자가 나타나지 않은 숙박업소는 2030년도로 맞춰줌
accomodation_num[is.na(accomodation_num$폐업일자),'폐업일자'] = 20300000


## 2021년도 데이터 생성(21년도에 폐업한 숙박업소도 일단 포함)
accomodation_num_21 = accomodation_num[which(accomodation_num$폐업일자 >= 20210000),]
dong_accomodation_num_21 = st_intersection(dong, accomodation_num_21)
dong_accomodation_num_21 = dong_accomodation_num_21 %>% group_by(ADM_DR_CD) %>%
  summarise(total_accomodation_num_21 = n())

dong_accomodation_num_21 = st_drop_geometry(dong_accomodation_num_21)
dong_table = left_join(dong_table,data.frame(dong_accomodation_num_21),by='ADM_DR_CD')

### 숙박업소가 없는 파출소 지역의 결측값 처리
dong_table[is.na(dong_table$total_accomodation_num_21),'total_accomodation_num_21'] = 0



# 6. 유흥업소 개수 추가

nightlife_num = read.csv('5개시도_유흥업소_단란주점현황.csv')
nightlife_num = nightlife_num[,c(1,5,6)]
nightlife_num = rename(nightlife_num, 'y' = 'lat')
nightlife_num = rename(nightlife_num, 'x' = 'lon')
nightlife_num = st_as_sf(nightlife_num, coords = c("x", "y"), crs = 4326)
nightlife_num = st_transform(nightlife_num, 5179)
st_crs(nightlife_num)

## 2021년도 데이터 생성
dong_nightlife_num = st_intersection(dong, nightlife_num)
dong_nightlife_num = dong_nightlife_num %>% group_by(ADM_DR_CD) %>%
  summarise(total_nightlife_num_21 = n())

dong_nightlife_num = st_drop_geometry(dong_nightlife_num)
dong_table = left_join(dong_table,data.frame(dong_nightlife_num),by='ADM_DR_CD')


### 유흥업소가 없는 파출소 지역의 결측값 처리
dong_table[is.na(dong_table$total_nightlife_num_21),'total_nightlife_num_21'] = 0


# 7. 공원 개수 추가
park_num = read.csv('5개시도_공원현황.csv')
park_num = park_num[,-c(1:2)]
park_num = rename(park_num, 'y' = 'lat')
park_num = rename(park_num, 'x' = 'lon')
park_num = st_as_sf(park_num, coords = c("x", "y"), crs = 4326)
park_num = st_transform(park_num, 5179)
st_crs(park_num)
dong_park_num = st_intersection(dong, park_num)
dong_park_num = dong_park_num %>% group_by(ADM_DR_CD) %>%
  summarise(total_park_num = n())

dong_park_num = st_drop_geometry(dong_park_num)
dong_table = left_join(dong_table,data.frame(dong_park_num),by='ADM_DR_CD')


## 공원이 없는 파출소 지역의 결측값 처리
dong_table[is.na(dong_table$total_park_num),'total_park_num'] = 0



# 8. 인구 수 데이터 추가 

### 동이랑 2021년 인구 수 조인
total_pop_2021 = read.csv("2021_인구_데이터.csv")
total_pop_2021$동 = str_replace_all(total_pop_2021$동,"\\.","\\·")
total_pop_2021 = (total_pop_2021 %>% filter(동 != "소계"))

dong = dong[order(dong$ADM_DR_NM),]
total_pop_2021 = total_pop_2021[order(total_pop_2021$동),]
total_pop_2021 = total_pop_2021[-which(total_pop_2021$동=="항동"),]
total_pop_2021 = rename(total_pop_2021,"ADM_DR_NM"="동")

### 상일동이 1동, 2동으로 분리되어 있으므로 합쳐줌
sangildong_pop = total_pop_2021[total_pop_2021$ADM_DR_NM == '상일1동', '인구'] + total_pop_2021[total_pop_2021$ADM_DR_NM == '상일2동', '인구']
total_pop_2021 = total_pop_2021[-which(total_pop_2021$ADM_DR_NM == '상일1동'),]
total_pop_2021[which(total_pop_2021$ADM_DR_NM == '상일2동'),'ADM_DR_NM'] = '상일동'
total_pop_2021[which(total_pop_2021$ADM_DR_NM == '상일동'),'인구'] = sangildong_pop
total_pop_2021[total_pop_2021$ADM_DR_NM == '상일동',]



### 관악구 신사동,  강남구 신사동 분리   1121068 관악구
total_pop_2021[(total_pop_2021$자치구 == "관악구" & total_pop_2021$ADM_DR_NM =="신사동") ,'ADM_DR_NM'] = "신사동(관악)"
total_pop_2021[(total_pop_2021$자치구 == "강남구" & total_pop_2021$ADM_DR_NM =="신사동") ,'ADM_DR_NM'] = "신사동(강남)"

dong_total_pop_2021 = inner_join(dong,total_pop_2021,"ADM_DR_NM")
dong_total_pop_2021 = st_drop_geometry(dong_total_pop_2021[,c("ADM_DR_CD","인구")])
dong_total_pop_2021 = rename(dong_total_pop_2021, 'total_pop_2021' = '인구')
dong_table = inner_join(dong_table,dong_total_pop_2021,"ADM_DR_CD")


# 9. 비상벨 개수 추가

emergency_num = read.csv('서울시_비상벨.csv')
emergency_num = subset(emergency_num, select = c('WGS84위도','WGS84경도','안전비상벨설치연도'))
emergency_num = rename(emergency_num, 'y' = 'WGS84위도')
emergency_num = rename(emergency_num, 'x' = 'WGS84경도')
emergency_num = st_as_sf(emergency_num, coords = c("x", "y"), crs = 4326)
emergency_num = st_transform(emergency_num, 5179)
st_crs(emergency_num)

## 2021년도 데이터 생성
dong_emergency_num_21 = st_intersection(dong, emergency_num)
dong_emergency_num_21 = dong_emergency_num_21 %>% group_by(ADM_DR_CD) %>%
  summarise(total_emergency_num_21 = n())

dong_emergency_num_21 = st_drop_geometry(dong_emergency_num_21)
dong_table = left_join(dong_table,data.frame(dong_emergency_num_21),by='ADM_DR_CD')

### 비상벨이가 없는 파출소 지역의 결측값 처리
dong_table[is.na(dong_table$total_emergency_num_21),'total_emergency_num_21'] = 0


# 10. 편의점 개수 추가

convenience_num = geojson_read('편의점현황.geojson',what = "sp")
convenience_num_sf = st_as_sf(convenience_num)
convenience_num_sf= rename(convenience_num_sf, 'y' = 'LON')
convenience_num_sf = rename(convenience_num_sf, 'x' = 'LAT')
convenience_num_sf = st_transform(convenience_num_sf, 5179)
st_crs(convenience_num_sf)
dong_convenience_num_sf = st_intersection(dong, convenience_num_sf)
dong_convenience_num_sf = dong_convenience_num_sf %>% group_by(ADM_DR_CD) %>%
  summarise(total_convenience_num = n())


dong_convenience_num_sf = st_drop_geometry(dong_convenience_num_sf)
dong_table = left_join(dong_table,data.frame(dong_convenience_num_sf),by='ADM_DR_CD')

## 편의점이 없는 파출소 지역의 결측값 처리
dong_table[is.na(dong_table$total_convenience_num),'total_convenience_num'] = 0


# 11. 주거면적 시각화 
ground_use_house <- ground_use[ground_use$UCB == '3110' | ground_use$UCB == '3120',]
seoul_house = st_intersection(seoulsi,ground_use_house)
seoul_house = seoul_house[,-3] 

## 공간보간법을 이용하여 주거면적 추가
ground_use_house$total_house_area = st_area(ground_use_house)
ground_use_house$id = c(1:nrow(ground_use_house))
ground_inter2 = aw_interpolate(dong, tid = ADM_DR_CD, source = ground_use_house, sid = id, 
                               weight = "sum", output = "tibble", 
                               extensive = "total_house_area")

ground_inter2 = ground_inter2[,c('ADM_DR_CD', 'total_house_area')]
ground_inter2$total_house_area =as.numeric(ground_inter2$total_house_area)
ground_inter2[is.na(ground_inter2$total_house_area),2] = 0


dong_table = left_join(dong_table, ground_inter2, by='ADM_DR_CD')
dong_table[is.na(dong_table$total_house_area),'total_house_area'] = 0


###### 주거지역 보간법 합계 확인 #############
sum(dong_table$total_house_area)     # 199166854
sum(st_area(seoul_house))      # 197580551 [m^2]


############ 20년도와 21년도 데이터로 나누기 ###################

## 데이터 중 경계면적, 인구수로 나누기 및 
## 데이터의 단위 변경 -> 면적은 km^2, 인구는 100명 단위로 하면 될듯

## 면적 데이터 num으로 변경 
dong$Boundary_area = st_area(dong)
st_drop_geometry(dong[,c('Boundary_area', 'ADM_DR_CD')])
dong_table = inner_join(st_drop_geometry(dong[,c('Boundary_area', 'ADM_DR_CD')]), dong_table, by='ADM_DR_CD')
dong_table$Boundary_area = as.numeric(dong_table$Boundary_area)

## df_112_pol_polygon 저장
st_write(df_112_pol_polygon, 'predict_112.shp')
write.csv(data.frame(st_drop_geometry(df_112_pol_polygon)), 'predict_112.csv', row.names = F, fileEncoding = 'CP949')

## dong_table_ratio 생성
dong_table_ratio = dong_table[,c('ADM_DR_CD', 'ADM_DR_NM')]
dong_table_ratio$total_cctv_num = dong_table$total_cctv_num_21*(10**6)/dong_table$Boundary_area
dong_table_ratio$total_industry_area = dong_table$total_industry_area/dong_table$Boundary_area
dong_table_ratio$total_house_area = dong_table$total_house_area/dong_table$Boundary_area
dong_table_ratio$officer_num = dong_table$officer_num*1000/dong_table$total_pop_2021
dong_table_ratio$total_nightlife_num = dong_table$total_nightlife_num_21*(10**6)/dong_table$Boundary_area
dong_table_ratio$total_emergency_num = dong_table$total_emergency_num*(10**6)/dong_table$Boundary_area
dong_table_ratio$total_park_num = dong_table$total_park_num*(10**6)/dong_table$Boundary_area
dong_table_ratio$total_security_num = dong_table$total_security_num_21*(10**6)/dong_table$Boundary_area
dong_table_ratio$total_convenience_num = dong_table$total_convenience_num*(10**6)/dong_table$Boundary_area
dong_table_ratio$total_accomodation_num = dong_table$total_accomodation_num_21*(10**6)/dong_table$Boundary_area
dong_table_ratio$pop = dong_table$pop*1000/dong_table$total_pop_2021
dong_table_ratio$total_pop = dong_table$total_pop_2021

########## 로지스틱 회귀분석 ###############
enforce_dong = read.csv('현재시행중인보안관.csv', fileEncoding = 'CP949')

## 시행중이면 1 시행하고 있지 않으면 0
enforce_dong$enforce = 1  
enforce_dong = rename(enforce_dong,"ADM_DR_NM" = "행정동")
dong_table = left_join(dong_table,enforce_dong,"ADM_DR_NM")
dong_table[is.na(dong_table$enforce),"enforce"] = 0
dong_table_ratio = left_join(dong_table_ratio,enforce_dong,"ADM_DR_NM")
dong_table_ratio[is.na(dong_table_ratio$enforce),"enforce"] = 0

# 비율로 하지 안았을때 값

model_logistic = glm(enforce ~ pop + officer_num + total_cctv_num_21 + total_security_num_21 +
                       total_nightlife_num_21 + total_park_num + total_emergency_num_21 +
                       total_convenience_num + total_industry_area + total_accomodation_num_21 + total_pop_2021
                     , data = dong_table,family=binomial)

summary(model_logistic)


# 비율로 했을때 값

model_logistic = glm(enforce ~ pop + officer_num + total_cctv_num + total_security_num +
                       total_nightlife_num + total_park_num + total_emergency_num +
                       total_convenience_num + total_industry_area + total_accomodation_num + total_pop
                     , data = dong_table_ratio, family=binomial)

summary(model_logistic)

library(modEvA)
RsqGLM(model=model_logistic)
summary(lm(model_logistic$fitted.values ~ dong_table_ratio$enforce))

