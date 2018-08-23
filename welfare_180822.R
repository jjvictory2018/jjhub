install.packages("ggmap")
install.packages("ggplot2")
installed.packages("dplyr")
library(ggmap)   #map그리기 
library(ggplot2) #시각화 
library(dplyr) #전처리

install.packages("foreign") #foreign패키지지 설ㅊ
library(foreign) #SPSS파일로드
library(readxl)  #엑셀파일 불러오기
raw_welfare = read.spss(file = "Koweps_hpc10_2015_beta1.sav",
                        to.data.frame = T)
welfare = raw_welfare   #복사본만들기 
head(welfare)  #데이터 검토  
tail(welfare)
dim(welfare)
str(welfare)
summary(welfare)
welfare = raw_welfare
welfare = rename(welfare,   #변수명을 쉬운단어로~
                 sex = h10_g3,  #성별
                 birth = h10_g4, #출생년도 
                 marriage = h10_g10, #혼인상태 
                 religion = h10_g11, #종교 
                 income = p1002_8aq1, #월급 
                 code_job = h10_eco9, #직종코드 
                 code_region = h10_reg7) #지역코드 
#데이터 분석절차
#1단계: 변수 검토 및 전처리
#2단계: 변수간 관계 분석 
#09-2 성별에 따른 월급 차이
#성별에 따라 월급이 다를까?
#분석절차 (1변수검토 및 전처리(성별,월급)2변수간관 계분석 (성별월급 평균표만들기, 그래프 만들기 ))
#성별 변수 검토 및 전처리
#1변수 검토
class(welfare$sex)
#2전처리
#이상치 확인
table(welfare$sex)
#이상치 결측처리
welfare$sex = ifelse(welfare$sex == 9, NA, welfare$sex)
#결측치 확인 
table(is.na(welfare$sex))
#성별항목이름 부여 
welfare$sex = ifelse(welfare$sex == 1, "male", "female")

#확인
(welfare$sex)

qplot(welfare$sex)

#변수검토
class(welfare$income)

#2. 전처리
#이상치 확인 
summary(welfare$income)
qplot(welfare$income)
qplot(welfare$income) + xlim(0, 1000)
summary(welfare$income)
#이상치 결측처리 
welfare$income = ifelse(welfare$income %in% c(0, 9999), NA, welfare$income)
#결측치 학인 
table(is.na(welfare$income))

#성별에 따른 월급 차이 분석
sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(sex) %>% 
  summarise(mean_income = mean(income))

sex_income

#그래프 만들기
ggplot(data = sex_income, aes(x = sex, y = mean_income)) + geom_col()

#09-3 나이와 월급 관계
#몇살때 월급을 가장 많이 받을 까?
#분석절차(1.변수검토 및 전처리 2. 변수간 관계분석)
#1. 변수검토
head(welfare)
class(welare$birth)
summary(welfare$birth)
qplot(welfare$birth)

#2. 전처리
#이상치 확인
summary(welfare$birth)

#결측치 확인
table(is.na(welfare$birth))

#이상치 결측처리
welfare$birth = ifelse(welfare$birth == 9999, NA, welfare$birth)

#3 파생변수 만들기 - 나이
welfare$age = 2018 - welfare$birth + 1
summary(welfare$age)

qplot(welfare$age)

#나이와 월급의 관계 분석
age_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age)
  summarize(mean_income = mean(income))

head(age_income)  
ggplot(data = age_income, aes(x = age, y = mean_income)) + geom_line()

#09-4 연령대에 따른 월급차이
#어떤 연령대의 월급이 가장 많을까?
#분석절차 (1.변수검토 및 전처리 2. 변수간관계분석)
#연령대 변수 검토 및 전처리
#파생변수 만들기 - 연령대
welfare = welfare %>% 
  mutate(age1 = ifelse(age < 30, "age_20",
                              ifelse(age < 40, "age_30",
                                     ifelse(age < 50, "age_40",
                                            ifelse(age < 60, "age_50",
                                                   ifelse(age < 70, "age_60", "age_70"))))))
                       
table(welfare$age1)
qplot(welfare$age1)

#연령대에 따른 월급차이 분석
#1. 연령대별 월급 평균표
age1_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age1) %>% 
  summarize(mean_income = mean(income))

age1_income

#2.그래프 만들기
ggplot(data = age1_income, aes(x = age1, y = mean_income)) + geom_col()

#막대정렬 : 20대 - 70대  나이순
ggplot(data = age1_income, aes(x = age1, y = mean_income, fill = sex)) + geom_col(position = "dodge")
scale_x_discrete(limits = c("age_20", "age_30", "age_40", "age_50", "age_60", "age_70"))


#09-5 연령대 및 성별 월급 차이
#성별 월급 차이는 연령대별로 다를까?
#분석절차 (1변수검토 및 전처리  2. 변수간관계분석)
#연령대별 월급차이 분석 
age_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age1, sex) %>% 
  summarise(mean_income = mean(income))
age_income

#2. 그래프 만들기
ggplot(data = age_income, aes(x = age1, y = mean_income, fill = sex)) + 
  geom_col() +
  scale_x_discrete(limits = c("age_20", "age_30", "age_40", "age_50", "age_60"))

#09-5 연령대 및 성별 월급차이
#성별 월급차이는 연령대 별로 다를까  
#분석절차(1변수 검토및 전처리 2 변수간 관계분석 )

#연령대 및 성별 월급차이 분석
#1. 연령대 및 성별 월급 병균표
sex_income = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age1, sex) %>% 
  summarise(mean_income = mean(income))
sex_income

#2.그래프
ggplot(data = sex_income, aes(x = age1, y = mean_income, fill = sex)) +
  geom_col() + 
  scale_x_discrete(limits = c("age_20","age_30","age_40","age_50","age_60","age_70"))

#성별 막대 분리
ggplot(data = sex_income, aes(x = age1, y = mean_income, fill = sex)) + 
  geom_col(position = "dodge") + 
  scale_x_discrete(limits = c("age_20", "age_30","age_40","age_50","age_60","age_70")) 

#나이 및 성별 월급 차이 분석
#성별 연령별 월급 평균표
sex_age = welfare %>% 
  filter(!is.na(income)) %>% 
  group_by(age, sex) %>% 
  summarise(mean_income = mean(income))
head(sex_age)

#그래프
ggplot(data = sex_age, aes(x = age, y = mean_income, col = sex)) + geom_line()

#09-6 직업별 월급 차이
#어떤 직업이 월급을 가장 많이 받을까
#분석절차(1.변수검토, 전처리 2. 변수간 관계분석 )
#1 변수검토
class(welfare$code_job)
table(welfare$code_job)

#전처리
library(readxl)
list_job = read_excel("Koweps_Codebook.xlsx", col_names = T, sheet = 2)
head(list_job)
dim(list_job)

#welfare에 직업명 결합
welfare = left_join(welfare, list_job, id = "code_job")
welfare %>% 
  filter(!is.na(code_job)) %>% 
  group_by(code_job, job) %>% 
  head(10)

#직업별 월급차이 분석
#1. 직업별 월급 평균표
job_income = welfare %>% 
  filter(!is.na(job) & !is.na(income)) %>% 
  group_by(job) %>% 
  summarise(mean_income = mean(income))
head(job_income)

#2 상위 10개 추출
top10 = job_income %>% 
  arrange(desc(mean_income)) %>% 
  head(10)

top10

#3그래프
ggplot(data = top10, aes(x = reorder(job, mean_income), y = mean_income))+
  geom_col() +
  coord_flip()
