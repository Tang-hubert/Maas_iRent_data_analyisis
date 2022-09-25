# for str_sub()
install.packages("stringr")                                # Install stringr package
library("stringr") 


setwd("C:/Users/Hubert Tang/Desktop/folders/履歷/和泰Maas_iRent數據分析比賽/iRent去識別化數據(僅供本次賽事使用)_20220627")

getwd()

df_consumer <- read.csv("iRent顧客租車交易資料.csv", fileEncoding ="Big5", sep = ",")

# View(df_consumer)
# 
# iRent_1	iRent客戶編號 @待
# iRent_2	客戶年齡區間
# iRent_3	客戶性別
# iRent_4	客戶居住城市
# iRent_5	客戶居住區域
# iRent_6	租車訂單號碼 @待
# iRent_7	預約時間 @待
# iRent_8	租用金額 @待
# iRent_9	租用開始時間
# iRent_10	租用結束時間 @待
# iRent_11	使用里程數
# iRent_12	租用車號 @待
# iRent_13	租用車種
# iRent_14	租用車型
# iRent_15	車輛出廠年月 @待
# 
# idea iRent_7 & iRent_9 差距小於15分鐘 判斷現場預約

# data format



# iRent_2	客戶年齡區間
class(df_consumer$iRent_2)
df_consumer$iRent_2 = as.factor(df_consumer$iRent_2)
plot(df_consumer$iRent_2)
table(df_consumer$iRent_2)

# iRent_3	客戶性別
df_consumer$iRent_3 = as.factor(df_consumer$iRent_3)
plot(df_consumer$iRent_3)
table(df_consumer$iRent_3)

# iRent_4	客戶居住城市
df_consumer$iRent_4 = as.factor(df_consumer$iRent_4)
plot(df_consumer$iRent_4)
table(df_consumer$iRent_4)

# iRent_5	客戶居住區域
df_consumer$iRent_5 = as.factor(df_consumer$iRent_5)
plot(df_consumer$iRent_5)
table(df_consumer$iRent_5)


# iRent_9	租用開始時間

# ----------------------------------------------- #
# rent_start_time_inaday_c <- c()
# for(i in 1:nrow(df_consumer)){
#   if((str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "06") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "07") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "08") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "09")){
#     rent_start_time_inaday_c <- c(rent_start_time_inaday_c,"06~10")
#     
#   } else if((str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "10") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "11") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "12") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "13")){
#     rent_start_time_inaday_c <- c(rent_start_time_inaday_c,"10~14")
#     
#   } else if((str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "14") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "15") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "16") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "17")){
# 
#     rent_start_time_inaday_c <- c(rent_start_time_inaday_c,"14~18")
#   } else if((str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "18") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "19") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "20") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "21")){
# 
#     rent_start_time_inaday_c <- c(rent_start_time_inaday_c,"18~22")
#   } else if((str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "22") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "23") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "00") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "01")){
# 
#     rent_start_time_inaday_c <- c(rent_start_time_inaday_c,"22~02")
#   } else if((str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "02") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "03") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "04") || (str_sub(df_consumer$iRent_9[i],  start = 12, end = 13) == "05")){
# 
#     rent_start_time_inaday_c <- c(rent_start_time_inaday_c,"02~06")
#   } 
# }
# df_consumer$rent_start_time_inaday <- rent_start_time_inaday_c
# 
# df_consumer$rent_start_time_inaday = as.factor(df_consumer$rent_start_time_inaday)
# plot(df_consumer$rent_start_time_inaday)
# table(df_consumer$rent_start_time_inaday)
# ----------------------------------------------- #

df_consumer$rent_start_time_inaday <- str_sub(df_consumer$iRent_9,  start = 12, end = 13)
df_consumer$rent_start_time_inaday <- as.factor(df_consumer$rent_start_time_inaday)
df_consumer$rent_start_time_inaday <- as.numeric(df_consumer$rent_start_time_inaday)

hist(df_consumer$rent_start_time_inaday)
table(df_consumer$rent_start_time_inaday)


# iRent_11	使用里程數
km_c <- c() 
df_consumer$iRent_11 <- as.double(df_consumer$iRent_11)
for(i in 1:nrow(df_consumer)){
  if ((df_consumer$iRent_11[i] > 0) && (df_consumer$iRent_11[i] < 5)){
    
    km_c <- c(km_c, "0~5")
  } else if((df_consumer$iRent_11[i] >= 5) && (df_consumer$iRent_11[i] < 10)){
    
    km_c <- c(km_c, "5~10")
  } else if((df_consumer$iRent_11[i] >= 10) && (df_consumer$iRent_11[i] < 20)){

    km_c <- c(km_c, "10~20")
  } else if((df_consumer$iRent_11[i] >= 20) && (df_consumer$iRent_11[i] < 50)){

    km_c <- c(km_c, "20~50")
  } else if((df_consumer$iRent_11[i] >= 50) && (df_consumer$iRent_11[i] < 100)){

    km_c <- c(km_c, "50~100")
  } else if((df_consumer$iRent_11[i] >= 100) && (df_consumer$iRent_11[i] < 300)){

    km_c <- c(km_c, "100~300")
  } else if((df_consumer$iRent_11[i] >= 300) && (df_consumer$iRent_11[i] < 500)){

    km_c <- c(km_c, "300~500")
  } else{

    km_c <- c(km_c, ">500")
  }
}
df_consumer$km <- km_c

df_consumer$km = as.factor(df_consumer$km)
plot(df_consumer$km)
table(df_consumer$km)


# iRent_13	租用車種
df_consumer$iRent_13 = as.factor(df_consumer$iRent_13)
plot(df_consumer$iRent_13)
table(df_consumer$iRent_13)

# iRent_14	租用車型
df_consumer$iRent_14 = as.factor(df_consumer$iRent_14)
plot(df_consumer$iRent_14)
table(df_consumer$iRent_14)

# ------ #
# ------ #
# ------ #

df_consumer_copy = df_consumer
df_consumer = df_consumer_copy

fit = im()

class(df_consumer)
class(df_consumer$iRent_1)
class(df_consumer$iRent_2)
class(df_consumer$iRent_3)
class(df_consumer$iRent_4)
class(df_consumer$iRent_5)
class(df_consumer$iRent_6)
class(df_consumer$iRent_7)
class(df_consumer$iRent_8)
class(df_consumer$iRent_9)
class(df_consumer$iRent_10)
class(df_consumer$iRent_11)
class(df_consumer$iRent_12)
class(df_consumer$iRent_13)
class(df_consumer$iRent_14)
class(df_consumer$iRent_15)
class(df_consumer$iRent_16)
class(df_consumer$iRent_17)

# df_consumer$iRent_7 = as.Date(df_consumer$iRent_7, format="%Y/%m/%d %H:%M:%S")
