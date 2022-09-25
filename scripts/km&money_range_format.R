# --- 里程數區間 --- #
# --------------------------- #
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
km_variables <- c("0~5", "5~10", "10~20", "20~50", "50~100", "100~300", "300~500", ">500")
df_consumer$km <- factor(df_consumer$km, levels = km_variables)
class(df_consumer$km)


# --- 金額區間 --- #
# --------------------------- #
money_c <- c()
df_consumer$iRent_8 <- as.numeric(df_consumer$iRent_8)

for(i in 1:nrow(df_consumer)){
  if (df_consumer$iRent_8[i] == 0){
    money_c <- c(money_c, "0")
  }
  else if ((df_consumer$iRent_8[i] > 0) && (df_consumer$iRent_8[i] <= 20)){
    money_c <- c(money_c, "0~20")
  }
  else if((df_consumer$iRent_8[i] > 20 ) && (df_consumer$iRent_8[i] <= 50)){
    money_c <- c(money_c, "20~50")
  }
  else if((df_consumer$iRent_8[i] > 50 ) && (df_consumer$iRent_8[i] <= 100)){
    money_c <- c(money_c, "50~100")
  }
  else if((df_consumer$iRent_8[i] > 100 ) && (df_consumer$iRent_8[i] <= 300)){
    money_c <- c(money_c, "100~300")
  }
  else if((df_consumer$iRent_8[i] > 300) && (df_consumer$iRent_8[i] <= 500)){
    money_c <- c(money_c, "300~500")
  }
  else if((df_consumer$iRent_8[i] > 500) && (df_consumer$iRent_8[i] <= 750)){
    money_c <- c(money_c, "500~750")
  }
  else if((df_consumer$iRent_8[i] > 750) && (df_consumer$iRent_8[i] <= 1000)){
    money_c <- c(money_c, "750~1000")
  }
  else if((df_consumer$iRent_8[i] > 1000) && (df_consumer$iRent_8[i] <= 1500)){
    money_c <- c(money_c, "1000~1500")  
  }
  else if((df_consumer$iRent_8[i] > 1500) && (df_consumer$iRent_8[i] <= 2000)){
    money_c <- c(money_c, "1500~2000")
  } 
  else if((df_consumer$iRent_8[i] > 2000) && (df_consumer$iRent_8[i] <= 2500)){
    money_c <- c(money_c, "2000~2500")
  } 
  else if((df_consumer$iRent_8[i] > 2500) && (df_consumer$iRent_8[i] <= 3000)){
    money_c <- c(money_c, "2500~3000")
  }
  else if((df_consumer$iRent_8[i] > 3000) && (df_consumer$iRent_8[i] <= 4000)){
    money_c <- c(money_c, "3000~4000")
  } 
  else if((df_consumer$iRent_8[i] > 4000) && (df_consumer$iRent_8[i] <= 6000)){
    money_c <- c(money_c, "4000~6000")
  } 
  else{
    money_c <- c(money_c, ">6000")
  } 
}
df_consumer$money <- money_c
money_variables <- c("0", "0~20", "20~50", "50~100", "100~300", "300~500", "500~750", "750~1000", "1000~1500", "1500~2000", "2000~2500", "2500~3000", "3000~4000", "4000~6000", ">6000")
df_consumer$money <- factor(df_consumer$money, levels = money_variables)
class(df_consumer$money)

# ---------------- #
# 使用時間
df_consumer$used_sec <- df_consumer$iRent_10 - df_consumer$iRent_9
class(df_consumer$used_sec) # difftime

# df_consumer$used_sec <- as.difftime(df_consumer$used_sec, units = "auto", format="%Y/%m/%d %H:%M:%S")
# df_consumer$used_sec <- as.difftime(df_consumer$used_sec, units = c("auto", "secs", "mins", "hours", "days", "weeks"))
# ?difftime 

df_consumer$time_diff_sec_num <- as.numeric(df_consumer$used_sec)
class(df_consumer$used_sec)

# ---------------- #
df_consumer_motor <- df_consumer[df_consumer$iRent_14 == "motor",]
df_consumer_car <- df_consumer[df_consumer$iRent_14 == "car",]
