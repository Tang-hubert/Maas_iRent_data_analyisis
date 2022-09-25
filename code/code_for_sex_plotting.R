# --------------------------- #
# path
path = "C:/Users/Hubert Tang/Desktop/folders/履歷/和泰Maas_iRent數據分析比賽/iRent去識別化數據(僅供本次賽事使用)_20220627/code/"
setwd(path)
getwd()

path_for_plot = "C:/Users/Hubert Tang/Desktop/folders/履歷/和泰Maas_iRent數據分析比賽/iRent去識別化數據(僅供本次賽事使用)_20220627/plot/進階數據圖表/汽機車,男與女圖表/"
# --------------------------- #
# Import data
df_consumer <- read.csv("iRent顧客租車交易資料.csv", fileEncoding ="Big5", sep = ",")

# --------------------------- #
# Library
install.packages("ggplot2")
library(ggplot2)
install.packages("stringr")
library(stringr)
install.packages("showtext")
library(showtext)
showtext_auto()

# --------------------------- #
# Functions
## hist
ggplot_save <- function(type, filename, plot_path) {
  if( type == "fill"){
    ggsave(
      str_c("(fill)", filename, ".pdf"),
      plot = last_plot(),
      device = "pdf",
      path = plot_path,
      width = 4000,
      height = 2300,
      units = "px"
    ) 
    return()
  }
  if ( type == "dodge"){
      ggsave(
        str_c("(dodge)", filename, ".pdf"),
        plot = last_plot(),
        device = "pdf",
        path = plot_path,
        width = 4000,
        height = 2300,
        units = "px"
      )
      return()
  }
}

## clear plots
clear_plots <- function(){
  dev.off(dev.list()["RStudioGD"])
}

### ploting ###
draw_ggplot <- function(vehicle_type, type, Factor_column, Factor_name, folder_name){
  if (vehicle_type == "Motor"){
    df = df_consumer_motor
  }
  if(vehicle_type == "Car"){
    df = df_consumer_car
  }
  
  title = str_c(vehicle_type, "&", Factor_name, " between F&M")
  
  if(type == "fill"){
    ggplot(df, aes(df[,Factor_column], fill = iRent_3)) + geom_bar() + 
      ggtitle(title) + 
      xlab(Factor_name) + ylab("Count") + labs(fill = "Type") #  +   coord_flip()
    ggplot_save(type = type, filename = title, plot_path = str_c(path_for_plot, folder_name))
    return()
  }
  if(type == "dodge"){
    ggplot(df, aes(df[,Factor_column], fill = iRent_3)) + geom_bar(position = "dodge") + 
      ggtitle(title) + 
      xlab(Factor_name) + ylab("Count") + labs(fill = "Type") #  +   coord_flip()
    ggplot_save(type = type, filename = title, plot_path = str_c(path_for_plot, folder_name))
    return()
  }
}
### ------- ###

# --------------------------- #
# data class filter
df_consumer$iRent_2 <- as.factor(df_consumer$iRent_2)
df_consumer$iRent_3 <- as.factor(df_consumer$iRent_3)
df_consumer$iRent_4 <- as.factor(df_consumer$iRent_4)
df_consumer$iRent_5 <- as.factor(df_consumer$iRent_5)
df_consumer$iRent_7 = as.POSIXct(df_consumer$iRent_7, format="%Y/%m/%d %H:%M:%S") # datetime format
df_consumer$iRent_8 <- as.numeric(df_consumer$iRent_8) # dollor int format
df_consumer$iRent_9 = as.POSIXct(df_consumer$iRent_9, format="%Y/%m/%d %H:%M:%S") # datetime format
df_consumer$iRent_10 = as.POSIXct(df_consumer$iRent_10, format="%Y/%m/%d %H:%M:%S") # datetime format
df_consumer$iRent_11 <- as.double(df_consumer$iRent_11) # mile double format
df_consumer$iRent_13 <- as.factor(df_consumer$iRent_13)
df_consumer$iRent_14 <- as.factor(df_consumer$iRent_14)
df_consumer$iRent_15 <- as.factor(df_consumer$iRent_15)

# Find code km&moeny_range_formet #

# --------------------------- #
# data( car and motor ) filter 1
df_consumer_motor <- df_consumer[df_consumer$iRent_14 == "motor",]
df_consumer_car <- df_consumer[df_consumer$iRent_14 == "car",]

# --------------------------- #
# data( male and female ) filter 2 (might not used)
df_consumer_motor_male <- df_consumer_motor[df_consumer_motor$iRent_3 == "M",]
df_consumer_motor_female <- df_consumer_motor[df_consumer_motor$iRent_3 == "F",]
df_consumer_car_male <- df_consumer_car[df_consumer_car$iRent_3 == "M",]
df_consumer_car_female <- df_consumer_car[df_consumer_car$iRent_3 == "F",]

# --------------------------- #
# iRent_1	iRent客戶編號 @待
# iRent_2	客戶年齡區間
# iRent_3	客戶性別
# iRent_4	客戶居住城市
# iRent_5	客戶居住區域 @ 待
# iRent_6	租車訂單號碼 @待
# iRent_7	預約時間 @待
# iRent_8	租用金額
# iRent_9	租用開始時間
# iRent_10	租用結束時間 @待
# iRent_11	使用里程數
# iRent_12	租用車號 
# iRent_13	租用車種
# iRent_14	租用車型
# iRent_15	車輛出廠年月

# --------------------------- #
# --------------------------- #
# --------------------------- # 
# iRent_2 客戶年齡區間

## config
Factor_column = "iRent_2"
Factor_name = "Age Distribution"
folder_name = "2客戶年齡區間"

## Motor
draw_ggplot(vehicle_type = "Motor", type = "fill", Factor_column, Factor_name, folder_name)
draw_ggplot(vehicle_type = "Motor", type = "dodge", Factor_column, Factor_name, folder_name)

## Car
draw_ggplot(vehicle_type = "Car", type = "fill", Factor_column, Factor_name, folder_name)
draw_ggplot(vehicle_type = "Car", type = "dodge", Factor_column, Factor_name, folder_name)

# --------------------------- #
# iRent_4	客戶居住城市

## config
Factor_column = "iRent_4"
Factor_name = "City Distribution"
folder_name = "4客戶居住城市"

## Motor
draw_ggplot(vehicle_type = "Motor", type = "fill", Factor_column, Factor_name, folder_name)
draw_ggplot(vehicle_type = "Motor", type = "dodge", Factor_column, Factor_name, folder_name)

## Car
draw_ggplot(vehicle_type = "Car", type = "fill", Factor_column, Factor_name, folder_name)
draw_ggplot(vehicle_type = "Car", type = "dodge", Factor_column, Factor_name, folder_name)

# --------------------------- #
# iRent_13	租用車種

## config
Factor_column = "iRent_13"
Factor_name = "Car Pattern"
folder_name = "13租用車種"

## Motor
draw_ggplot(vehicle_type = "Motor", type = "fill", Factor_column, Factor_name, folder_name)
draw_ggplot(vehicle_type = "Motor", type = "dodge", Factor_column, Factor_name, folder_name)

## Car
draw_ggplot(vehicle_type = "Car", type = "fill", Factor_column, Factor_name, folder_name)
draw_ggplot(vehicle_type = "Car", type = "dodge", Factor_column, Factor_name, folder_name)
# --------------------------- #
# iRent_15	車輛出廠年月

# remember to do   '''coord_flip()''' 

## config
Factor_column = "iRent_15"
Factor_name = "Leave Time"
folder_name = "15車輛出廠年月"

## Motor
draw_ggplot(vehicle_type = "Motor", type = "fill", Factor_column, Factor_name, folder_name)
draw_ggplot(vehicle_type = "Motor", type = "dodge", Factor_column, Factor_name, folder_name)

## Car
draw_ggplot(vehicle_type = "Car", type = "fill", Factor_column, Factor_name, folder_name)
draw_ggplot(vehicle_type = "Car", type = "dodge", Factor_column, Factor_name, folder_name)

# --------------------------- #
# --------------------------- #
# 有進行里程數區塊更改
# iRent_11	使用里程數區間

## config
Factor_column = "km"
Factor_name = "Km Range"
folder_name = "11使用里程數"

## Motor
draw_ggplot(vehicle_type = "Motor", type = "fill", Factor_column, Factor_name, folder_name)
draw_ggplot(vehicle_type = "Motor", type = "dodge", Factor_column, Factor_name, folder_name)

## Car
draw_ggplot(vehicle_type = "Car", type = "fill", Factor_column, Factor_name, folder_name)
draw_ggplot(vehicle_type = "Car", type = "dodge", Factor_column, Factor_name, folder_name)
# --------------------------- #
# 有進行金額區塊更改
# iRent_8	租用金額區間

## config
Factor_column = "money"
Factor_name = "Money Range"
folder_name = "8租用金額區間"

## Motor
draw_ggplot(vehicle_type = "Motor", type = "fill", Factor_column, Factor_name, folder_name)
draw_ggplot(vehicle_type = "Motor", type = "dodge", Factor_column, Factor_name, folder_name)

## Car
draw_ggplot(vehicle_type = "Car", type = "fill", Factor_column, Factor_name, folder_name)
draw_ggplot(vehicle_type = "Car", type = "dodge", Factor_column, Factor_name, folder_name)
# --------------------------- #
# --------------------------- #
# --------------------------- #


# TESTing time

#test datetime - datetime using as.POSIXct
a = df_consumer_car_female$iRent_10[1] - df_consumer_car_female$iRent_9[1]
class(a)
a

class(df_consumer$money)
