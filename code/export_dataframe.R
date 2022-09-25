# export dataframe
write.csv(df_consumer, str_c(path_for_plot,"df_add_km&money&timediff.csv"), row.names = FALSE, fileEncoding ="Big5")
write.csv(df_consumer_motor, str_c(path_for_plot,"df_motor_add_km&money&timediff.csv"), row.names = FALSE, fileEncoding ="Big5")
write.csv(df_consumer_car, str_c(path_for_plot,"df_car_add_km&money&timediff.csv"), row.names = FALSE, fileEncoding ="Big5")
