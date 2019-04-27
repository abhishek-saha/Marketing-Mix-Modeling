setwd("C:\\Users\\sahaa\\OneDrive - Emory University\\Spring - Marketing Analytics\\Project - Marketing Mix")

library(data.table)

promo_data <- fread("promo_ad.csv")

summary(promo_data)

TV_data <- promo_data[vehicle == "TV"]
TV_data$year <- year(as.Date(TV_data$tran_wk))
TV_data$week <- week(as.Date(TV_data$tran_wk))

half_life <- 8

tv_alpha <- 1 - (0.5)^(1/half_life)

# 2016
base_adstock <- 0
adstock_grp_df <- data.frame(year = 2016, week_counter = 0, adstock_grp = base_adstock, reach=0)
year <- 2016
TV_data_2016 <- TV_data[year==2016]
for(i in 1:52){
  tv_week_data <- TV_data_2016[week == i]
  if(nrow(tv_week_data)>0){
    new_grp <- tv_week_data$amount
    new_adstock <- tv_alpha * new_grp + (1 - tv_alpha) * base_adstock
    #adstock_grp_df <- rbind(adstock_grp_df, c(0,base_adstock))
  }
  else{
    new_adstock <- (1 - tv_alpha) * base_adstock
  }
  reach <- 0.95 * (1 - exp(-0.020 * new_adstock) )
  new_df <- c(year, i, new_adstock, reach)
  adstock_grp_df <- rbind(adstock_grp_df, new_df)
  base_adstock <- new_adstock
}

# 2017
year <- 2017
TV_data_2017 <- TV_data[year==2017]
for(i in 1:53){
  tv_week_data <- TV_data_2017[week == i]
  if(nrow(tv_week_data)>0){
    new_grp <- tv_week_data$amount
    new_adstock <- tv_alpha * new_grp + (1 - tv_alpha) * base_adstock
    #adstock_grp_df <- rbind(adstock_grp_df, c(0,base_adstock))
  }
  else{
    new_adstock <- (1 - tv_alpha) * base_adstock
  }
  reach <- 0.95 * (1 - exp(-0.020 * new_adstock) )
  new_df <- c(year, i, new_adstock, reach)
  adstock_grp_df <- rbind(adstock_grp_df, new_df)
  base_adstock <- new_adstock
}
adstock_grp_df <- data.table(adstock_grp_df)
adstock_grp_df <- adstock_grp_df[adstock_grp_df$week_counter != 0]


# Radio

Radio_data <- promo_data[vehicle == "Radio"]
Radio_data$year <- year(as.Date(Radio_data$tran_wk))
Radio_data$week <- week(as.Date(Radio_data$tran_wk))

half_life <- 4

radio_alpha <- 1 - (0.5)^(1/half_life)

# 2016
base_adstock <- 0
radio_adstock_grp_df <- data.frame(year = 2016, week_counter = 0, adstock_grp = base_adstock, reach=0)
year <- 2016
radio_data_2016 <- Radio_data[year==2016]
for(i in 1:52){
  radio_week_data <- radio_data_2016[week == i]
  if(nrow(radio_week_data)>0){
    new_grp <- radio_week_data$amount
    new_adstock <- radio_alpha * new_grp + (1 - radio_alpha) * base_adstock
  }
  else{
    new_adstock <- (1 - radio_alpha) * base_adstock
  }
  reach <- 0.90 * (1 - exp(-0.025 * new_adstock) )
  new_df <- c(year, i, new_adstock, reach)
  radio_adstock_grp_df <- rbind(radio_adstock_grp_df, new_df)
  base_adstock <- new_adstock
}

# 2017
year <- 2017
radio_data_2017 <- Radio_data[year==2017]
for(i in 1:53){
  radio_week_data <- radio_data_2017[week == i]
  if(nrow(radio_week_data)>0){
    new_grp <- radio_week_data$amount
    new_adstock <- radio_alpha * new_grp + (1 - radio_alpha) * base_adstock
    #adstock_grp_df <- rbind(adstock_grp_df, c(0,base_adstock))
  }
  else{
    new_adstock <- (1 - radio_alpha) * base_adstock
  }
  reach <- 0.90 * (1 - exp(-0.025 * new_adstock) )
  new_df <- c(year, i, new_adstock, reach)
  radio_adstock_grp_df <- rbind(radio_adstock_grp_df, new_df)
  base_adstock <- new_adstock
}
radio_adstock_grp_df <- data.table(radio_adstock_grp_df)
radio_adstock_grp_df <- radio_adstock_grp_df[radio_adstock_grp_df$week_counter != 0]

write.csv(adstock_grp_df, "TV_reach.csv")
write.csv(radio_adstock_grp_df, "Radio_reach.csv")
