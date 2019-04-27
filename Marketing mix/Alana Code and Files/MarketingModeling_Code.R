library(data.table)
setwd('D:/Emory/Marketing/Marketing Modeling/SanMiguel')

holiday = fread("holiday.csv")
ad = fread('promo_ad.csv')
seasonality = fread('seasonality.csv')
trans_supp = fread('transaction_table_supp.csv')
prod_supp = fread('product_table_supp.csv')
#trans = fread('transaction_table.csv')
#prod = fread('product_table.csv')

holiday$holiday_index = 1
holiday$tran_wk = as.Date(holiday$tran_wk)

seasonality$tran_wk = as.Date(seasonality$tran_wk)

# add year, week, and discount per unit to the original transaction table
trans_supp$tran_dt = as.Date(trans_supp$tran_dt)
# the transaction week is represented by the Sunday
trans_supp$tran_wk = as.Date(cut(trans_supp$tran_dt+1,"week"))-1
# unit discount amount
trans_supp$discount = trans_supp$tran_prod_discount_amt/trans_supp$tran_prod_sale_qty

# merge in holiday_index and seas_index
trans_supp[seasonality[trans_supp,seas_index,on=.(tran_wk=tran_wk),by=.EACHI],
           seas_index := seas_index, on=.(tran_wk=tran_wk)]
trans_supp[holiday[trans_supp,holiday_index,on=.(tran_wk=tran_wk),by=.EACHI],
           holiday_index := holiday_index, on=.(tran_wk=tran_wk)]
trans_supp[is.na(trans_supp)] = 0

# function for mode
mode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# product_1 cleaning---------------------
prod_1 = trans_supp[prod_id == 138936951,]
prod_1_weekly = prod_1[,c("weekly_sales","weekly_paid","weekly_qty","shelf_price","discount_majority") := 
         list(sum(tran_prod_sale_amt),sum(tran_prod_paid_amt),sum(tran_prod_sale_qty),
              mode(prod_unit_price), mode(discount)), by=.(tran_wk)]
# get the weekly sales, paid, qty, mode shelf price, mode discount per unit
prod_1_weekly = unique(prod_1_weekly[,c("prod_id","tran_wk","weekly_sales","weekly_paid",
                                        "weekly_qty","shelf_price","discount_majority",
                                        "seas_index","holiday_index")])
prod_1_weekly$year = year(prod_1_weekly$tran_wk)
prod_1_weekly$week = week(prod_1_weekly$tran_wk)

# product_2 cleaning-----------------------
prod_2 = trans_supp[prod_id == 138936952,]
prod_2_weekly = prod_2[,c("weekly_sales","weekly_paid","weekly_qty","shelf_price","discount_majority") := 
                         list(sum(tran_prod_sale_amt),sum(tran_prod_paid_amt),sum(tran_prod_sale_qty),
                              mode(prod_unit_price), mode(discount)), by=.(tran_wk)]
# get the weekly sales, paid, qty, mode shelf price, mode discount per unit
prod_2_weekly = unique(prod_2_weekly[,c("prod_id","tran_wk","weekly_sales","weekly_paid",
                                        "weekly_qty","shelf_price","discount_majority",
                                        "seas_index","holiday_index")])
prod_2_weekly$year = year(prod_2_weekly$tran_wk)
prod_2_weekly$week = week(prod_2_weekly$tran_wk)

# product_3 cleaning----------------------
prod_3= trans_supp[prod_id == 138936953,]
prod_3_weekly = prod_3[,c("weekly_sales","weekly_paid","weekly_qty","shelf_price","discount_majority") := 
                         list(sum(tran_prod_sale_amt),sum(tran_prod_paid_amt),sum(tran_prod_sale_qty),
                              mode(prod_unit_price), mode(discount)), by=.(tran_wk)]
# get the weekly sales, paid, qty, mode shelf price, mode discount per unit
prod_3_weekly = unique(prod_3_weekly[,c("prod_id","tran_wk","weekly_sales","weekly_paid",
                                        "weekly_qty","shelf_price","discount_majority",
                                        "seas_index","holiday_index")])
# only has 102 weeks of data
prod_3_weekly$year = year(prod_3_weekly$tran_wk)
prod_3_weekly$week = week(prod_3_weekly$tran_wk)

# merge in the TV reach and Radio Reach ---------------

tv_reach = fread("TV_reach.csv")
radio_reach = fread("radio_reach.csv")

# these two vehicles apply to all products

prod_1_weekly[tv_reach[prod_1_weekly,reach,on=.(year=year, week_counter=week),by=.EACHI],
           tv_reach := reach, on=.(year=year, week=week_counter)]
prod_1_weekly[radio_reach[prod_1_weekly,reach,on=.(year=year, week_counter=week),by=.EACHI],
              radio_reach := reach, on=.(year=year, week=week_counter)]

prod_2_weekly[tv_reach[prod_2_weekly,reach,on=.(year=year, week_counter=week),by=.EACHI],
              tv_reach := reach, on=.(year=year, week=week_counter)]
prod_2_weekly[radio_reach[prod_2_weekly,reach,on=.(year=year, week_counter=week),by=.EACHI],
              radio_reach := reach, on=.(year=year, week=week_counter)]

prod_3_weekly[tv_reach[prod_3_weekly,reach,on=.(year=year, week_counter=week),by=.EACHI],
              tv_reach := reach, on=.(year=year, week=week_counter)]
prod_3_weekly[radio_reach[prod_3_weekly,reach,on=.(year=year, week_counter=week),by=.EACHI],
              radio_reach := reach, on=.(year=year, week=week_counter)]

# looking at ads for each product--------

ad$tran_wk = as.Date(ad$tran_wk)

## product 1
ad_1 = ad[ad$prod_assoc==138936951,]
# only Flyer
prod_1_weekly[ad_1[prod_1_weekly,amount,on=.(tran_wk=tran_wk),by=.EACHI],
              flyer := amount, on=.(tran_wk=tran_wk)]

## product 2
ad_2 = ad[ad$prod_assoc==138936952,]
ad_2_flyer = ad_2[ad_2$vehicle=="Flyer",]
ad_2_display = ad_2[ad_2$vehicle=="Store Display",]
# flyer and store display
prod_2_weekly[ad_2_flyer[prod_2_weekly,amount,on=.(tran_wk=tran_wk),by=.EACHI],
              flyer := amount, on=.(tran_wk=tran_wk)]
prod_2_weekly[ad_2_display[prod_2_weekly,amount,on=.(tran_wk=tran_wk),by=.EACHI],
              display := amount, on=.(tran_wk=tran_wk)]

## product 3
ad_3 = ad[ad$prod_assoc==138936953,]
ad_3_flyer = ad_3[ad_3$vehicle=="Flyer",]
ad_3_display = ad_3[ad_3$vehicle=="Store Display",]
# flyer and store display
prod_3_weekly[ad_3_flyer[prod_3_weekly,amount,on=.(tran_wk=tran_wk),by=.EACHI],
              flyer := amount, on=.(tran_wk=tran_wk)]
prod_3_weekly[ad_3_display[prod_3_weekly,amount,on=.(tran_wk=tran_wk),by=.EACHI],
              display := amount, on=.(tran_wk=tran_wk)]

# Paid Search, Web Display, Email are for all products -------------

ad_email = ad[ad$vehicle=="Email",]
ad_search = ad[ad$vehicle=="Paid Search",]
ad_web = ad[ad$vehicle=="Web Display",]

# merge them back in the product transaction tables
prod_1_weekly[ad_email[prod_1_weekly,amount,on=.(tran_wk=tran_wk),by=.EACHI],
              email := amount, on=.(tran_wk=tran_wk)]
prod_2_weekly[ad_email[prod_2_weekly,amount,on=.(tran_wk=tran_wk),by=.EACHI],
              email := amount, on=.(tran_wk=tran_wk)]
prod_3_weekly[ad_email[prod_3_weekly,amount,on=.(tran_wk=tran_wk),by=.EACHI],
              email := amount, on=.(tran_wk=tran_wk)]

prod_1_weekly[ad_search[prod_1_weekly,amount,on=.(tran_wk=tran_wk),by=.EACHI],
              paid_search := amount, on=.(tran_wk=tran_wk)]
prod_2_weekly[ad_search[prod_2_weekly,amount,on=.(tran_wk=tran_wk),by=.EACHI],
              paid_search := amount, on=.(tran_wk=tran_wk)]
prod_3_weekly[ad_search[prod_3_weekly,amount,on=.(tran_wk=tran_wk),by=.EACHI],
              paid_search := amount, on=.(tran_wk=tran_wk)]

prod_1_weekly[ad_web[prod_1_weekly,amount,on=.(tran_wk=tran_wk),by=.EACHI],
              web := amount, on=.(tran_wk=tran_wk)]
prod_2_weekly[ad_web[prod_2_weekly,amount,on=.(tran_wk=tran_wk),by=.EACHI],
              web := amount, on=.(tran_wk=tran_wk)]
prod_3_weekly[ad_web[prod_3_weekly,amount,on=.(tran_wk=tran_wk),by=.EACHI],
              web := amount, on=.(tran_wk=tran_wk)]

prod_1_weekly[is.na(prod_1_weekly)] = 0
prod_2_weekly[is.na(prod_2_weekly)] = 0
prod_3_weekly[is.na(prod_3_weekly)] = 0

# write out csv files --------------
write.csv(prod_1_weekly,file="product_1_weekly.csv",row.names = FALSE)
write.csv(prod_2_weekly,file="product_2_weekly.csv",row.names = FALSE)
write.csv(prod_3_weekly,file="product_3_weekly.csv",row.names = FALSE)
