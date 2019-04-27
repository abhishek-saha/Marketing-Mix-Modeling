library(data.table)
library(fastDummies)
library(baseline)
library(Metrics)
library(plyr)
library(dplyr)
library(arules)
library(corrplot)
options(max.print = 1000000000)


holiday = fread("SanMiguel/holiday.csv")
ad = fread('promo_ad.csv')
seasonality = fread('SanMiguel/seasonality.csv')
trans_supp = fread('SanMiguel/transaction_table_supp.csv')
prod_supp = fread('SanMiguel/product_table_supp.csv')
data = fread('transaction_table.csv')
products = fread('product_table.csv')



data = rbind(data, trans_supp)
products = rbind(products, prod_supp)

data <- merge(data, products, by='prod_id', all=TRUE)

data$tran_dt = as.Date(data$tran_dt)
data$tran_wk = as.Date(cut(data$tran_dt+1,"week"))-1

holiday$holiday_index = 1
holiday$tran_wk = as.Date(holiday$tran_wk)
holiday <- holiday[!((holiday == 'PrLIBERTY') & (tran_wk == '2017-04-16'))]

seasonality$tran_wk = as.Date(seasonality$tran_wk)

# add year, week, and discount per unit to the original transaction table
trans_supp$tran_dt = as.Date(trans_supp$tran_dt)
# the transaction week is represented by the Sunday
trans_supp$tran_wk = as.Date(cut(trans_supp$tran_dt+1,"week"))-1
# unit discount amount
trans_supp$discount = trans_supp$tran_prod_discount_amt/trans_supp$tran_prod_sale_qty
sum_sales = sum(trans_supp$tran_prod_sale_qty)

trans_supp <- trans_supp[(trans_supp$tran_wk != '2015-12-27') &  (trans_supp$tran_wk !='2017-12-31')]


#trans_supp$discount_percent = trans_supp$tran_prod_discount_amt*trans_supp$tran_prod_sale_qty/(trans_supp$tran_prod_sale_amt)
#trans_supp$avg_unit_price = trans_supp$prod_unit_price*trans_supp$tran_prod_sale_qty

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
prod_1_weekly = prod_1[,c("weekly_sales","weekly_paid","weekly_qty","avg_shelf_price",'discount_percent') := 
         list(sum(tran_prod_sale_amt),sum(tran_prod_paid_amt),sum(tran_prod_sale_qty),
              sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty),  sum(tran_prod_discount_amt)/sum(tran_prod_sale_amt)), by=.(tran_wk)]
# get the weekly sales, paid, qty, mode shelf price, mode discount per unit
prod_1_weekly = unique(prod_1_weekly[,c("prod_id","tran_wk","weekly_sales","weekly_paid",
                                        "weekly_qty","avg_shelf_price",'discount_percent',
                                        "seas_index","holiday_index")])
prod_1_weekly$year = year(prod_1_weekly$tran_wk)
prod_1_weekly$week = week(prod_1_weekly$tran_wk)

# product_2 cleaning-----------------------
prod_2 = trans_supp[prod_id == 138936952,]
prod_2_weekly = prod_2[,c("weekly_sales","weekly_paid","weekly_qty","avg_shelf_price",'discount_percent') := 
                         list(sum(tran_prod_sale_amt),sum(tran_prod_paid_amt),sum(tran_prod_sale_qty),
                              sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty),  sum(tran_prod_discount_amt)/sum(tran_prod_sale_amt)), by=.(tran_wk)]
# get the weekly sales, paid, qty, mode shelf price, mode discount per unit
prod_2_weekly = unique(prod_2_weekly[,c("prod_id","tran_wk","weekly_sales","weekly_paid",
                                        "weekly_qty","avg_shelf_price",'discount_percent',
                                        "seas_index","holiday_index")])
prod_2_weekly$year = year(prod_2_weekly$tran_wk)
prod_2_weekly$week = week(prod_2_weekly$tran_wk)

# product_3 cleaning----------------------
prod_3= trans_supp[prod_id == 138936953,]
prod_3_weekly = prod_3[,c("weekly_sales","weekly_paid","weekly_qty","avg_shelf_price", 'discount_percent') := 
                         list(sum(tran_prod_sale_amt),sum(tran_prod_paid_amt),sum(tran_prod_sale_qty),
                              sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty), sum(tran_prod_discount_amt)/sum(tran_prod_sale_amt)), by=.(tran_wk)]
# get the weekly sales, paid, qty, mode shelf price, mode discount per unit
prod_3_weekly = unique(prod_3_weekly[,c("prod_id","tran_wk","weekly_sales","weekly_paid",
                                        "weekly_qty","avg_shelf_price",'discount_percent',
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
#write.csv(prod_1_weekly,file="product_1_weekly.csv",row.names = FALSE)
#write.csv(prod_2_weekly,file="product_2_weekly.csv",row.names = FALSE)
#write.csv(prod_3_weekly,file="product_3_weekly.csv",row.names = FALSE)




# histograms --------------------------------------------------------------

hist(prod_1_weekly$tv_reach)
hist(prod_1_weekly$radio_reach^(1/2))
hist(prod_1_weekly$avg_shelf_price)
hist(prod_2_weekly$avg_shelf_price)
hist(prod_3_weekly$avg_shelf_price)
hist(prod_1_weekly$seas_index^(1/3), breaks = 15)

# substitutes -------------------------------------------------------------


subcat = unique(prod_supp$category_desc)

all_sub = list()
sub = data[category_desc == subcat[1],]
# create a list of products bought in one transaction within the same category
transactionData = ddply(sub,c("cust_id","tran_dt"),
                        function(df1)paste(df1$prod_id,
                                           collapse = ","))
transactionData$cust_id = NULL
transactionData$tran_dt = NULL
colnames(transactionData) = c("items")

# get the table of purchases by transaction including co-purchases
# transform it to the transaction table form
transactionData = data.frame(lapply(transactionData,as.factor))
items = strsplit(as.character(transactionData$items), ",")
tr = as(items,"transactions")

# generate the association rule to find the products that have been purchased together
rules = apriori(tr, parameter = list(supp=0, conf=0,minlen=2, maxlen=2))
# extract the results showing lift
out = capture.output(inspect(rules))
rhs = gsub("[^{]+\\{([^}]*)\\}[^{]+\\{([^}]*)\\}.*", "\\2", out)[-1]
lhs = gsub("[^{]+\\{([^}]*)\\}[^{]+\\{([^}]*)\\}.*", "\\1", out)[-1]
lift = vector()
for (j in (2:length(out)) ){
  lift[j-1] = substr(out[j],61,64)
}
lift_value = as.data.frame(lift)

co_buy = as.data.frame(cbind(lhs, rhs, lift))
sub_list = as.data.table(co_buy[lift<1,])
sub_list[,list(rhs),by=lhs]
colnames(sub_list) = c("item","substitute","lift")
all_sub[[1]] = sub_list


all_sub_data = do.call(rbind, all_sub)
all_sub_data$lift = NULL
all_sub_data[,avg_price := mean(sub[sub$prod_id == substitute]$prod_unit_price), by = substitute]
all_sub_data[,item_avg_price := mean(sub[sub$prod_id == item]$prod_unit_price), by = item]

test =  all_sub_data[all_sub_data$item %in% c('138936951','138936952','138936953')]
test$dist <- abs((test$avg_price - test$item_avg_price)/test$item_avg_price)

subs_1 <- test[(test$item == '138936951') & (test$dist < .5)]
subs_2 <- test[(test$item == '138936952') & (test$dist < .5)]
subs_3 <- test[(test$item == '138936953') & (test$dist < .5)]


sub1_avg_unit <- data[data$prod_id %in% subs_1$substitute,sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty) ,by = .(tran_wk)]
sub2_avg_unit <- data[data$prod_id %in% subs_2$substitute,sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty) ,by = .(tran_wk)]
sub3_avg_unit <- data[data$prod_id %in% subs_3$substitute,sum(tran_prod_sale_amt)/sum(tran_prod_sale_qty) ,by = .(tran_wk)]
sub1_avg_unit <- sub1_avg_unit[(sub1_avg_unit$tran_wk != '2015-12-27') &  (sub1_avg_unit$tran_wk !='2017-12-31')]
sub2_avg_unit <- sub2_avg_unit[(sub2_avg_unit$tran_wk != '2015-12-27') &  (sub2_avg_unit$tran_wk !='2017-12-31')]
sub3_avg_unit <- sub3_avg_unit[(sub3_avg_unit$tran_wk != '2015-12-27') &  (sub3_avg_unit$tran_wk !='2017-12-31')]

prod_1_weekly <- merge(prod_1_weekly, sub1_avg_unit, by = 'tran_wk')
colnames(prod_1_weekly)[18] <- 'substitute_avg_price'
prod_2_weekly <- merge(prod_2_weekly, sub2_avg_unit, by = 'tran_wk')
colnames(prod_2_weekly)[19] <- 'substitute_avg_price'
prod_3_weekly <- merge(prod_3_weekly, sub3_avg_unit, by = 'tran_wk')
colnames(prod_3_weekly)[19] <- 'substitute_avg_price'
# modeling ----------------------------------------------------------------


#adding actual holiday back in so it can be individually accounted for
prod_1_weekly <- merge(prod_1_weekly, holiday, by = 'tran_wk', all.x = TRUE)
prod_1_weekly <- prod_1_weekly[,1:19]
prod_1_weekly <- dummy_cols(prod_1_weekly, select_columns = c('holiday'))


prod_2_weekly <- merge(prod_2_weekly, holiday, by = 'tran_wk', all.x = TRUE)
prod_2_weekly <- prod_2_weekly[,1:20]
prod_2_weekly <- dummy_cols(prod_2_weekly, select_columns = c('holiday'))


prod_3_weekly <- merge(prod_3_weekly, holiday, by = 'tran_wk', all.x = TRUE)
prod_3_weekly <- prod_3_weekly[,1:20]
prod_3_weekly <- dummy_cols(prod_3_weekly, select_columns = c('holiday'))



#holidays to remove
remove_holidays <- c('holiday_POPEVISIT','holiday_PrLIBERTY','holiday_PrEASTER','holiday_ALLSAINTS','holiday_IMMACULATE',
                     'holiday_EASTER','holiday_RESTORATION','holiday_PrASSUMPTION','holiday_CORPUS','holiday_PrXMAS')

model_prod1 <- lm(log(weekly_qty) ~ avg_shelf_price + discount_percent +   seas_index + tv_reach + radio_reach + flyer + 
                    email + paid_search + web +
                    holiday_NEWYEAR+ holiday_CARNIVAL +
                    holiday_LIBERTY+ holiday_LABOR  + holiday_PORTUGAL+
                    holiday_ASSUMPTION+ holiday_REPUBLIC  +
                    holiday_XMAS +substitute_avg_price , data = prod_1_weekly)
summary(model_prod1)

model_prod2 <- lm(log(weekly_qty) ~ avg_shelf_price + discount_percent +  seas_index + tv_reach + radio_reach + flyer +
                    display+ email + paid_search + web +
                    holiday_NEWYEAR+ holiday_CARNIVAL +
                    holiday_LIBERTY+ holiday_LABOR  + holiday_PORTUGAL+
                    holiday_ASSUMPTION+ holiday_REPUBLIC  +
                    holiday_XMAS +substitute_avg_price, data = prod_2_weekly)
summary(model_prod2)

model_prod3 <- lm(log(weekly_qty) ~ avg_shelf_price + discount_percent +  seas_index + tv_reach + radio_reach + flyer +
                    display+ email + paid_search + web +
                    holiday_NEWYEAR+ holiday_CARNIVAL +
                    holiday_LIBERTY+ holiday_LABOR  + holiday_PORTUGAL+
                    holiday_ASSUMPTION+ holiday_REPUBLIC  +
                     holiday_XMAS+ substitute_avg_price, data = prod_3_weekly)
summary(model_prod3)

# next steps --------------------------------------------------------------


prod1_coeffs <- data.frame(model_prod1$coefficients)
prod1_coeffs_t <- transpose(prod1_coeffs)
colnames(prod1_coeffs_t) <- paste('coef',rownames(prod1_coeffs), sep = '_')
prod1_coeffs_t <- prod1_coeffs_t[rep(1, each = nrow(prod_1_weekly)),]
prod_1_final <- cbind(prod_1_weekly, prod1_coeffs_t)

prod2_coeffs <- data.frame(model_prod2$coefficients)
prod2_coeffs_t <- transpose(prod2_coeffs)
colnames(prod2_coeffs_t) <- paste('coef',rownames(prod2_coeffs), sep = '_')
prod2_coeffs_t <- prod2_coeffs_t[rep(1, each = nrow(prod_2_weekly)),]
prod_2_final <- cbind(prod_2_weekly, prod2_coeffs_t)

prod3_coeffs <- data.frame(model_prod3$coefficients)
prod3_coeffs_t <- transpose(prod3_coeffs)
colnames(prod3_coeffs_t) <- paste('coef',rownames(prod3_coeffs), sep = '_')
prod3_coeffs_t <- prod3_coeffs_t[rep(1, each = nrow(prod_3_weekly)),]
prod_3_final<- cbind(prod_3_weekly, prod3_coeffs_t)


prod_1_final[,yhat := exp(`coef_(Intercept)` + coef_avg_shelf_price*avg_shelf_price + coef_discount_percent*discount_percent +
               coef_seas_index*seas_index + coef_tv_reach*tv_reach + coef_radio_reach*radio_reach+
               coef_flyer*flyer + coef_email*email + coef_paid_search*paid_search + coef_web*web + coef_holiday_NEWYEAR*holiday_NEWYEAR+
               coef_holiday_CARNIVAL*holiday_CARNIVAL + coef_holiday_LIBERTY*holiday_LIBERTY+ coef_holiday_LABOR*holiday_LABOR+
               coef_holiday_PORTUGAL*holiday_PORTUGAL + coef_holiday_ASSUMPTION*holiday_ASSUMPTION +
               coef_holiday_XMAS*holiday_XMAS + coef_holiday_REPUBLIC*holiday_REPUBLIC +
                 coef_substitute_avg_price*substitute_avg_price) , by = 'tran_wk'] 
rmse(prod_1_final$weekly_qty, prod_1_final$yhat)
mae(prod_1_final$weekly_qty, prod_1_final$yhat)
mape(prod_1_final$weekly_qty, prod_1_final$yhat)


prod_2_final[,yhat := exp(`coef_(Intercept)` + coef_avg_shelf_price*avg_shelf_price + coef_discount_percent*discount_percent +
                            coef_seas_index*seas_index + coef_tv_reach*tv_reach + coef_radio_reach*radio_reach+coef_display*display +
                            coef_flyer*flyer + coef_email*email + coef_paid_search*paid_search + coef_web*web + coef_holiday_NEWYEAR*holiday_NEWYEAR+
                            coef_holiday_CARNIVAL*holiday_CARNIVAL + coef_holiday_LIBERTY*holiday_LIBERTY+ coef_holiday_LABOR*holiday_LABOR+
                             coef_holiday_PORTUGAL*holiday_PORTUGAL + coef_holiday_ASSUMPTION*holiday_ASSUMPTION +
                            coef_holiday_XMAS*holiday_XMAS + coef_holiday_REPUBLIC*holiday_REPUBLIC 
                          +coef_substitute_avg_price*substitute_avg_price) , by = 'tran_wk'] 
rmse(prod_2_final$weekly_qty, prod_2_final$yhat)
mae(prod_2_final$weekly_qty, prod_2_final$yhat)
mape(prod_2_final$weekly_qty, prod_2_final$yhat)


prod_3_final[,yhat := exp(`coef_(Intercept)` + coef_avg_shelf_price*avg_shelf_price + coef_discount_percent*discount_percent +
                            coef_seas_index*seas_index + coef_tv_reach*tv_reach + coef_radio_reach*radio_reach+ coef_display*display +
                            coef_flyer*flyer + coef_email*email + coef_paid_search*paid_search + coef_web*web + coef_holiday_NEWYEAR*holiday_NEWYEAR+
                            coef_holiday_CARNIVAL*holiday_CARNIVAL + coef_holiday_LIBERTY*holiday_LIBERTY+ coef_holiday_LABOR*holiday_LABOR+
                             coef_holiday_PORTUGAL*holiday_PORTUGAL + coef_holiday_ASSUMPTION*holiday_ASSUMPTION +
                            coef_holiday_XMAS*holiday_XMAS + coef_holiday_REPUBLIC*holiday_REPUBLIC + 
                            coef_substitute_avg_price*substitute_avg_price) , by = 'tran_wk']
rmse(prod_3_final$weekly_qty, prod_3_final$yhat)
mae(prod_3_final$weekly_qty, prod_3_final$yhat)
mape(prod_3_final$weekly_qty, prod_3_final$yhat)


corrplot(cor(prod_1_weekly[,c(3:9,12:18,20:38)]))
corrplot(cor(prod_2_weekly[,c(3:9,12:18,21:39)]))
corrplot(cor(prod_3_weekly[,c(3:9,12:18,21:39)]))



# calculating dueTos ------------------------------------------------------



list_of_vars_in_model1 <- c('avg_shelf_price','discount_percent','seas_index','tv_reach','radio_reach','flyer',
                              'email','paid_search','web','holiday_NEWYEAR','holiday_CARNIVAL',
                              'holiday_LIBERTY','holiday_LABOR','holiday_PORTUGAL',
                              'holiday_ASSUMPTION','holiday_REPUBLIC','holiday_XMAS','substitute_avg_price' )

x_base1 <- c(mean(prod_1_weekly$avg_shelf_price),mean(prod_1_weekly$discount_percent),
             mean(prod_1_weekly$seas_index),0,0,0,0,0,0,0,0,0,0,0,0,0,0,mean(prod_1_weekly$substitute_avg_price))

 write.csv(dueto, 'duetos_prod1.csv')
# prod1_coeffs <- data.frame(model_prod1$coefficients)
# prod1_coeffs_t <- transpose(prod1_coeffs)
# colnames(prod1_coeffs_t) <- paste('coef',rownames(prod1_coeffs), sep = '_')
# prod1_coeffs_t <- prod1_coeffs_t[rep(1, each = nrow(prod_1_weekly)),]
# prod_1_final <- cbind(prod_1_weekly, prod1_coeffs_t)
# 
# prod_1_final[,yhat := exp(`coef_(Intercept)` + coef_avg_shelf_price*avg_shelf_price + coef_discount_percent*discount_percent +
#                             coef_seas_index*seas_index + coef_tv_reach*tv_reach + coef_radio_reach*radio_reach+
#                             coef_flyer*flyer + coef_email*email + coef_paid_search*paid_search + coef_web*web + coef_holiday_NEWYEAR*holiday_NEWYEAR+
#                             coef_holiday_CARNIVAL*holiday_CARNIVAL + coef_holiday_LIBERTY*holiday_LIBERTY+ coef_holiday_LABOR*holiday_LABOR+
#                             coef_holiday_PORTUGAL*holiday_PORTUGAL + coef_holiday_ASSUMPTION*holiday_ASSUMPTION +
#                             coef_holiday_XMAS*holiday_XMAS + coef_holiday_REPUBLIC*holiday_REPUBLIC +
#                             coef_substitute_avg_price*substitute_avg_price) , by = 'tran_wk'] 


for (i in 18:18){
  var <- list_of_vars_in_model1[i]
  base <- x_base1[i]
  temp_vals <- prod_1_final[,..var]
  prod_1_final[[var]] <- base
  prod_1_final$temp_col <- prod_1_final[,(exp(`coef_(Intercept)` + coef_avg_shelf_price*avg_shelf_price + coef_discount_percent*discount_percent +
                              coef_seas_index*seas_index + coef_tv_reach*tv_reach + coef_radio_reach*radio_reach+
                              coef_flyer*flyer + coef_email*email + coef_paid_search*paid_search + coef_web*web + coef_holiday_NEWYEAR*holiday_NEWYEAR+
                              coef_holiday_CARNIVAL*holiday_CARNIVAL + coef_holiday_LIBERTY*holiday_LIBERTY+ coef_holiday_LABOR*holiday_LABOR+
                              coef_holiday_PORTUGAL*holiday_PORTUGAL + coef_holiday_ASSUMPTION*holiday_ASSUMPTION +
                              coef_holiday_XMAS*holiday_XMAS + coef_holiday_REPUBLIC*holiday_REPUBLIC +
                              coef_substitute_avg_price*substitute_avg_price)), by = 'tran_wk']$V1
  setnames(prod_1_final, 'temp_col', paste('yhat',var,sep='_'))
  prod_1_final[[var]] <- temp_vals
}
#dueto <- data.frame(tran_wk = prod_1_final$tran_wk,avg_shelf_price = (prod_1_final$yhat - prod_1_final$yhat_avg_shelf_price))
#dueto <- cbind(dueto, discount_percent = (prod_1_final$yhat - prod_1_final$yhat_discount_percent))
#dueto <- cbind(dueto, seas_index = (prod_1_final$yhat - prod_1_final$yhat_seas_index))
#dueto <- cbind(dueto, tv_reach = (prod_1_final$yhat - prod_1_final$yhat_tv_reach))
#dueto <- cbind(dueto, radio_reach = (prod_1_final$yhat - prod_1_final$yhat_radio_reach))
#dueto <- cbind(dueto, flyer = (prod_1_final$yhat - prod_1_final$yhat_flyer))
#dueto <- cbind(dueto, email = (prod_1_final$yhat - prod_1_final$yhat_email))
#dueto <- cbind(dueto, paid_search = (prod_1_final$yhat - prod_1_final$yhat_paid_search))
#dueto <- cbind(dueto, web = (prod_1_final$yhat - prod_1_final$yhat_web))
#dueto <- cbind(dueto, NEWYEAR = (prod_1_final$yhat - prod_1_final$yhat_holiday_NEWYEAR))
#dueto <- cbind(dueto, CARNIVAL = (prod_1_final$yhat - prod_1_final$yhat_holiday_CARNIVAL))
#dueto <- cbind(dueto, LIBERTY = (prod_1_final$yhat - prod_1_final$yhat_holiday_LIBERTY))
#dueto <- cbind(dueto, LABOR = (prod_1_final$yhat - prod_1_final$yhat_holiday_LABOR))
#dueto <- cbind(dueto, PORTUGAL = (prod_1_final$yhat - prod_1_final$yhat_holiday_PORTUGAL))
#dueto <- cbind(dueto, ASSUMPTION = (prod_1_final$yhat - prod_1_final$yhat_holiday_ASSUMPTION))
#dueto <- cbind(dueto, REPUBLIC = (prod_1_final$yhat - prod_1_final$yhat_holiday_REPUBLIC))
#dueto <- cbind(dueto, XMAS = (prod_1_final$yhat - prod_1_final$yhat_holiday_XMAS))
#dueto <- cbind(dueto, substitute_avg_price = (prod_1_final$yhat - prod_1_final$yhat_substitute_avg_price))





list_of_vars_in_model23 <- c('avg_shelf_price','discount_percent','seas_index','tv_reach','radio_reach','flyer','display',
                               'email','paid_search','web',
                               'holiday_NEWYEAR','holiday_CARNIVAL',
                               'holiday_LIBERTY','holiday_LABOR','holiday_PORTUGAL',
                               'holiday_ASSUMPTION','holiday_XMAS',
                               'holiday_REPUBLIC','substitute_avg_price')
x_base23 <- c(mean(prod_1_weekly$avg_shelf_price),mean(prod_1_weekly$discount_percent),
               mean(prod_1_weekly$seas_index),0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,mean(prod_1_weekly$substitute_avg_price))

# 
 write.csv(dueto2, 'duetos_prod2.csv')
# prod2_coeffs <- data.frame(model_prod2$coefficients)
# prod2_coeffs_t <- transpose(prod2_coeffs)
# colnames(prod2_coeffs_t) <- paste('coef',rownames(prod2_coeffs), sep = '_')
# prod2_coeffs_t <- prod2_coeffs_t[rep(1, each = nrow(prod_2_weekly)),]
# prod_2_final <- cbind(prod_2_weekly, prod2_coeffs_t)
# 
# prod_2_final[,yhat := exp(`coef_(Intercept)` + coef_avg_shelf_price*avg_shelf_price + coef_discount_percent*discount_percent +
#                             coef_seas_index*seas_index + coef_tv_reach*tv_reach + coef_radio_reach*radio_reach+coef_display*display +
#                             coef_flyer*flyer + coef_email*email + coef_paid_search*paid_search + coef_web*web + coef_holiday_NEWYEAR*holiday_NEWYEAR+
#                             coef_holiday_CARNIVAL*holiday_CARNIVAL + coef_holiday_LIBERTY*holiday_LIBERTY+ coef_holiday_LABOR*holiday_LABOR+
#                             coef_holiday_PORTUGAL*holiday_PORTUGAL + coef_holiday_ASSUMPTION*holiday_ASSUMPTION +
#                             coef_holiday_XMAS*holiday_XMAS + coef_holiday_REPUBLIC*holiday_REPUBLIC 
#                         +coef_substitute_avg_price*substitute_avg_price) , by = 'tran_wk'] 
i = 19
    var <- list_of_vars_in_model23[i]
    base <- x_base23[i]
    temp_vals <- prod_2_final[,..var]
    prod_2_final[[var]] <- base
    prod_2_final$temp_col <- prod_2_final[,exp(`coef_(Intercept)` + coef_avg_shelf_price*avg_shelf_price + coef_discount_percent*discount_percent +
                                                         coef_seas_index*seas_index + coef_tv_reach*tv_reach + coef_radio_reach*radio_reach+ coef_display*display +
                                                         coef_flyer*flyer + coef_email*email + coef_paid_search*paid_search + coef_web*web + coef_holiday_NEWYEAR*holiday_NEWYEAR+
                                                         coef_holiday_CARNIVAL*holiday_CARNIVAL + coef_holiday_LIBERTY*holiday_LIBERTY+ coef_holiday_LABOR*holiday_LABOR+
                                                         coef_holiday_PORTUGAL*holiday_PORTUGAL + coef_holiday_ASSUMPTION*holiday_ASSUMPTION +
                                                         coef_holiday_XMAS*holiday_XMAS + coef_holiday_REPUBLIC*holiday_REPUBLIC + 
                                                         coef_substitute_avg_price*substitute_avg_price) , by = 'tran_wk']$V1
    setnames(prod_2_final, 'temp_col', paste('yhat',var,sep='_'))
    prod_1_final[[var]] <- temp_vals

  #dueto2 <- data.frame(tran_wk = prod_2_final$tran_wk,avg_shelf_price = (prod_2_final$yhat - prod_2_final$yhat_avg_shelf_price))
  #dueto2 <- cbind(dueto2, discount_percent = (prod_2_final$yhat - prod_2_final$yhat_discount_percent))
  #dueto2 <- cbind(dueto2, seas_index = (prod_2_final$yhat - prod_2_final$yhat_seas_index))
  #dueto2 <- cbind(dueto2, tv_reach = (prod_2_final$yhat - prod_2_final$yhat_tv_reach))
  #dueto2 <- cbind(dueto2, radio_reach = (prod_2_final$yhat - prod_2_final$yhat_radio_reach))
  #dueto2 <- cbind(dueto2, flyer = (prod_2_final$yhat - prod_2_final$yhat_flyer))
  #dueto2 <- cbind(dueto2, display = (prod_2_final$yhat - prod_2_final$yhat_display))
  #dueto2 <- cbind(dueto2, email = (prod_2_final$yhat - prod_2_final$yhat_email))
  #dueto2 <- cbind(dueto2, paid_search = (prod_2_final$yhat - prod_2_final$yhat_paid_search))
  #dueto2 <- cbind(dueto2, web = (prod_2_final$yhat - prod_2_final$yhat_web))
  #dueto2 <- cbind(dueto2, NEWYEAR = (prod_2_final$yhat - prod_2_final$yhat_holiday_NEWYEAR))
  #dueto2 <- cbind(dueto2, CARNIVAL = (prod_2_final$yhat - prod_2_final$yhat_holiday_CARNIVAL))
  #dueto2 <- cbind(dueto2, LIBERTY = (prod_2_final$yhat - prod_2_final$yhat_holiday_LIBERTY))
  #dueto2 <- cbind(dueto2, LABOR = (prod_2_final$yhat - prod_2_final$yhat_holiday_LABOR))
  #dueto2 <- cbind(dueto2, PORTUGAL = (prod_2_final$yhat - prod_2_final$yhat_holiday_PORTUGAL))
  #dueto2 <- cbind(dueto2, ASSUMPTION = (prod_2_final$yhat - prod_2_final$yhat_holiday_ASSUMPTION))
  #dueto2 <- cbind(dueto2, XMAS = (prod_2_final$yhat - prod_2_final$yhat_holiday_XMAS))
  #dueto2 <- cbind(dueto2, REPUBLIC = (prod_2_final$yhat - prod_2_final$yhat_holiday_REPUBLIC))
  #dueto2 <- cbind(dueto2, substitute_avg_price = (prod_2_final$yhat - prod_2_final$yhat_substitute_avg_price))

     write.csv(dueto3, 'duetos_prod3.csv')
    prod3_coeffs <- data.frame(model_prod3$coefficients)
    prod3_coeffs_t <- transpose(prod3_coeffs)
    colnames(prod3_coeffs_t) <- paste('coef',rownames(prod3_coeffs), sep = '_')
    prod3_coeffs_t <- prod3_coeffs_t[rep(1, each = nrow(prod_3_weekly)),]
    prod_3_final<- cbind(prod_3_weekly, prod3_coeffs_t)

    prod_3_final[,yhat := exp(`coef_(Intercept)` + coef_avg_shelf_price*avg_shelf_price + coef_discount_percent*discount_percent +
                                coef_seas_index*seas_index + coef_tv_reach*tv_reach + coef_radio_reach*radio_reach+ coef_display*display +
                                coef_flyer*flyer + coef_email*email + coef_paid_search*paid_search + coef_web*web + coef_holiday_NEWYEAR*holiday_NEWYEAR+
                                coef_holiday_CARNIVAL*holiday_CARNIVAL + coef_holiday_LIBERTY*holiday_LIBERTY+ coef_holiday_LABOR*holiday_LABOR+
                                coef_holiday_PORTUGAL*holiday_PORTUGAL + coef_holiday_ASSUMPTION*holiday_ASSUMPTION +
                                coef_holiday_XMAS*holiday_XMAS + coef_holiday_REPUBLIC*holiday_REPUBLIC +
                                coef_substitute_avg_price*substitute_avg_price) , by = 'tran_wk']

i=18
    var <- list_of_vars_in_model23[i]
    base <- x_base23[i]
    temp_vals <- prod_3_final[,..var]
    prod_3_final[[var]] <- base
    print(prod_3_final[,exp(`coef_(Intercept)` + coef_avg_shelf_price*avg_shelf_price + coef_discount_percent*discount_percent +
                              coef_seas_index*seas_index + coef_tv_reach*tv_reach + coef_radio_reach*radio_reach+ coef_display*display +
                              coef_flyer*flyer + coef_email*email + coef_paid_search*paid_search + coef_web*web + coef_holiday_NEWYEAR*holiday_NEWYEAR+
                              coef_holiday_CARNIVAL*holiday_CARNIVAL + coef_holiday_LIBERTY*holiday_LIBERTY+ coef_holiday_LABOR*holiday_LABOR+
                              coef_holiday_PORTUGAL*holiday_PORTUGAL + coef_holiday_ASSUMPTION*holiday_ASSUMPTION +
                              coef_holiday_XMAS*holiday_XMAS + coef_holiday_REPUBLIC*holiday_REPUBLIC + 
                              coef_substitute_avg_price*substitute_avg_price) , by = 'tran_wk'])
    prod_3_final$temp_col <- prod_3_final[,exp(`coef_(Intercept)` + coef_avg_shelf_price*avg_shelf_price + coef_discount_percent*discount_percent +
                                                 coef_seas_index*seas_index + coef_tv_reach*tv_reach + coef_radio_reach*radio_reach+ coef_display*display +
                                                 coef_flyer*flyer + coef_email*email + coef_paid_search*paid_search + coef_web*web + coef_holiday_NEWYEAR*holiday_NEWYEAR+
                                                 coef_holiday_CARNIVAL*holiday_CARNIVAL + coef_holiday_LIBERTY*holiday_LIBERTY+ coef_holiday_LABOR*holiday_LABOR+
                                                 coef_holiday_PORTUGAL*holiday_PORTUGAL + coef_holiday_ASSUMPTION*holiday_ASSUMPTION +
                                                 coef_holiday_XMAS*holiday_XMAS + coef_holiday_REPUBLIC*holiday_REPUBLIC + 
                                                 coef_substitute_avg_price*substitute_avg_price) , by = 'tran_wk']$V1
    setnames(prod_3_final, 'temp_col', paste('yhat',var,sep='_'))
    prod_3_final[[var]] <- temp_vals

#dueto3 <- data.frame(tran_wk = prod_3_final$tran_wk,avg_shelf_price = (prod_3_final$yhat - prod_3_final$yhat_avg_shelf_price))
#dueto3 <- cbind(dueto3, discount_percent = (prod_3_final$yhat - prod_3_final$yhat_discount_percent))
#dueto3 <- cbind(dueto3, seas_index = (prod_3_final$yhat - prod_3_final$yhat_seas_index))
#dueto3 <- cbind(dueto3, tv_reach = (prod_3_final$yhat - prod_3_final$yhat_tv_reach))
#dueto3 <- cbind(dueto3, radio_reach = (prod_3_final$yhat - prod_3_final$yhat_radio_reach))
#dueto3 <- cbind(dueto3, flyer = (prod_3_final$yhat - prod_3_final$yhat_flyer))
#dueto3 <- cbind(dueto3, display = (prod_3_final$yhat - prod_3_final$yhat_display))
#dueto3 <- cbind(dueto3, email = (prod_3_final$yhat - prod_3_final$yhat_email))
#dueto3 <- cbind(dueto3, paid_search = (prod_3_final$yhat - prod_3_final$yhat_paid_search))
#dueto3 <- cbind(dueto3, web = (prod_3_final$yhat - prod_3_final$yhat_web))
#dueto3 <- cbind(dueto3, NEWYEAR = (prod_3_final$yhat - prod_3_final$yhat_holiday_NEWYEAR))
#dueto3 <- cbind(dueto3, CARNIVAL = (prod_3_final$yhat - prod_3_final$yhat_holiday_CARNIVAL))
#dueto3 <- cbind(dueto3, LIBERTY = (prod_3_final$yhat - prod_3_final$yhat_holiday_LIBERTY))
#dueto3 <- cbind(dueto3, LABOR = (prod_3_final$yhat - prod_3_final$yhat_holiday_LABOR))
# dueto3 <- cbind(dueto3, PORTUGAL = (prod_3_final$yhat - prod_3_final$yhat_holiday_PORTUGAL))
#dueto3 <- cbind(dueto3, ASSUMPTION = (prod_3_final$yhat - prod_3_final$yhat_holiday_ASSUMPTION))
#dueto3 <- cbind(dueto3, XMAS = (prod_3_final$yhat - prod_3_final$yhat_holiday_XMAS))
#dueto3 <- cbind(dueto3, REPUBLIC = (prod_3_final$yhat - prod_3_final$yhat_holiday_REPUBLIC))
# dueto3 <- cbind(dueto3, substitute_avg_price = (prod_3_final$yhat - prod_3_final$yhat_substitute_avg_price))

    
dueto$sum_rows <- rowSums(dueto[,2:19])
dueto2$sum_rows <- rowSums(dueto2[,2:20])
dueto3$sum_rows <- rowSums(dueto3[,2:20])

dueto <- merge(dueto, prod_1_final[,c(1,5)], by = 'tran_wk')
dueto <- merge(dueto, prod_1_final[,c(1,58)], by = 'tran_wk')
dueto2 <- merge(dueto, prod_2_final[,c(1,5)], by = 'tran_wk')
dueto2 <- merge(dueto, prod_2_final[,c(1,60)], by = 'tran_wk')
dueto3 <- merge(dueto, prod_3_final[,c(1,5)], by = 'tran_wk')
dueto3 <- merge(dueto, prod_3_final[,c(1,60)], by = 'tran_wk')
                                   