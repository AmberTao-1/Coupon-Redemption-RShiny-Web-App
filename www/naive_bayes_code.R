offline <- read_csv("ccf_offline_stage1_train.csv")

offline_hyt_al <- offline
offline_hyt_al$Date_received <- as.Date(offline_hyt_al[["Date_received"]], "%Y%m%d")
offline_hyt_al$Date <- as.Date(offline_hyt_al[["Date"]], "%Y%m%d")
offline_hyt_al$Used_time <- offline_hyt_al$Date - offline_hyt_al$Date_received
offline_hyt_al$respones <- ifelse(is.na(offline_hyt_al$Used_time), "No", ifelse(offline_hyt_al$Used_time > 15, "No","Yes"))

offline_hyt_al <- offline_hyt_al %>% mutate(
  User_id = factor(User_id),
  Merchant_id = factor(Merchant_id),
  Coupon_id = factor(Coupon_id),
  Discount_rate = factor(Discount_rate),
  Distance = factor(Distance),
  respones = factor(respones)
)

set.seed(0)
offlinetrain.index_hyt <- sample(row.names(offline_hyt_al), 0.6*dim(offline_hyt_al)[1])
offlinevalid.index_hyt <-setdiff(row.names(offline_hyt_al), offlinetrain.index_hyt)
# training DF
offlinetrain.df_hyt <- offline_hyt_al[offlinetrain.index_hyt, ]
# validating DF
offlinevalid.df_hyt <- offline_hyt_al[offlinevalid.index_hyt, ]
# Naive Bayes
offline.nb_hyt <- naiveBayes(offlinetrain.df_hyt$respones ~  Merchant_id + Coupon_id + Discount_rate + Distance, data = offlinetrain.df_hyt)

# test data
offline_test <- read_csv("ccf_offline_stage1_test_revised.csv")
offline_test_hyt <- offline_test
offline_test_hyt <- offline_test_hyt %>% mutate(
  User_id = factor(User_id),
  Merchant_id = factor(Merchant_id),
  Coupon_id = factor(Coupon_id),
  Discount_rate = factor(Discount_rate),
  Distance = factor(Distance)
)

# Control function of the NavieBayes 
# input Dataset, choose the validating DF or the test DF
func_nb_hyt <- function(x){
  a <- NULL
  if (x == "Validation"){
    a <- offlinevalid.df_hyt
  } else {
    a <- offline_test_hyt
  }
  pred.offline.prob <- predict(offline.nb_hyt, newdata = a, type = "raw")
  pred.offline.class <- predict(offline.nb_hyt, newdata = a)
  prob <- as.data.frame(pred.offline.prob)[2]
  mutate(a,naive_bayes_predicion=pred.offline.class) %>% cbind(nb_prob = prob$Yes)
}
