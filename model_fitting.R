
metric_value<-English_collection_dependency_tree_metrics
metric_value<- metric_value[order(metric_value$node), ]
plot(log(metric_value$node), log(metric_value$mean_edge_length))

mean_english = aggregate(metric_value, list(metric_value$node), mean)

plot(mean_english$node, mean_english$mean_edge_length)
plot(log(mean_english$node), log(mean_english$mean_edge_length))
lines(log(mean_english$node), log(mean_english$mean_edge_length), col="tomato", lwd=2)
lines(log(mean_english$node), log((mean_english$node+1)/3), col="steelblue", lwd=2)

#non_linear_regression
a_initial = 4
b_initial = 4
nonlinear_model = nls(mean_edge_length~a*node^b,data=mean_english,
                        start = list(a = a_initial, b = b_initial), trace = TRUE)


deviance(nonlinear_model)
AIC(nonlinear_model)

plot(log(metric_value$node), log(metric_value$mean_edge_length),
     xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue")
lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)

linear_model = lm(log(mean_edge_length)~log(node), metric_value)

a_initial = exp(coef(linear_model)[1])
b_initial = coef(linear_model)[2]

nonlinear_model = nls(mean_edge_length~a*node^b,data=metric_value,
                      start = list(a = a_initial, b = b_initial), trace = TRUE)



model_0 <- function(lang_dict){
  df<-data.frame("Language"=character(30),"RSS"=numeric(30), "n"=numeric(30), "p"=numeric(30), 
                 "s"=numeric(30), "AIC"=numeric(30))
  df[,1]<-lang_dict
  for(i in 1:length(lang_dict)) {
    print(lang_dict[i])
    dat<-compute_mean_edge(lang_dict, i)
    RSS <- sum((dat$mean_edge_length-(dat$node +1)/3)^2)
    n <- length(dat$node)
    p <- 0
    df$s[i] <- sqrt(RSS/(n - p))
    df$AIC[i] <- n*log(2*pi) + n*log(RSS/n) + n + 2*(p + 1)
  }
  return(df)
}


model_1<- function(dat){
  linear<- lm(log(mean_edge_length) ~ log(node), dat)
  b1_init<- -(linear$coefficients[1]/log(2))
  mod1 = nls(mean_edge_length ~ (node/2)^b, dat, start = list(b = b1_init), trace = FALSE,
             control =  nls.control(maxiter = 1000, warnOnly = TRUE))
  return(mod1)
}

model_1_plus <- function(dat){
  linear<- lm(log(mean_edge_length) ~ log(node), dat)
  b1p_init = -(linear$coefficients[1]/log(2))
  d1p_init = 0
  mod1p = nls(mean_edge_length ~ (node/2)^b + d, data =  dat,
              start = list(b = b1p_init, d = d1p_init), trace = F,
              control =  nls.control(maxiter = 1000, warnOnly = TRUE))
  
  return(mod1p) 
}

model_2<- function(dat){
  linear =  lm(log(mean_edge_length) ~ log(node), dat)
  b2_init = linear$coefficients[2]
  a2_init = exp(linear$coefficients[1])
  mod2 = nls(mean_edge_length ~ a*(node^b), dat,
             start = list(a = a2_init, b = b2_init), trace = FALSE,
             control =  nls.control(maxiter = 1000, warnOnly = TRUE))
  return(mod2)
}

model_2_plus <- function(dat){
  linear<-lm(log(mean_edge_length) ~ log(node), dat)
  b2p_init = linear$coefficients[2]
  a2p_init = exp(linear$coefficients[1])
  d2p_init = 1.5   # seems that 1.5 work well with almost all the languages (Chinese critic language)
  
  mod2p = nls(mean_edge_length ~ a * node^b + d, dat,
              start = list(a = a2p_init, b = b2p_init, d = d2p_init), trace = FALSE,
              control =  nls.control(maxiter = 1000, warnOnly = TRUE))
  return(mod2p)
}

model_3<- function(dat){
  linear<- lin(dat)
  c3_init = linear$coefficients[2]
  a3_init = exp(linear$coefficients[1])
  
  mod3 = nls(mean_edge_length ~ a * exp(c*node), dat,
             start = list(a = a3_init, c= c3_init), trace = F,
             control =  nls.control(maxiter = 1000, warnOnly = TRUE))
  return(mod3)
}

mod3<-model_3(English_collection_dependency_tree_metrics)
c<-coef(mod3)[2]
a<-coef(mod3)[1]

model_3_plus <- function(dat){
  linear<- lin(dat)
  c_init = linear$coefficients[2]
  a_init = exp(linear$coefficients[1])
  #c_init<-16.046865237
  #a_init<-0.002634201
  d_init =  1.5
  mod3p = nls(mean_edge_length ~ a * exp(c*node) + d, dat,
             start = list(a = a_init, c= c_init, d=d_init), trace = F,
             control =  nls.control(maxiter = 1000, warnOnly = TRUE))
  return(mod3p)
}



lin<-function(dat) {
  lin.mod<-lm(log(mean_edge_length) ~ (node), dat)
  return(lin.mod)
}
require(minpack.lm)


model_4<-function(dat){
  lin.model = lm(formula = exp(mean_edge_length) ~ node,  data = dat)
  initial.a = lin.model$coefficients[2]
  model.4 = nls(formula = mean_edge_length ~ a*log(node),
                data = dat, start = list(a = initial.a),
                trace = F, control =  nls.control(maxiter = 1000, warnOnly = TRUE))
  return(model.4)
}

model_4_plus<-function(dat){
  lin.model = lm(formula = exp(mean_edge_length) ~ node,  data = dat)
  initial.a = lin.model$coefficients[2]
  initial.d = lin.model$coefficients[1]
  
  model.4 = nls(formula = mean_edge_length ~ a*log(node) + d,
    data = dat, start = list(a = initial.a, d = initial.d),
    trace = F, control =  nls.control(maxiter = 1000, warnOnly = TRUE))
  return(model.4)
}




library(readr)
driver_model_fitting<- function(){
  
  AIC_score<-numeric(length = length(lang_dict))
  #for each of the languages
  for(i in seq(lang_dict)){
    metric_value <- read_delim(paste( paste("~/CSN_SML/metric_data", lang_dict[i], sep = "/"), 
                                      "collection_dependency_tree_metrics.txt", sep = "_"), 
                               "\t", escape_double = FALSE, trim_ws = TRUE)
    
    mod2<-model_2(metric_value)
    AIC_score[i]<-AIC(mod2)
  }
}
metric_val_ord<- metric_value[order(metric_value$node),]













