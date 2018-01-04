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





model_1<- function(dat){
  linear<- lm(log(mean_edge_length) ~ log(node), dat)
  inter = -(linear$coefficients[1]/log(2))
  
  ######## model 1 #######
  # formula1 = function(n, b) (n/2)^b 
  # b*log(n) - b*log(2)
  b1_init<- inter
  mod1 = nls(mean_edge_length ~ (node/2)^b, dat, start = list(b = b1_init), trace = FALSE)
  
  return(mod1)
}

driver_model_fitting<- function(){
  
  #for each of the languages
  for(i in seq(lang_dict)){
    metric_value <- read_delim("~/CSN_SML/metric_data/Arabic_collection_dependency_tree_metrics.txt", 
                               "\t", escape_double = FALSE, trim_ws = TRUE)
    
    mod1<-model_1()
  }
}

paste( paste("~/CSN_SML/metric_data/", lang_dict[1], sep = "/"), 
       "collection_dependency_tree_metrics.txt", sep = "_")








