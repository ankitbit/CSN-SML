<<<<<<< HEAD


write_summary<- function() {
  
  summary_data<-data.frame("Language"=character(30),
    "sample_size"=numeric(30), "mean_n"=numeric(30), 
                           "sigma_n"=numeric(30), "mean_d"=numeric(30), "sigma_d"=numeric(30))
  summary_data[,1]<-lang_dict
  
  for(i in seq(lang_dict)){
    metric_value<- compute_mean_edge(lang_dict, i)
    summary_data$sample_size[i]<- nrow(metric_value)
    summary_data$mean_n[i]<- mean(metric_value$node)
    summary_data$sigma_n[i]<- sd(metric_value$node)
    summary_data$mean_d[i]<- mean(metric_value$mean_edge_length)
    summary_data$sigma_d[i]<- sd(metric_value$mean_edge_length)
  }
  return(summary_data)
}

#commands
summary_data<- write_summary()
dir.create("summary_data")
write.table(summary_data, 
            paste("~/CSN_SML/summary_data/summary_data.txt")
            , sep="\t", row.names = F)




compute_AIC<- function(lang_dict) {
  df<- data.frame("Language"=character(30), "Model 1"=numeric(30), "Model 1+"=numeric(30), 
                  "Model 2"=numeric(30),
                  "Model 2+"=numeric(30),"model 3"=numeric(30), "model 3+"=numeric(30), 
                  "Model 4"=numeric(30), "Model 4+"=numeric(30))
  df[,1]=lang_dict
  for(i in seq(lang_dict)){
    print(lang_dict[i])
    dat<-compute_mean_edge(lang_dict, i)
    df$Model.1[i] <- AIC(model_1(dat))
    df$Model.1.[i]<- AIC(model_1_plus(dat))
    df$Model.2 [i] <-AIC(model_2(dat))
    df$Model.2.[i] <-AIC(model_2_plus(dat))
    df$model.3 [i] <- AIC(model_3(dat))
    df$model.3.[i] <-AIC(model_3_plus(dat))
    df$Model.4[i]<-AIC(model_4(dat))
    df$Model.4.[i]<-AIC(model_4_plus(dat))
  }
  return(df)
}


compute_delta_AIC <-function(lang_dict) {
  df<- data.frame("Language"=character(30), "Model 0"=numeric(30), "Model 1"=numeric(30), 
                  "Model 1+"=numeric(30), 
                  "Model 2"=numeric(30),
                  "Model 2+"=numeric(30),"model 3"=numeric(30), "model 3+"=numeric(30), 
                  "Model 4"=numeric(30), "Model 4+"=numeric(30))
  #AIC_score<-compute_AIC(lang_dict)
  #df[,1]=lang_dict
  df<- AIC_table
  #Change for model 0 here (if required)
  for(i in 1:length(lang_dict)){
    print(lang_dict[i])
    
    AIC_min <- min(AIC_table[i,2:10])
    print(AIC_min)
    df[i,2:10]<- df[i,2:10]-AIC_min
  }
  
  return(df)
}

AIC_score_delta<-compute_delta_AIC(lang_dict)


write.table(AIC_score, 
            paste("~/CSN_SML/summary_data/AIC_Score.txt")
            , sep="\t", row.names = F)

AIC_score<-compute_AIC(lang_dict)




compute_s<-function(lang_dict){
  bp <- data.frame("Language"=character(30), "1(b)"=numeric(30), "1_Plus(b)"=numeric(30), 
                   "1_plus(d)"=numeric(30), "2(a)"=numeric(30), "2(b)"=numeric(30),
                   "2_Plus(a)"=numeric(30), "2_Plus(b)"=numeric(30), "2_Plus(d)"=numeric(30),
                   "3(a)"=numeric(30), "3(c)"=numeric(30), 
                   "3_Plus(a)"=numeric(30), "3_Plus(c)"=numeric(30), "3_Plus(d)"=numeric(30),
                   "4(a)"=numeric(30), "4_Plus(a)"=numeric(30),"4_Plus(d)"=numeric(30))
  name_bp<-c("Language", "1(b)", "1+(b)", "1+(d)", "2(a)", "2(b)", "2+(a)", "2+(b)", "2+(d)",
             "3(a)", "3(c)", "3+(a)", "3+(c)", "3+(d)", "4(a)", "4+(a)", "4+(d)")
  names(bp)<-name_bp
  
  df<- data.frame("Language"=character(30), "Model 1"=numeric(30), "Model 1+"=numeric(30), 
                  "Model 2"=numeric(30),
                  "Model 2+"=numeric(30),"model 3"=numeric(30), "model 3+"=numeric(30), 
                  "Model 4"=numeric(30), "Model 4+"=numeric(30))
  df[,1]=lang_dict
  bp[,1]=lang_dict
  
  for(i in 1:length(lang_dict)) {
    print(lang_dict[i])
    dat<-compute_mean_edge(lang_dict, i)
    mod1<-model_1(dat)
    mod2<-model_2(dat)
    mod3<-model_3(dat)
    mod4<-model_4(dat)
    mod1p<-model_1_plus(dat)
    mod2p<-model_2_plus(dat)
    mod3p<-model_3_plus(dat)
    mod4p<-model_4_plus(dat)
    
    df$Model.1 [i]<- sqrt(deviance(mod1)/df.residual(mod1))
    df$Model.1.[i]<- sqrt(deviance(mod1p)/df.residual(mod1p))
    df$Model.2 [i]<- sqrt(deviance(mod2)/df.residual(mod2))
    df$Model.2.[i]<- sqrt(deviance(mod2p)/df.residual(mod2p))
    df$model.3 [i]<- sqrt(deviance(mod3)/df.residual(mod3))
    df$model.3.[i]<- sqrt(deviance(mod3p)/df.residual(mod3p))
    df$Model.4 [i]<- sqrt(deviance(mod4)/df.residual(mod4))
    df$Model.4.[i]<- sqrt(deviance(mod4p)/df.residual(mod4p))
    
    
    bp$`1(b)`[i]<-coef(mod1)["b"]
    bp$`1+(b)`[i]<-coef(mod1p)["b"]
    bp$`1+(d)`[i]<-coef(mod1p)["d"]
    bp$`2(a)`[i]<-coef(mod2)["a"]
    bp$`2(b)`[i]<-coef(mod2)["b"]
    bp$`2+(a)`[i]<-coef(mod2p)["a"]
    bp$`2+(b)`[i]<-coef(mod2p)["b"]
    bp$`2+(d)`[i]<-coef(mod2p)["d"]
    bp$`3(a)`[i]<-coef(mod3)["a"]
    bp$`3(c)`[i]<-coef(mod3)["c"]
    bp$`3+(a)`[i]<-coef(mod3p)["a"]
    bp$`3+(c)`[i]<-coef(mod3p)["c"]
    bp$`3+(d)`[i]<-coef(mod3p)["d"]
    bp$`4(a)`[i]<-coef(mod4)["a"]
    bp$`4+(a)`[i]<-coef(mod4p)["a"]
    bp$`4+(d)`[i]<-coef(mod4p)["d"]
    
    
  }
  names(bp)<-name_bp
  my_list<-list(df,bp)
  return(my_list)
}

best_parameters<-data.frame(result[2])
names(best_parameters)<-name_bp
View(best_parameters)
name_bp<-c("Language", "1(b)", "1+(b)", "1+(d)", "2(a)", "2(b)", "2+(a)", "2+(b)", "2+(d)",
           "3(a)", "3(c)", "3+(a)", "3+(c)", "3+(d)", "4(a)", "4+(a)", "4+(d)")

s<-compute_s(lang_dict)




##For printing purposes
##Using xtables
library(xtable)
print(xtable(best_parameters[,1:9])) #For best parameters for models 1 to 2 (all variants)

names(data.frame(cbind(lang_dict, best_parameters[,10:17])))
table2<-best_parameters[,10:17]
table2<-cbind(lang_dict, table2)
names(table2)<-c("Language","3(a)", "3(c)", "3+(a)", "3+(c)", "3+(d)", "4(a)", "4+(a)", "4+(d)")
print(xtable(table2))


#Printing the values of AIC
result_model_0<-model_0(lang_dict)

AIC_table<- data.frame(cbind(lang_dict,result_model_0[6],AIC_score[,2:9]))
names(AIC_table)<-c("Language", "Model 0", "Model 1", "Model 1+", "Model 2", "Model 2+",
                    "Model 3", "Model 3+", "Model 4", "Model 4+")
print(xtable(AIC_table))

#Printing the value of Delta AIC
print(xtable(AIC_score_delta))

#Printing the values of Residual Standard Error
RSE_table<-data.frame(cbind(lang_dict, result_model_0[5], s[,2:9]))
names(RSE_table)<-c("Language", "Model 0", "Model 1", "Model 1+", "Model 2", "Model 2+",
                    "Model 3", "Model 3+", "Model 4", "Model 4+")
print(xtable(RSE_table))

=======


write_summary<- function() {
  
  summary_data<-data.frame("Language"=character(30),
    "sample_size"=numeric(30), "mean_n"=numeric(30), 
                           "sigma_n"=numeric(30), "mean_d"=numeric(30), "sigma_d"=numeric(30))
  summary_data[,1]<-lang_dict
  
  for(i in seq(lang_dict)){
    metric_value<- compute_mean_edge(lang_dict, i)
    summary_data$sample_size[i]<- nrow(metric_value)
    summary_data$mean_n[i]<- mean(metric_value$node)
    summary_data$sigma_n[i]<- sd(metric_value$node)
    summary_data$mean_d[i]<- mean(metric_value$mean_edge_length)
    summary_data$sigma_d[i]<- sd(metric_value$mean_edge_length)
  }
  return(summary_data)
}

#commands
summary_data<- write_summary()
dir.create("summary_data")
write.table(summary_data, 
            paste("~/CSN_SML/summary_data/summary_data.txt")
            , sep="\t", row.names = F)




compute_AIC<- function(lang_dict) {
  df<- data.frame("Language"=character(30), "Model 1"=numeric(30), "Model 1+"=numeric(30), 
                  "Model 2"=numeric(30),
                  "Model 2+"=numeric(30),"model 3"=numeric(30), "model 3+"=numeric(30), 
                  "Model 4"=numeric(30), "Model 4+"=numeric(30))
  df[,1]=lang_dict
  for(i in seq(lang_dict)){
    print(lang_dict[i])
    dat<-compute_mean_edge(lang_dict, i)
    df$Model.1<- AIC(model_1(dat))
    df$Model.1.<- AIC(model_1_plus(dat))
    df$Model.2<-AIC(model_2(dat))
    df$Model.2.<-AIC(model_2_plus(dat))
    df$model.3<- AIC(model_3(dat))
    df$model.3.<-0
    df$Model.4<-AIC(model_4(dat))
    df$Model.4.<-AIC(model_4_plus(dat))
  }
  return(df)
}

bengali<- compute_mean_edge(lang_dict,3)


## Model Summary on aggregated data
generate_aggregate<-funct