

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


