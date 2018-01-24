<<<<<<< HEAD




visualize<- function(metric_value, lang) {
  
  
  pdf(paste("Sample", "pdf", sep = "."))
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Sample"))
  mod2<-model_2(metric_val_ord)
  lines(log(metric_val_ord$node), log(fitted(mod2)), col = "tomato", lwd=2)
  dev.off()
}






visualize<- function(metric_value) {
  #Arabic
  metric_value<-compute_mean_edge(lang_dict,1)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Arabic"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Basque
  metric_value<-compute_mean_edge(lang_dict,2)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Basque"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Bengali
  metric_value<-compute_mean_edge(lang_dict,3)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Bengali"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Bulgarian
  metric_value<-compute_mean_edge(lang_dict,4)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Bulgarian"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Catalan
  metric_value<-compute_mean_edge(lang_dict,5)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Catalan"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Czech
  metric_value<-compute_mean_edge(lang_dict,6)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Czech"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Danish
  metric_value<-compute_mean_edge(lang_dict,7)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Danish"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Dutch
  metric_value<-compute_mean_edge(lang_dict,8)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Dutch"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #English
  metric_value<-compute_mean_edge(lang_dict,9)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("English"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Estonian- Excellent fit
  metric_value<-compute_mean_edge(lang_dict,10)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Estonian"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #German
  metric_value<-compute_mean_edge(lang_dict,11)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("German"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Greek (Modern)
  metric_value<-compute_mean_edge(lang_dict,12)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Greek (Modern)"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Greek (Ancient)- Plot is to be observed and accounted for fitting a little strange model 
  metric_value<-compute_mean_edge(lang_dict,13)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_3_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Greek (Ancient)"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Finnish
  metric_value<-compute_mean_edge(lang_dict,14)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Finnish"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Hindi
  metric_value<-compute_mean_edge(lang_dict,15)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Hindi"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Hungarian
  metric_value<-compute_mean_edge(lang_dict,16)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Hungarian"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Italian
  metric_value<-compute_mean_edge(lang_dict,17)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Italian"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Japanese
  metric_value<-compute_mean_edge(lang_dict,18)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Japanese"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Latin
  metric_value<-compute_mean_edge(lang_dict,19)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Latin"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Persian
  metric_value<-compute_mean_edge(lang_dict,20)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Persian"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Portuguese
  metric_value<-compute_mean_edge(lang_dict,21)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Portuguese"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Romanian
  metric_value<-compute_mean_edge(lang_dict,22)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Romanian"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Russian
  metric_value<-compute_mean_edge(lang_dict,23)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Russian"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Slovak
  metric_value<-compute_mean_edge(lang_dict,24)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Slovak"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Slovene
  metric_value<-compute_mean_edge(lang_dict,25)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Slovene"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Spanish
  metric_value<-compute_mean_edge(lang_dict,26)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Spanish"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Swedish
  metric_value<-compute_mean_edge(lang_dict,27)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Swedish"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Tamil
  metric_value<-compute_mean_edge(lang_dict,28)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Tamil"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Telugu
  metric_value<-compute_mean_edge(lang_dict,29)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Telugu"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
  #Turkish
  metric_value<-compute_mean_edge(lang_dict,30)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Turkish"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  
}



