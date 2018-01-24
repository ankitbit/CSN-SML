


visualize1<- function(metric_value, lang) {
  
  
  pdf(paste("Sample", "pdf", sep = "."))
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Sample"))
  mod2<-model_2(metric_val_ord)
  lines(log(metric_val_ord$node), log(fitted(mod2)), col = "tomato", lwd=2)
  dev.off()
}






visualize<- function() {
  dir.create("Visualization")
  #Arabic
  pdf(paste("Arabic", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,1)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Arabic"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Basque
  pdf(paste("Basque", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,2)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Basque"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Bengali
  pdf(paste("Bengali", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,3)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Bengali"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Bulgarian
  pdf(paste("Bulgarian", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,4)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Bulgarian"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Catalan
  pdf(paste("Catalan", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,5)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Catalan"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Czech
  pdf(paste("Czech", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,6)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Czech"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Danish
  pdf(paste("Danish", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,7)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Danish"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Dutch- Excellent fit
  pdf(paste("Dutch", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,8)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Dutch"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #English
  pdf(paste("English", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,9)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("English"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Estonian- Excellent fit
  pdf(paste("Estonian", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,10)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Estonian"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #German
  pdf(paste("German", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,11)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("German"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Greek (Modern)
  pdf(paste("Greek_Modern", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,12)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Greek (Modern)"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Greek (Ancient)- Plot is to be observed and accounted for fitting a little strange model 
  pdf(paste("Greek_Ancient", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,13)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_3_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Greek (Ancient)"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Finnish
  pdf(paste("Finnish", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,14)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Finnish"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Hindi
  pdf(paste("Hindi", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,15)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Hindi"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Hungarian
  pdf(paste("Hungarian", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,16)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Hungarian"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Italian
  pdf(paste("Italian", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,17)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Italian"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Japanese
  pdf(paste("Japanese", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,18)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Japanese"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Latin
  pdf(paste("Latin", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,19)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Latin"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Persian
  pdf(paste("Persian", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,20)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Persian"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Portuguese
  pdf(paste("Portuguese", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,21)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Portuguese"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Romanian
  pdf(paste("Romanian", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,22)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Romanian"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Russian
  pdf(paste("Russian", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,23)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Russian"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Slovak
  pdf(paste("Slovak", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,24)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Slovak"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Slovene
  pdf(paste("Slovene", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,25)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Slovene"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Spanish
  pdf(paste("Spanish", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,26)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Spanish"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Swedish
  pdf(paste("Swedish", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,27)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Swedish"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Tamil
  pdf(paste("Tamil", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,28)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Tamil"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Telugu
  pdf(paste("Telugu", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,29)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_4_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Telugu"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
  #Turkish
  pdf(paste("Turkish", "pdf", sep = "."))
  metric_value<-compute_mean_edge(lang_dict,30)
  metric_value = metric_value[order(metric_value$node), ]
  nonlinear_model<-model_2_plus(metric_value)
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("Turkish"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
  dev.off()
  
}



