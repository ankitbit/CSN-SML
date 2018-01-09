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


visualize(metric_value, lang_dict[3])
=======




visualize<- function(metric_value) {
  
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("English"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
}


visualize(metric_value)
>>>>>>> a5aab7eff7ebdcb40d1f04a12db89b5cd3f7912f
