



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
