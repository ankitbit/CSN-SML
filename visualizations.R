



visualize<- function(metric_value) {
  
  plot(log(metric_value$node), log(metric_value$mean_edge_length),
       xlab = "log(vertices)", ylab = "log(mean dependency length)", pch=16, col="skyblue",
       main = paste("English"))
  lines(log(metric_value$node), log(fitted(nonlinear_model)), col = "tomato", lwd=2)
}


visualize(metric_value)
