metric_value<-English_collection_dependency_tree_metrics
metric_value<- metric_value[order(metric_value$node), ]
plot(log(metric_value$node), log(metric_value$mean_edge_length))

