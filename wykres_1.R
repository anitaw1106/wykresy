rm(list=ls())

library(dplyr)
library(ggplot2)


d <- as_tibble(
  x = read.csv(
    file = "http://michal.ramsza.org/lectures/2_r_programming/data/data_2.csv", encoding = "UTF-8"))


grouped_d <- d %>% count(Brand,Engine_capacity)
grouped_d <- grouped_d %>% arrange(desc(Brand), Engine_capacity)

names(grouped_d) <- c("Brand", "Engine_capacity", "NumberOfOffers")



final_plt <- 
  ggplot(grouped_d, aes(x = Engine_capacity, y = Brand, size = NumberOfOffers, alpha = NumberOfOffers)) + 
  geom_point(colour = "blue") +
  labs(x = "EngineCapacity", 
       y = "Brand",
       colour ="NumOfOffers") + theme_bw() 
 
print(final_plt)


marka <- as.matrix(grouped_d)
marka_un <- unique(grouped_d$Brand)
k <- unique(grouped_d$Engine_capacity)
capacities <- k[order(k)]


final_matrix = matrix(0,length(marka_un),length(capacities))
colnames(final_matrix) = capacities
rownames(final_matrix) = marka_un

for (i in capacities){
  for (j in marka_un){
    num_of_offers <-  grouped_d$NumberOfOffers[which(grouped_d$Brand==j & grouped_d$Engine_capacity==i)]
    if(length(num_of_offers)!=0){
      final_matrix[j,as.character(i)] <-num_of_offers
      }
  }
}

k <- dist(final_matrix) 
fit <- cmdscale(k, eig=TRUE, 2) 
fit 

x <- fit$points[,1]
y <- fit$points[,2]


plot_1 <- ggplot(data = NULL, aes(x=-x,y=-y, label = marka_un))+
  geom_point(col = "blue")+
  geom_text(aes(label=marka_un, hjust=-0.3, vjust=-0.3), col="blue", alpha = 0.3)+
  labs(x="x",
       y="y")+
  theme_bw()


print(plot_1)



