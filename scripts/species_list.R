library(dplyr)

data = read.csv("data/ACAD Regional 2024.06.03-filtered.csv")
str(data)

landbirds = subset(data, Region == "Northern Pacific Rainforest" & Group  == "landbird" & X.pop.b >= 10)

nrow(landbirds)
landbirds[order(-landbirds$X.pop.b), c("Common.Name", "X.pop.b")]

landbirds = data %>% filter(Region == "Northern Pacific Rainforest",  Group  == "landbird", X.pop.b >= 10)

landbirds$Common.Name

traits = read.csv("data/trait_data.csv")
traits %>% filter(home_range_radius_m < 500) %>% pull(common_name)
