library(dplyr)
library(rgdal)

world_spdf <- readOGR( 
  dsn = paste0(getwd(), "/world_shape_file/"),
  layer="TM_WORLD_BORDERS_SIMPL-0.3",
  verbose=FALSE
)

world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)
world_spdf@data <- cbind(id=rownames(world_spdf@data), world_spdf@data)
Data <- world_spdf@data
Geo <- read.csv("data/res_merged.csv")

lyear = c(rep(2020, 12), rep(2021, 4))
lmonth = c(1:12, 1:4)

world_spdf@data <- Data %>% left_join(Geo %>% filter(year==2020, month==1), by=c("ISO2"="Country")) %>% 
  mutate(Year=2020, Mon=1)

for (i in 2:16){
  world_spdf@data <- rbind(world_spdf@data, 
                           Data %>% left_join(Geo%>% filter(year==lyear[i], month==lmonth[i]), 
                                              by=c("ISO2"="Country")) 
                           %>% mutate(Year=lyear[i], Mon=lmonth[i]))
}

hashtag <- read.csv("data/hash_bymonth_excv_top10.csv", encoding="UTF-8") 
