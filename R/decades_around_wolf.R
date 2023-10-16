tabulated_data <- read.csv("./clean_data/wolf_deer_year.csv", row.names = 1)
#tabulated_data$SAKD <- tabulated_data$SAKD/2.58999

unique_dmus <- unique(tabulated_data$dmu)
around_origin <- tabulated_data[1,]
around_origin <- around_origin[-1,]
yrs_around <- 5


for (dmu in unique_dmus) {
  tmp <- tabulated_data[tabulated_data$dmu==dmu,]
  if(max(tmp$wolfyear) < yrs_around){
    next
  }
  
  origin <- min(tmp$year[tmp$wolfyear>0])
  tmp$wolfyear <- tmp$year - origin
  tmp$wolfyear <- tmp$wolfyear + (tmp$wolfyear>=0)
  tmp <- tmp[tmp$wolfyear %in% seq(-yrs_around,yrs_around,1),]
  around_origin <- rbind(around_origin, tmp)
}

hunting_yrs <- (2012-yrs_around):(2012+yrs_around-1)
hunting_data <- tabulated_data[tabulated_data$year %in% hunting_yrs & 
                                 tabulated_data$wolfyear>0,]
hunting_data$year <- factor(hunting_data$year, 
                            levels = seq(from = (2012-yrs_around), 
                                         by = 1, length.out = 10))

hunting_data_wowolf <- tabulated_data[tabulated_data$year %in% hunting_yrs & 
                                 tabulated_data$wolfyear==0,]
hunting_data_wowolf$year <- factor(hunting_data_wowolf$year, 
                            levels = seq(from = (2012-yrs_around), 
                                         by = 1, length.out = 10))


png("./Fig/deer_den_to_wolf_year.png", width = 10/1.2, height = 6/1.2, units = "in", res = 400)
par(mar = c(2.5,2.5,1,.5), mgp = c(1.5, 0.5, 0), mfrow = c(3,1))
boxplot(SAKD~wolfyear, data = around_origin, drop = F, 
        xlab = "Year since wolf",
        ylab = "Deer density")
abline(v = yrs_around + .5)
points(SAKD~as.factor(wolfyear), data = around_origin, col = "gray50", cex = 0.5, pch = 16)
text(2.5,34,"pre-wolf")
text(8.5,34,"post-wolf")
anova(lm(SAKD~as.factor(wolfyear), data = around_origin))

boxplot(SAKD~year, data = hunting_data, drop = F, 
        xlab = "Year, DMU w/ wolf",
        ylab = "Deer density")
points(SAKD~year, data = hunting_data, col = "gray50", cex = 0.5, pch = 16)
abline(v = yrs_around + .5) # hunting
abline(v =  yrs_around + 3.5) # post hunting
text(2.5,36,"pre-hunting")
text(7,36,"wolf-hunting")
text(9.5,36,"post-hunting")
#text(10, 10, "DMU w/ wolf")
anova(lm(SAKD~year, data = hunting_data))
summary(lm(SAKD~year, data = hunting_data))

boxplot(SAKD~year, data = hunting_data_wowolf, drop = F, 
        xlab = "Year,DMU w/o wolf",
        ylab = "Deer density")
points(SAKD~year, data = hunting_data_wowolf, col = "gray50", cex = 0.5, pch = 16)
abline(v = yrs_around + .5)
abline(v =  yrs_around + 3.5) # post hunting
text(2.5,36,"pre-hunting")
text(7,36,"wolf-hunting")
text(9.5,36,"post-hunting")

anova(lm(SAKD~year, data = hunting_data_wowolf))
summary(lm(SAKD~year, data = hunting_data_wowolf))
dev.off()
