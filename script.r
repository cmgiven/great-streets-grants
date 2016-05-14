library('maptools')
library('ggplot2')

data <- read.csv('data.csv', stringsAsFactors = FALSE)
data <- data[1:219,] # the last row is bad
data$Official.Amount <- as.numeric(gsub('[$,]','',data$Official.Amount))

nc_data <- read.csv('comp_table_cltr00.csv', stringsAsFactors = FALSE)

dc <- readShapePoly('Neighborhood_Clusters/Neighborhood_Clusters.shp')

ncLookup <- function(x) {
  pt <- SpatialPoints(data.frame(x = x$Longitude, y = x$Latitude))
  nc <- over(pt, dc)
  return(nc$NAME)
}

data$Cluster <- ncLookup(data)

data <- merge(data, nc_data)

ggplot(data, aes(x = PctPoorPersons_2007_11, y = Pct16andOverEmployed_2007_11, size = Official.Amount, color = Ward)) +
  geom_point(alpha = .2, position = position_jitter(width=1,height=1))

ggplot(data, aes(PctPoorPersons_2007_11, fill = Ward)) +
  geom_histogram(bins = 5)

write.csv(data, 'merged_data.csv', row.names = FALSE)
