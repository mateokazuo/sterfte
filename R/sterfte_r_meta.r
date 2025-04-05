# make metadata for visualizations

library(sf) # for gpkg files
library(spdep)

linearizer <- function(vec, a=0,b=1) {
  # linear map vector to [a,b]
  M = max(vec)
  m = min(vec)
  if (M==m) {return(rep(b,length(vec)))}
  return(a + (vec-m) * (b-a) / (M-m))
}

data <- st_read("D:/Trashcan/rthings/gemsterfte.gpkg")
data$gemeentenaam[101] <- "Ysselstein"
data$gemeentenaam[148] <- "Gravenhage"
data$gemeentenaam[208] <- "Hertogenbosch"
data <- data[order(data$gemeentenaam), ]
data$gemeentenaam[107] <- "'s-Gravenhage"
data$gemeentenaam[130] <- "'s-Hertogenbosch"
data$gemeentenaam[327] <- "IJsselstein"
head(data)

points <- st_point_on_surface(data$Shape)
coords <- st_coordinates(points)

geo <- data.frame(
  # add nationwide point at 0,0
  index = c(0:(nrow(data))),
  code = c("NL01  ", data[[1]]),
  name = c("Nederland", data[[2]]),
  xPos = c(0, linearizer(coords[,1])),
  yPos = c(0, linearizer(coords[,2]))
)


write.csv(geo,"D:/Trashcan/sterfte_meta.csv",row.names = FALSE)
