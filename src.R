user<-system2("whoami", stdout=TRUE)
wd<-switch(user,
           sarahlotspeich=file.path("~/Dropbox/Vanderbilt/Fall 2017/VandyHacks/"),
           valeriewelty=file.path("~/Git_Repos/Collaboration_Russell_Project/russell_project"))
setwd(wd)

libraries <- read.csv("Data/Library_Location_and_Facilities.csv")
metro_art <- read.csv("Data/Metro_Public_Art_Collection.csv", stringsAsFactors = FALSE)
public_art <- read.csv("Data/Art_in_Public_Places.csv", stringsAsFactors = FALSE)
historic <- read.csv("Data/Historic_Markers.csv", stringsAsFactors = FALSE)
wifi <- read.csv("Data/Metro_Public_WiFi_Locations.csv", stringsAsFactors=FALSE)
beer <- read.csv("Data/Beer_Permit_Locations.csv", stringsAsFactors = FALSE)
parks <- read.csv("Data/Parks_-_Park_Locations.csv", stringsAsFactors = FALSE)
water <- read.csv("Data/WaterFountains_HydrationSystems.csv")
bikeracks <- read.csv("Data/Downloads/BikeRacks.csv")

#geocode public wifi locations
library(ggmap)
wifi.geo <- geocode(paste(wifi$Street.Address, wifi$City, wifi$State, wifi$Zip.Code))

combined_nash <- data.frame(Title = c(metro_art$Artwork, public_art$Title, historic$TITLE, wifi$Site.Name, beer$Business.Name))
combined_nash$BikeRack <- as.numeric(grepl("(Bike Rack)", combined_nash$Title))
combined_nash$Title <- gsub("\\(Bike Rack\\)", "", combined_nash$Title)
combined_nash$ArtistName <- c(paste(metro_art$First.Name, metro_art$Last.Name), paste(public_art$First.Name, public_art$Last.Name), rep("", nrow(historic) + nrow(wifi) + nrow(beer)))
combined_nash$Description <- c(metro_art$Description, public_art$Description, historic$MARKER.TEXT, wifi$Site.Type, rep("", nrow(beer)))
combined_nash$Longitude <- c(metro_art$Longitude, public_art$Longitude, historic$LONGITUDE, wifi.geo$lon, beer$Longitude)
combined_nash$Latitude <- c(metro_art$Latitude, public_art$Latitude, historic$LATITUDE, wifi.geo$lat, beer$Latitude)
combined_nash$DataSet <- c(rep("Art", nrow(metro_art) + nrow(public_art)), rep("Historical", nrow(historic)), rep("Free Wifi", nrow(wifi)), rep("Beer License", nrow(beer)))
combined_nash <- combined_nash[,c(7, 1:6)]
combined_nash$PhotoLink <- c(metro_art$Photo, public_art$Photo, rep("", nrow(historic) + nrow(wifi) + nrow(beer)))

write.csv(combined_nash, "combined_nash.csv", row.names = FALSE)

#append column to parks dataset for parks that have a Greenway Trailhead
parks$GreenwayTrailhead <- grepl("Greenway Trailhead", parks$Notes)
parks$ModelAirplaneField <- grepl("Airplane", parks$Notes)
parks$Amphetheater <-  grepl("amphetheater", parks$Notes)
#parks$PoolType[grepl("outdoor pool", parks$Notes)] <- 

#create more detailed hiking data set
subset(parks, Hiking.Trails=="Yes")

#Percy Warner Park
hikes <- hikes$Park.Name

#maps
UrbanAreasUS.shp <- readOGR(dsn = path.expand("~/Dropbox/R Ladies/tl_2016_us_uac10/tl_2016_us_uac10.shp"), layer="tl_2016_us_uac10")
NashvilleTN.shp <- subset(UrbanAreasUS.shp, UrbanAreasUS.shp@data$NAME10 == "Nashville-Davidson, TN")
NashvilleTN.df <- tidy(NashvilleTN.shp, region = "NAME10")

NashvilleTN.map <- ggplot() + geom_polygon(data = NashvilleTN.df, aes(x = long, y = lat, group = group), fill="white", col="black") +
  ggtitle("Nashville Art in Public Places") + coord_equal(ratio = 1) +
  theme(axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(),axis.title.x=element_blank(),axis.title.y=element_blank()) +
  north(NashvilleTN.df, location = "topleft") + scalebar(NashvilleTN.df, location = "bottomright", dist = 10, dd2km = TRUE, model = 'WGS84', st.size=2.5)

