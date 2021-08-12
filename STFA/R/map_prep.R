# Install and load necessary packages
#if (!require("pacman")) install.packages("pacman")
#pacman::p_load(tidyverse, readr, writexl, readxl, sf, clock, lubridate, lwgeom)
library(tidyverse)
library(readr)
library(writexl)
library(readxl)
library(sf)
library(sfheaders)
library(clock)
library(lubridate)
library(lwgeom)

# Loading all datasets and image
gps <- read_csv("data/gps.csv") # Add gps data
car <- read_csv("data/car-assignments.csv") # Add car assignments
Abila_st <- st_as_sf(st_read(dsn = "data", layer = "Abila"))
Kronos_sf <- st_as_sf(st_read(dsn = "data", layer = "Kronos_Island"))

# Car data manipulation
car$CarID <- as_factor(car$CarID) # Change the column format to nominal format
car$FullName <- paste(car$FirstName,car$LastName, sep = " ") # Create new column with combined first and last name
car$RoleNName <- paste(car$CarID, car$CurrentEmploymentTitle, car$FullName, sep = " ") # Create new column with combined ID, Role and Full Name
car$Name <- paste(car$CurrentEmploymentTitle, car$FullName, sep = " ") # Create new column with combined ID, Role and Full Name

# GPS data manipulation
gps$Timestamp <- date_time_parse(gps$Timestamp,
                                 zone = "UTC",
                                 format = "%m/%d/%Y %H:%M:%S") # Readjust loyalty timestamp
gps$day <- as_factor(get_day(gps$Timestamp)) # Extract day of month and convert to factor data type
gps$id <- as_factor(gps$id) # Change to factor data type
gps$date <- as_date(gps$Timestamp) # Create a separate column just for dates in the gps data
gps$hour <- hour(gps$Timestamp) # Create a separate column just for hours in the gps data
gps$period <- case_when( # Segment hour into 5 separate periods
  gps$hour >= 21 ~ "Late Evening 9pm to 11.59pm",
  gps$hour >= 18 ~ "Evening 6pm to 8.59pm",
  gps$hour >= 12 ~ "Afternoon 12noon to 5.59pm",
  gps$hour >= 6 ~ "Morning 6am to 11.59am",
  TRUE ~ "Late Night 12mn to 5.59am"
)
gps$period <- factor(gps$period, # Order periods accordingly
                     levels = c("Morning 6am to 11.59am", 
                                "Afternoon 12noon to 5.59pm", 
                                "Evening 6pm to 8.59pm",
                                "Late Evening 9pm to 11.59pm",
                                "Late Night 12mn to 5.59am"))
gps$dayofmonth <- day(gps$Timestamp) # Extract day of month from timestamp in a new column
gps$weekday <- wday(gps$Timestamp, label = TRUE) # Extract day of week from timestamp in a new column
gps$day <- as_factor(get_day(gps$Timestamp)) # Extract day of month and convert to factor data type
gps$id <- as_factor(gps$id) # Change to factor data type
#gps_sf <- st_as_sf(gps, coords = c("long", "lat"), crs = 4326) # Changing into a shapefile


# Isolating the locations

gps_name <- left_join(gps,car, by = c("id" = "CarID")) # Merge car assignments to gps data
gps_name$Timestamp <- as.POSIXct(gps_name$Timestamp, format = "%m/%d/%Y  %H:%M:%S", tz = "GMT") # Timestamp switching to month-day-year format
gps_name <- gps_name[with(gps_name,order(id,Timestamp)),] # Sort first by ID in ascending order and then Timestamp by oldest to newest
gps_name <- gps_name %>% # Add running number in the first column
  mutate(No = 1:n()) %>% 
  dplyr::select(No, everything()) 
gps_name <- gps_name %>% # Create additional column indicating time taken from previous timestamp for same ID
  mutate(Delta = Timestamp - lag(Timestamp, default = first(Timestamp)))
gps_name$Delta <- as.numeric(gps_name$Delta) # Convert Delta column to numeric format
gps_name$Delta_Hours <- round(gps_name$Delta / 60 / 60, 1) # Create column to convert Delta seconds into hours with one decimal place
gps_sf <- st_as_sf(gps_name, coords = c("long", "lat"), crs = 4326) # Changing into a shapefile

rm(gps) # Remove unused earlier dataset

spots <- gps_name %>% # Filtering out stationary gps coordinates of more than 10 mins
  filter(Delta > 600)
spots$No <- rep(1:2965, times = 1) # Redo running number in the first column

spots$Location <- 1 # Create a Location column
spots <- spots %>% mutate( # Create additional column with location names based on latlong
  Location = case_when(
    between(lat, 36.05092013, 36.05102938) & 
      between(long, 24.82586806, 24.82598723)  ~ "Abila Airport", # 35 features
    between(lat, 36.07434876, 36.07443715) & 
      between(long, 24.84592966, 24.84598782)  ~ "Abila Scrapyard", # 4 features
    between(lat, 36.06342076, 36.06349309) & 
      between(long, 24.85096457, 24.85103679)  ~ "Abila Zacharo", # 66 features
    between(lat, 36.07712237, 36.07715385) & 
      between(long, 24.87617634, 24.87621582)  ~ "Ahaggo Museum", # 5 features
    between(lat, 36.07522801, 36.07530344) & 
      between(long, 24.85626503, 24.85634849)  ~ "Albert's Fine Clothing", # 20 features
    between(lat, 36.08172086, 36.08182543) & 
      between(long, 24.85086882, 24.85096705)  ~ "Bean There Done That", # 46 features
    between(lat, 36.05402149, 36.05413903) & 
      between(long, 24.90116515, 24.90128202)  ~ "Brew've Been Served", # 106 features
    between(lat, 36.07332048, 36.07336116) & 
      between(long, 24.86416419, 24.86420583)  ~ "Brewed Awakenings", # 36 features
    between(lat, 36.06582469, 36.065941) & 
      between(long, 24.90097567, 24.90108865)  ~ "20 Building Control Stenig's Home", # 20 features
    between(lat, 36.05851786, 36.05860144) & 
      between(long, 24.8808655, 24.88092654)  ~ "Carlyle Chemical Inc.", # 30 features
    between(lat, 36.07818062, 36.07821857) & 
      between(long, 24.87211555, 24.8721508)  ~ "4 CFO Ingrid's Home", # 27 features
    between(lat, 36.07682044, 36.07685752) & 
      between(long, 24.8658641, 24.86589901)  ~ "10 CIO Ada's Home", # 35 features
    between(lat, 36.0721156, 36.07215701) & 
      between(long, 24.87458425, 24.8746267)  ~ "32 COO Orhan's Home", # 29 features
    between(lat, 36.07062423, 36.07073983) & 
      between(long, 24.89517609, 24.89526281)  ~ "Chostus Hotel", # 11 features
    between(lat, 36.05462322, 36.05469486) & 
      between(long, 24.88977034, 24.88983886)  ~ "Coffee Cameleon", # 29 features
    between(lat, 36.08954231, 36.08962196) & 
      between(long, 24.86066508, 24.8607611)  ~ "Desafio Golf Course", # 10 features
    between(lat, 36.07292088, 36.07301365) & 
      between(long, 24.88396447, 24.88405897)  ~ "26 Drill Site Manager Marin's Home", # 26 features
    between(lat, 36.08442031, 36.08449538) & 
      between(long, 24.86416741, 24.8642387)  ~ "7 Drill Technician Elsa's Home", # 25 features
    between(lat, 36.08424703, 36.08432477) & 
      between(long, 24.8563809, 24.8564637)  ~ "9 Drill Technician Gustav's Home", # 13 features
    between(lat, 36.0726185, 36.07380904) & 
      between(long, 24.87510166, 24.87613744)  ~ "28 Drill Technician Isande's Home", # 26 features
    between(lat, 36.06922564, 36.06931513) & 
      between(long, 24.88416486, 24.88426267)  ~ "27 Drill Technician Kare's Home", # 20 features
    between(lat, 36.08542073, 36.08550845) & 
      between(long, 24.86036422, 24.86045943)  ~ "2 Engineer Lars's Home", # 37 features
    between(lat, 36.08664252, 36.08672442) & 
      between(long, 24.85756416, 24.85766744)  ~ "3 Engineer Felix's Home", # 22 features
    between(lat, 36.07622023, 36.07626546) & 
      between(long, 24.87466429, 24.87471053)  ~ "35 Environmental Safety Advisor Willem's Home", # 33 features
    between(lat, 36.07212045, 36.07213193) & 
      between(long, 24.84132949, 24.84134818)  ~ "Frank's Fuel", # 2 features
    between(lat, 36.05492145, 36.05503511) & 
      between(long, 24.90176782, 24.90188061)  ~ "Frydos Autosupply n' More", # 29 features
    between(lat, 36.04802098, 36.04805422) & 
      between(long, 24.87956497, 24.87957691)  ~ "GasTech", # 738 features
    between(lat, 36.05970763, 36.05981097) & 
      between(long, 24.85797552, 24.8580772)  ~ "Gelatogalore", # 47 features
    between(lat, 36.06034564, 36.06043016) & 
      between(long, 24.85646426, 24.85657454)  ~ "General Grocer", # 12 features
    between(lat, 36.05572125, 36.05584094) & 
      between(long, 24.90246542, 24.90258487)  ~ "Guy's Gyros", # 143 features
    between(lat, 36.06362146, 36.06371539) & 
      between(long, 24.88586605, 24.88595859)  ~ "Hallowed Grounds", # 70 features
    between(lat, 36.07660977, 36.07669909) & 
      between(long, 24.85756408, 24.85764247)  ~ "Hippokampos", # 155 features
    between(lat, 36.08412146, 36.08420924) & 
      between(long, 24.85896842, 24.85905081)  ~ "11 Hydraulic Technician Axel's Home", # 23 features
    between(lat, 36.08782802, 36.08793196) & 
      between(long, 24.85627136, 24.8563725)  ~ "19 Hydraulic Technician Vira's Home", # 24 features
    between(lat, 36.06641679, 36.06650723) & 
      between(long, 24.88256875, 24.88265687)  ~ "1 IT Helpdesk Nils's Home", # 31 features
    between(lat, 36.06729646, 36.06736745) & 
      between(long, 24.87788423, 24.87795559)  ~ "5 IT Technician Isak's Home", # 21 features
    between(lat, 36.06722012, 36.06731624) & 
      between(long, 24.8858687, 24.88596759)  ~ "8 IT Technician Lucas's Home", # 23 features
    between(lat, 36.06749651, 36.0675518) & 
      between(long, 24.87330651, 24.873366)  ~ "Jack's Magical Beans", # 31 features
    between(lat, 36.06582037, 36.06584879) & 
      between(long, 24.85236427, 24.85241027)  ~ "Kalami Kafenion", # 47 features
    between(lat, 36.05442247, 36.05453641) & 
      between(long, 24.89986596, 24.89998054)  ~ "Katerina's Cafe", # 158 features
    between(lat, 36.05292229, 36.05296701) & 
      between(long, 24.84936915, 24.84941679)  ~ "Kronos Capital", # 6 features
    between(lat, 36.06582196, 36.06587998) & 
      between(long, 24.8497762, 24.84983936)  ~ "Kronos Mart", # 9 features
    between(lat, 36.06523446, 36.06534083) & 
      between(long, 24.83307421, 24.83318494)  ~ "Kronos Pipe and Irrigation", # 7 features
    between(lat, 36.06402993, 36.06410072) & 
      between(long, 24.84137818, 24.84144338)  ~ "Maximum Iron and Steel", # 9 features
    between(lat, 36.05840347, 36.05849041) & 
      between(long, 24.88546548, 24.88553455)  ~ "Nationwide Refinery", # 41 features
    between(lat, 36.05859158, 36.05859887) & 
      between(long, 24.85790261, 24.85799357)  ~ "Octavio's Office Supplies", # 3 features
    between(lat, 36.05192066, 36.05197575) & 
      between(long, 24.87076418, 24.87082137)  ~ "Ouzeri Elian", # 67 features
    between(lat, 36.06764972, 36.06775002) & 
      between(long, 24.90243213, 24.9025445)  ~ "34 Perimeter Control Edvard's Home", # 20 features
    between(lat, 36.06324941, 36.06330782) & 
      between(long, 24.85226894, 24.8523291)  ~ "Roberts and Sons", # 9 features
    between(lat, 36.05942407, 36.05952152) & 
      between(long, 24.89476557, 24.8948649)  ~ "Shared Home A - 6 Linnea 25 Kanon 29 Bertrand", # 72 features
    between(lat, 36.06332304, 36.06343537) & 
      between(long, 24.89607033, 24.89617856)  ~ "Shared Home B - 14 Lidelse 18 Birgitta 21 Hennie", # 60 features
    between(lat, 36.06242283, 36.06253955) & 
      between(long, 24.89877023, 24.89888179)  ~ "Shared Home C - 17 Sven 24 Minke 33 Brand", # 68 features
    between(lat, 36.05842222, 36.05853828) & 
      between(long, 24.90096522, 24.90107874)  ~ "Shared Home D - 22 Adra 23 Varja 30 Felix", # 73 features
    between(lat, 36.0603222, 36.06044736) & 
      between(long, 24.90556693, 24.90569385)  ~ "Shared Home E - 13 Inga 15 Loreto 16 Isia 21 Hennie", # 85 features
    between(lat, 36.05282139, 36.05288367) & 
      between(long, 24.86856868, 24.8686314)  ~ "Shoppers' Delight", # 17 features
    between(lat, 36.06772112, 36.06784956) & 
      between(long, 24.89906521, 24.89917328)  ~ "12 Site Control Hideki's Home", # 21 features
    between(lat, 36.05409586, 36.05420832) & 
      between(long, 24.90806584, 24.90817838)  ~ "Stewart and Sons Fabrication", # 36 features
    between(lat, 36.06774029, 36.06776587) & 
      between(long, 24.87148791, 24.87150031)  ~ "U-Pump", # 4 features
    between(lat, 36.05012433, 36.05021624) & 
      between(long, 24.9003978, 24.90047475)  ~ "Anonymous Site 1", # 6 features
    between(lat, 36.06314781, 36.06324321) & 
      between(long, 24.90010823, 24.90018668)  ~ "Anonymous Site 2", # 7 features
    between(lat, 36.05893131, 36.05900826) & 
      between(long, 24.89277554, 24.89284962)  ~ "Anonymous Site 3", # 7 features
    between(lat, 36.08061881, 36.08067087) & 
      between(long, 24.84681621, 24.84688282)  ~ "Anonymous Site 4", # 7 features
    between(lat, 36.06944928, 36.0695319) & 
      between(long, 24.84147082, 24.84157048)  ~ "Anonymous Site 5", # 8 features
    between(lat, 36.05149231, 36.05253234) & 
      between(long, 24.87495168, 24.87611086)  ~ "Anonymous Site 6", # 13 features
    between(lat, 36.05543848, 36.05657576) & 
      between(long, 24.86618187, 24.86735)  ~ "Anonymous Site 7", # 7 features 
    between(lat, 36.07099038, 36.07200089) & 
      between(long, 24.86869468, 24.86985682)  ~ "Anonymous Site 8", # 10 features 
  ))

spots$concat_spots_cc <- paste(spots$date,spots$Location,spots$hour) # Create a separate column of unique values using concatenated values in the distilled GPS data
spots_median <- spots %>% # Extract the median lat & long coordinates of locations
  group_by(Location) %>%
  summarise(lat.median = median(lat), long.median = median(long), .groups = "drop") %>%
  filter(!is.na(Location)) %>% # Exclude remaining few unmatched locations
  ungroup()

spots_median <- spots_median %>% # Add additional column to classify locations into major buckets
  mutate(Location.Type = case_when(
    Location %in% c("Anonymous Site 1",
                    "Anonymous Site 2",
                    "Anonymous Site 3",
                    "Anonymous Site 4",
                    "Anonymous Site 5",
                    "Anonymous Site 6",
                    "Anonymous Site 7",
                    "Anonymous Site 8") ~ "Unknown",
    Location %in% c("Bean There Done That",
                    "Brew've Been Served",
                    "Brewed Awakenings",
                    "Coffee Cameleon",
                    "Jack's Magical Beans",
                    "Hallowed Grounds") ~ "Coffee Cafe",
    Location %in% c("Abila Zacharo",
                    "Gelatogalore",
                    "Guy's Gyros",
                    "Hippokampos",
                    "Kalami Kafenion",
                    "Katerina's Cafe",
                    "Ouzeri Elian") ~ "Food Joints",
    Location %in% c("GasTech") ~ "HQ",
    Location %in% c("1 IT Helpdesk Nils's Home",
                    "10 CIO Ada's Home",
                    "11 Hydraulic Technician Axel's Home",
                    "12 Site Control Hideki's Home",
                    "19 Hydraulic Technician Vira's Home",
                    "2 Engineer Lars's Home",
                    "20 Building Control Stenig's Home",
                    "26 Drill Site Manager Marin's Home",
                    "27 Drill Technician Kare's Home",
                    "28 Drill Technician Isande's Home",
                    "3 Engineer Felix's Home",
                    "32 COO Orhan's Home",
                    "34 Perimeter Control Edvard's Home",
                    "35 Environmental Safety Advisor Willem's Home",
                    "4 CFO Ingrid's Home",
                    "5 IT Technician Isak's Home",
                    "7 Drill Technician Elsa's Home",
                    "8 IT Technician Lucas's Home",
                    "9 Drill Technician Gustav's Home",
                    "Shared Home A - 6 Linnea 25 Kanon 29 Bertrand",
                    "Shared Home B - 14 Lidelse 18 Birgitta 21 Hennie",
                    "Shared Home C - 17 Sven 24 Minke 33 Brand",
                    "Shared Home D - 22 Adra 23 Varja 30 Felix",
                    "Shared Home E - 13 Inga 15 Loreto 16 Isia 21 Hennie") ~ "Residential",
    Location %in% c("Abila Scrapyard",
                    "Carlyle Chemical Inc.",
                    "Kronos Pipe and Irrigation",
                    "Maximum Iron and Steel",
                    "Nationwide Refinery",
                    "Stewart and Sons Fabrication") ~ "Industrial",    
    Location %in% c("Ahaggo Museum",
                    "Albert's Fine Clothing",
                    "Kronos Mart",
                    "Octavio's Office Supplies",
                    "Shoppers' Delight",
                    "General Grocer",
                    "Roberts and Sons") ~ "Leisure & Shopping",
    Location %in% c("Abila Airport",
                    "Chostus Hotel",
                    "Desafio Golf Course",
                    "Kronos Capital") ~ "Complex",
    Location %in% c("Frank's Fuel",
                    "Frydos Autosupply n' More",
                    "U-Pump") ~ "Transport",
  ))
spots_median_sf <- st_as_sf(spots_median, coords = c("long.median", "lat.median"), crs = 4326) # Changing into a shapefile

spots <- spots %>%
  mutate(Timestamp_start = Timestamp - Delta)

spots$start_time <- format(spots$Timestamp_start, format = "%H:%M")
spots$end_time <- format(spots$Timestamp, format = "%H:%M")

spots$start_timestamp <- format(spots$Timestamp_start, format = "%d %b %H:%M")
spots$end_timestamp <- format(spots$Timestamp, format = "%d %b %H:%M")

spots$start_date <- format(spots$Timestamp_start, format = "%Y-%m-%d")

spots_table <- spots %>%
  dplyr::select(start_date, Name, start_timestamp, end_timestamp,Delta_Hours, Location) %>%
  left_join(dplyr::select(spots_median_sf,Location, Location.Type), by = c("Location" = "Location"))

spots_table$geometry <- NULL
colnames(spots_table) <- c("start_date","Name","Start_Time","End_Time","Duration_Hours","Location", "Location_Type")

rm(spots) # Remove unused earlier dataset
rm(spots_median) # Remove unused earlier dataset

# Creating the map

Abila_st_union <- st_combine(Abila_st) # Dissolve Abila road network
Abila_st_union <- st_union(Abila_st_union)
Abila_st_proj <- st_transform(Abila_st_union, crs = 3857) # Transform to necessary projection
Abila_st_buffer <- st_buffer(Abila_st_proj, dist = 25, nQuadSegs = 5) # Create a buffer around the dissolved Abila road network

rm(Abila_st) # Remove unused earlier dataset
rm(Abila_st_union) # Remove unused earlier dataset
rm(Abila_st_proj) # Remove unused earlier dataset

#---------------error below

#gps_path <- st_sfc(st_linestring(st_coordinates(gps_sf)), crs = 4326)

#gps_sf <- gps_name %>%
#  group_by(id, Name, RoleNName, date, lat, long) #%>%
#  summarize(m = mean(Timestamp), 
#            do_union=FALSE) #%>%
  #ungroup()

#gps_sf <- st_as_sf(gps_sf, coords = c("long", "lat"), crs = 4326) %>% # Changing into a shapefile
#  st_combine() %>%
#  st_cast("LINESTRING")

#rm(gps_sf)

#gps_path <- gps_name %>%
#  group_by(id, Name, RoleNName, date, lat, long) %>%
#  sf_linestring(x = "long", y = "lat")
#  summarize(m = mean(Timestamp), 
#            do_union=FALSE) %>%

#st_cast("LINESTRING")



#gps_sf <- st_as_sf(gps_name, coords = c("long", "lat"), crs = 4326) # Changing into a shapefile



#gps_path <- gps_name %>%
#  group_by(id, Name, RoleNName, date) #%>%
#  summarize(m = mean(Timestamp), 
#            do_union=FALSE,
#            .groups = "drop") %>%
#  st_geometry() %>%
#  st_cast("LINESTRING")

#gps_path <- st_as_sf(gps_path, coords = c("long", "lat"), crs = 4326) %>%
#  st_cast("LINESTRING")

#typeof(gps_path)
#class(gps_path)

#  sf_linestring(x)
#gps_path <- gps_sf %>% # Creating a movement path
#  group_by(id, Name, RoleNName, date) %>%
#  summarize(m = mean(Timestamp), 
#            do_union=FALSE #, 
#            .groups = "drop"
#            ) %>%
#  left_join(dplyr::select(car,CarID,RoleNName), by = c("id" = "CarID")) %>% #Add in RoleNName column
#  ungroup() 

#gps_path <- st_as_sf(gps_path, coords = c("long", "lat"), crs = 4326) %>% # Changing into a shapefile
#  st_cast("LINESTRING")

#gps_sf <- st_as_sf(gps, coords = c("long", "lat"), crs = 4326) # Changing into a shapefile

#---------------------- error above

# Create blue polygon as background to mimic sea
long.sea <- c(24.91075,24.91075,24.8232,24.8232,24.91075)
lat.sea <- c(36.09543,36.0445,36.0445,36.09543,36.09543)
sea <-data.frame(long.sea, lat.sea)

rm(long.sea) # Remove unused earlier dataset
rm(lat.sea) # Remove unused earlier dataset

sea_sf <- st_as_sf(sea, coords = c("long.sea", "lat.sea"))
st_crs(sea_sf) <- 4326
sea_poly<- st_sf(
  st_cast(
    st_combine(sea_sf$geometry),"POLYGON"
  ))

rm(sea) # Remove unused earlier dataset
rm(sea_sf) # Remove unused earlier dataset

# Clip a smaller Kronos island around Abila
Kronos_sf_small <- st_crop(Kronos_sf, c(xmin = 24.8232, xmax = 24.91075, ymin = 36.0445, ymax = 36.09543))

rm(Kronos_sf) # Remove unused earlier dataset
rm(car) # Remove unused earlier dataset
rm(gps_name) # Remove unused earlier dataset
#rm(gps_path) # Remove unused earlier dataset

#write_csv(gps_name,"gps_name.csv")

#--------------- Table for Heatmaply



