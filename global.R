#### Load the required packages ####
library(shiny) # shiny features
library(shinydashboard) # shinydashboard functions
library(DT)  # for DT tables
library(dplyr)  # for pipe operator & data manipulations
library(plotly) # for data visualization and plots using plotly 
library(ggplot2) # for data visualization & plots using ggplot2
library(ggtext) # beautifying text on top of ggplot
library(sf) # for spatial data manipulation
library(readxl) # for reading Excel files
library(psych) # for data summary
library(maps) # for USA states map - boundaries used by ggplot for mapping
library(ggcorrplot) # for correlation plot
library(shinycssloaders) # to add a loader while graph is populating
library(leaflet)
library(mapview)
library(webshot)

# Load Attribute Data
attribute_data <- read_excel(normalizePath("Data/Data_Lengkap_2.xlsx"))

# Combine spatial data
spatial_data <- st_read("Data/Batas_Jawa_Simplified_Small.shp")
spatial_data$geometry <- st_make_valid(spatial_data$geometry)

## Add a new column `Wilayah` into the attribute dataset
my_data <- attribute_data %>%
  mutate(Wilayah = tolower(attribute_data$Wilayah))

# Convert spatial data to a data frame
state_map_df <- spatial_data %>%
  mutate(NAMOBJ = tolower(NAMOBJ)) %>%
  as.data.frame()

# Calculate centroids for spatial data
spatial_data <- spatial_data %>%
  mutate(centroid = st_centroid(geometry))

# Extract x and y coordinates from centroids
spatial_data <- spatial_data %>%
  mutate(
    x = st_coordinates(centroid)[, 1],
    y = st_coordinates(centroid)[, 2]
  )


# Column names without state. This will be used in the selectinput for choices in the shinydashboard
c1 = my_data %>% 
  select(-"Wilayah", -"Provinsi", -"Klaster") %>% 
  names()

# Menggabungkan data spasial dengan data lainnya
final_data <- right_join(my_data, state_map_df, by = c("Wilayah" = "NAMOBJ")) %>%
  filter(Wilayah != "Jawa Timur") %>%  # Kecualikan baris dengan nama "Jawa Timur"
  filter(!is.na(Klaster)) %>%          # Menghilangkan kategori NA
  mutate(Klaster = factor(Klaster, levels = c("Sangat Rendah", "Cukup Rendah", "Cukup Tinggi", "Sangat Tinggi"))) %>%
  st_as_sf()  # Pastikan data adalah objek sf

# Membuat custom palette warna untuk urutan kategori
colorPalette <- colorFactor(
  palette = RColorBrewer::brewer.pal(4, "YlGnBu"),  # Palet warna dengan 4 warna
  domain = final_data$Klaster,                      # Gunakan variabel kategori Klaster
  ordered = TRUE                                    # Pastikan kategori terurut
)




