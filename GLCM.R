# -----------------------------------------
# Grey Level Co-Occurrence Matrix 
#
# To Calculate texture of an image
# 
# https://rstudio-pubs-static.s3.amazonaws.com/536921_af2c31c083544a3a9588da9c86692636.html

library(raster)
library(glcm)

# --------------------
# Load the csv DF of current Point Data
# NB: Perform Whole Process for Each Year
# df = read.csv("satellite_data/golaTrapPoints_30m_Sent12_q1_2022_popDens_hansYr.csv")
df = read.csv("satellite_data/golaTrapPoints_15m_Sent12_q1_2021_popDens_hansYr.csv")

# --------------------
# Load the Trap Point Data
traps = df[, 2:4] # read.csv("Data/Malaise_traps_points.csv", row.names = 1)
# Give them a spatial component
xy = traps[,c(1,2)]
spdf <- SpatialPointsDataFrame(coords = xy, data = traps,
                               proj4string = CRS("+proj=longlat +datum=WGS84"))

# --------------------
# Load the Satellite & Indices Raster
sent = brick("satellite_data/Sentinel_2/Sent2_q1_20220227_Bands_and_indices.tif")
# sent = brick("satellite_data/Sentinel_2/Sent2_q1_20210118_Bands_and_Indices.tif")
bands = c("blue", "green", "red", "RE1", "RE2", "RE3", "NIR", "SWIR", "MIR", "NBR", "VARI", "GNDVI", "EVI", "PVI", "MSAVI", "NDRE", "NDII5", "NDII7")
names(sent) = bands
ent = c()
###
for (i in 1:length(bands)) {
j = spatialEco::raster.entropy(sent[[i]], d = 5)
ent[i] = max(values(j), na.rm = T)
}
# Find Layer with Greatest Entropy to use in GLCM
print(Sys.time())




# # ------- CRS ------- #
# # --------------------
# # NB: Everything has different CRS
# # If we Transform Raster CRS then Raster cell values change
# # Thus we must transform the Point CRS to each Raster CRS & then extract the point values
p = spTransform(spdf, crs(sent))

# # -------------- Calculate Grey Level Co_Occurrence Matrix
# # ------------------and perform analysis on it

# Using the glcm package - this produces the GLC Matrix at any or all 4 angles, and mirrors by default
# Also performs defined analysis over the Matrix
df2 = df
s = Sys.time()
print(s)

for (band in 1:18) { # For every band in the Raster Brick
 # band = 1
  img = sent[[band]]    # Create a Single Raster Layer
  
  glcm = glcm(img,
              window = c(7,7), 
              shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)),
              n_grey = 8,
              statistics = c("mean", "contrast", "entropy"))  

# # --------------------------------------------------------
# # - Apply Buffers around Points &
# # - calculate Statistical Values of Raster Layers within the Buffers
# # - Add new values to DF  
# # --------------------------------------------------------

# n = names(img)
# plot(img)
# hist(glcm[[1]])  

glcm_spot = extract(x = glcm,
                    y = p,
                    # buffer=15,
                    # fun=mean,
                    df=T)
  
# Change 1st column name to be the same as the DF trap column name - eases merging with DF
colnames(glcm_spot)[1] = "SITE_ID"
# Insert trap names from the spatial points DF into the trap column - for merging with DF
glcm_spot$SITE_ID = p$SITE_ID

# Add the currently processed Band Colour Name to the glcm df
clnm = colnames(glcm_spot[2:4])
colnames(glcm_spot)[2:4] = paste0(names(img), "_",clnm)

# Bind the current glcm values with the major DF
df2 = dplyr::right_join(df2, glcm_spot, by="SITE_ID")

}  

print(Sys.time()-s)

write.csv(df2, "satellite_data/golaTrapPoints_15m_Sent12_q1_2022_popDens_hansYr_GLCM7_8.csv")
# write.csv(df2, "satellite_data/golaTrapPoints_15m_Sent12_q1_2021_popDens_hansYr_GLCM.csv")



