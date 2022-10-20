library(readxl)
library(dplyr)

####################################
# --- Read in Site Data
site = readxl::read_excel(path = "Data/CocoaBugs_field_data_20220813.xlsx", sheet = "Section_all") 

str(site)
# Section and Entry Order are not required
site = within(data = site, expr = rm(Entry_order))

# Surveyor, Site ID, Stumps, Burn, Oil Palm need to be as Factor
fac = c("Surveyor", "SITE_ID", "recent_burning_y_n", "stumps_gt_10cm__y_n", "stumps_gt_30cm_y_n", "oil_palm_y_n")
site[fac] <- lapply(site[fac], as.factor)
rm(fac)

# Edit Site ID to be the same as for the count data
site$SITE_ID = ifelse(site$Section=="GRNP", paste0("GRNP",site$SITE_ID), site$SITE_ID)
site$SITE_ID = gsub(pattern = " ", replacement = "_", x = site$SITE_ID)
# site$SITE_ID = paste0(site$Section, "_", site$SITE_ID)
site$SITE_ID = gsub(pattern = "_new", replacement = "", x = site$SITE_ID)
# site$SITE_ID = gsub("^[^_]*_","", site$SITE_ID) # Remove everything before & including the first underscore - matching zero or more characters that are not an underscore ([^_]*) from the start (^) of the string, followed by an underscore (_) 


site = within(data = site, expr = rm(Section))

# Time deployed & collected are not required
# All deployments are for 5 days apart from 1 of 4 days range(site$date_collect-site$date_deploy) -
# - and all over a 6 week period range(site$date_deploy) - so dates are unlikely to be of any use
site = within(data = site, expr = rm(date_deploy, time_deploy, datetime_deploy, date_collect, time_collect, datetime_collect))

# Lat & Lon need to be Numeric
site$Latitude = as.numeric(site$Latitude)
site$Longitude = as.numeric(site$Longitude)
# NA have been identified on 1 site - site had to be moved and coordinates supplied independently
# Install them here 
s67 = readxl::read_excel(path = "Data/CocoaBugs_field_data_20220813.xlsx", sheet = "Gola_selected_sites_WGS1984")
s67Lat = s67$Latitude[s67$SITE_ID==67]
s67Lon = s67$Longitude[s67$SITE_ID==67]
site$Latitude[site$SITE_ID=="67 new"] = s67Lat
site$Longitude[site$SITE_ID=="67 new"] = s67Lon
rm(s67, s67Lat, s67Lon)
gc()

# Canopy ht, bare ground, grass, cocoa, liana, forest, agri need to be converted to numeric
names(site)
site[,5:11] = apply(site[,5:11], 2, function(x) gsub(pattern = " ", replacement = "", x))
site[,5:11] = apply(site[,5:11], 2, function(x) gsub(pattern = "%", replacement = "", x))
site[,5:11] = apply(site[,5:11], 2, function(x) gsub(pattern = ">", replacement = "", x))
site[,5:11] = apply(site[,5:11], 2, function(x) gsub(pattern = "<", replacement = "", x))
site[,5:11] = apply(site[,5:11], 2, function(x) gsub(pattern = "m", replacement = "", x))

# site[,5:11] <- lapply(site[,5:11], as.numeric)
site[,5:11] <- lapply(site[,5:11], as.factor)

# Audiomoth & BAR recorder not relevant unless we get sound data
site = within(data = site, expr = rm(Audiomoth, BAR_Recorder))

# Delete rows with NO sample tube & then delete sample tube column
site = site[site$sample_tube=="present",]
site = within(data = site, expr = rm(sample_tube))

write.csv(site, "Data/X_Site_Env_data.csv")


###############################
# --- Read in Survey Data

### ID - Link between Count & Site codes
id = readxl::read_excel(path = "Data/Malaise_traps_ZG0001592.client.otuids.xlsx", sheet = "SO00140.Leray2.QC")

### Individual Taxonomic Unit ("species") counts
sp = readxl::read_excel(path = "Data/Malaise_traps_ZG0001592.client.otuids.xlsx", sheet = "ZG0001592.client.otuids")

str(sp)
sp = sp[!is.na(sp$Phylum),]        # Remove rows with NA ID
sp = sp[sp$Class=="Insecta",]       # Select only ITU identified as Insecta
sp1 = as.data.frame(t(sp[, 19:length(sp)])) # Transpose only count data
rownames(sp1) = gsub("_MiSe.*", "", rownames(sp1)) # Reduce Row Names to equate to DNA ID from the
rownames(sp1) = gsub(".*_", "", rownames(sp1))     # ID sheet to then get Site Name for counts

sp1[is.na(sp1)]=0 # There are some NA values in the counts, which I change to value 0

sp1$DNA_ID = rownames(sp1)
sp1 = select(sp1, ncol(sp1), everything())


str(id)
# Columns that are not required
id = within(data = id, expr = rm(Library_type, Date_received, Kit_ID, Volume_filtered,Qubit,MiSeq,PCR_notes,Latitude, Longitude, Marker, Company, Project_name, Sample_type))
id = id[,-5]
# id = id[!is.na(id$Customer_ID),]        # Remove rows with NA ID
# id = id[!grepl("GRNP",id$Customer_ID),] # Remove rows with GRNP ID
# id$Customer_ID = gsub(pattern = "-", replacement = " ", x = id$Customer_ID)
# id$Customer_ID = gsub(pattern = " NEW", replacement = "_new", x = id$Customer_ID)
# id$Customer_ID = gsub(pattern = "Section .* ", replacement = "", x = id$Customer_ID)
colnames(id) = gsub(" ", "_", colnames(id))


############################
# Add Counts to Sites
y = right_join(id, sp1, by="DNA_ID")

y1 =y
y1$DNA_ID = gsub("[^0-9.-]", "", y1$DNA_ID) # Remove the split DNA ID so I can group & Sum them
y2 = aggregate(y1[,5:ncol(y1)], y1[,1:2], FUN = sum ) # Groups unique sets of DNA_ID and Site name and sums the sightings for all species
y3 = y2[,3:ncol(y2)]
y3[y3>0]=1
rowSums(y3)

# Edit the Site Codes column
y2$Customer_ID = gsub(pattern = "Section ", replacement = "", x = y2$Customer_ID)
y2$Customer_ID = gsub(pattern = " NEW", replacement = "_new", x = y2$Customer_ID)
y2$Customer_ID = gsub(pattern = "GRNP-", replacement = "GRNP", x = y2$Customer_ID)
y2$Customer_ID = gsub(pattern = " ", replacement = "_", x = y2$Customer_ID)
y2$Customer_ID = gsub(pattern = "-", replacement = "_", x = y2$Customer_ID)
names(y2)[2] = "SITE_ID"
y2$SITE_ID = gsub(pattern = "_new", replacement = "", x = y2$SITE_ID)
y2$SITE_ID = gsub("^[^_]*_","", y2$SITE_ID) # Remove everything before & including the first underscore - matching zero or more characters that are not an underscore ([^_]*) from the start (^) of the string, followed by an underscore (_)

# Sites named 66,67 and 68,81 are just named 66 and 68 in X Data so I change the names here to comply with that
y2$SITE_ID = gsub(pattern = ",67", replacement = "", x = y2$SITE_ID)
y2$SITE_ID = gsub(pattern = ",81", replacement = "", x = y2$SITE_ID)
y2$SITE_ID = gsub(pattern = "Waypoint_", replacement = "", x = y2$SITE_ID)


write.csv(y2, "Data/Y_Counts_per_site.csv")



####
# - Bind the Environment & Count data into 1 DF for imported into analysis or to have satellite data added

df = right_join(site, y2, by = "SITE_ID")
write.csv(df, "Data/XY_No_Satellite_Gola.csv")



