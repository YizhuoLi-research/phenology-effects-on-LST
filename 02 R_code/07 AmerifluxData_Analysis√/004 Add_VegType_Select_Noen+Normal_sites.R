# Based on the site names, we retrieved the corresponding latitude, longitude, 
# elevation, vegetation type (IGBP), climate classification (Koeppen), mean annual
# temperature, and precipitation from the AmeriFlux website (https://ameriflux.lbl.gov/). 
# 
# 
# This information was manually added to "1330_Noen+Normal_Results_23" and saved 
# as "1330_Noen+Normal_Results_23_all-info". 

# From "1330_Noen+Normal_Results_23_all-info", we filtered sites with vegetation 
# types DBF (Deciduous Broadleaf Forests), MF (Mixed Forests), and DNF (Deciduous 
# Needleleaf Forests, absent in the current data), 
# resulting in "1330_Noen+Normal_Results_17_all-info". 
# Here is the codes:

##################   00 重新筛选Ameriflux的植被类型  ###################


setwd("D:/VegetationImpact")


# 过滤数据，只保留DBF和MF类型
df <- read.csv("./AmerifluxData_Analysis/1330_Noen+Normal_Results_23_all-info.csv")
head(df)
selected_df <- df[df$Veg %in% c("DBF", "MF"), ]
head(selected_df)

write.csv(selected_df, "./AmerifluxData_Analysis/1330_Noen+Normal_Results_17_all-info.csv", row.names = FALSE)


##########################################################################


# This file was used for further analysis and visualization, 
# saved in "D:/VegetationImpact/AmerifluxData_Analysis".