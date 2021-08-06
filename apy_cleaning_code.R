library(data.table)
library(dplyr)
library(readxl)
library(tidyr)

df<-read_excel("D:/Report 97-98.xls",skip = 2)
id=""
##filling NAs in `State/Crop/District' column based on previous row value
for (i in 2:nrow(df)) {
  id<-ifelse(is.na(df$`State/Crop/District`[i]),df$`State/Crop/District`[i-1],df$`State/Crop/District`[i])
  df$`State/Crop/District`[i]<-id
}

df$chk<-grepl("[[:digit:]]",df$`State/Crop/District`)   #districts name contains numbers  
df1<-df %>% filter(chk=="FALSE")                        #filtering out crops
df1<-df1[!is.na(df1$`State/Crop/District`),]
df1$Unit<-gsub("\\d+", "", df1$`Production (Tonnes)`) #pick only strings from Production column
df1$Unit<-ifelse(is.na(df1$`Production (Tonnes)`),"Tonnes",df1$Unit)     #converting all NAs to Tonnes

lgd<-read_excel("D:/LGD_codes_apy.xlsx",sheet = "Sheet1") #LGD codes for state and district

#crop name
df2<-df[!(tolower(df$`State/Crop/District`) %in% tolower(lgd$state_name)),]  #filtering out state names to keep crop names only
df2$crop<-""
id1<-""

#filling up crop column with crop names for dataframe - df2 (after figuring out pattern in the data)
for (i in 1:nrow(df2)) {
  id1<-ifelse(is.na(df2$`Area (Hectare)`[i]),df2$`State/Crop/District`[i],id1)
  df2$crop[i]<-id1
}
df2<-df2 %>% filter(chk=="TRUE")  #removing crop names from `State/Crop/District` column in df2

#state name
df3<-df[!(tolower(df$`State/Crop/District`) %in% tolower(df2$crop)),] #filtering out crop names
df3$state_name<-""
id2<-""

#filling up state_name column with State names for dataframe - df2 (after figuring out pattern in the data)
for (i in 1:nrow(df3)) {
  id2<-ifelse(is.na(df3$`Yield (Tonnes/Hectare)`[i]),df3$`State/Crop/District`[i],id2)
  df3$state_name[i]<-id2
}

df2$id<-paste0(df2$`State/Crop/District`,df2$`Area (Hectare)`,df2$`Production (Tonnes)`,df2$`Yield (Tonnes/Hectare)`) #creating unique identifier in df2 for join
df3$id<-paste0(df3$`State/Crop/District`,df3$`Area (Hectare)`,df3$`Production (Tonnes)`,df3$`Yield (Tonnes/Hectare)`) #creating unique identifier in df3 for join
df3<-df3 %>% select(id,state_name)

df4<-left_join(df2,df3,by="id")  #joining df2 and df3
df4<-df4[!duplicated(df4$id),]
df4$district_name<-gsub("\\d+.", "", df4$`State/Crop/District`)  #clean district name

df4$id<-paste0(tolower(df4$state_name),tolower(df4$district_name))  #creating unique identifier for join with lgd code
df6<-df1 %>% select(`State/Crop/District`,Unit)
df6<-df6[!duplicated(df6),]

lgd$id<-tolower(paste0(lgd$state_name,lgd$district_name)) ##creating unique identifier for join with df4
lgd_req<-lgd %>% select(id,state_code,district_code)      ##selecting 3 columns only
lgd_req$id<-gsub("[\r\n]", "",lgd_req$id)                 #strips line and spaces
df4$id<-gsub("[\r\n]", "",df4$id)                   #strips line and spaces
lgd_req<-lgd_req[!duplicated(lgd_req),]
df5<-left_join(df4,lgd_req,by="id")           #joining to get lgd codes
df6<-df6 %>% filter(!Unit=="")
df6<-df6 %>% filter(!Unit==".")               #filter to try and keep crop names only
df5<-left_join(df5,df6,by=c("crop"="State/Crop/District"))

df5<-df5 %>% select(state_name,state_code,district_name,district_code,Year,crop,Season,
                    `Area (Hectare)`,`Production (Tonnes)`,`Yield (Tonnes/Hectare)`,
                    Unit)

df5$Year<-"1997-1998"

crop_code<-read_excel("D:/crop_code.xlsx",sheet = "crop_code")  #reading the crop code file
crop_code$cropid<-tolower(crop_code$crop_name)
crop_code<-crop_code[!duplicated(crop_code$cropid),]

df5$cropid<-tolower(df5$crop)
df5<-df5[!duplicated(df5),]

df7<-left_join(df5,crop_code,by="cropid")   #joining to get crop codes
df8<-df7 %>% filter(!is.na(CROP_CODE))
df8$Unit<-as.character(df8$Unit)

#Organizing dataset
df8$Production_unit<-ifelse(substr(df8$Unit,1,1)=="T","Tonnes",
                            ifelse(substr(df8$Unit,1,1)=="B","Bales","Nuts"))
df8$Area_unit<-"Hectares"
df8$Yield_unit<-paste0(df8$Production_unit,"/",df8$Area_unit)
df8<-df8 %>% select("Year","state_name","state_code","district_name",
                    "district_code","Season","CROP_CODE","crop_name",
                    "Crop_Type","Area (Hectare)","Area_unit","Production (Tonnes)",
                    "Production_unit","Yield (Tonnes/Hectare)","Yield_unit")

#changing column names
colnames(df8)<-c("Year","State_name","State_code","District_name",
                 "District_code","Season","Crop_code","Crop_name",
                 "Crop_category","Area","Area_unit","Production",
                 "Production_unit","Yield","Yeild_unit")

df8<-df8 %>% mutate(across(where(anyNA), ~ replace_na(.,""))) #replacing NA with blank
df8<-df8[!duplicated(df8),]
write.csv(df8,"D:/apy_97-98.csv",row.names = F)
