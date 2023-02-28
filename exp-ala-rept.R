#loading necessary packages
library(tidyverse)

#read file
act_rept <- read.csv("dwc-reptiles-act.csv")

#check columns and select to interested 
str(act_rept)
colnames(act_rept)
head(act_rept)

#selected columns of interest, mainly those with columns of that has most data
act_df <- act_rept %>% select(species, genus, family, order, vernacularName, sex, 
                              individualCount, basisOfRecord, occurrenceStatus, provenance,  
                              year, month, habitat)

#quick check and explore
summary(act_df) 
str(act_df) #some "empty" cells, be aware

act_df %>% count(species) #remove records with no species name and count no of. records

act_df <- act_df %>% filter(species != "") #remove records with no species names as it is less informative

act_df %>% count(family, genus)

act_df %>% count(sex) #very little of the records actually have sex recorded
act_df %>% count(individualCount) #more than half has individual count records
act_df %>% count(basisOfRecord) #all has basis of record
act_df %>% count(occurrenceStatus) #no 'absent' type data 
act_df %>% count(provenance) #20% from published dataset, many not recorded info
act_df %>% count(year) #check for NAs when use
act_df %>% count(month) #check for NAs when use
act_df %>% count(habitat) #inconsistent usage of terms, might be difficult to use for this quick analysis

#change to factor
act_df$family <- as.factor(act_df$family)
act_df$basisOfRecord <- as.factor(act_df$basisOfRecord)
act_df$month <- as.factor(act_df$month)


#we'll focus on family first as it is a manageable category for visualisation
fam.count <- act_df %>% filter(year != "") %>% count(family, year)
fam.count

#do we have more records as the year goes by?
act.year <- act_df %>% filter(year!="")
ggplot(act.year, aes(x = year)) +
  geom_bar(stat="count")
#yes, although there seems to be a peak somewhere in the 1990s as well. could this be published data? specific species?
ggplot(act.year, aes(x = year, fill = provenance)) +
  geom_bar(stat="count")
#ah very interesting! early records before 1990s are mostly published dataset. then the rise of records with no tags which I assume are citizen science records?
ggplot(act.year, aes(x = year, fill = basisOfRecord)) +
  geom_bar(stat="count") + 
  facet_wrap(~provenance)
#we get an even clearer view of this in this graph. most published dataset records are preserved specimesn which are older! 
#very little published dataset is actually based on observation/occurence as also seen below
ggplot(act_df %>% filter(provenance == "Published dataset"), aes(x=year, fill= basisOfRecord)) +
  geom_bar(stat="count")
#only more recent papers have started including observation data. however only a very small portion of our dataset has been used in publishings!

#now, what do the records observe in terms of thje 
#how many records for each reptile family?
ggplot(fam.count, aes(x = family, y = n)) +
  geom_bar(stat = "identity")
#most records are for skinks, followed by agamids

#does the trend change over time for which family is recorded?
ggplot(act.year, aes(x = year, fill = family)) +
  geom_bar(stat="count")

ggplot(fam.count, aes(x = year, y = n, colour = family)) +
  geom_line()
#there are a lot of skinks recorded in the ACT!

ggplot(fam.count, aes(x = year, y = n)) + 
  geom_line() + 
  facet_wrap(~family, scales ="free_y")
#some taxa only recorded during a short period of time

#is there a monthly trend of recorded taxa?
fam.month <- act_df  %>% filter(year != "", month !=  "") %>% count(family, month, year)
ggplot(fam.month, aes(x = month, y = n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~family, scales ="free_y")
#so more reptile records during the summer which is expected! trend is less so for sea turtles although very little data.
#most reptiles are recorded in months Oct-Dec. some are present for longer through the season (Agamids, Elapids, Geckos, Skinks). 
#Varanids are recorded more slighlty later in the season around January-February.

ggplot(fam.month, aes(x = year, y = n, colour = month)) +
  geom_line() +
  facet_wrap(~family, scales ="free")

ggplot(fam.month, aes(x = year, y = n, colour = family)) +
  geom_line() +
  facet_wrap(~month, scales ="free")

