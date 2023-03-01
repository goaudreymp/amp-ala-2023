#loading necessary packages
library(tidyverse)
library(viridis)

#read file
act_rept <- read.csv("dwc-reptiles-act.csv")

#check columns and select to interested 
head(act_rept)
colnames(act_rept)

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

act_df %>% count(sex) #very little of the records actually have sex recorded - remove
act_df %>% count(individualCount) #more than half has individual count records
act_df %>% count(basisOfRecord) #all has basis of record 
act_df %>% count(occurrenceStatus) #no 'absent' type data - remove 
act_df %>% count(provenance) #20% from published dataset, many not recorded info
act_df %>% count(year) #check for NAs when use
act_df %>% count(month) #check for NAs when use
act_df %>% count(habitat) #inconsistent usage of terms, might be difficult to use for this quick analysis - remove

act_df <- act_df %>% select(-sex, -occurrenceStatus, -habitat)

#change meaningful variables to factor
act_df$family <- as.factor(act_df$family)
act_df$basisOfRecord <- factor(act_df$basisOfRecord,
                               labels = c("Human Observation", "Material Sample", "Observation", "Occurrence", "Preserved Specimen"))
act_df$month <- as.factor(act_df$month)
act_df$provenance <- factor(act_df$provenance, 
                            levels = c("Published dataset", "Individual sightings", ""), 
                            labels = c("Published", "Sightings", "Unknown"))

#adding a season factor to visualise
act_df <- act_df %>% mutate(season = case_when(month %in% c(12, 1, 2) ~ "summer",
                                                     month %in% c(3, 4, 5) ~ "autumn",
                                                     month %in% c(6, 7, 8) ~ "winter",
                                                     month %in% c(9, 10, 11) ~ "spring"))
act_df$season <- factor(act_df$season)
str(act_df)

#Temporal Trends in ALA's ACT Reptilia Records

#Type Trends
#how do the type of records change over the years?
act.year <- act_df %>% filter(year!="") #remove records without years
head(act.year)

#cumulative no of records
act.y.sum <- act.year %>% group_by(year) %>% summarize(n = n())
act.y.sum$cumulative <- cumsum(act.y.sum$n)
head(act.y.sum)

ggplot(act.y.sum, aes(x = year, y = cumulative)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(limits = c(0, 10000), expand = c(0,0), breaks = c(0, 2000, 4000, 6000, 8000, 10000)) +
  xlab("Year") +
  ylab("Cumulative No. of Records") +
  theme_minimal() +
  theme(legend.position = "none", axis.line = element_line(colour = "black"))

#yes, although there seems to be a peak somewhere in the 1990s as well. 

#could this be published data? specific species?
ggplot(act.year, aes(x = year, fill = provenance)) +
  geom_bar(stat="count") +
  scale_y_continuous(limits = c(0, 900), expand = c(0,0), breaks = c(0, 200, 400, 800)) +
  xlab("Year") +
  ylab("No. of Records") +
  labs(fill = "") +
  theme_minimal() +
  theme(legend.position = "right", axis.line = element_line(colour = "black"))

#ah very interesting! early records before 1990s are mostly published dataset. then the rise of records with unknown provenance
ggplot(act.year, aes(x = year, fill = basisOfRecord)) +
  geom_bar(stat="count") + 
  facet_wrap(~provenance, scales = "free_y") +
  scale_y_continuous(expand = c(0,0)) +
  xlab("Year") +
  ylab("No. of Records") +
  labs(fill = "") +
  theme_minimal() +
  theme(legend.position = "right", axis.line = element_line(colour = "black"))
#we get an even clearer view of this in this graph. most published dataset records are preserved specimesn which are older! 
#very little published dataset is actually based on observation/occurence 

#only more recent papers have started including observation data. however only a very small portion of our dataset has been used in publishings!

#Taxon Trends
#we'll focus on family as it is a manageable category for visualisation
fam.count <- act_df %>% filter(year != "") %>% count(family, year)
head(fam.count)

f.col <- viridis(length(unique(fam.count$family))) #colors for visualisation

#now, what do the records observe in terms of thje 
#how many records for each reptile family?
ggplot(fam.count, aes(x = family, y = n, fill = family)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = f.col) +
  scale_y_continuous(expand = c(0,0)) +
  xlab("Reptilia Family") +
  ylab("No. of Records") +
  labs(fill = "") +
  theme_minimal() +
  theme(legend.position = "right", axis.line = element_line(colour = "black"))
#most records are for skinks, followed by agamids

#does the trend change over time for which family is recorded?
ggplot(act.year, aes(x = year, fill = family)) +
  geom_bar(stat="count") +
  scale_fill_manual(values = f.col) +
  scale_y_continuous(expand = c(0,0)) +
  xlab("Year") +
  ylab("No. of Records") +
  labs(fill = "") +
  theme_minimal() +
  theme(legend.position = "right", axis.line = element_line(colour = "black"), )
#there are a lot of skinks recorded in the ACT! this seems consistent over the years

#is there a monthly trend of recorded taxa?
fam.month <- act_df  %>% filter(year != "", month !=  "") %>% count(family, month, year, season)
head(fam.month)

s.col <- c("#D55E00", "#CC79A7", "#009E73","#56B4E9")

ggplot(fam.month, aes(x = month, y = n, fill = season)) +
  geom_bar(stat = "identity") +
  facet_wrap(~family, scales ="free_y", ncol=4) +
  scale_fill_manual(values = s.col)  +
  scale_y_continuous(expand = c(0,0)) +
  labs(fill = "") +
  xlab("Month") +
  ylab("No. of Records") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), strip.text = element_text(face = "bold"))
#so more reptile records during the summer which is expected! trend is less so for sea turtles although very little data.
#most reptiles are recorded in months Oct-Dec. some are present for longer through the season (Agamids, Elapids, Geckos, Skinks).
#Varanids are recorded more slighlty later in the season around January-February.

#do we also tend to see more individuals (rather than just no of records?)
fam.ind <- act_df %>% filter(individualCount > 0, month !="") %>%  group_by(family, month, season) %>% summarize(summed =sum(individualCount))
head(fam.ind)

ggplot(fam.ind, aes(x = month, y = summed, fill = season)) +
  geom_bar(stat = "identity") +
  facet_wrap(~family, scales = "free_y", ncol=4) +
  scale_fill_manual(values = s.col)  +
  scale_y_continuous(expand = c(0,0)) +
  labs(fill = "") +
  xlab("Month") +
  ylab("No. of Individuals") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), strip.text = element_text(face = "bold"))
