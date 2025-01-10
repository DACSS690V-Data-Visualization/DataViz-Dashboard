
# clean memory ------------------------------------------------------------
rm(list = ls())


# read in data ------------------------------------------------------------
#set working directory

#Loading the dataset
location='https://github.com/DACSS-Visual/tabular_univar_cat/raw/main/data/'
file='eduwa.rda'
link=paste0(location,file)
load(file=url(link))
table(eduwa$LocaleType)

#Ensuring no more than 70 characters per row.
str(eduwa,width = 70,strict.width='cut')

#ensuring the dataset loading correctly
head(eduwa)

#including all values
absoluteT <- table(eduwa$LocaleType, exclude = 'nothing')  
#naming all missing values
names(absoluteT)[is.na(names(absoluteT))] <- "Unknown"    

#Making the absolute count into percent
propT <- prop.table(absoluteT) * 100
cat("Proportional frequencies (percent):\n")

#read the updated values
print(propT)

#then making it a dataframe
tableFreq <- as.data.frame(absoluteT)
print(head(tableFreq))

#Selecting only Suburb
suburbEduwa <- eduwa[eduwa$LocaleType == 'Suburb',]

#making sure the selected data is clean
suburbEduwa$LocaleSub <- droplevels(suburbEduwa$LocaleSub)
suburbTable <- table(suburbEduwa$LocaleSub)

# see data ----------------------------------------------------------
#print the final output
print(suburbTable)

# see data types ----------------------------------------------------------
str(suburbTable)

# deliverable 1 ----------------------------------------------------------
library(ggplot2)

#Creating the barplot to display all the selected variables in the Eduwa dataset
del1Draft <- ggplot(tableFreq, aes(x = reorder(Var1, -Freq), y = Freq, fill = Var1))+ #using different colors to also serve as a legend to decipher the dataset.
  geom_bar(stat = "identity", color = "black") +
  #labeling the graph for easy reading
  labs(
    title = "Information on Schools in Different Locale Types",
    subtitle = "Count of different locale types, Suburb subset",
    caption = "Source: Eduwa dataset",
    x = NULL, 
    y = NULL,
  ) +
  theme_minimal() +
  #making sure all text are easily readable and visible
  geom_text(aes(label = Freq), vjust = -0.2) +
  scale_fill_discrete(name = "Locale Categories") +
  theme(axis.text.y = element_blank())

# save del1Draft ----------------------------------------------------------
saveRDS(del1Draft, file = "del1Draft.rds")


# deliverable 2 ----------------------------------------------------------

#del2Draft= base + geom_histogram(aes(x=Student.Teacher.Ratio))
#del2Draft


# save del2Draft ----------------------------------------------------------
#saveRDS(del2Draft, file = "del2Draft.rds")


# deliverable 3 ----------------------------------------------------------

#del3Draft= base + geom_point(aes(x=Student.Teacher.Ratio,
#y=Free.Lunch))
#del3Draft 

# save del3Draft ----------------------------------------------------------
#saveRDS(del3Draft, file = "del3Draft.rds")


# deliverable 4  ----------------------------------------------------------

#library(sf)
#county_map=sf::read_sf("WA_County_Boundaries.geojson")
#head(county_map)
#head(mydata)

# merge data into map ----------------------------------------------------------
#mydataCounty=aggregate(data=mydata,Free.Lunch~County,FUN = mean)
#myMapLunch=merge(county_map,mydataCounty,by.x='JURISDIC_2',"County")

# prepare plot

#base=ggplot(myMapLunch)
#del4Draft=base + geom_sf(aes(fill=Free.Lunch))
#del4Draft

# save del4Draft ----------------------------------------------------------
#saveRDS(del4Draft, file = "del4Draft.rds")