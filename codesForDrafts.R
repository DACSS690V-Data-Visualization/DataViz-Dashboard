
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
del1Draft
# save del1Draft ----------------------------------------------------------
saveRDS(del1Draft, file = "del1Draft.rds")


# deliverable 2 ----------------------------------------------------------

#importing the dataset
linkMass="https://github.com/DACSS-Visual/tabular_bivar_catcat/raw/refs/heads/main/data/MSP%20DFS%20Arrests%2019-20Q1.xlsx"

#upload the dataset
library(rio)
library(dplyr)
library(ggplot2)
library(ggpmisc)   
library(tibble)

arrests=rio::import(linkMass,which = 1)
head(arrests)
colnames(arrests)[colnames(arrests) == "Arrest Type"] <- "Arrest_type" #update the column name
colnames(arrests)

unique(arrests$Arrest_type)

#Data exploration based on arrest type and age

#statistical overview of the dataset
summary(arrests)
view(arrests[c("Arrest_type", "Age")]) #viewing the two variables of interest

summary(arrests[c("Arrest_type", "Age")]) #statistical relevance

colSums(is.na(arrests[c("Arrest_type", "Age")])) #checking for missing data first

#Remove missing data 
arrests_clean <- na.omit(arrests[c("Arrest_type", "Age")])
colSums(is.na(arrests_clean))

#Creating a table to store values of median and max age.
age_summary <- arrests_clean |>
  group_by(Arrest_type) |>
  summarise(
    median_age = median(Age, na.rm = TRUE),
    max_age = max(Age, na.rm = TRUE))

head(age_summary) #ensuring the code ran correctly.

# Create the TAnnot function to generate tables with summary values
TAnnot = function(wvalues, posX, posY, label) {
  if (nrow(wvalues) == 0) {
    return(NULL)  
  }
  
  output = tibble(
    x = posX,
    y = posY,
    tb = list(
      tibble(!!label := round(wvalues[[label]], 3))
    )
  )
  return(output)
}


# Creating the table annotations for each arrest type for median and max
F_TMedian = TAnnot(age_summary[age_summary$Arrest_type == "F",], 
                   posX = age_summary$median_age[age_summary$Arrest_type == "F"], 
                   posY = 1, label = "median_age")  # Felony median at y = 1

F_TMax = TAnnot(age_summary[age_summary$Arrest_type == "F",], 
                posX = age_summary$max_age[age_summary$Arrest_type == "F"], 
                posY = 1, label = "max_age")  # Felony max at y = 1

M_TMedian = TAnnot(age_summary[age_summary$Arrest_type == "M",], 
                   posX = age_summary$median_age[age_summary$Arrest_type == "M"], 
                   posY = 2, label = "median_age")  # Misdemeanor median at y = 2

M_TMax = TAnnot(age_summary[age_summary$Arrest_type == "M",], 
                posX = age_summary$max_age[age_summary$Arrest_type == "M"], 
                posY = 2, label = "max_age")  # Misdemeanor max at y = 2

O_TMedian = TAnnot(age_summary[age_summary$Arrest_type == "O",], 
                   posX = age_summary$median_age[age_summary$Arrest_type == "O"], 
                   posY = 3, label = "median_age")  # Other median at y = 3

O_TMax = TAnnot(age_summary[age_summary$Arrest_type == "O",], 
                posX = age_summary$max_age[age_summary$Arrest_type == "O"], 
                posY = 3, label = "max_age")  # Other max at y = 3

W_TMedian = TAnnot(age_summary[age_summary$Arrest_type == "W",], 
                   posX = age_summary$median_age[age_summary$Arrest_type == "W"], 
                   posY = 4, label = "median_age")  # Warrant median at y = 4

W_TMax = TAnnot(age_summary[age_summary$Arrest_type == "W",], 
                posX = age_summary$max_age[age_summary$Arrest_type == "W"], 
                posY = 4, label = "max_age")  # Warrant max at y = 4



# Filter out NULL annotations
annotations <- list(F_TMedian, F_TMax, M_TMedian, M_TMax, 
                    O_TMedian, O_TMax, W_TMedian, W_TMax) 

annotations <- annotations[!sapply(annotations, is.null)] 


# Creating a violin plot to display the output
Option1 <- ggplot(arrests_clean, aes(x = Age, y = Arrest_type)) +
  geom_violin(trim = FALSE, color = "black", aes(fill = Arrest_type)) +  
  
  # Customize the x-axis based on thet age
  scale_x_continuous(name = "Age", limits = c(20, 80)) +
  
  # Manually apply the same color to all categories.
  scale_fill_manual(name = "Arrest Type", 
                    values = c("W" = "brown", "O" = "brown", "M" = "brown", "F" = "brown"),  
                    labels = c("W" = "W = Warrant",
                               "O" = "O = Other",
                               "M" = "M = Misdemeanor",
                               "F" = "F = Felony")) +  
  
  labs(title = "Arrest Type by Median and Max Age", subtitle = "Massachusetts State Police Arrest Details by age and type of offense",
       caption = "Source: Mass.gov") +
  
  theme_minimal() +
  
  theme(legend.position = "right")

# Add table annotations 
for (annotation in annotations) {
  Option1 <- Option1 + geom_table(data = annotation, aes(x = x, y = y, label = tb), vjust = 0.5)
}

# Print the final plot
print(Option1)


# save del2Draft as Option1 ----------------------------------------------------------
saveRDS(Option1, file = "Option1.rds")


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