
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

#upload the needed libaries
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
    median = median(Age, na.rm = TRUE),
    max= max(Age, na.rm = TRUE))

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


# Creating the table annotations for each arrest type for median and max and showing them on the far right side 
F_TMedian = TAnnot(age_summary[age_summary$Arrest_type == "F",], 
                   posX = 74, 
                   posY = 1, label = "median")

F_TMax = TAnnot(age_summary[age_summary$Arrest_type == "F",], 
                posX = 80,  
                posY = 1, label = "max")  

M_TMedian = TAnnot(age_summary[age_summary$Arrest_type == "M",], 
                   posX = 74, 
                   posY = 2, label = "median")

M_TMax = TAnnot(age_summary[age_summary$Arrest_type == "M",], 
                posX = 80, 
                posY = 2, label = "max")  

O_TMedian = TAnnot(age_summary[age_summary$Arrest_type == "O",], 
                   posX = 74,  
                   posY = 3, label = "median")

O_TMax = TAnnot(age_summary[age_summary$Arrest_type == "O",], 
                posX = 80,  
                posY = 3, label = "max")  

W_TMedian = TAnnot(age_summary[age_summary$Arrest_type == "W",], 
                   posX =74,  
                   posY = 4, label = "median")

W_TMax = TAnnot(age_summary[age_summary$Arrest_type == "W",], 
                posX = 80,  
                posY = 4, label = "max")  


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
  
  labs(title = "Arrest Patterns and the Correlation between Crime Type and Age", subtitle = "Massachusetts State Police Arrest Details: by Median and Max Age",
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
#necessary libraries
library(sf)
library(rio)
library(tmap)
library(dplyr)
library(ggplot2)
library(leaflet)
library(classInt) 
library(htmlwidgets)

#uploading the data
linkBoston="https://github.com/DACSS-Visual/SpatialData/raw/refs/heads/main/data/BostonContrib.xlsx"
bostonCont=rio::import(linkBoston)
#Ensure the data uploaded correctly.
#head(bostonCont)
#changing the column name

colnames(bostonCont)[colnames(bostonCont) == "Tender Type Description"] <- "Tender_Type" #update the column name
colnames(bostonCont)
#Ensure the data uploaded correctly.
View(bostonCont)

summary(bostonCont) #learning more about the dataset
#uploading the second datatset with the Boston zip codes


linkZips='https://raw.githubusercontent.com/DACSS-Visual/SpatialData/refs/heads/main/data/zip_codes.json'
bostonZips=sf::read_sf(linkZips)
#Ensure the data uploaded correctly.
head(bostonZips)
summary(bostonZips)


##DATA EXPLORATION -------------------------------------------------------------

#Finding the total distribution by the tender type
bostonCont |>
  group_by(Tender_Type) |>
  summarize(TotalContrib = sum(Amount, na.rm = TRUE)) |>
  ggplot(aes(x = reorder(Tender_Type, TotalContrib), 
             y = TotalContrib, fill = Tender_Type)) +
  geom_col() +
  coord_flip() +
  labs(title = "Total Contributions by Tender Type",
       x = "Tender Type",
       y = "Contribution Amount") +
  theme_minimal() 
 

#Viewing the relationships between contribution amount and zip code of the person
ggplot(bostonCont, aes(x = Zip, y = Amount)) +
  geom_point(alpha = 0.5, aes(color = Tender_Type)) +
  labs(title = "Contribution Amounts by Zip Code",
       x = "Zip Code",
       y = "Contribution Amount",
       color = "Tender Type") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 50))


#Checking how the contribution per tender type and visualizing it
# Calculate total contribution amount per tender type
total_by_tender <- bostonCont |>
  group_by(Tender_Type) |>
  summarize(TotalAmount = sum(Amount, na.rm = TRUE)) |>
  arrange(desc(TotalAmount))  

# Print the result
print(total_by_tender)

#data viz to see the output
ggplot(total_by_tender, aes(x = reorder(Tender_Type, -TotalAmount), y = TotalAmount, fill = Tender_Type)) +
  geom_col() +
  labs(title = "Total Contribution Amount by Tender Type",
       x = "Tender Type",
       y = "Total Amount") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 40,hjust = 1))

#heatmap to show contribution by tender type
bostonCont |>
  group_by(Zip, Tender_Type) |>
  summarize(TotalContrib = sum(Amount, na.rm = TRUE)) |>
  ggplot(aes(x = Tender_Type, y = Zip, fill = TotalContrib)) +
  geom_tile() +
  scale_fill_viridis_c(option = "brown") +
  labs(title = "Contributions by Tender Type and Zip Code",
       x = "Tender Type",
       y = "Zip Code",
       fill = "Total Contribution") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 40,hjust = 1))

##Cleaning data for the visualization
aggregBoston <- bostonCont |>
  filter(Tender_Type %in% c("Credit Card", "Check")) |>
  group_by(Tender_Type, Zip) |> 
  summarize(TotalContrib = sum(Amount, na.rm = TRUE), .groups = "drop") |>
  tidyr::pivot_wider(names_from = Tender_Type, values_from = TotalContrib, values_fill = 0)
# Join with the overall total contributions for prioritization
aggregBoston <- aggregBoston |>
  mutate(
    # Average based on Credit Card and Check
    Average = (`Credit Card` + Check) / 2,
    
    # Rate based on Credit Card as a percentage of the total contributions for Credit Card + Check
    Rate = ifelse(`Credit Card` + Check > 0, `Credit Card` / (`Credit Card` + Check) * 100, NA),
    
    # Proportion based on Check contributions as a proportion of Credit Card contributions
    Proportion = ifelse(`Credit Card` > 0, Check / `Credit Card`, NA))

#Merge total tender type data with the aggregated Credit Card and Check 
aggregBoston <- aggregBoston |>
  left_join(total_aggreg, by = "Zip") |>
  mutate(
    # Calculate Credit Card and Check as a percentage of all contributions
    CreditCardPctOfTotal = ifelse(TotalAllTypes > 0, `Credit Card` / TotalAllTypes * 100, NA),
    CheckPctOfTotal = ifelse(TotalAllTypes > 0, Check / TotalAllTypes * 100, NA))
#View(aggregBoston)
head(bostonZips)
Updated_bostonZips <- bostonZips |>
  left_join(aggregBoston, by = c("ZIP5" = "Zip"))
#View(Updated_bostonZips)

#Filtering down to only columns needed for visualizations 
FinalbostonZips <- Updated_bostonZips |>
  select(
    ZIP5,                   
    geometry,  
    Shape_Length,
    Shape_Area,
    Check,                  
    `Credit Card`,          
    Average,               
    Rate,                   
    Proportion,             
    TotalAllTypes,          
    CreditCardPctOfTotal,  
    CheckPctOfTotal )

#making sure the data updated correctly 
head(FinalbostonZips)

#Now to remove NA values for cleaner viz
FinalbostonZips <- FinalbostonZips|>
  filter(!is.na(Check))
#viewing the newly clean data

head(FinalbostonZips)

##Plotting -------------------------------------------------------------
#plotting a choropleth with the average in contributions 
ggplot(Updated_bostonZips) +
  geom_sf(aes(fill = Average), color = "white") +
  scale_fill_viridis_c(option = "plasma", na.value = "grey") +
  labs(
    title = "Average Contributions by ZIP Code",
    subtitle = "Tender Types: Credit Card and Check",
    fill = "Average",
    caption = "Data source: Massachusetts Office of Campaign and Political Finance"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40))

# save del3Draft ----------------------------------------------------------

#Making an interactive map 
del3Draft <- tm_shape(Updated_bostonZips) +
  tm_polygons("Average", palette = "plasma", title = "The Average") + 
  tm_view(set.view = c(-71.0589, 42.3601, 10)) +
  tm_layout(
    main.title = "A Map of Average Political Contributions in Boston by Zip Codes",
    main.title.size = 1.3,
    main.title.position = "center",
    title = "For Payment made by Credit Cards & Checks",
    title.size = 1,
    title.position = c("left", "bottom"),
    legend.outside = TRUE,
    legend.title.size = 1,
    legend.text.size = 0.75 )

#Save the viz
saveRDS(del3Draft, file = "del3Draft.rds")

# deliverable 4  ----------------------------------------------------------
#creating a choropleth map
Cggplot_map <- function(data, var, title, subtitle) {
  ggplot(data) +
    geom_sf(aes(fill = !!sym(var)), color = "white") +
    scale_fill_viridis_c(option = "plasma", na.value = "grey") +
    labs(title = title,
         subtitle = subtitle,
         fill = paste(var, "Contributions"),
         caption = "Data source: Massachusetts Office of Campaign and Political Finance") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 40))}

#creating the second interactive map
Ctmap_interactive <- function(data, var, title) {
  tm_shape(data) +
    tm_polygons(var, palette = "plasma", title = title, alpha = 0.5) -> tmap_chorop
  leaflet_map <- tmap_leaflet(tmap_chorop)
  leaflet_map |>
    addProviderTiles("OpenStreetMap") |> 
    addPolygons(data = data, fillOpacity = 0.5, color = "blue", weight = 1)}

#For credit cards
Cggplot_map(FinalbostonZips, "Credit Card", 
                  "Total Credit Card Contributions by Zip Code", 
                  "By Zip Code")
Ctmap_interactive(FinalbostonZips, "Credit Card", "Credit Card Contributions")

#For checks
Cggplot_map(FinalbostonZips, "Check", 
                  "Total Check Contributions by Zip Code", 
                  "By Zip Code")
Ctmap_interactive(FinalbostonZips, "Check", "Check Contributions")

#Layering and combining maps 
leaflet_map2 <- tmap_leaflet(tm_shape(FinalbostonZips) +
                               tm_polygons("Credit Card", palette = "plasma", title = "Contributions", alpha = 0.5))

leaflet_map2 |>
  addProviderTiles("OpenStreetMap") |>
  addPolygons(data = FinalbostonZips, fillOpacity = 0.5, color = "blue", weight = 1, group = "Credit Card") |>  
  addPolygons(data = FinalbostonZips, fillOpacity = 0.5, color = "red", weight = 1, group = "Check") |>  
  addLayersControl(
    overlayGroups = c("Credit Card", "Check"),
    options = layersControlOptions(collapsed = FALSE)
  )
# save del4Draft ----------------------------------------------------------
saveRDS(leaflet_map2, "del4Draft.rds")
saveWidget(leaflet_map2, "del4Draft.html")


