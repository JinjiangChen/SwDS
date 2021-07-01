library(readxl)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)


# Read Excel files and merge
thrift_data_1 <- read_excel("data_thrift1.xlsx", sheet=1)
thrift_data_2 <- read_excel("data_thrift2.xlsx", sheet=1)
names(thrift_data_1)[8] <- "Did the item sell?"# rename column name so both are the same
names(thrift_data_2)[8] <- "Did the item sell?"

thrift_data <- rbind(thrift_data_1, thrift_data_2)


# Items Sold
thrift_data$`Item Sold` <- gsub(",", "", thrift_data$`Item Sold`) # remove commas
thrift_data$`Item Sold` <- as.numeric(word(thrift_data$`Item Sold`, 1)) # only keep the first word


# Price
thrift_data$Price <- gsub("��", "£", thrift_data$Price) # replace �� with £ so they're the same as the others
thrift_data$Price <- gsub(",", "", thrift_data$Price) # remove commas
thrift_data$Price <- gsub("£", " ", thrift_data$Price) # replace the £ with a space (to separate prices when there are 2 prices)
thrift_data$Price <- as.double(word(thrift_data$Price, 2)) # the price is the second word (since the 1st is a space) 

View(thrift_data)

for (i in 2:nrow(thrift_data)) {
  if (is.na(thrift_data$`Search Term`[i])){
    thrift_data$`Search Term`[i] = thrift_data$`Search Term`[i-1]
  }else{
    thrift_data$`Search Term`[i] = thrift_data$`Search Term`[i]
  }
    
}


for (i in 2:nrow(thrift_data)) {
  if (is.na(thrift_data$`System Mean Price`[i])){
    thrift_data$`System Mean Price`[i] = thrift_data$`System Mean Price`[i-1]
  }else{
    thrift_data$`System Mean Price`[i] = thrift_data$`System Mean Price`[i]
  }
  
}


thrift_data$Category <- gsub("Selected category", "", thrift_data$Category)
thrift_data$Category <- gsub("Clothing", "Clothes", thrift_data$Category)
thrift_data$Category <- gsub(" Equipment", "", thrift_data$Category)
thrift_data$Category <- gsub("Books & Magazines", "Books, Comics & Magazines", thrift_data$Category)
thrift_data$Category <- gsub("Mobile Phones, Smart Watches, Accessories & Communication", "Mobile Phones & Communication", thrift_data$Category)
thrift_data$Category <- gsub("Industrial Supplies", "Industrial", thrift_data$Category)
thrift_data$Category <- gsub("Photography", "Photo", thrift_data$Category)
thrift_data$Category <- gsub("Dolls & Bears", "Dolls & Teddy Bears", thrift_data$Category)
thrift_data$Category <- gsub("Baby Essentials", "Baby", thrift_data$Category)
thrift_data$Category <- gsub("Jewellery", "Jewelry", thrift_data$Category)
thrift_data$Category <- gsub('Computers/Tablets & Networking', "Computers, Tablets & Network Hardware", thrift_data$Category)
thrift_data$Category <- gsub('Collectables', "Collectibles", thrift_data$Category)
thrift_data$Category <- gsub('Movies & TV', "Films & TV", thrift_data$Category)
thrift_data$Category[is.na(thrift_data$Category)] <- "Unknown"




colSums(is.na(thrift_data))
which(is.na(thrift_data[, 7]))


thrift = thrift_data
thrift$'Item Sold'[is.na(thrift$'Item Sold')] = 0

names(thrift)[1] <- 'Search_Term'
names(thrift)[7] <- 'Item_Sold'
active = which(is.na(thrift$`Did the item sell?`))
active_list = thrift[active,]
sold_list = thrift[-active,]
sold = sold_list %>% 
  group_by(Search_Term) %>% 
  summarise(Item_Sold = max(Item_Sold)) %>% 
  ungroup() %>% 
  inner_join(sold_list)


thrift <- rbind(sold, active_list)

colSums(is.na(thrift))
which(is.na(thrift[, 2]))


thrift$Condition[is.na(thrift$Condition)] = 'Unknown'
thrift$Category[is.na(thrift$Category)] = 'Unknown'
thrift$`Did the item sell?`[is.na(thrift$`Did the item sell?`)] = 'No'


thrift$Condition = tolower(thrift$Condition)

thrift$Condition <- gsub("new without tags", "opened – never used", thrift$Condition)


thrift %>%
  select(Category, Price) %>%
  ggplot(aes(x = Category, y = log(Price)))+
  geom_boxplot(aes(fill = Category), outlier.size = 0.2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
thrift %>%
  select(Condition, Price) %>%
  ggplot(aes(x = Condition, y = log(Price)))+
  geom_boxplot(aes(fill = Condition), outlier.size = 0.2)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


separate(data = thrift, col = thrift$`Did the item sell?`, into = c("sell?", "Date"), sep = " ")

data("stop_words")

thrift %>%
  select(Title) %>%
  unnest_tokens(word, Title) %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)

