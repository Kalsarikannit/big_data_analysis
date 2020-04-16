rm(list = ls())

library(RWeka)
library(ggthemes)
library(GGally)
library(ggExtra)
library(caret)
library(glmnet)
library(corrplot)
library(leaflet)
library(RColorBrewer)
library(kableExtra)
library(RColorBrewer)
library(plotly)

df = read.csv("~/Downloads/AB_NYC_2019.csv", na.strings=c("", "NA"),)

str(df)

## EDA
# Find NAs
df %>% summarise_all(~(sum(is.na(.))))

# Remove irrelavant and NA columns. id, name, host_id, host_name, lat, long, 
airbnb = df[,-c(1,2,3,4,7,8,13,14)]

# Group by neighborhood and room type, also sum(price)
airbnb_nh = df %>%
  group_by(neighbourhood_group) %>%
  summarise(price = round(mean(price), 2))

airbnb_rt = df %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise(mean_nhood_group = mean(price))

airbnb_sub = df %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise(mean_nhood_group = mean(price),
            sum_nhood_group = sum(price))

## Visualization
th = theme_fivethirtyeight() + theme(axis.title = element_text(), axis.title.x = element_text()) 

# Histogram & Density
ggplot(df, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +
  th +
  ggtitle("Distribution of price",
          subtitle = "The distribution is very skewed") +
  theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(xintercept = round(mean(airbnb$price), 2), size = 2, linetype = 3)

# Histogram & Density with log10 transformation
ggplot(df, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +
  th +
  ggtitle("Transformed distribution of price",
          subtitle = expression("With" ~'log'[10] ~ "transformation of x-axis")) +
  #theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_vline(xintercept = round(mean(airbnb$price), 2), size = 2, linetype = 3) +
  scale_x_log10() +
  annotate("text", x = 1800, y = 0.75,label = paste("Mean price = ", paste0(round(mean(airbnb$price), 2), "$")),
           color =  "#32CD32", size = 8)

# Histgram & Density with log10 for negihborhood areas
ggplot(df, aes(price)) +
  geom_histogram(bins = 30, aes(y = ..density..), fill = "purple") + 
  geom_density(alpha = 0.2, fill = "purple") +
  th +
  ggtitle("Transformed distribution of price\n by neighbourhood groups",
          subtitle = expression("With" ~'log'[10] ~ "transformation of x-axis")) +
  geom_vline(data = airbnb_nh, aes(xintercept = price), size = 2, linetype = 3) +
  geom_text(data = airbnb_nh,y = 1.5, aes(x = price + 1400, label = paste("Mean  = ",price)), color = "darkgreen", size = 4) +
  facet_wrap(~neighbourhood_group) +
  scale_x_log10() 

# Above Average Price Objects by Neighborhood Areas
df %>% filter(price >= mean(price)) %>% group_by(neighbourhood_group, room_type) %>% tally %>% 
  ggplot(aes(reorder(neighbourhood_group,desc(n)), n, fill = room_type)) +
  th +
  xlab(NULL) +
  ylab("Number of objects") +
  ggtitle("Number of above average price objects",
          subtitle = "Most of them are entire homes or apartments") +
  geom_bar(stat = "identity")

# Boxplot of price by room type 
ggplot(df, aes(x = room_type, y = price)) +
  geom_boxplot(aes(fill = room_type)) + scale_y_log10() +
  th + 
  xlab("Room type") + 
  ylab("Price") +
  ggtitle("Boxplots of price by room type",
          subtitle = "Entire homes and apartments have the highest avg price") +
  geom_hline(yintercept = mean(df$price), color = "purple", linetype = 2)

# Summary for price distribution
df %>% arrange(desc(price)) %>% top_n(10) %>%
  ggplot(aes(x = price, fill = neighbourhood_group)) +
  geom_histogram(bins = 50) +
  scale_x_log10() + 
  ggtitle("Summary of price distributions") +
  facet_wrap(~room_type + neighbourhood_group)

# price & Availability
ggplot(df, aes(availability_365, price)) +
  th +
  geom_point(alpha = 0.2, color = "slateblue") +
  geom_density(stat = "identity", alpha = 0.2) +
  xlab("Availability during year") +
  ylab("Price") +
  ggtitle("Relationship between availability",
          subtitle = "there is not clear relationship") 

# Price & Number of Reviews
ggplot(df, aes(number_of_reviews, price)) +
  th + theme(axis.title = element_text(), axis.title.x = element_text()) +
  geom_point(aes(size = price), alpha = 0.05, color = "slateblue") +
  xlab("Number of reviews") +
  ylab("Price") +
  ggtitle("Relationship between number of reviews",
          subtitle = "The most expensive objects have small number of reviews (or 0)")

# Number of objects by neighborhood areas
df %>% group_by(neighbourhood_group) %>% tally() %>% 
  ggplot(aes(x = reorder(neighbourhood_group, n), n)) +
  geom_bar(stat = "identity", fill = "purple") +
  theme_fivethirtyeight() +
  ggtitle("Number of objects by neighbourhood group") +
  geom_text(aes(x = neighbourhood_group, y = 1, label = paste0(n),
                colour = ifelse(neighbourhood_group %in%
                                  c("Manhattan", "Brooklyn", 
                                    "Queens"), '1', '2')),
            hjust=-1.5, vjust=.5, size = 4, 
            fontface = 'bold') +
  coord_flip() +
  scale_color_manual(values=c("white","black"), guide = F)

# Leaflet map
pal <- colorFactor(palette = c("red", "green", "blue", "purple", "yellow"), domain = df$neighbourhood_group)

leaflet(data = df) %>% addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%  
  addCircleMarkers(~longitude, ~latitude, color = ~pal(neighbourhood_group), 
                   weight = 1, radius=1, fillOpacity = 0.1, opacity = 0.1,  label = paste("Name:", df$name)) %>% 
  addLegend("bottomright", pal = pal, values = ~neighbourhood_group,
            title = "Neighbourhood groups",
            opacity = 1
  )

# Correlation Matrix
airbnb_cor <- df[, sapply(df, is.numeric)]
airbnb_cor <- airbnb_cor[complete.cases(airbnb_cor), ]
correlation_matrix <- cor(airbnb_cor, method = "spearman")
corrplot(correlation_matrix, method = "color")


## Analysis
set.seed(123)
index = sample(nrow(df), nrow(df) * 0.7)
airbnb_train <- df[index,]
airbnb_test  <- df[-index,]

# First Linear Regression Model
first_model <- train(price ~ latitude + longitude + room_type + minimum_nights  + availability_365 + neighbourhood_group, data = airbnb_train, method = "lm")
summary(first_model)

# Plot first model
plot(first_model$finalModel)

# Second Linear Regression Model
learn <- airbnb_train %>% filter(price < quantile(airbnb_train$price, 0.9) & price > quantile(airbnb_train$price, 0.1)) %>% tidyr::drop_na()
second_model <- lm(log(price) ~ room_type + neighbourhood_group + latitude + longitude 
                   + number_of_reviews + availability_365
                   + reviews_per_month + 
                     calculated_host_listings_count + minimum_nights, data = learn)
# Summarize the results
summary(second_model)

# Plot second model
plot(second_model)

# Other analysis

# Function turning numercial columns to categorical
categorize = function(arr){
  result = c()
  for (val in arr){
    if(val < mean(arr)){
      result = c(result, -1)
    }else if(val == mean(arr)){
      result = c(result, 0)
    }else{
      result = c(result, 1)
    }
  }
  return(result)
}

airbnb$price = as.factor(categorize(df$price))
airbnb$minimum_nights = as.factor(categorize(df$minimum_nights))
airbnb$number_of_reviews = as.factor(categorize(df$number_of_reviews))
airbnb$calculated_host_listings_count = as.factor(categorize(df$calculated_host_listings_count))
airbnb$availability_365 = as.factor(categorize(df$availability_365))

set.seed(123)
index = sample(nrow(airbnb), nrow(airbnb) * 0.7)
airbnb_train1 <- airbnb[index,]
airbnb_test1  <- airbnb[-index,]

# J48 Decision Tree
WOW("J48")
m=J48(price~., data = airbnb_train1)
m=J48(price~., data = airbnb_train1, control=Weka_control(U=FALSE, M=2, C=0.5))
e1=evaluate_Weka_classifier(m, seed=1, numFolds=3)
e1

# Naive bayes
WOW("NaiveBayes")
NB <- make_Weka_classifier("weka/classifiers/bayes/NaiveBayes")
# build default NB model
nb_model=NB(price~., data=airbnb_train1)
# turn on discretization
nb_model=NB(price~., data=airbnb_train1, control=Weka_control(D=TRUE))
# turn on kernel estimation
nb_model=NB(price~., data=airbnb_train1, control=Weka_control(K=TRUE))
e2 <- evaluate_Weka_classifier(nb_model, numFolds = 3, seed = 1, class = TRUE)
e2

# K-nearest Neighbor
WOW("IBk")
knn <- make_Weka_classifier("weka/classifiers/lazy/IBk")
knn_model=knn(price~., data=airbnb_train1)
knn_model=NB(price~., data=airbnb_train1)
knn_model=NB(price~., data=airbnb_train1, control=Weka_control(K=3))
e3 <- evaluate_Weka_classifier(knn_model, numFolds = 3, seed = 1, class = TRUE)
e3

# SVM
WOW("SMO")
svm <- make_Weka_classifier("weka/classifiers/functions/SMO")
svm_model=svm(price~., data=airbnb_train1)
e4 <- evaluate_Weka_classifier(svm_model, numFolds = 3, seed = 1, class = TRUE)
e4

# Random Forest
WOW("weka/classifiers/trees/RandomForest")
rf <- make_Weka_classifier("weka/classifiers/trees/RandomForest")
# build default model with 100 trees
rf_model=rf(price~., data=airbnb_train1)
# build a model with 10 trees instead
rf_model=rf(price~., data=airbnb_train1, control=Weka_control(I=10))
e5 <- evaluate_Weka_classifier(rf_model, numFolds = 3, seed = 1, class = TRUE)
e5
