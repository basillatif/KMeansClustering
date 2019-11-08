getwd()
setwd("/Users/basillatif/Desktop/MSBA/Visualization\ Projects/Google\ App\ Store\ KMeans")
data <- read.csv("googleplaystore.csv")
library(dplyr)
#ctrl + shift + enter to run the entire script 
#to check column values -> data$ColumnName
#to see the whole dataset: environment tab + click on data

###### Data cleaning and Manipulation
# replace Nan in Rating with Mean for Column
data$Rating[is.nan(data$Rating)] <- mean(data$Rating, na.rm = TRUE)

#Replace 'Varies with device' with undefined 
data <- data.frame(lapply(data, function(x) {gsub("Varies with device", "undefined", x)}))

#Create a unique app ID by concatenating app name and reviews
data <- mutate(data, uid=paste(App,Reviews,Size,sep=""))

data$uid <-  paste(data$App,data$Reviews,data$Size,sep="")

#Create the sum of ratings column
data$sum_of_ratings <- as.numeric(data$Rating) * as.numeric(data$Reviews)

#Create a column that tracks how many installs it takes to get one review
data$reviews_per_installs <- as.numeric(data$Reviews) / as.numeric(data$Installs)

#Create a column that sees if bigger apps get more downloads
data$size_per_installs <- as.numeric(data$Size) / as.numeric(data$Installs)

#Make distinct buckets for all categories
data$Category<-as.character(data$Category)

data$Category[data$Category!="ART_AND_DESIGN" & data$Category !="AUTO_AND_VEHICLES" & data$Category!="BEAUTY" & data$Category !="BOOKS_AND_REFERENCE" & data$Category!="BUSINESS" & data$Category != "COMICS" & data$Category != "COMMUNICATION" & data$Category != "DATING" & data$Category != "EDUCATION" & data$Category != "ENTERTAINMENT" & data$Category != "EVENTS" & data$Category != "FINANCE" & data$Category != "FOOD_AND_DRINK" & data$Category != "HEALTH_AND_FITNESS" & data$Category != "HOUSE_AND_HOME" & data$Category != "LIBRARIES_AND_DEMO" & data$Category != "LIFESTYLE" & data$Category != "GAME" & data$Category != "FAMILY" & data$Category != "MEDICAL" & data$Category != "SOCIAL" & data$Category != "SHOPPING" & data$Category != "PHOTOGRAPHY" & data$Category != "SPORTS" & data$Category != "TRAVEL_AND_LOCAL" & data$Category != "TOOLS" & data$Category != "PERSONALIZATION" & data$Category != "PRODUCTIVITY" & data$Category != "PARENTING" & data$Category != "WEATHER" & data$Category != "VIDEO_PLAYERS" & data$Category != "NEWS_AND_MAGAZINES" & data$Category != "MAPS_AND_NAVIGATION"] <- "Other"

data$Category<-as.factor(data$Category)

#Make buckets for Ratings
data <- mutate(data, Rating_group = ifelse(as.numeric(as.character(Rating))>=0 & as.numeric(as.character(Rating))<1,"0-1",
                                           ifelse(as.numeric(as.character(Rating))>=1 & as.numeric(as.character(Rating)) < 2,"1-2",
                                                  ifelse(as.numeric(as.character(Rating))>=2 & as.numeric(as.character(Rating))<3,"2-3",
                                                         ifelse(as.numeric(as.character(Rating))>=3 & as.numeric(as.character(Rating))<4,"3-4",
                                                                ifelse(as.numeric(as.character(Rating))>=4,"4-5","N/A"))))))
####Clustering
#1. Aggregate up to the app level 
app_data <- data %>% group_by(uid) %>% summarise(App=first(App), Category=first(Category), 
            Rating=first(Rating), Reviews=first(Reviews), Size=first(Size), 
            Installs=first(Installs), Type=first(Type), Price=first(Price), 
            Content.Rating=first(Content.Rating), Genres=first(Genres), 
            sum_of_ratings=first(sum_of_ratings), reviews_per_installs = first(reviews_per_installs), 
            Rating_group=first(Rating_group))
head(app_data)
#2. Remove columns that won't be useful
clustering_app_data<-subset(app_data,select=-c(App,uid,Size,Genres, Type, Content.Rating))
head(clustering_app_data)
#3. Normalize
normalize <- function(x){
  return ((x - min(x))/(max(x) - min(x)))
}
clustering_app_data = mutate(clustering_app_data, 
                      Rating = normalize(as.numeric(Rating)),
                      Reviews = normalize(as.numeric(Reviews)), 
                      Installs = normalize(as.numeric(Installs)),
                      Price = normalize(as.numeric(Price)), 
                      sum_of_ratings = normalize(sum_of_ratings))
head(clustering_app_data)

#4. Dummy Code Variables
library(ade4)
clustering_app_data <- as.data.frame(clustering_app_data)
clustering_app_data <-  clustering_app_data %>% 
cbind(acm.disjonctif(clustering_app_data[,c("Category","Rating_group")]))%>%ungroup()
head(clustering_app_data)

#5. Remove the original (non-dummy-coded) variables
clustering_app_data<-clustering_app_data %>%select(-Category,-Rating_group)

#Clustering steps:
#Remove columns that were created for factor levels that were not represented in the sample.
clustering_app_data <- clustering_app_data[, colSums(clustering_app_data != 0, na.rm = TRUE) > 0]

#Now run k-Means and look at the within SSE curve - 3 or 5 seems like the best
#solution here...

SSE_curve <- c()
for (n in 1:15) {
  kcluster <- kmeans(clustering_app_data, n)
  sse <- sum(kcluster$withinss)
  SSE_curve[n] <- sse
}

SSE_curve

print("SSE curve for the ideal k value")
plot(1:15, SSE_curve, type="b", xlab="Number of Clusters", ylab="SSE")

#Let's go with 4...
kcluster<- kmeans(clustering_app_data, 4)

names(kcluster)

print("the size of each of the clusters")
kcluster$size

#What do the centers of each of the resulting clusters look like?
kcluster$centers
# 
# #Let's add a new column with the cluster assignment for each obs in the sample.
# segment<-kcluster$cluster 
# data$Reviews <- as.numeric(data$Reviews)
# data$Installs <- as.numeric(data$Installs)
# data$Price <- as.numeric(data$Price)
# clustering_app_data<-cbind(clustering_app_data,segment)
# app_data <- as.data.frame(app_data)
# segment <- data.frame(segment, col.names="segment" )
# dim(app_data)
# app_data <- cbind(app_data,segment)
# uid_seg <- app_data[c("uid","segment")]
# data <- merge(data, uid_seg, by = "uid")
# data <- data[,-4]
# names(data)
# 
# write.csv(data, "results_app_store_cluster.csv")
# 
