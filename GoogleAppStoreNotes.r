Replace NaN in Rating with mean 
Replace 'varies with device' in Size with undefined 
Replace 'Nan' and 'varies with device' in current version with undefined 
Replace 'varies with device' in Android version with undefined 


data$Size[data$Size == 'Varies with device'] <- undefined

data <- data.frame(lapply(data, function(x) {gsub("Varies with device", "undefined", x)}))

Analyze:
Free vs paid apps
Content rating 
Genres
Installs 
Reviews 
Category // Rating // Reviews // Size // Installs //Type

Questions: 
  Which category/genres of apps have the most installs?
  What size of apps have the most installs?
  Which category/genres of apps have the highest ratings?
  What category/genres of apps have the most reviews?
Dependent variables: category or genre
Independent variables: installs, ratings, reviews





