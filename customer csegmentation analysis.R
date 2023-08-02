# suppress warning message / global option output
#options(warn = -1)

# Import the required Packages
library(tidyverse)  
#The tidyverse package is an "umbrella-package" that installs several packages useful for data analysis which work together well such as tidyr(swiftly convert between different data formats), dplyr(data manipulation), ggplot2, tibble, etc. 
library(readxl) #Read Excel Files
library(kableExtra)
library(flextable) # for beautiful tables

library(DataExplorer) # data handling and visualization,
library(dlookr) # **Data Diagnosis, Exploration, transform. compare before and after results  
# https://yuzar-blog.netlify.app/posts/2021-01-30-r-package-reviews-dlookr-diagnose-explore-and-transform-your-data/
library(highcharter) # Interactive data visualizations
library(tm) # Text mining

library(wordcloud) # Wordclouds
library(corrplot) 
library(viridis) # Viridis Color Scales for ggplot2
library(xts)
library(rfm) # RFM analysis

library(clustertend)# used to statistically evaluate clustering tendency
library(factoextra) # DETERMINING THE OPTIMAL NUMBER OF CLUSTERS
library(NbClust)  # DETERMINING THE OPTIMAL NUMBER OF CLUSTERS
library(clValid) # COMPARE CLUSTERING ALGORITHMS
library(fpc) # Computing Dunn index and other cluster validation statistics





# Load the dataset
df<-read_excel("D:/work/job/prepare/cluster/Online_Retail.xlsx")
view(df)

# First Glimpse

# First and last 5 rows
head(df,5)
tail(df,5)

# Unique customers, products and country
n_distinct(df$CustomerID)
n_distinct(df$Description)
n_distinct(df$Country)
n_distinct(df$InvoiceNo)

# Data Structure
str(df)
# The dataset consists of 541,909 rows and 8 features. Most of features have correct format, but we can see that the InvoiceNo and the InvoiceDate features are in the bad format. 

# Summary of data set
#summary(df)
summary(df) %>% kable() %>% kable_styling()

# Data Cleaning
# Check missing values
cat("Number of missing value:", sum(is.na(df)), "\n")

# Plot Missing values {dlookr}
plot_na_pareto(df)

# plot percentage of missing values per feature {naniar}
# gg_miss_var(df,show_pct=TRUE)

# Delete rows where CustomerID is missing
df<-df %>%
  na.omit(CustomerID)

# Replace NA Description values with the string 'N/A'
df$Description<-replace_na(df$Description, "N/A")


# Check outlines for Quantity and UnitPrice
diagnose_outlier(df) %>% flextable()
plot_outlier(df,Quantity, UnitPrice)

#check min and max UnitPrice
min(df$UnitPrice)
max(df$UnitPrice)
# From the plots we can see that there are some negative values in the quantity, and some zero value inputs. we need remove them.
# But before we remove them, we need check the positive outline values in the Quantity to see if the positive and negative outlines are connected.  



# Check canceled orders. Quantities are negative, and arrange them by descending order
order_canceled <- df %>%
  filter(Quantity<0) %>%
  arrange(Quantity)

head(order_canceled, 2)

df %>% 
  filter(CustomerID == 16446)


# We find that except the InvoiceNo, InvoiceDate and the quantity value sign, other information from original order and the canceled order are same.        
# drop the InvoiceNo, InvoiceDate and inverse the quantity
orignal_info <- order_canceled %>%
  subset(select = -c(InvoiceNo, InvoiceDate)) %>%
  mutate(Quantity  = Quantity *(-1))
head(orignal_info, 5)

# Find the original Orders that be canceled
orignal_orders <- merge(df, orignal_info)
head(orignal_orders)
dim(orignal_orders)
dim(order_canceled)
# The number of canceled Order is larger than number of the order, for some of the original order occurred before the time range in this dataset.

# Remove the original Order that will be canceled 
df<- anti_join(df, orignal_info)

# filter all canceled orders , as well as the rows where the UnitPrice is 0 .
df <- df %>% 
  filter(Quantity > 0) %>% 
  filter(UnitPrice >0) 

diagnose_outlier(df) %>% flextable()
plot_outlier(df, Quantity, UnitPrice)

# Feature engineering
# Create new feature Sales = Quantity * UnitPrice.
df <- df %>%
  mutate(sales = Quantity * UnitPrice )

# Create new features: date, time , year, month, hour, day of weak from InvoiceDate for the analysis in the next step.
# Extract new date and time columns from InvoiceDate column
df$InvoiceDate <- as.character(df$InvoiceDate)
df$date <- sapply(df$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][1]})
df$time <- sapply(df$InvoiceDate, FUN = function(x) {strsplit(x, split = '[ ]')[[1]][2]})
# Create new month, year and hour columns
df$year <- sapply(df$date, FUN = function(x) {strsplit(x, split = '[-]')[[1]][1]})
df$month <- sapply(df$date, FUN = function(x) {strsplit(x, split = '[-]')[[1]][2]})
df$hour <- sapply(df$time, FUN = function(x) {strsplit(x, split = '[:]')[[1]][1]})
# Convert date format to date type
df$date <- as.Date(df$date, "%Y-%m-%d")
# Create day of the week feature
df$day_week <- wday(df$date, label = TRUE)


# Exploratory Data Analysis & Data visualization

# Take a look of the transactions in each county
bar <- ggplot(data = df) + 
  geom_bar(
    mapping = aes(x = Country, fill = Country), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL, title = "Transaction per Country")

bar + coord_flip()
bar + coord_polar()

# We can see that the most of the orders come from the UK.To get a better look of the transactions that happen outside the UK, we will plot invoices excluding UK this time and lets arange it also in a descending order.
F
T_excludeUK <- df %>% 
  filter(Country != "United Kingdom")

bar <- ggplot(data = T_excludeUK) + 
  geom_bar(
    mapping = aes(x = Country, fill = Country), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(title = "Transaction per Country Excluding UK")

bar + coord_flip()
bar + coord_polar()



# Take a look of the sales in each county
Sales_country <- df %>%
  group_by(Country) %>%
  summarize(sales = round(sum(sales),0), transcations=n_distinct(InvoiceNo),customers = n_distinct(CustomerID))  %>% 
  ungroup() %>%
  arrange(desc(sales))


# Sales map By Country  
Sales_country$log_sales <- round(log(Sales_country$sales),2)

highchart(type = "map") %>%
  hc_add_series_map(worldgeojson,
                    Sales_country,
                    name="sales in online store (log)",
                    value = "log_sales", joinBy = c("name","Country")) %>%
  hc_title(text = "Sales In Online Store By Country (log)") %>%
  hc_colorAxis(stops = color_stops()) %>%
  hc_legend(valueDecimals = 0, valueSuffix = "%") %>%
  hc_mapNavigation(enabled = TRUE)


# Top 10 sales with their transactions by country excluding UK 
Top_10_exclUK <- Sales_country %>%
  filter(Country != "United Kingdom") %>%
  top_n(10,sales)
Top_10_exclUK

highchart() %>% 
  hc_xAxis(categories = Top_10_exclUK$Country) %>% 
  hc_add_series(name = "Sales", data = Top_10_exclUK$sales) %>%
  hc_add_series(name = "Transactions", data = Top_10_exclUK$transcations) %>%
  hc_chart(
    type = "column",
    options3d = list(enabled = TRUE, beta = 15, alpha = 15)
  ) %>%
  hc_title(
    text="Top 10 Sales with their Transactions excl. UK"
  )


# Take a look of Top 5 sales by country excluding UK over time
## line plot
top_5 <- df %>%
  filter(Country == 'Netherlands' | Country == 'EIRE' | Country == 'Germany' | Country == 'France' 
         | Country == 'Australia') %>%
  group_by(Country, date) %>%
  summarise(sales = sum(sales), transactions = n_distinct(InvoiceNo), 
                   customers = n_distinct(CustomerID)) %>%
  mutate(aveOrdVal = (round((sales / transactions),2))) %>%
  ungroup() %>%
  arrange(desc(sales))

head(top_5)
ggplot(top_5, aes(x = date, y = sales, colour = Country)) + geom_smooth(method = 'auto', se = FALSE) + 
  labs(x = ' Country', y = 'Revenue', title = 'Sales by Country over Time') + 
  theme(panel.grid.major = element_line(colour = NA),
        legend.text = element_text(colour = "skyblue4"),
        legend.title = element_text(face = "bold"),
        panel.background = element_rect(fill = NA),
        legend.key = element_rect(fill = "gray71"),
        legend.background = element_rect(fill = NA))




# Stacked plot
Top_5_exclUK <- df %>%
  filter(Country == 'Netherlands' | Country == 'EIRE' | Country == 'Germany' | Country == 'France' 
         | Country == 'Australia') %>%
  group_by(Country, year, month) %>%
  summarise(sales = round(sum(sales),0))  %>% 
  ungroup()

Top_5_exclUK$YearMonth <- paste(Top_5_exclUK$year, Top_5_exclUK$month) 

head(Top_5_exclUK,5)


ggplot(Top_5_exclUK, aes(fill=Country, y=sales, x=YearMonth)) + 
  geom_bar(position="stack", stat="identity")+
  labs(title = "Sales By Country Over Time",
       x = "Month",
       y = "Sales",
       fill = "Country") +
  scale_fill_viridis(discrete=TRUE, name="")+
  geom_area(color = "black") 
# stat="identity" calculate the sum of the y variable, grouped by the x variable and use bars to display the sums.

# line plot
# convert wide format to long format
library(zoo)
plotdata <- spread(Top_5_exclUK, Country, sales)
head(plotdata ,5)
# generate graph
highchart() %>% 
  hc_xAxis(categories = plotdata$YearMonth) %>% 
  hc_add_series(name = "Netherlands", 
                data = plotdata$Netherlands) %>% 
  hc_add_series(name = "EIRE", 
                data = plotdata$EIRE) %>%
  hc_add_series(name = "Germany", 
                data = plotdata$Germany) %>%
  hc_add_series(name = "France", 
                data = plotdata$France) %>%
  hc_add_series(name = "Australia", 
                data = plotdata$Australia) 
head(df,5)


df %>%
  filter(Country == 'Netherlands' | Country == 'EIRE' | Country == 'Germany' | Country == 'France' 
         | Country == 'Australia')  %>% 
  hchart(., type = "column", 
         hcaes(x = date, 
               y = sales, 
               group = Country)) %>% 
  hc_yAxis(opposite = FALSE,
           labels = list(format = "{value}")) %>% 
  hc_tooltip(pointFormat = '{point.x: %Y-%m-%d}{point.y:.0f}% ') %>%
  hc_title(text = "Sales By Country Over Time")




# Take a look of the total sales by date 
Sales_date <- df %>%
  group_by(date) %>%
  summarise(sales = sum(sales)) %>% 
  ungroup()


time_series <- xts(
  Sales_date$sales, order.by = Sales_date$date)


highchart(type = "stock") %>% 
  hc_add_series(time_series,
                type = "line",
                color = "green") %>%
  hc_title(text = "Sales By Date") %>% 
  hc_subtitle(text = "Sales generated from the online store") 

# Sales by Day of Week
df %>%
  group_by(day_week) %>%
  summarise(sales = sum(sales))  %>% 
  ungroup()%>%
  hchart(type = 'column', hcaes(x = day_week, y = sales)) %>% 
  hc_yAxis(title = list(text = "Sales")) %>%  
  hc_xAxis(title = list(text = "Day of the Week")) %>% 
  hc_title(text = "Sales by Day of Week")

# Transaction by Day of Week
df %>%
  group_by(day_week) %>%
  summarise(Transaction = n_distinct(InvoiceNo))  %>% 
  ungroup() %>%
  hchart(type = 'column', hcaes(x = day_week, y = Transaction)) %>% 
  hc_yAxis(title = list(text = "Transaction")) %>%  
  hc_xAxis(title = list(text = "Day of the Week")) %>% 
  hc_title(text = "Transaction by Day of Week")

# Sales by Hour of The Day
df %>%
  group_by(hour) %>%
  summarise(sales = sum(sales)) %>% 
  ungroup() %>%
  hchart(type = 'column', hcaes(x = hour, y = sales)) %>% 
  hc_yAxis(title = list(text = "Sales")) %>%  
  hc_xAxis(title = list(text = "Hour of the Day")) %>% 
  hc_title(text = "Sales by Hour of The Day")


# Transaction by Hour of The Day
df %>%
  group_by(hour) %>%
  summarise(Transaction = n_distinct(InvoiceNo))  %>% 
  ungroup() %>%
  hchart(type = 'column', hcaes(x = hour, y = Transaction)) %>% 
  hc_yAxis(title = list(text = "Transaction")) %>%  
  hc_xAxis(title = list(text = "Hour of the Day")) %>% 
  hc_title(text = "Transaction by Hour of The Day")


# Analysis products  categories by extracting and cleaning the text information
# Unique product 
products_list <- unique(df$Description)
docs <- Corpus(VectorSource(products_list))
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove English common stop words
docs <- tm_map(docs, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stop words as a character vector like color, texture，size, etc.
docs <- tm_map(docs, removeWords, c("pink", "blue","red","set","white","black","sign","ivory","cover","hanging","wall","green","metal","vintage","heart", "paper", "silver", "glass","large","small","holder"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
# Explore the 20 most common words from product Descriptions,
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=20, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

## The following product categories are mainly offered by the online store: - Decoration products - Christmas related products - Bags - Flowers and presents

# Customer Segmentation &  RFM analysis & Clustering 
# RFM feature engineering 
custormer_info <- df %>%
  select("InvoiceNo","InvoiceDate","CustomerID","sales") %>%
  mutate(days=as.numeric(max(df$date)-as.numeric(df$date))+1) %>%
  group_by(CustomerID) %>%
  summarise(Recency = min(days), Frequency = n_distinct(InvoiceNo), Monetary = round(sum(sales),0))  %>% 
  ungroup()
  

# Summary of data set
dim(custormer_info)
summary(custormer_info) 
head(custormer_info,2)

diagnose_outlier(custormer_info) %>% flextable()
plot_outlier(custormer_info, Recency, Frequency, Monetary)

# Plot Histograms
custormer_info %>% 
  gather(Attributes, value, 2:4) %>% 
  ggplot(aes(x=value)) +
  geom_histogram( fill = "lightblue2", color = "black") + 
  facet_wrap(~Attributes, scales = "free") +
  labs(x = "Value", y = "Counts")

# plot boxplot
custormer_info %>% 
  select(-"CustomerID") %>% 
  gather(key = value_groups, value = value) %>% 
  ggplot(aes(y=value, fill=value_groups)) +
  geom_boxplot() +
  facet_wrap(.~value_groups, scales = "free_y")

# qq plot
custormer_info %>% 
  select("Recency", "Frequency","Monetary")%>%
  gather(key = value_groups, value = value) %>% 
  ggplot(aes(sample=value, colour=value_groups)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap(.~value_groups, scales = "free_y")




# interactive histograms for RFM features
h1 <- hchart(
  custormer_info$Recency, 
  color = "#B71C1C", name = "Recency")

h2 <- hchart(
  custormer_info$Frequency, 
  color = "#668F80", name = "Frequency")

h3 <- hchart(
  custormer_info$Monetary, 
  color = "#4A6670", name = "Monetary Value")

require(htmltools)
browsable(hw_grid(h1, h2, h3, ncol = 3, rowheight = 500))

check_Cor <- custormer_info %>% select("Recency", "Frequency", "Monetary")
corrplot(cor(check_Cor), 
         type = "upper", method = "ellipse", tl.cex = 0.9)

# scale
rfm_Scaled <- scale( select(custormer_info, -"CustomerID"))
rownames(rfm_Scaled) <- custormer_info$CustomerID
summary(rfm_Scaled)
head(rfm_Scaled,2)



# cluster validation: 
# Step one, assess the clustering tendency to see if applying clustering is suitable for the data. Compute Hopkins statistic 
set.seed(123)
hopkins(rfm_Scaled, n = nrow(rfm_Scaled)-1)
# It can be seen that the rfm_Scaled data set is highly clusterable (the H value = $H 0.007978077 which is far below the threshold 0.05).

# Step two, use Statistical testing methods to evaluate the goodness of the clustering results, choose the best algorithm and the optimal number of clusters
# Use clValid to computer intern measures
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(rfm_Scaled, nClust = 2:6,
                  clMethods = clmethods, validation = "internal", maxitems=nrow(rfm_Scaled))
# Summary
summary(intern)
# It can be seen that hierarchical clustering with two clusters performs the best in each case, and the optimal number of clusters seems to be two using the three measures.

# Stability measures
stab <- clValid(rfm_Scaled, nClust = 2:6, clMethods = clmethods,
                validation = "stability", maxitems=nrow(rfm_Scaled))
# Display only optimal Scores
optimalScores(stab)

# The values of APN, ADM and FOM ranges from 0 to 1, AD has a value between 0 and infinity, and smaller values are  preferred
# For the APN and ADM measures, hierarchical clustering with two clusters again gives the best score. For AD measure, PAM with six clusters has the best. For FOM measure, Kmeans with six clusters has the best score.


# Determine the optimal number of clusters for K-Means clustering 
# Elbow method
fviz_nbclust(rfm_Scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
# Silhouette method
fviz_nbclust(rfm_Scaled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")
# Gap statistic
# nboot = 50 to keep the function speedy.
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(rfm_Scaled, kmeans, nstart = 25, method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")
# Elbow method: 4 clusters solution suggested
# Silhouette method: 4 clusters solution suggested
# Gap statistic method: 1 clusters solution suggested


# The NbClust package provides 30 indices for choosing the best number of clusters
nb <- NbClust(rfm_Scaled, distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "kmeans")
fviz_nbclust(nb)
#  According to the majority rule, the best number of clusters is  3 .



###### fix fviz_nbclust function ###############################################################
fviz_nbclust <- function (x, FUNcluster = NULL, method = c("silhouette", "wss", 
                                                           "gap_stat"), diss = NULL, k.max = 10, nboot = 100, verbose = interactive(), 
                          barfill = "steelblue", barcolor = "steelblue", linecolor = "steelblue", 
                          print.summary = TRUE, ...) 
{
  set.seed(123)
  if (k.max < 2) 
    stop("k.max must bet > = 2")
  method = match.arg(method)
  if (!inherits(x, c("data.frame", "matrix")) & !("Best.nc" %in% 
                                                  names(x))) 
    stop("x should be an object of class matrix/data.frame or ", 
         "an object created by the function NbClust() [NbClust package].")
  if (inherits(x, "list") & "Best.nc" %in% names(x)) {
    best_nc <- x$Best.nc
    if (any(class(best_nc) == "numeric") ) 
      print(best_nc)
    else if (any(class(best_nc) == "matrix") )
      .viz_NbClust(x, print.summary, barfill, barcolor)
  }
  else if (is.null(FUNcluster)) 
    stop("The argument FUNcluster is required. ", "Possible values are kmeans, pam, hcut, clara, ...")
  else if (!is.function(FUNcluster)) {
    stop("The argument FUNcluster should be a function. ", 
         "Check if you're not overriding the specified function name somewhere.")
  }
  else if (method %in% c("silhouette", "wss")) {
    if (is.data.frame(x)) 
      x <- as.matrix(x)
    if (is.null(diss)) 
      diss <- stats::dist(x)
    v <- rep(0, k.max)
    if (method == "silhouette") {
      for (i in 2:k.max) {
        clust <- FUNcluster(x, i, ...)
        v[i] <- .get_ave_sil_width(diss, clust$cluster)
      }
    }
    else if (method == "wss") {
      for (i in 1:k.max) {
        clust <- FUNcluster(x, i, ...)
        v[i] <- .get_withinSS(diss, clust$cluster)
      }
    }
    df <- data.frame(clusters = as.factor(1:k.max), y = v, 
                     stringsAsFactors = TRUE)
    ylab <- "Total Within Sum of Square"
    if (method == "silhouette") 
      ylab <- "Average silhouette width"
    p <- ggpubr::ggline(df, x = "clusters", y = "y", group = 1, 
                        color = linecolor, ylab = ylab, xlab = "Number of clusters k", 
                        main = "Optimal number of clusters")
    if (method == "silhouette") 
      p <- p + geom_vline(xintercept = which.max(v), linetype = 2, 
                          color = linecolor)
    return(p)
  }
  else if (method == "gap_stat") {
    extra_args <- list(...)
    gap_stat <- cluster::clusGap(x, FUNcluster, K.max = k.max, 
                                 B = nboot, verbose = verbose, ...)
    if (!is.null(extra_args$maxSE)) 
      maxSE <- extra_args$maxSE
    else maxSE <- list(method = "firstSEmax", SE.factor = 1)
    p <- fviz_gap_stat(gap_stat, linecolor = linecolor, 
                       maxSE = maxSE)
    return(p)
  }
}

.viz_NbClust <- function (x, print.summary = TRUE, barfill = "steelblue", 
                          barcolor = "steelblue") 
{
  best_nc <- x$Best.nc
  if (any(class(best_nc) == "numeric") )
    print(best_nc)
  else if (any(class(best_nc) == "matrix") ) {
    best_nc <- as.data.frame(t(best_nc), stringsAsFactors = TRUE)
    best_nc$Number_clusters <- as.factor(best_nc$Number_clusters)
    if (print.summary) {
      ss <- summary(best_nc$Number_clusters)
      cat("Among all indices: \n===================\n")
      for (i in 1:length(ss)) {
        cat("*", ss[i], "proposed ", names(ss)[i], 
            "as the best number of clusters\n")
      }
      cat("\nConclusion\n=========================\n")
      cat("* According to the majority rule, the best number of clusters is ", 
          names(which.max(ss)), ".\n\n")
    }
    df <- data.frame(Number_clusters = names(ss), freq = ss, 
                     stringsAsFactors = TRUE)
    p <- ggpubr::ggbarplot(df, x = "Number_clusters", 
                           y = "freq", fill = barfill, color = barcolor) + 
      labs(x = "Number of clusters k", y = "Frequency among all indices", 
           title = paste0("Optimal number of clusters - k = ", 
                          names(which.max(ss))))
    return(p)
  }
}
# assign them to the factoextra namespace
environment(fviz_nbclust) <- asNamespace("factoextra")
assignInNamespace("fviz_nbclust",fviz_nbclust,"factoextra")
environment(.viz_NbClust) <- asNamespace("factoextra")
assignInNamespace(".viz_NbClust",.viz_NbClust,"factoextra")
###### fix the functions###############################################################



# Visualize cluster plots
# Hierarchical clustering
hc.res <- eclust(rfm_Scaled, "hclust", k = 2, hc_metric = "euclidean",hc_method = "ward.D2", graph = FALSE)
hc.res5 <- eclust(rfm_Scaled, "hclust", k = 5, hc_metric = "euclidean",hc_method = "ward.D2", graph = FALSE)
### pam clustering
pam.res <- pam(rfm_Scaled, 6, metric = "euclidean", stand = FALSE)
### Clustering: Compute k-means with k = 3 
set.seed(123)
km.res <- kmeans(rfm_Scaled, centers = 3, nstart = 35)
p1 <- fviz_cluster(hc.res, geom = "point",  ggtheme = theme_minimal())+ggtitle("Hierarchical clustering K=2")
p2 <- fviz_cluster(hc.res5, geom = "point",  ggtheme = theme_minimal())+ggtitle("Hierarchical clustering K=5")
p3 <- fviz_cluster(pam.res, geom = "point",  ggtheme = theme_minimal())+ggtitle("Pam clustering K=6")
p4 <- fviz_cluster(km.res, geom = "point", data=rfm_Scaled,ggtheme = theme_minimal())+ggtitle("k-means clustering K=3")
plot_grid(p1, p2, p3, p4, nrow=2, ncol=2)


# Analysis clustering results
names(km.res)
# Lets also check the amount of customers in each cluster.
km.res$size
# centroids from model on normalized data
km.res$centers 
# In original customer information 
rfm_k3 <- custormer_info %>% 
  mutate(Cluster = km.res$cluster)
rfm_k3 %>% select(-"CustomerID")%>% group_by(Cluster) %>% summarise_all("mean") %>% 
  ungroup() %>% kable() %>% kable_styling()
# As we can see from the results, We have 3 demonstrable clusters. Cluster 1 has the newest transactions, the highest frequency, and transaction amount compared to the other 2 clusters.
#  with cluster 1 containing the most valuable customers.



# rfm features pairwise plot for each cluster
library(GGally)
rfm_df <- as.data.frame(rfm_Scaled)
rfm_df$cluster <- km.res$cluster
rfm_df$cluster <- as.character(rfm_df$cluster)
ggpairs(rfm_df, 1:3, mapping = ggplot2::aes(color = cluster, alpha = 0.5), 
        diag = list(continuous = wrap("densityDiag")), 
        lower=list(continuous = wrap("points", alpha=0.9)))

# rfm features boxplot for each cluster
f1<-ggplot(rfm_df, aes(x = cluster, y = Recency)) + 
  geom_boxplot(aes(fill = cluster))
f2<-ggplot(rfm_df, aes(x = cluster, y = Frequency)) + 
  geom_boxplot(aes(fill = cluster))
f3<-ggplot(rfm_df, aes(x = cluster, y = Monetary)) + 
  geom_boxplot(aes(fill = cluster))
plot_grid(f1, f2, f3, ncol=3)

# Parallel coordiante plots allow us to put each feature on seperate column and lines connecting each column
ggparcoord(data = rfm_df, columns = 1:3, groupColumn = 4, alphaLines = 0.4, title = "Parallel Coordinate Plot for the rfm Data", scale = "globalminmax", showPoints = TRUE) + theme(legend.position = "bottom")


#### RFM analysis
rfm_score <- rfm_table_customer(data=rfm_k3, customer_id=CustomerID, n_transactions=Frequency, recency_days=Recency,  total_revenue=Monetary,recency_bins = 5,frequency_bins =5, monetary_bins = 5)
rfm_details<-rfm_score$rfm

rfm_segments <- rfm_details %>% 
  mutate(segment = ifelse(recency_score >= 4 & frequency_score >= 4 & monetary_score >= 4, "Champion", 
                          ifelse(recency_score >= 2 & frequency_score >= 3 & monetary_score >= 3, "Loyal Customer", 
                                 ifelse(recency_score >= 3 & frequency_score <= 3 & monetary_score <= 3, "Potential Loyalist",
                                        ifelse(recency_score >= 4 & frequency_score <= 1 & monetary_score <= 1, "New Customer",
                                               ifelse((recency_score == 3 | recency_score == 4) & frequency_score <= 1 & monetary_score <= 1, "Promising",
                                                      ifelse((recency_score == 2 | recency_score == 3) & (frequency_score == 2 | frequency_score == 3) & 
                                                               (monetary_score == 2 | monetary_score == 3), "Need Attention",
                                                             ifelse((recency_score == 2 | recency_score == 3) & frequency_score <= 2 & monetary_score <= 2, "About to Sleep",
                                                                    ifelse(recency_score <= 2 & frequency_score > 2 & monetary_score > 2, "At Risk",
                                                                           ifelse(recency_score <= 1 & frequency_score >= 4 & monetary_score >= 4, "Can't lose them",
                                                                                  ifelse(recency_score <= 2 & frequency_score == 2 & monetary_score == 2, "Hibernating", "Lost")))))))))))
head(rfm_segments,5)

# checking segments size

segments_summary<-rfm_segments %>%
  group_by(segment) %>% 
  summarise(counts=n())  %>% 
  ungroup() %>%
  arrange(-counts) 

head(segments_summary,8)

fig1<-rfm_plot_median_recency(rfm_segments)
fig2<-rfm_plot_median_frequency(rfm_segments)
fig3<-rfm_plot_median_monetary(rfm_segments)
plot_grid(fig1, fig2, fig3)

rfm_score$threshold

