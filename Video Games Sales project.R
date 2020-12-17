# This code contains data exploration and prediction models on video game sales.
# Data set used here is Video Game Sales with Rating data set.
# This data set has been created by Rush Kirubi on Kaggle
# Created in Dec 2020.


# 1. Download and prepare the data set ------------------------------------

# Ensuring all packages are installed and loaded
if(!require(tidyverse)) install.packages("tidyverse",
                                         repos = "http://cran.us.r-project.org")
if(!require(funModeling)) install.packages("funModeling",
                                           repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret",
                                     repos = "http://cran.us.r-project.org")
if(!require(corrgram)) install.packages("corrgram",
                                        repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales",
                                      repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr",
                                      repos = "http://cran.us.r-project.org")
if(!require(tm)) install.packages("tm",
                                  repos = "http://cran.us.r-project.org")
if(!require(wordcloud)) install.packages("wordcloud",
                                         repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra",
                                         repos = "http://cran.us.r-project.org")
# For model in train function
if(!require(Cubist)) install.packages("Cubist",
                                      repos = "http://cran.us.r-project.org")
if(!require(nnls)) install.packages("nnls",
                                    repos = "http://cran.us.r-project.org")

library(tidyverse)
library(funModeling)
library(scales)
library(caret)
library(corrgram)
library(ggpubr)
library(wordcloud)
library(tm)
library(gridExtra)

# Downloading Video Game Sales with Rating data set,
# created by Rush Kirubi from my GitHub repository
url_path <-
  "https://raw.githubusercontent.com/Golfik/Video-Games-Sales/master/
Video_Games_Sales_as_at_22_Dec_2016.csv"

raw.videosales <- read.csv(url(url_path),na.strings = c("","NA"))
rm(url_path)

# Create a duplicate of data_set for analysis (leaving raw.videosales for reference)
video_sales <- raw.videosales



# 2. Data overview --------------------------------------------------------

# First look in the data set
glimpse(video_sales)

# Checking out duplicate values - non found
duplicated(video_sales) %>% sum()

# Looking deeper to see if there are any obvious outliers
summary(video_sales)

# Checking out if there are many NAs and how many unique values
status(video_sales)



# 3. Exploring, visualizing and cleaning the data set ---------------------


# 3.1. Global and regional sales variables ----

# Analyzing density of Global Sales
ggplot(video_sales,aes(Global_Sales)) + 
  geom_histogram(aes(y=..density..),binwidth = 0.5) + 
  geom_density(alpha=0.2,fill="#EF3B2C") +
  labs(x="Global Sales [M units]", y ="Density",
       title="Density plot of Global Sales variable") + 
  theme_bw()

# Checking out top 10 best selling games globally,
# to see what outliers may influence long tail on previous plot
video_sales %>% 
  select(Name,Platform, Genre, User_Count, Global_Sales) %>% 
  arrange(desc(Global_Sales)) %>% 
  head(10)

# Global Sales density plot of all up to 3rd quantile,
# with shown mean value (zooming to values without tail)
tot.mean <- video_sales %>% 
  arrange(desc(Global_Sales)) %>% 
  select(Name, Global_Sales) %>% 
  summarise(tot.mean=mean(Global_Sales)) %>% 
  unlist()
video_sales %>% 
  arrange(desc(Global_Sales)) %>% 
  filter(between(Global_Sales,0,0.47)) %>% 
  ggplot(aes(Global_Sales)) + 
  geom_histogram(aes(y=..density..),binwidth = 0.01) + 
  geom_density(alpha=0.2,fill="#EF3B2C") + 
  geom_vline(aes(xintercept=tot.mean), linetype="dashed", size=1) + 
  labs(x="Global Sales [M units]", y ="Density",
       title="Density of Global Sales, exluding top quantile",
       subtitle="Dashed line marking mean value of 0.5335 (all values)") + 
  theme_bw()
rm(tot.mean)

# Changing the Year of Release to numeric type
video_sales <- video_sales %>% 
  mutate(Year_of_Release=as.numeric(Year_of_Release))

# Global Sales, profit per title and number of games through years
plot1 <- video_sales %>% 
  group_by(Year_of_Release) %>% 
  summarise(sum_of_sales=sum(Global_Sales)) %>% 
  ggplot() +
  geom_point(aes(x=Year_of_Release, y=sum_of_sales),
             stat = 'identity',show.legend = FALSE) + 
  geom_line(aes(x=Year_of_Release, y=sum_of_sales),
            stat = 'identity',show.legend = FALSE) + 
  labs(x = "Year", y = "Global Sales", title="Global sales [M units] through years") + 
  theme_bw()
plot2 <- video_sales %>% 
  group_by(Year_of_Release) %>% 
  summarise(sales_pertitle=sum(Global_Sales)/n()) %>% 
  ggplot() + 
  geom_point(aes(x=Year_of_Release, y=sales_pertitle),
             stat = 'identity',show.legend = FALSE) + 
  geom_line(aes(x=Year_of_Release, y=sales_pertitle),
            stat = 'identity',show.legend = FALSE) + 
  labs(x = "Year", y = "Sales per title",
       title="Units sold [M units] per game title through years") + 
  theme_bw() +
  scale_color_brewer(palette = "RdYlBu")
plot3 <- video_sales %>% 
  group_by(Year_of_Release) %>% 
  summarise(n=n()) %>% 
  ggplot() + 
  geom_point(aes(x=Year_of_Release, y=n),
             stat = 'identity',show.legend = FALSE) + 
  geom_line(aes(x=Year_of_Release, y=n),
            stat = 'identity',show.legend = FALSE) + 
  labs(x = "Year", y = "Number of games",
       title="Number of games released through years") + 
  theme_bw()
ggarrange(plot1,plot2,plot3,ncol=1, align="hv")
rm(plot1,plot2,plot3)
 
# Remove 2020 as weird and 2017 as not enough data (insight from previous graph),
# as well as the name of original data set suggest is shouldn't be included
video_sales %>% 
  group_by(Year_of_Release) %>% 
  summarise(n=n()) %>% 
  arrange(desc(Year_of_Release))
video_sales <- video_sales %>% filter(Year_of_Release < 2017)

# Share of sales between regions through years
video_sales %>% 
  select(Year_of_Release, NA_Sales, EU_Sales, JP_Sales, Other_Sales) %>% 
  gather(Region, Sales, NA_Sales, EU_Sales, JP_Sales, Other_Sales) %>% 
  group_by(Year_of_Release,Region) %>% 
  ggplot() +
  geom_bar(aes(x=Year_of_Release, y=Sales, fill=Region),
           position="fill", stat="identity") + 
  labs(x = "Year", y = "Sales",
       title="Share of sales between regions through years") + 
  theme_bw() + 
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer(palette = "RdYlBu",
                    labels=c("EU", "Japan", "North America", "Other")) 


# 3.2. Publisher and Developer analysis -----------------------------------

# Global sales by publisher, as total sum of sales made
plot1 <- video_sales %>% 
  gather(Region, Sales, NA_Sales, EU_Sales, JP_Sales, Other_Sales) %>% 
  group_by(Publisher,Region) %>% 
  summarise(sum_Sales=sum(Sales),total=sum(Global_Sales)) %>% 
  arrange(desc(total)) %>% 
  head(40) %>% 
  ggplot(aes(x=reorder(Publisher,desc(total)), y=sum_Sales,fill=Region)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 15, hjust=0.95)) + 
  labs(x="Publisher", y ="Global Sales [M units]",
       title="Top 10 best selling publishers globally, as total sales made") +
  scale_fill_brewer(palette = "PRGn", labels=c("EU", "Japan", "North America", "Other")) 

# Global Sales by developer, as total sum of sales
plot2 <- video_sales %>% 
  filter(!is.na(Developer)) %>% 
  gather(Region, Sales, NA_Sales, EU_Sales, JP_Sales, Other_Sales) %>% 
  group_by(Developer,Region) %>% 
  summarise(sum_Sales=sum(Sales),total=sum(Global_Sales)) %>% 
  arrange(desc(total)) %>% 
  head(40) %>% 
  ggplot(aes(x=reorder(Developer,desc(total)), y=sum_Sales,fill=Region)) + 
  geom_bar(stat = "identity") + 
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 15, hjust=0.95)) + 
  labs(x="Developer", y ="Global Sales [M units]",
       title="Top 10 best selling developers globally, as total sales made") +
  scale_fill_brewer(palette = "PRGn", labels=c("EU", "Japan", "North America", "Other"))

# Arranging two previous plots, and clearing environment
ggarrange(plot1,plot2,ncol=1,nrow=2,common.legend=TRUE, legend="bottom")
rm(plot,plot2)

# Global sales made per title by top publishers
plot1 <- video_sales %>%
  group_by(Publisher) %>% 
  summarise(sale_pertitle=sum(Global_Sales)/n()) %>% 
  arrange(desc(sale_pertitle)) %>% 
  head(10) %>% 
  ggplot(aes(y=sale_pertitle,x=reorder(Publisher,sale_pertitle))) + 
  geom_bar(stat = "identity", fill="#006CD1") + 
  theme_bw() + 
  labs(x="Publisher", y ="Sales per title [M units]",
       title="Top 10 publishers by sales per title") +
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50))

# Global sales per title by top developers
plot2 <- video_sales %>% 
  filter(!is.na(Developer)) %>% 
  group_by(Developer) %>% 
  summarise(sale_pertitle=sum(Global_Sales)/n()) %>% 
  arrange(desc(sale_pertitle)) %>% 
  head(10) %>% 
  ggplot(aes(y=sale_pertitle,x=reorder(Developer,sale_pertitle))) + 
  geom_bar(stat = "identity", fill="#006CD1") + 
  theme_bw() + 
  labs(x="Developer", y ="Sales per title [M units]",
       title="Top 10 developers by sales per title") + 
  coord_flip() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 50))

# Arranging two previous plots, and clearing environment
ggarrange(plot1,plot2,ncol=1,nrow=2, align="v")
rm(plot1,plot2)

# How many games has top Publisher released?
video_sales %>% 
  group_by(Publisher) %>% 
  mutate(sale_per_title=sum(Global_Sales)/n(),no_of_games=n()) %>% 
  arrange(desc(sale_per_title)) %>% 
  select(Publisher,sale_per_title, no_of_games) %>% 
  unique() %>% 
  head(10)

# How many games has top Developer released?
video_sales %>% 
  group_by(Developer) %>% 
  mutate(sale_per_title=sum(Global_Sales)/n(),no_of_games=n()) %>% 
  arrange(desc(sale_per_title)) %>% 
  select(Developer,sale_per_title, no_of_games) %>% 
  unique() %>% 
  head(10)

# How will publisher factor look like if we penalize studios with small number
# of titles with function with limit to 1?
video_sales %>% 
  group_by(Publisher) %>% 
  mutate(sale_per_title=sum(Global_Sales)/n(),no_of_games=n(),
         adj.factor=no_of_games*sin(1/no_of_games),
         Publisher.fct=sale_per_title*no_of_games*sin(1/no_of_games)) %>% 
  arrange(desc(sale_per_title)) %>% 
  select(Publisher,sale_per_title, no_of_games, adj.factor, Publisher.fct) %>% 
  unique() %>% 
  head(10)

# How will developer factor look like if we penalize studios with small number
# of titles with function with limit to 1?
video_sales %>% 
  group_by(Developer) %>% 
  mutate(sale_per_title=sum(Global_Sales)/n(),no_of_games=n(),
         adj.factor=no_of_games*sin(1/no_of_games),
         Developer.fct=sale_per_title*no_of_games*sin(1/no_of_games)) %>% 
  arrange(desc(sale_per_title)) %>% 
  select(Developer,sale_per_title, no_of_games, adj.factor, Developer.fct) %>% 
  unique() %>% 
  head(10)

# Add new Developer and Publisher factors to the video_sales data set,
# and removing original Publisher and Developer
video_sales <- video_sales %>% 
  group_by(Publisher) %>% 
  mutate(Publisher.fct=(sum(Global_Sales)/n())*n()*sin(1/n())) %>% 
  ungroup()
video_sales <- video_sales %>% 
  group_by(Developer) %>% 
  mutate(Developer.fct=(sum(Global_Sales)/n())*n()*sin(1/n())) %>% 
  ungroup()
video_sales <- video_sales %>% 
  select(-Publisher,-Developer)


# 3.3. Platform and Genre analysis ----------------------------------------

# Number of game titles released per platform (only top)
video_sales %>% 
  group_by(Platform) %>% 
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(15) %>%
  ggplot(aes(x=reorder(Platform,n),y=n)) +
  geom_segment(aes(x=reorder(Platform,n),
                   xend=reorder(Platform,n), y=0, yend=n), color="grey") +
  geom_point(color="#FD8D3C", size=4) +
  coord_flip() +
  theme_bw() +
  labs(x="Platform", y ="Number of titles released",
       title="Top 15 platforms with highest number of games released")

# Top selling games, and their genres and platforms
video_sales %>% 
  arrange(desc(Global_Sales)) %>% 
  head(15) %>% 
  ggplot(aes(y=Global_Sales,x=reorder(Name,Global_Sales),fill=Genre)) + 
  geom_bar(stat="identity") + geom_text(aes(label=Platform), hjust=1.5) +
  coord_flip() +
  theme_bw() + 
  labs(x="Global Sales [M of units]", y ="Game title and its platform",
       title="Top 15 selling games globally") + 
  scale_fill_brewer(palette = "RdYlBu")

# Sales per genre 
video_sales %>% 
  group_by(Genre) %>% 
  summarise(n_titles=n(), total_sales=sum(Global_Sales)) %>% 
  arrange(desc(total_sales))

# Erasing NAs from Genre
video_sales <- video_sales %>% 
  filter(!is.na(Genre))

# Changing Genre to a factor
video_sales$Genre <- as.factor(video_sales$Genre)

# Average sales per one title by each game genre
video_sales %>% 
  group_by(Genre) %>% 
  summarise(sale_pertitle=sum(Global_Sales)/n()) %>% 
  arrange(desc(sale_pertitle)) %>% 
  ggplot(aes(y=sale_pertitle,x=reorder(Genre,desc(sale_pertitle)))) + 
  geom_bar(stat = "identity", fill="#006CD1") + 
  theme_bw() + 
  labs(x="Genre", y ="Sales per title [M units]",
       title="Sales per one title per game genre") + 
  theme(axis.text.x = element_text(angle = 15, hjust=0.95))


# 3.4. User and Critic Score and Count analysis ---------------------------

# User Score and Critic Score frequency plot,
# across different Genre (dark theme for visibility)
plot1 <- ggplot(video_sales, aes(as.numeric(User_Score), color=Genre)) +
  geom_freqpoly(size=1) +
  theme_dark() +
  scale_color_brewer(palette = "Paired") +
  labs(x="User Score", y ="Count",
       title="Distribution of User Scores across game genres")
plot2 <- ggplot(video_sales, aes(as.numeric(Critic_Score), color=Genre)) +
  geom_freqpoly(size=1) +
  theme_dark() +
  scale_color_brewer(palette = "Paired") +
  labs(x="Critic Score", y ="Count",
       title="Distribution of Critic Scores across game genres")
ggarrange(plot1,plot2,ncol=1,nrow=2,common.legend=TRUE, legend="bottom")
rm(plot1,plot2)

# Checking out User_Score - why it's character, 
# then change tbd to NA, and variable type to numeric
video_sales %>% 
  group_by(User_Score) %>% 
  summarise(n=n()) %>%
  arrange(desc(n)) %>%
  head(10)

video_sales$User_Score[video_sales$User_Score=="tbd"] <- NA

video_sales$User_Score <- as.numeric(video_sales$User_Score)

#Check the same for Critic Score.
# There are no character values, and it can be changed to numeric
video_sales %>% 
  group_by(Critic_Score) %>% 
  summarise(n=n()) %>% 
  arrange(desc(n)) %>% 
  head(10)

video_sales$Critic_Score <- as.numeric(video_sales$Critic_Score)

# Correlation plot between User Score and Critic Score
video_sales %>%
  na.omit() %>%
  ggplot(aes(x=User_Score,y=Critic_Score)) +
  geom_point() +
  geom_smooth(method="loess") +
  theme_bw() +
  labs(x="User Score", y ="Critic Score",
       title="Correlation between User and Critic Score")

# Influence of scoring difference between User and Critic Score on Global Sales
video_sales %>%
  na.omit() %>%
  mutate(diff=(User_Score*10-Critic_Score)) %>%
  group_by(diff) %>%
  summarise(sums=sum(Global_Sales)) %>%
  ggplot(aes(x=diff,y=sums)) +
  geom_point(color="#006CD1") +
  theme_bw() +
  labs(x="Difference between User and Critic Score",
       y ="Global Sales [M of units]",
       title="Influence of scoring difference between Users and Critics on Global Sales")

# User and Critic Score impact on sales
video_sales %>%
  na.omit() %>%
  mutate(User_Score=User_Score*10) %>%
  gather(Type, Score, User_Score, Critic_Score) %>%
  group_by(Type,Score) %>%
  summarise(totalsales=sum(Global_Sales)) %>%
  ggplot(aes(x=Score,y=totalsales, fill=Type)) +
  geom_area(position="identity") +
  facet_grid(Type~.) +
  theme_bw() +
  scale_fill_brewer(palette="Dark2", label=c("Critic Score", "User Score*10")) +
  labs(x="Score", y ="Global Sales [M of units]",
       title="Global Sales vs. Critic and User Score")

# Count analysis of users and critics, and their influence on Global Sales
video_sales %>%
  na.omit() %>%
  gather(Type, Count, User_Count, Critic_Count) %>%
  group_by(Type,Count) %>%
  summarise(totalsales=sum(Global_Sales)) %>%
  ggplot(aes(x=Count,y=totalsales,fill=Type)) +
  geom_area(position="identity") +
  facet_grid(~Type, scales="free") +
  theme_bw() +
  labs(x="Count", y ="Global Sales [M of units]",
       title="Global Sales vs. Critic and User score Count") +
  scale_fill_brewer(palette="Dark2", label=c("Critic Count", "User Count"))

# User and Critic Score (as average) and Count across years
plot1 <- video_sales %>%
  na.omit() %>%
  group_by(Year_of_Release) %>%
  summarise(totalusers=mean(User_Score*10/User_Count),
            totalcritics=mean(Critic_Score/Critic_Count)) %>%
  gather(Type, Count, totalusers, totalcritics) %>%
  ggplot() +
  geom_line(aes(x=Year_of_Release, y=Count, color=Type), size=1) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2", label=c("Critics", "Users")) +
  labs(x="Year of Release", y ="Sum of avg Scores (normalized) ",
       title="Distribution of Critic and User Scores (avg per title) across years")
plot2 <- video_sales %>%
  na.omit() %>%
  group_by(Year_of_Release) %>%
  summarise(totalusers=sum(User_Count),
            totalcritics=sum(Critic_Count)) %>%
  gather(Type, Count, totalusers, totalcritics) %>%
  ggplot() +
  geom_line(aes(x=Year_of_Release, y=Count, color=Type), size=1) +
  theme_bw() +
  scale_color_brewer(palette = "Dark2", label=c("Critics", "Users")) +
  labs(x="Year of Release", y ="Sum of Counts",
       title="Distribution of Critic and User score Counts across years")
ggarrange(plot1,plot2,ncol=1,nrow=2,common.legend=TRUE, legend="bottom", align="v")
rm(plot1,plot2)

# Are there any titles that has not been scored by Users or by Critics?
video_sales %>% filter(User_Count==0 | Critic_Count==0) %>%
  select(Name,Year_of_Release,Global_Sales,User_Score,User_Count,
         Critic_Score,Critic_Count)

# Imputing User and Critic Score and Count NAs with medians
video_sales$User_Score[is.na(video_sales$User_Score)] <-
  median(video_sales$User_Score, na.rm = TRUE)
video_sales$User_Count[is.na(video_sales$User_Count)] <-
  median(video_sales$User_Count, na.rm = TRUE)
video_sales$Critic_Score[is.na(video_sales$Critic_Score)] <-
  median(video_sales$Critic_Score, na.rm = TRUE)
video_sales$Critic_Count[is.na(video_sales$Critic_Count)] <-
  median(video_sales$Critic_Count, na.rm = TRUE)


# 3.5. Rating variable ----------------------------------------------------

# Exploring unique values of Rating variable
video_sales %>% group_by(Rating) %>% summarise(n=n()) %>% arrange(desc(n))

# Changing the K-A rating to E, and RP to NA value
video_sales$Rating[video_sales$Rating=="RP"] <- NA
video_sales$Rating[video_sales$Rating=="K-A"] <- "E"

# Chaining Rating variable to factor
video_sales$Rating <- as.factor(video_sales$Rating)

# Analysis of game rating influence on global sales per one title
video_sales %>%
  filter(!is.na(Rating)) %>%
  group_by(Rating) %>%
  summarise(sales=sum(Global_Sales)/n(),n=n()) %>% 
  ggplot(aes(x=reorder(Rating,sales), y=sales)) +
  geom_segment( aes(x=reorder(Rating,sales), xend=reorder(Rating,sales),
                    y=0, yend=sales), color="black") +
  geom_point( color="blue", size=4) +
  geom_text(aes(label=paste("#games:",n)),hjust=0.9, vjust=1.75) +
  coord_flip() +
  theme_bw() +
  labs(x="Rating", y ="Global Sales per title [M units]",
       title="Global Sales per one title per game rating")

# Excluding Rating NA values from data set (as they can't be imputed with median)
video_sales <- video_sales %>% filter(!is.na(Rating))


# 3.6. Game title analysis ------------------------------------------------


# 3.6.1. Creating a wordcloud ---------------------------------------------

# Creating text corpus from Name string
text <- Corpus(VectorSource(video_sales$Name))

# Cleaning Name string
text <- text %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, stopwords("english"))

# Changing the format of the data
text <- as.matrix(TermDocumentMatrix(text)) 

# Sum count of words in text, rearrange  and change into data frame
words <- sort(rowSums(text),decreasing=TRUE) 
text.df <- data.frame(word = names(words),freq=words)

# Inspecting new data frame
text.df %>% select(freq) %>% head(10)

# Excluding "the" as word
text.df <- text.df[!text.df$word=="the",]

# Creating wordcloud of cleaned Name string
wordcloud(text.df$word, text.df$freq, min.freq = 50, max.words=200,
          random.order=FALSE, max.freq=400, rot.per=0.35,
          colors=brewer.pal(12, "Paired"))


# 3.6.2. Checking correlation between words and Global Sales --------------

# Creating a sum sales column to each word in text.df 
c <- count(text.df)[[1]]
for (i in 1:c) {
  text.df$sales[i] <-
    video_sales$Global_Sales[str_detect(video_sales$Name,
                                        fixed(text.df$word[i],
                                              ignore_case=TRUE))] %>%
    sum()
}

# Global Sales vs frequency of the word plot
text.df %>%
  ggplot(aes(x=freq, y=sales)) +
  geom_point() +
  geom_smooth(method=loess, se=TRUE) +
  theme_bw() +
  labs(x="Frequency of the word used in game title",
       y ="Global Sales [M units]",
       title="Sales per frequency of words used in the game title")

# Removing Name variable from video_sales data set
video_sales <- video_sales %>% select(-Name)

# Cleaning the environment
rm(text.df, text, c, i, words)


# 3.7. Final check of the data set ----------------------------------------

# Checking out medians and outliers
summary(video_sales)

# Checking if there are any NAs left 
is.na(video_sales) %>% sum()

# Checking out the status
status(video_sales)



# 4. Modeling -------------------------------------------------------------


# 4.1. Correlation between variables and PCA ------------------------------

# Correlation plot of variables in video_sales data set
corrgram(video_sales[,sapply(video_sales, is.numeric)],
         order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt,
         main="Correlogram of variables selected for modeling")


# Creating a dataframe with only numerical values
# from *video_sales* to use in PCA analysis
pca_data <- video_sales %>% select(-Platform,-Genre,-Rating)

# PCA calculations
pr.vsales <- prcomp(pca_data, scale=TRUE)
summary(pr.vsales)

# Variance and cumulative variance plots
pr.var <- pr.vsales$sdev^2
pve <- pr.var/sum(pr.var)
par(mfrow=c(2,1))
plot(pve, type="b", lwd=2, col="#FD8D3C",
     main="Variance of PCA",
     xlab="Number of Principal Components",
     ylab="% of Variance")
plot(cumsum(pve), type="b", lwd=2, col="#41B6C4",
     main="Cumulative variance of PCA",
     xlab="Number of Principal Components",
     ylab="% of Cumulative Variance")
abline(h=0.9, lty=2)

# Cleaning the environment
rm(pca_data,pr.vsales,pr.var,pve)


# 4.2. Global Sales prediction models and their results -------------------


# 4.2.1. Creating a training and testing sub-sets -------------------------

# Removed the regional sales variables, as they make Global Sales 1,
# and we don't want to use them as predictors
globalsales <- video_sales %>%
  select(-NA_Sales,-EU_Sales,-Other_Sales, -JP_Sales)

# Setting seed for reproducibility
set.seed(1)

# Creating train and test sets following 80/20 Pareto Principle
global.test_index <- createDataPartition(y = globalsales$Global_Sales,
                                         times = 1, p = 0.2, list = FALSE)
global.test <- globalsales %>% slice(global.test_index)
global.train <- globalsales %>% slice(-global.test_index)

# Making sure that both test and train have same set of data
global.train <- global.train %>%
  semi_join(global.test, by = "Platform") %>%
  semi_join(global.test, by="Genre") %>%
  semi_join(global.test, by="Rating")


# 4.2.2. Models for predicting Global Sales -------------------------------

# Linear regression model (10-fold cv, pre-processed)
global.lm.fit <- train(Global_Sales~.,data=global.train, method="lm",
                       trControl=trainControl(method="cv",number=10),
                       preProcess = c("center", "scale"))

global.lm.pred <- predict(global.lm.fit,global.test)

# Cubist (10-fold cv, pre-processed), with tuned committees and neighbors
global.cubist.fit <- train(Global_Sales~.,data=global.train, method="cubist",
                           trControl=trainControl(method="cv",number=10),
                           tuneGrid=data.frame(committees=seq(1,9,1),
                                               neighbors=seq(1,9,1)),
                           preProcess = c("center", "scale"))

global.cubist.pred <- predict(global.cubist.fit,global.test)

plot(global.cubist.fit,highlight = TRUE)
global.cubist.fit$bestTune

# Regression trees (10-fold cv, pre-processed), with tuned cp
global.rpart.fit <- train(Global_Sales~.,data=global.train, method="rpart",
                          tuneGrid=data.frame(cp=seq(0,0.05,0.005)),
                          trControl=trainControl(method="cv",number=10),
                          preProcess = c("center", "scale"))

global.rpart.pred <- predict(global.rpart.fit,global.test)

ggplot(global.rpart.fit, highlight=TRUE)
global.rpart.fit$bestTune

# Random forest (10-fold cv, pre-processed), with 100 trees and tuned mtry
global.rf.fit <- train(Global_Sales~., data = global.train, method="rf",
                                   trControl=trainControl(method="cv", number=10),
                                   tuneGrid=data.frame(mtry=seq(1,25,1)),
                                   ntree=100, preProcess = c("center", "scale"))

global.rf.pred <- predict(global.rf.fit,global.test)

global.rf.fit$bestTune
ggplot(global.rf.fit, highlight=TRUE)

# Non-negative least squares NNLS (10-fold cv, pre-processed)
global.nnls.fit <- train(Global_Sales~.,data=global.train, method="nnls",
                         trControl=trainControl(method="cv",number=10),
                         preProcess = c("center", "scale"))

global.nnls.pred <- predict(global.nnls.fit,global.test)


# 4.2.3. Predicting Global Sales results ----------------------------------

# Creating a list of fitted models, and plotting RMSE, MAE and R-squared results
models <- list(lm=global.lm.fit,cubist=global.cubist.fit,rpart=global.rpart.fit,
               rf=global.rf.fit,nnls=global.nnls.fit)

bwplot(resamples(models),metric=c("RMSE","Rsquared","MAE"),
       main="Global Sales learning models comparison")

# Creating a data frame out of results for fitted models
mae <- list(median(global.lm.fit$resample$MAE), 
            median(global.cubist.fit$resample$MAE), 
            median(global.rpart.fit$resample$MAE),
            median(global.rf.fit$resample$MAE),
            median(global.nnls.fit$resample$MAE))
rmse <- list(median(global.lm.fit$resample$RMSE),
             median(global.cubist.fit$resample$RMSE),
             median(global.rpart.fit$resample$RMSE),
             median(global.rf.fit$resample$RMSE),
             median(global.nnls.fit$resample$RMSE))
rsqr <- list(median(global.lm.fit$resample$Rsquared),
             median(global.cubist.fit$resample$Rsquared),
             median(global.rpart.fit$resample$Rsquared),
             median(global.rf.fit$resample$Rsquared),
             median(global.nnls.fit$resample$Rsquared))

global.results <- data.frame(Model=c("LM","Cubist","Rpart","RandomForest","NNLS"),
                             Median_MAE=unlist(mae), Median_RMSE=unlist(rmse),
                             Median_Rsquared=unlist(rsqr))
global.results

# Actual vs Predicted output plots for all models
plot.lm <- data.frame(Actuals=global.test$Global_Sales,
                      Predicted=global.lm.pred) %>% mutate(Type="LM")
plot.cubist <- data.frame(Actuals=global.test$Global_Sales,
                          Predicted=global.cubist.pred) %>% mutate(Type="Cubist")
plot.rpart <- data.frame(Actuals=global.test$Global_Sales,
                         Predicted=global.rpart.pred) %>% mutate(Type="Rpart")
plot.rf <- data.frame(Actuals=global.test$Global_Sales, 
                      Predicted=global.rf.pred) %>% mutate(Type="RandomForest")
plot.nnls <- data.frame(Actuals=global.test$Global_Sales, 
                        Predicted=global.nnls.pred) %>% mutate(Type="NNLS")

final.plot <- rbind(plot.lm,plot.cubist,plot.rpart,plot.rf,plot.nnls)

ggplot(final.plot, aes(x=Predicted, y=Actuals)) +
  geom_point() +
  geom_smooth(method="loess", se=TRUE) +
  facet_wrap(.~Type) +
  theme_bw()

# Cleaning the environment
rm(final.plot,mae,models,rmse,rsqr, plot.cubist,plot.lm,plot.nnls,plot.rf,plot.rpart)


# 4.3. NA Sales prediction models and their results -----------------------


# 4.3.1. Creating a training and testing sub-sets -------------------------

# Creating a new data frame for modeling of NA_Sales output. 
# Global Sales removed as the variable that we should not know
# (all sales shares are equal to 1)
NAsales <- video_sales %>% select(-Global_Sales)

# Setting seed for reproducibility
set.seed(1)

# Creating train and test sets following 80/20 Pareto Principle
na.test_index <- createDataPartition(y = NAsales$NA_Sales,
                                     times = 1, p = 0.2, list = FALSE)
na.test <- NAsales%>% slice(na.test_index)
na.train <- NAsales %>% slice(-na.test_index)

# Making sure that both test and train have same set of data
na.train <- na.train %>%
  semi_join(na.test, by="Rating") %>%
  semi_join(na.test, by="Platform") %>%
  semi_join(na.test, by="Genre")


# 4.3.2. Models for predicting NA Sales -----------------------------------

# Linear regression model (10-fold cv, pre-processed)
na.lm.fit <- train(NA_Sales~.,data=na.train, method="lm",
                   trControl=trainControl(method="cv",number=10),
                   preProcess = c("center", "scale"))

na.lm.pred <- predict(na.lm.fit,na.test)

# Cubist (10-fold cv, pre-processed), with tuned committees and neighbors parameter
na.cubist.fit <- train(NA_Sales~.,data=na.train, method="cubist",
                       trControl=trainControl(method="cv",number=10),
                       tuneGrid=data.frame(committees=seq(1,9,1),
                                           neighbors=seq(1,9,1)),
                       preProcess = c("center", "scale"))

na.cubist.pred <- predict(na.cubist.fit,na.test)

plot(na.cubist.fit,highlight = TRUE)
na.cubist.fit$bestTune

# Rpart (10-fold cv, pre-processed), with tuned cp parameter
na.rpart.fit <- train(NA_Sales~.,data=na.train, method="rpart",
                      tuneGrid=data.frame(cp=seq(0,0.05,0.005)),
                      trControl=trainControl(method="cv",number=10),
                      preProcess = c("center", "scale"))

na.rpart.pred <- predict(na.rpart.fit,na.test)

ggplot(na.rpart.fit,highlight = TRUE)
na.rpart.fit$bestTune

# Random Forest (10-fold cv, pre-processed), with 100 trees and tunable mtry parameter
na.rf.fit <- train(NA_Sales~.,data=na.train, method="rf",
                   tuneGrid=data.frame(mtry=seq(1,25,1)), ntree=100,
                   trControl=trainControl(method="cv",number=10),
                   preProcess = c("center", "scale"))

na.rf.pred <- predict(na.rf.fit,na.test)

ggplot(na.rf.fit,highlight = TRUE)
na.rf.fit$bestTune

# Non-negative least squares NNLS (10-fold cv, pre-processed)
na.nnls.fit <- train(NA_Sales~.,data=na.train, method="nnls",
                     trControl=trainControl(method="cv",number=10),
                     preProcess = c("center", "scale"))

na.nnls.pred <- predict(na.nnls.fit,na.test)


# 4.3.3. Predicting NA Sales results --------------------------------------

# Creating a list of fitted models, and plotting RMSE, MAE and R-squared results
models <- list(lm=na.lm.fit, cubist=na.cubist.fit, rpart=na.rpart.fit,
               randomforest=na.rf.fit, nnls=na.nnls.fit)

bwplot(resamples(models),metric=c("RMSE","Rsquared","MAE"),
       main="NA Sales learning models comparison")

# Creating a data frame out of results for fitted models
mae <- list(median(na.lm.fit$resample$MAE),
            median(na.cubist.fit$resample$MAE),
            median(na.rpart.fit$resample$MAE),
            median(na.rf.fit$resample$MAE),
            median(na.nnls.fit$resample$MAE))
rmse <- list(median(na.lm.fit$resample$RMSE),
             median(na.cubist.fit$resample$RMSE),
             median(na.rpart.fit$resample$RMSE),
             median(na.rf.fit$resample$RMSE),
             median(na.nnls.fit$resample$RMSE))
rsqr <- list(median(na.lm.fit$resample$Rsquared),
             median(na.cubist.fit$resample$Rsquared),
             median(na.rpart.fit$resample$Rsquared),
             median(na.rf.fit$resample$Rsquared),
             median(na.nnls.fit$resample$Rsquared))

na.results <- data.frame(Model=c("LM","Cubist","Rpart","RandomForest","NNLS"),
                         Median_MAE=unlist(mae), Median_RMSE=unlist(rmse),
                         Median_Rsquared=unlist(rsqr))
na.results

# Comparison of models results - Actual vs Predicted plots
plot.lm <- data.frame(Actuals=na.test$NA_Sales,
                      Predicted=na.lm.pred) %>% mutate(Type="LM")
plot.cubist <- data.frame(Actuals=na.test$NA_Sales,
                          Predicted=na.cubist.pred) %>% mutate(Type="Cubist")
plot.rpart <- data.frame(Actuals=na.test$NA_Sales,
                         Predicted=na.rpart.pred) %>% mutate(Type="Rpart")
plot.rf <- data.frame(Actuals=na.test$NA_Sales,
                      Predicted=na.rf.pred) %>% mutate(Type="RandomForest")
plot.nnls <- data.frame(Actuals=na.test$NA_Sales,
                        Predicted=na.nnls.pred) %>% mutate(Type="NNLS")

final.plot <- rbind(plot.lm,plot.cubist,plot.rpart,plot.rf,plot.nnls)

ggplot(final.plot, aes(x=Predicted, y=Actuals)) +
  geom_point() +
  geom_smooth(method="loess", se=TRUE) +
  facet_wrap(.~Type) + theme_bw()

# Variable importance table (top 10), arranged side-by-side
na.lm.imp <- varImp(na.lm.fit)
na.cubist.imp <- varImp(na.cubist.fit)
na.rpart.imp <- varImp(na.rpart.fit)
na.rf.imp <- varImp(na.rf.fit)
na.nnls.imp <- varImp(na.nnls.fit)


imp.lm <- data.frame(LM.Variable=rownames(arrange(na.lm.imp$importance,
                                        desc(Overall)))[1:10],
                     Overall=arrange(na.lm.imp$importance,
                                     desc(Overall))[1:10,])
imp.cubist <- data.frame(Cubist.Variable=rownames(arrange(na.cubist.imp$importance,
                                                          desc(Overall)))[1:10],
                         Overall=arrange(na.cubist.imp$importance,
                                         desc(Overall))[1:10,])
imp.rpart <- data.frame(Rpart.Variable=rownames(arrange(na.rpart.imp$importance,
                                                        desc(Overall)))[1:10],
                        Overall=arrange(na.rpart.imp$importance,
                                        desc(Overall))[1:10,])
imp.rf <- data.frame(RF.Variable=rownames(arrange(na.rf.imp$importance,
                                                  desc(Overall)))[1:10],
                     Overall=arrange(na.rf.imp$importance,
                                     desc(Overall))[1:10,])
imp.nnls <- data.frame(NNLS.Variable=rownames(arrange(na.nnls.imp$importance,
                                                      desc(Overall)))[1:10],
                       Overall=arrange(na.nnls.imp$importance,
                                       desc(Overall))[1:10,])

grid.arrange(tableGrob(imp.cubist),tableGrob(imp.rf),nrow=1)
grid.arrange(tableGrob(imp.lm),tableGrob(imp.rpart),tableGrob(imp.nnls),nrow=1)

# Cleaning the environment
rm(final.plot,mae,models,rmse,rsqr, plot.cubist,plot.lm,plot.nnls,plot.rf,plot.rpart)
