#M.Awais
# Loading Required library
library(DataExplorer)
library(ggplot2)
library(ggthemes)
library(corrplot)
# Loading CSV datafile 
df <- read.csv('pisa.csv')

# Dimension of dataset
dim(df)
# names of featues 
names(df)

# brief summary of dataset
str(df)
# statistic of each variable 
summary(df) 

# Types of featues on visual Graph
plot_str(df, type = "d", max_level = 1)


# working on dataset
# BASIC INFO AABOUT DATASET

# cheeking the docmentation
?introduce
# basic information about dataset 
introduce(df)


# CHEEKING THE STRUCTURE OF DATASET

# cheek the docomentation
?plot_str
# plot the structure of Dataset
plot_str(df, type = "d")
     

# CHEEK THE MISSINING VALUE IN DATASET 

# cheek the docmentation 
?plot_missing

# plot frequency of missing vlaues
plot_missing(df,
             geom_label_args = list("size"=2.5,"label.padding"=unit(0.2,"lines")),
             missing_only=TRUE,
             theme_config = list(legend.position = c("right"))
             )
             

# COUNTING MISSING VLAUES 

sum(is.na(df$read30MinsADay))
# or we we can use summary()
summary(df)


#  DISTRIBUTION OF DATASET 

# cheek the doc
?plot_histogram

# plot histogram
plot_histogram(df,
               geom_histogram_args = list(bins = 35L),
               title = "Histogram",
               ggtheme = theme_minimal(),
               nrow = 3L,
               ncol = 3L
               )


# DENSITY PLOT / SOMOOTH HISTOGRAM

?plot_density

# plot density graph 
plot_density(df,
             title = "Density Plot",
             ggtheme = theme_minimal(),
             nrow = 3L,
             ncol = 3L
             )

#  BAR CHAR FOR CATAGORIAL VAIRALES / DISCRETE 

# cheek the doc
?plot_bar

# bar chart for discrete variable
plot_bar(df,
         title = "Bar Chart",
         ggtheme = theme_minimal(),
         nrow = 5L,
         ncol = 5L,
         )
# ploting raceeth colums
plot_bar(df$raceeth,
         title = "Bar chart",
         ggtheme = theme_minimal(),
         nrow = 1L,
         ncol = 1L
         )


# BOXPLOT FOR OUTLIER DETECTION 

?boxplot
boxplot(df$grade,notch = TRUE)
boxplot(df$minutesPerWeekEnglish,notch = TRUE)
boxplot(df$readingScore,notch = TRUE)
boxplot(df$schoolSize,notch = TRUE)
boxplot(df$studentsInEnglish)

#  CORELATION HEATMAP TO FEATUES VARIABLE

?corrplot
library(corrplot)

smaple <- df[,15:20]
smaple <- na.omit(smaple)
m <- cor(smaple)
corrplot(m,
         method = "number",
         bg='white')



# configuring the plot report
config <- configure_report(
  add_introduce = TRUE,
  add_plot_intro = TRUE,
  add_plot_str = TRUE,
  add_plot_missing = TRUE,
  add_plot_histogram = TRUE,
  add_plot_density = TRUE,
  add_plot_qq = FALSE,
  add_plot_bar = TRUE,
  add_plot_correlation = TRUE,
  add_plot_prcomp = FALSE,
  add_plot_boxplot = TRUE,
  add_plot_scatterplot = TRUE,
  global_ggtheme = quote(theme_minimal(base_size = 18))
)

create_report(df,
              output_file = "update.html",
              output_dir = getwd(),
              report_title = "Exploratory Data Analysis-pisa2009train(CSV)",
              config = configure_report(add_introduce = TRUE,
                                        plot_intro_args = list('title'="Percentage"),
                                        add_plot_missing = TRUE,
                                        add_plot_str = TRUE,
                                        # add_plot_missing=TRUE,
                                        add_plot_histogram = TRUE,
                                        add_plot_density = TRUE,
                                        add_plot_qq = FALSE,
                                        add_plot_bar = TRUE,
                                        add_plot_correlation = TRUE,
                                        add_plot_prcomp = FALSE,
                                        add_plot_scatterplot = TRUE,
              )
              
)
# replacing na with mean of that column

df$minutesPerWeekEnglish[is.na(df$minutesPerWeekEnglish)]<-mean(df$minutesPerWeekEnglish,na.rm=TRUE)
df$schoolSize[is.na(df$schoolSize)]<-mean(df$schoolSize,na.rm=TRUE)
df$studentsInEnglish[is.na(df$studentsInEnglish)]<-mean(df$studentsInEnglish,na.rm=TRUE)

####################### linear regression ####################

#ploating with dependent and independent varaibles

scatter.smooth( df$readingScore ~ df$minutesPerWeekEnglish,main="Reading score vs minutesperweek")
scatter.smooth( df$readingScore ~ df$schoolSize,main="Reading score vs school size")
scatter.smooth( df$readingScore ~ df$studentsInEnglish,main="Reading score vs student in eng")

#Checking co relation

cor(df$readingScore,df$minutesPerWeekEnglish)
#build model

linearMod <- lm(readingScore ~minutesPerWeekEnglish, data=df)  # build linear regression model on full data
print(linearMod)

#getting summary of model
summary(linearmod)

AIC(linearMod)        
BIC(linearMod)

# creating train and test data set/

set.seed(100)  # setting seed to reproduce results of random sampling
trainingRowIndex <- sample(1:nrow(df), 0.8*nrow(df))  # row indices for training data
trainingDat <- df[trainingRowIndex, ]  # model training data
testData  <- df[-trainingRowIndex, ]   # test data


# Build the model on training data -

lmMod <- lm(readingScore ~ minutesPerWeekEnglish, data=trainingDat)  # build the model
distPred <- predict(lmMod, testData)  # predict values

#Model summary

summary(lmMod)
AIC(lmMod)

# calculate accuracy
actuals_preds <- data.frame(cbind(actuals=testData$readingScore, predicteds=distPred))
correlation_accuracy <- cor(actuals_preds) 
head(actuals_preds)     #gettig actual and preds to compare result


###############Multiple linear Regression######################
names(df)
model =lm(readingScore ~ minutesPerWeekEnglish+schoolSize+studentsInEnglish, data=trainingDat)
distPredM <- predict(model, testData)

#summary
summary(model)
AIC(model)

# calculate accuracy
actuals_predsM <- data.frame(cbind(actuals=testData$readingScore, predicteds=distPredM))
correlation_accuracyM <- cor(actuals_predsM) 
head(actuals_predsM)     #gettig actual and preds to compare result
