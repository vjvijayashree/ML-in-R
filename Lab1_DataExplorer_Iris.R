#install.packages('DataExplorer')
library(DataExplorer)
library(dplyr)
getwd()
df = read.csv('Iris.csv', header=T)
df
# Remove Id
df= select(df,-Id)
plot_str(df)
plot_missing(df)
plot_histogram(df)
plot_density(df)
plot_boxplot(df, 'Species')
boxplot(df)
plot_scatterplot(df, 'Species')
plot_scatterplot(df,'SepalLengthCm')
plot_correlation(df,'continuous')
ds = df [ , 1:4]
plot_scatterplot(ds,"SepalLengthCm")
