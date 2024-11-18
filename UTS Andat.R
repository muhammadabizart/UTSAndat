#UTS No 2
#Read in your data
library(readr)
setwd("C:/Users/ASUS/Documents")
andat=read.csv("DataAnalisisDataB4.CSV",sep = ",",header = T);andat

#Check the packaging (Memeriksa format data)
str(andat)
summary(andat)

#Look the top and bottom data (Melihat data teratas dan terbawah)
head(andat)
tail(andat)

#Check your "n"s (Memeriksa jumlah observasi)
dim(andat)
nrow(andat)
ncol(andat)
sum(is.na(andat))

#Make a plot (Membuat visualisasi data)
library(ggplot2)
plot1=ggplot(Andat,aes(x=TimeSpentOnCourse, y=CompletionRate))+
  geom_point(color="navy")
plot2=ggplot(Andat,aes(x=NumberOfVideosWatched, y=CompletionRate))+
  geom_point(color="maroon")
print(plot1)
print(plot2)

#Follow up
#Uji ANOVA
anova_test=aov(CompletionRate ~ TimeSpentOnCourse + NumberOfVideosWatched, data = Andat);anova_test
summary(anova_test)


#UTS No 3
setwd("C:/Users/ASUS/Documents")
data=read.csv("DataAnalisisDataB4.CSV",sep = ",",header = T)

# Membuat model regresi linear
model=lm(CompletionRate ~ TimeSpentOnCourse + NumberOfVideosWatched, data = data);model
summary(model)

# Plot histogram CompletionRate
ggplot(data, aes(x = CompletionRate)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  ggtitle("Histogram CompletionRate (Data Aktual)") +
  xlab("CompletionRate") +
  ylab("Kepadatan")

# Membuat data berdistribusi normal untuk perbandingan
normal_data=rnorm(1000, mean = mean(data$CompletionRate, na.rm = TRUE), 
                  sd = sd(data$CompletionRate, na.rm = TRUE))

# Plot histogram distribusi normal
ggplot(data.frame(normal_data), aes(x = normal_data)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "orange", alpha = 0.5) +
  geom_density(color = "green") +
  ggtitle("Histogram Distribusi Normal") +
  xlab("X") +
  ylab("Y")

# Perbaikan model regresi linear
model=lm(CompletionRate ~ TimeSpentOnCourse + CourseCompletion, data = data);model
summary(model)

