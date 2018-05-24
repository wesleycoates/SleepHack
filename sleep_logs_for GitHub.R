# This project entails reading a couple of tabs in an Excel spreadsheet that 
# contain sleep logs for two "treatments": a larger mouth guard and a smaller
# one.  I wore the larger one for about a year and switched to the smaller, and
# I want to see if there are any significant differences between the two sets.

# Start by loading the readxl package and adding it to the R library
install.packages("readxl")
library("readxl")
# update.packages() #do this periodically

# Next step is to explore the spreadsheet a bit by calling the below function. To
# navigate to the file you must first set the proper folder with the spreadsheet
# to the working directory.  Click on the cogwheel with More, and "Set as Working
# Directory" to make that work.  Typing dir() in the Console shows your files.

excel_sheets("fitbit_sleep_data_forR.xlsx")

# That function provides the names of the tabs in the workbook.  Read them into R
# while also defining each dataframe with a variable name.  These are mouthguards.

large_guard <- read_xlsx("fitbit_sleep_data_forR.xlsx", 
                         sheet = "Treatment A")

small_guard <- read_xlsx("fitbit_sleep_data_forR.xlsx", 
                         sheet = "Treatment B")

# with this step I'll turn the zeroes into NA's (lots), for second column ONLY.
# small_guard[small_guard == 0] <- NA || This code applies to the entire dataframe
# whereas the two segments of code below only apply to the 2nd column

large_guard[,2] [large_guard[,2] == 0] <- NA
small_guard[,2] [small_guard[,2] == 0] <- NA

# What I really need is to omit records where the column "Minutes Asleep" is
# zero.  I am defining two new variables to this effect, with code that omits
# those rows that have any zeroes in that second column. SKIPPING FOR NOW
# noNA_large_guard <- large_guard$`Minutes Asleep` != 0
# noNA_small_guard <- small_guard$'Minutes Asleep' != 0

# Now let's look at the structure of each dataframe
str(large_guard)
str(small_guard)

# And print each table out.  Each is available in the Environment.
large_guard
small_guard

# YAY! Finally can do some descriptive statistics on these. But I want no NA's.
sapply(large_guard, median, na.rm = TRUE)
sapply(small_guard, median, na.rm = TRUE)

# This code deletes all rows with any NA values na.omit. Also does summaries.
large_noNA <- na.omit(large_guard)
names(large_noNA)
summary(large_noNA)
small_noNA <- na.omit(small_guard)
names(small_noNA)
summary(small_noNA)

# Therefore, moving forward, I want to use large_noNA and small_noNA as my two
# main files for analyses. 

# I want to add columns giving the day of the week too, to both data frames

LDays <- weekdays(as.Date(large_noNA$Date))
SDays <- weekdays(as.Date(small_noNA$Date))

large_noNA$Weekday <- LDays
small_noNA$Weekday <- SDays

# And get some basic stats on these weekdays, creating Crosstabs
LDays.tab <- table(LDays)
SDays.tab <- table(SDays)

# Plus I need to add in the large vs. small so that I can do the T-test later

large_noNA$Size <- paste("large")
small_noNA$Size <- paste("small")

# some frequencies and descriptives
summary(large_noNA)
summary(small_noNA)
median(large_noNA$`Time in Bed`)
median(small_noNA$`Time in Bed`)
fivenum(large_noNA$`Number of Awakenings`)
fivenum(small_noNA$`Number of Awakenings`)

# install the psych package to get a nice block of descriptive statistics
install.packages("psych")
library("psych")

describe(large_noNA)
describe(small_noNA)

# these are some nice comparative boxplots

boxplot(large_noNA$`Minutes Asleep`, small_noNA$`Minutes Asleep`,
        col = (c("beige", "green")),
        notch = TRUE,
        main = "Minutes Asleep - Large and Small Mouthguards",
        xlab = "Large and Small")
boxplot(large_noNA$`Minutes Awake`, small_noNA$`Minutes Awake`,
        main = "Minutes Awake - Large and Small Mouthguards")
boxplot(large_noNA$`Number of Awakenings`, small_noNA$`Number of Awakenings`,
        main = "Number of Awakenings - Large and Small Mouthguards")

# I need a sample of each table to do this plot properly:
# plot(large_noNA$`Minutes Awake`, small_noNA$`Minutes Awake`)

# To study the categorical variable Weekday, and sleep lost on those days, I 
# will use some analyses combining these together.

# This will create a histogram of the Number of Awakenings variable
hist(large_noNA$`Number of Awakenings`,
     col = "beige",
     main = "Number of awakenings per night - \nLarge Mouthguard",
     xlab = "Number of Awakenings")
hist(small_noNA$`Number of Awakenings`,
     col = "beige",
     main = "Number of awakenings per night - \nSmall Mouthguard",
     xlab = "Number of Awakenings")

# those looked crazy so I transformed using the standard deviation z-score
Lawakenings.z <- scale(large_noNA$`Number of Awakenings`)
Sawakenings.z <- scale(small_noNA$`Number of Awakenings`)

# then created new histograms and descriptions (using the psych package)
hist(Lawakenings.z)
hist(Sawakenings.z)

describe(Lawakenings.z)
describe(Sawakenings.z)

# These last two runs tell me a lot about the differences between the two 
# mouthguards, and how skewed (and kurtosis) the small guard is.  What does
# this mean???  Am I waking up with more outliers with the small guard?

# Going to do the same concept but with logarithm, adding 1 to each value,
# thereby getting around any zero/log math issues. 
Lawakenings.ln1 <- log(large_noNA$`Number of Awakenings` + 1)
Sawakenings.ln1 <- log(small_noNA$`Number of Awakenings` + 1)

hist(Lawakenings.ln1)
hist(Sawakenings.ln1)

describe(Lawakenings.ln1)
describe(Sawakenings.ln1)

# this creates a summary table of the Number of Awakenings variable
large_counts <- table(large_noNA$`Number of Awakenings`)
small_counts <- table(small_noNA$`Number of Awakenings`)
# and as percentages if that's useful
round(prop.table(large_counts), 2)

# This will create scatterplots for some variables
plot(large_noNA$`Minutes Asleep`, large_noNA$`Minutes Awake`,
     xlab = "Minutes Asleep",
     ylab = "Minutes Awake",
     main = "Large mouthguard - Minutes Asleep by Minutes Awake")
abline(lm(large_noNA$`Minutes Awake` ~ large_noNA$`Minutes Asleep`,
          col = "red"))
plot(small_noNA$`Minutes Asleep`, small_noNA$`Minutes Awake`,
     xlab = "Minutes Asleep",
     ylab = "Minutes Awake",
     main = "Small mouthguard - Minutes Asleep by Minutes Awake")
abline(lm(small_noNA$`Minutes Awake` ~ small_noNA$`Minutes Asleep`,
          col = "red"))

# and a basic scatterplot matrix, WHICH ISN'T WORKING AT THE MOMENT
pairs(~'Minutes Awake' + 'Minutes Asleep' + 'Number of Awakenings',
      data = large_noNA,
      pch = 20,
      main = "Large Mouthguard - Simple Scatterplot Matrix")

# with another psych package matrix
pairs.panels(large_noNA[c(3,2,4)], gap = 0)

# This will do some correlations on the quantitative variables of interest.
# Begin by defining new variables with dataframes that select only the 
# quantitative variables of interest:
Lquant <- large_noNA[c(2, 3, 4, 5)]
Squant <- small_noNA[c(2, 3, 4, 5)]

# Then run the correlation on both dataframes.
cor(Lquant)
cor(Squant)

# Install and run the Hmisc package to CORR the whole matrix
install.packages("Hmisc")
library("Hmisc")

rcorr(as.matrix(Lquant))
rcorr(as.matrix(Squant))

# Time for the crystal ball: Regression analyses.  These are not the best data..
# I guess in this case I want to predict the number of minutes awake based
# upon my other variables.  Ideally in real life I would want to keep those
# numbers as small as possible--as few minutes awake as I can.
# Create the model here
regL1 <- lm(`Minutes Awake` ~
            `Minutes Asleep` + `Number of Awakenings` + Weekday,
            data = large_noNA)
# And summarize the model here
summary(regL1)

# Now doing the same for the small mouthguard dataframe
regS1 <- lm(`Minutes Awake` ~
              `Minutes Asleep` + `Number of Awakenings` + Weekday,
            data = small_noNA)
summary(regS1)

# Chi square is not appropriate for this since no real categorical variables,
# but if I was interested, I'd use the following:
# Building on the tables for Day of Week, and with some other cat var:
# chisq.test(LDays.tab)

# Time to try out the t-test to compare two sets of quantitative outcomes.  
# I am going to need to combine my two dataframes into one, giving them another
# new column at the end distinguishing between large v small mouthguard. 
# I just added the column using the paste function (way up on line 77 or so)

# Now I must combine the into one file:
full_data <- rbind(large_noNA, small_noNA)

# And then run the t-test.  This is comparing Minutes Awake with Size
t.test(full_data$`Minutes Awake` ~ full_data$Size)

# And this one compares Number of Awakenings with Size
t.test(full_data$`Number of Awakenings` ~ full_data$Size)

# Lastly this one compares Minutes Asleep with mouthguard Size
t.test(full_data$`Minutes Asleep` ~ full_data$Size)

# This is fascinating and why I set about learning this project and R in the
# first place.  It seems the small mouth guard yields significantly fewer
# Minutes Awake and fewer Awakenings than the large mouth guard.  Minutes
# Asleep are not statistically different.

# Trying out ANOVA, comparing the days of the week and Minutes Awake

Lanova <- aov(large_noNA$`Minutes Awake` ~ large_noNA$Weekday)
summary(Lanova)

Sanova <- aov(small_noNA$`Minutes Awake` ~ small_noNA$Weekday)
summary(Sanova)

# Finishing off with one last piece of frequencies, since I want to
# understand nuance between the days of the week better.
# Gmodels lets you do CrossTable.  This run is on Weekday and Size:

install.packages("gmodels")
library("gmodels")

CrossTable(full_data$`Weekday`, full_data$'Size')
