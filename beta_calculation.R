# Importing data:

library(readxl)
data = read_excel('data.set.excess.xlsx')

# Fitting the regressions:

regressor = lm(as.matrix(data[1:50]) ~ data$X__1)
summary(regressor)
regressor$coefficients

