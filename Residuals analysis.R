# Importing data:

library(readxl)
data = read_excel("portfolio_data.xlsx", 
                             col_types = c("date", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                          "numeric", "numeric"))

# Time series:

ts = ts(data[ , 'T_0'],
        frequency = 52)  #Trimestral (4 trimester in a year), for instance 12 is monthly
plot(data$Time, ts,
     type = 'l',
     main = 'Portfolio returns',
     ylab = 'Returns')  

# Autocorellation analysis:

plot(x, y,
     main = 'Autocorrellation',
     xlab = 'Returns at t-1',
     ylab = 'Returns at t')

library(forecast)
Acf(data[ , 'T_0'], # Function of autocorrelation
    main = 'Autocorrelation plot',
    xlab = 'Lag',
    ylab = 'Coefficients')

# Fittinf a simple model with lag_1:

n = length(data$T_0)
x = data$T_0[1:n-1]
y = data$T_0[2:n]
regressor = lm(y ~ x)
summary(regressor)

plot(x, y,
     main = 'Yields ',
     xlab = 'Y at t-1',
     ylab = 'Y at t')
abline(regressor$coefficients,
       col = 'red',
       lwd = 2)

# White test:

res = regressor$residuals

x2 = x^2 
regressor2 = lm(res^2 ~ x + x2)
r2 = summary(regressor2)$r.squared

1-pchisq(n*r2, 2) # There is omoskedasticity

res = regressor$residuals

x2 = x^2 
regressor2 = lm(res^2 ~ x + x2)
r2 = summary(regressor2)$r.squared

1-pchisq(n*r2, 2) # The p-value is 0 so We reject the Hp of omoskedasticity


# Analysis of residuals:

residuals = ts(regressor$residuals,
               frequency = 52)
plot(residuals, # It seems there is a trend and also some oulayers
     type = 'p',
     col = 'blue',
     ylab = 'Residuals',
     xlab = 'Time')
abline(h = 0,
       col = 'black')
abline(h = 1.96*summary(regressor)$sigma,
       lty = 2,
       col = 'green')
abline(h = -1.96*summary(regressor)$sigma,
       lty = 2,
       col = 'red')

library(lmtest) # H0 is there isn't autocorrelation between residuals
dwtest(regressor)

library(lmtest) # We can use the package to do directly the test
bgtest(regressor, 
       order = 1,
       type = 'Chisq',
       fill = NA)

