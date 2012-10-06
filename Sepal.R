

   # Sepal.Length        subset(airquality, Temp > 80, select = c(Ozone, Temp))

results <- list(subset(iris, Species =="virginica", c( Sepal.Length )  ))
str(results)

mean <- lapply(results, FUN=colMeans)