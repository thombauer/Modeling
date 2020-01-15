# Damit setzen wir das Working Directory auf den Ordner dieser Datei
this.dir <- dirname(parent.frame(2)$ofile)
setwd(this.dir)

# Benoetigte Libraries laden
library(ggplot2)
library(scales)
library(stats)
library(data.table)
library(caret)
library(miscTools)

# Daten einlesen 
hotels <- fread("hotels.csv")

splits <- createMultiFolds(hotels$`Preis in Mio`, k = 5, times = 10)

scores <- numeric()
for (split in splits) {
  train <- hotels[split, ]
  test <- hotels[-split, ]
  
  model <- lm(`Preis in Mio` ~ Gewinn + Quadratmeter, data = train)
  r2 <- rSquared(test$`Preis in Mio`, test$`Preis in Mio` - predict(model, test))
  scores <- c(scores, r2)
}

print(mean(scores))
