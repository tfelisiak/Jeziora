library(dplyr)
library(tidyverse)
library(readr)
library(factoextra)
library(corrplot)

d <- na.omit(d)

#ITS ph
d$ITSph=d$ph+0.013*(100-d$nas)

par(mfrow = c(2, 4))

#HISTOGRAMY
hist(d$przew, main = "Histogram - Przewodność", xlab = "Wartości przewodności", col = "red")
hist(d$ph, main = "Histogram - pH", xlab = "Wartości pH", col = "blue")
hist(d$bar, main = "Histogram - Ciśnienie", xlab = "Wartości ciśnienia (bar)", col = "yellow")
hist(d$nas, main = "Histogram - NAS", xlab = "Wartości NAS", col = "coral")
hist(d$chla, main = "Histogram - Chlorofil", xlab = "Wartości chlorofilu (chla)", col = "purple")
hist(d$namon, main = "Histogram - Amoniak", xlab = "Wartości amoniaku (namon)", col = "green")
hist(d$temp, main = "Histogram - Temperatura", xlab = "Wartości temperatury", col = "pink")
kolumny_do_testu <- c("chla", "temp", "bar", "nas", "przew", "ph", "namon")

for(kolumna in kolumny_do_testu) {
  test <- shapiro.test(d[[kolumna]])
  hipoteza <- ifelse(test$p.value > 0.05, 0, 1)
  wyniki_testow <- rbind(wyniki_testow, data.frame(Kolumna = kolumna, Wartość_p = test$p.value, Hipoteza = hipoteza))
}


print(wyniki_testow)

#chla - niezgodny z rozkładem normalny, 1
#temp - zgodny z rozkładem normalnym, 0
#bar - niezgodny z rozkładem normalny, 1 
#nas - niezgodny z rozkładem normalny, 1
#przew - niezgodny z rozkładem normalny, 1 
#ph- zgodny z rozkładem normalny, 0
#namon - niezgodny z rozkładem normalny, 1

summary(d)

install.packages("psych")
library(psych)

describe(d)

# BOXPLOTY
par(mfrow = c(2, 4))

boxplot(d$ph, main = "Boxplot - pH", ylab = "Wartości pH", col = "red")
boxplot(d$chla, main = "Boxplot - Chlorofil (chla)", ylab = "Wartości chlorofilu", col = "blue")
boxplot(d$namon, main = "Boxplot - Amoniak (namon)", ylab = "Wartości amoniaku", col = "purple")
boxplot(d$temp, main = "Boxplot - Temperatura", ylab = "Wartości temperatury", col = "green")
boxplot(d$nas, main = "Boxplot - NAS", ylab = "Wartości NAS", col = "yellow")
boxplot(d$bar, main = "Boxplot - Ciśnienie", ylab = "Wartości ciśnienia", col = "pink")
boxplot(d$przew, main = "Boxplot - Przewodność", ylab = "Wartości przewodności", col = "coral")

wyniki_kurtozy <- data.frame(Kolumna = character(0), Kurtoza = numeric(0), Interpretacja = character(0), stringsAsFactors = FALSE)


# TEST KURTOZA

install.packages("e1071")
library(e1071)

kolumny_do_testu <- c("chla", "temp", "bar", "nas", "przew", "ph", "namon")

for(kolumna in kolumny_do_testu) {
  kurtoza <- kurtosis(d[[kolumna]])
  interpretacja <- ifelse(kurtoza > 3, "Leptokurtyczny", ifelse(kurtoza < 3, "Platykurtyczny", "Normalny"))
  wyniki_kurtozy <- rbind(wyniki_kurtozy, data.frame(Kolumna = kolumna, Kurtoza = kurtoza, Interpretacja = interpretacja))
}

print(wyniki_kurtozy)

install.packages("gridExtra")
install.packages("grid")

# Załaduj pakiety
library(gridExtra)
library(grid)
getwd()
# Tworzymy plik graficzny
png("wyniki_ks.png", width = 1200, height = 800)

# Tworzymy tabelę z ramki danych wyniki_kurtozy i zapisujemy ją jako obraz
grid.table(wyniki_ks_test)

# Kończymy zapis
dev.off()
#TEST KOŁMOGOROWA - SMIRNOWA 

wyniki_ks_test <- data.frame(Kolumna = character(0), Wartość_p = numeric(0), Interpretacja = character(0), stringsAsFactors = FALSE)

kolumny_do_testu <- c("chla", "temp", "bar", "nas", "przew", "ph", "namon")

for(kolumna in kolumny_do_testu) {
  sample_data <- unique(sample(d[[kolumna]], 51, replace = FALSE))
  ks_test <- ks.test(sample_data, "pnorm", mean(sample_data), sd(sample_data))
  interpretacja <- ifelse(ks_test$p.value < 0.05, "Niezgodny z normalnym", "Zgodny z normalnym")
  wyniki_ks_test <- rbind(wyniki_ks_test, data.frame(Kolumna = kolumna, Wartość_p = ks_test$p.value, Interpretacja = interpretacja))
}

print(wyniki_ks_test)

#HISTOGRAM GĘSTOŚCI
library(ggplot2)
library(tidyr)

d_long <- d %>%
  gather(key = "variable", value = "value")

ggplot(d_long, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "purple") +
  stat_function(fun = dnorm, 
                args = list(mean = mean(d_long$value, na.rm = TRUE), 
                            sd = sd(d_long$value, na.rm = TRUE)), 
                color = "green", size = 1) +
  facet_wrap(~ variable, scales = "free") 


#8 zmiennych

install.packages("combinat")
library(combinat)

kombinacje <- list()

for (i in 0:8) {
  kombinacje[[i + 1]] <- combn(d, i, simplify = FALSE)
}

kombinacje

kombinacje_counts <- numeric(9)

for (i in 0:8) {
  kombinacje_counts[i + 1] <- length(combn(d, i, simplify = FALSE))
}

kombinacje_counts[1] <- 1

kombinacje_counts

install.packages("gtools")
library(gtools)

x <- c(0,1)

permutations(2, 8, x,repeats.allowed = TRUE)


mnoznik=data.frame(permutations(2, 8, x,repeats.allowed = TRUE)
)
model=lm(ITSph~.,d)

predict(model)
model2 <- predict(model)

#model=lm((ITSph~mnoznik[i,1]*chla+mnoznik[i,2]*temp+mnoznik[i,3]*bar+mnoznik[i,4]*nas+mnoznik[i,5]*przew+mnoznik[i,6]*ph+mnoznik[i,7]*namon), data=d)
MODELD <- data.frame(model2)

plot(model2)
points(d$ITSph, col = "orange")


model1=lm(ITSph~., data = d[,c(1,
                            mnoznik[i,1]*1,
                            mnoznik[i,2]*2,
                            mnoznik[i,3]*3,
                            mnoznik[i,4]*4,
                            mnoznik[i,5]*5,
                            mnoznik[i,6]*6,
                            mnoznik[i,7]*7,
                            8)])
predict(model1)


wyniki <- data.frame(i = 1:51)

# 256 modeli, 51 przewidywań
przewidywania_mat <- matrix(nrow = 51, ncol = 256)  # Macierz 51 x 256

for (j in 1:256) {
  model1 <- lm(ITSph ~ ., d[, c(mnoznik[j, 1] * 1,
                               mnoznik[j, 2] * 2,
                               mnoznik[j, 3] * 3,
                               mnoznik[j, 4] * 4,
                               mnoznik[j, 5] * 5,
                               mnoznik[j, 6] * 6,
                               mnoznik[j, 7] * 7,
                               8)])
  przewidywania_mat[, j] <- predict(model1)
}

wyniki <- cbind(wyniki, przewidywania_mat)
n
colnames(wyniki) <- c("i", paste0("pred_", 1:256))

print(head(wyniki))

wyniki_d <- wyniki[,-1]

heatmap(as.matrix(wyniki_d))

library(tidyr)
colnames(wyniki)

nowe_dane_long <- pivot_longer(wyniki, 
                               cols = -i,  # Weź wszystkie kolumny poza pierwszą (indeksem i) do przekształcenia
                               names_to = "pred", 
                               values_to = "value")

ggplot(nowe_dane_long, aes(x = pred, y = i, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "purple", high = "green", mid = "white", midpoint = median(nowe_dane_long$value, na.rm = TRUE)) +
  labs(title = "Tile Plot z wyników nowe_dane",
       x = "Przewidywania (pred_1 do pred_256)",
       y = "Indeks (i)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)

install.packages("rpart")
library(rpart)

model_drzewo <- rpart(ITSph ~ ., data = d, method = "anova")
o
install.packages("rpart.plot")
library(rpart.plot)
rpart.plot(model_drzewo, main = "Drzewo Decyzyjne dla ITSph")

predykcje_drzewo <- predict(model_drzewo)
predykcje_drzewo <- as.data.frame(predykcje_drzewo)

plot(d$ITSph, predykcje_drzewo, main = "Rzeczywiste vs. Predykowane", xlab = "Rzeczywiste ITSph", ylab = "Predykowane ITSph", col = "blue", pch = 16,
abline(0, 1, col = "red", lwd = 2)


liczba_modeli <- 256  # Liczba kombinacji (2^8)
przewidywania_mat <- matrix(nrow = nrow(d), ncol = liczba_modeli)  # Macierz 51 x 256
e
for (j in 1:liczba_modeli) {
  wybrane_kolumny <- mnoznik[j, ] == 1
  kolumny_modelu <- c(which(wybrane_kolumny), 8) 
  
  if (length(kolumny_modelu) > 1) {
    model <- lm(ITSph ~ ., data = d[, kolumny_modelu, drop = FALSE])
    
    przewidywania_mat[, j] <- predict(model)
  } else {
    przewidywania_mat[, j] <- NA
  }
}

wyniki <- as.data.frame(przewidywania_mat)
colnames(wyniki) <- paste0("Model_", 1:liczba_modeli)

różnice <- wyniki - d$ITSph

różnice_wektor <- unlist(różnice, use.names = FALSE)

hist(różnice_wektor, 
     main = "Histogram różnic", 
     xlab = "Różnica (Predykcja - Rzeczywiste)", 
     col = "cyan",
     breaks = 50)


rzeczywiste <- d$ITSph

przewidywane <- predictions_df[, 7]

if (length(rzeczywiste) != length(przewidywane)) {
  rzeczywiste <- rzeczywiste[!is.na(przewidywane)]
  przewidywane <- przewidywane[!is.na(przewidywane)]
}

plot(rzeczywiste, type = "b", col = "red", pch = 16, lty = 1, lwd = 2,
     xlab = "Numer obserwacji", ylab = "Wartość ITSph", 
     main = "Porównanie Obserwacji i Predykcji ITSph (Model 1)")
lines(przewidywane, type = "b", col = "blue", pch = 17, lty = 2, lwd = 2)

# Legenda
legend("topright", legend = c("Rzeczywiste", "Przewidywane"), 
       col = c("red", "blue"), pch = c(16, 17), lty = c(1, 2), lwd = 2)

