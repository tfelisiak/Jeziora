# Jeziora
Celem projektu jest analiza danych, ocena ich zgodności z rozkładem normalnym, eksploracja statystyczna oraz budowa modeli predykcyjnych. Poniżej przedstawiam opis wszystkich kroków wykonanych w kodzie.

1. Przygotowanie danych 
• Biblioteki: Załadowano szereg bibliotek (m.in. dplyr, tidyverse, factoextra), potrzebnych 
do analizy danych i wizualizacji. 
• Usuwanie braków danych: Dane zostały oczyszczone za pomocą funkcji na.omit, 
eliminując brakujące wartości. 
• Obliczenie ITSph: Stworzono zmienną ITSph na podstawie istniejących danych (ph i 
nas).

2. Eksploracja danych 
2.1 Wizualizacja danych (Histogramy) 
Dla każdej zmiennej (np. ph, chla, temp, bar, nas, przew, namon) utworzono histogramy, aby 
zweryfikować rozkład zmiennych.

![image](https://github.com/user-attachments/assets/9548ffe4-7df1-4cfc-ae0e-c3698615077f)

2.2 Test Shapiro-Wilka 
Test Shapiro-Wilka został użyty do sprawdzenia zgodności zmiennych z rozkładem 
normalnym. Wyniki zapisano w tabeli wyniki_testow. 
• Interpretacja wyników: 
o 0: Zmienna zgodna z rozkładem normalnym. 
o 1: Zmienna niezgodna z rozkładem normalnym. 
Histogram gęstości:

![image](https://github.com/user-attachments/assets/d9487a58-247b-4fc0-99cb-b05f131f49d2)

2.3 Szczegółowe statystyki 
Za pomocą funkcji describe() (z pakietu psych) obliczono szczegółowe statystyki dla wszystkich 
zmiennych. 
2.4 Boxploty 
Utworzono wykresy pudełkowe (boxplot) dla każdej zmiennej, aby zidentyfikować 
ewentualne obserwacje odstające.

![image](https://github.com/user-attachments/assets/f113188f-e2cf-4d2b-b824-011c2d9703a5)

4. Testy statystyczne 
3.1 Test kurtozy 
Za pomocą funkcji kurtosis (z pakietu e1071) obliczono kurtozę dla każdej zmiennej. Interpretacja: 
• Leptokurtyczny: Kurtoza > 3 
• Platykurtyczny: Kurtoza < 3 
• Normalny: Kurtoza = 3

![image](https://github.com/user-attachments/assets/4865ef9a-56e7-4ca5-a2db-3a82fa83131f)

3.2 Test Kołmogorowa-Smirnowa 
Przeprowadzono test Kołmogorowa-Smirnowa dla każdej zmiennej, aby sprawdzić zgodność 
z rozkładem normalnym. Wyniki zapisano w tabeli.

![image](https://github.com/user-attachments/assets/da33a4ff-9fc9-4be4-bbc0-b1062fef2e70)

5. Budowa modeli predykcyjnych 
4.1 Permutacje zmiennych 
Użyto funkcji COMBN(z pakietu COMBINAT), aby wygenerować wszystkie możliwe kombinacje 
(2^8 = 256) zmiennych dla modeli regresyjnych. 
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
4.2 Modele regresyjne 
Utworzono 256 modeli regresji liniowej z różnych kombinacji zmiennych objaśniających. Dla 
każdego modelu: 
• Obliczono przewidywania (predict()). 
• Zapisano wyniki w macierzy przewidywania_mat (rozmiar 51 x 256). 
Wyniki wizualizowano za pomocą: 
• Heatmapy: Pokazuje wartości przewidywań dla każdego modelu.

![image](https://github.com/user-attachments/assets/7911c7a7-e901-4dea-a3a5-b76b08e4324e)

• Tile Plot: Graficzna reprezentacja wyników z funkcji ggplot2.

![image](https://github.com/user-attachments/assets/dc121f9c-75ce-499b-8b59-5889c60690c2)

4.3 Ocena modeli 
• Histogram różnic: Obliczono różnice między rzeczywistymi wartościami a 
przewidywaniami. 
• MSE (Mean Squared Error): Obliczono dla każdego modelu w celu wyboru 
najlepszego. 
6. Wizualizacja danych 
• Histogramy gęstości: Użyto funkcji ggplot2 do przedstawienia dopasowania zmiennych 
do rozkładu normalnego.

![image](https://github.com/user-attachments/assets/5f0b431d-e1c4-4bf8-a97b-8f90d25b038d)

Wykresy porównawcze: Dla modelu ph i predykcji stworzono wykres rzeczywistych 
vs przewidywanych wartości.

![image](https://github.com/user-attachments/assets/beef3a13-3790-4934-8caf-e3b984004b43)

