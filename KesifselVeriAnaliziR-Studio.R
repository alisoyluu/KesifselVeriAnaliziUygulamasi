
######################################### VERİ ÜRETEREK KEŞİFSEL VERİ ANALİZİ #########################################
# Rastgele veri oluşturma için Mass kütüphanesini yükledim.
install.packages("MASS")
library(MASS)

# Veri setini oluşturma

set.seed(50) # Tekrarlanabilirlik için seed belirledim ki sonrasında ihtiyaç halinde, set.seed(50) kodu ile R bana yine aynı değerleri üretebilsin.
n <- 250 # Gözlem sayımız 250
# Normal dağılıma göre 250 gözlem sayısına sahip random olarak, ortalama ve standart sapmasını benim belirlediğim; 5 farklı bağımsız değişken için gözlemleri ürettim.

  ## Bağımsız değişken 1
  mean_1 <- 9
  sd_1 <- 2.2
  x1 <- mean_1 + sd_1 * rnorm(n)

  ## Bağımsız değişken 2
  mean_2 <- 15
  sd_2 <- 1.1
  x2 <- mean_2 + sd_2 * rnorm(n)

  ## Bağımsız değişken 3
  mean_3 <- 6
  sd_3 <- 1.5
  x3 <- mean_3 + sd_3 * rnorm(n)

  ## Bağımsız değişken 4
  mean_4 <- 17
  sd_4 <- 0.5
  x4 <- mean_4 + sd_4 * rnorm(n)

  ## Bağımsız değişken 5

  mean_5 <- 5
  sd_5 <- 2.7
  x5 <- mean_5 + sd_5 * rnorm(n)
  
# Bağımlı değişken oluşturdum.
y <- 6 + 2.5*x1 + 0.5*x2 - 3.5*x3 + 1.3*x4 - 2.4*x5 + rnorm(n, mean = 0, sd = 3) 

# Oluşturulan veri setini bir veri çerçevesine dönüştürdüm.
my_data <- data.frame(y, x1, x2, x3, x4, x5)

# Oluşturulan veri setinin ilk 6 satırını gözlemledim.
head(my_data)

# Oluşturulan veri setinde değişken türlerini gözlemledim.
str(my_data)

#Aktarım ve gerekli dönüşümleri yaptım.
# Örnek veri setini oluşturdum.
set.seed(50)
n <- 250
mean_1f <- 9
sd_1f <- 2.2
x1f <- mean_1f + sd_1f * rnorm(n)

mean_2f <- 15
sd_2f <- 1.1
x2f <- mean_2f + sd_2f * rnorm(n)

y_f <- 6 + 2.5*x1 + 0.5*x2 + rnorm(n, mean = 0, sd = 3)

# x1 değişkenini faktörel değişkene dönüştürme
x1f_factor <- as.factor(x1f)

# x2 değişkenini sayısal değişkene dönüştürme
x2f_numeric <- as.numeric(x2f)

# Değişken türlerini kontrol etme
str(x1f_factor)
str(x2f_numeric)

#eksik veri tespiti yapıldı, eksik veri yok kendimiz oluşturduğumuz için hazır data olsaydı bakardık bu şekilde.
any(is.na(my_data)) 

# Örnek veri setini oluşturdum.
set.seed(50)
n <- 250
mean_1ev <- 9
sd_1ev <- 2.2
x1_ev <- mean_1ev + sd_1ev * rnorm(n)

mean_2ev <- 15
sd_2ev <- 1.1
x2_ev<- mean_2ev + sd_2ev * rnorm(n)

y_ev <- 6 + 2.5*x1_ev + 0.5*x2_ev + rnorm(n, mean = 0, sd = 3)

# Eksik veri tespitini yaptım.
is_na_x1 <- any(is.na(x1_ev)) 
is_na_x2 <- any(is.na(x2_ev)) 

# Eksik veriler olsaydı ortalama ile doldurmayı bu şekilde yapardım.
mean_x1_ev <- mean(x1_ev, na.rm = TRUE)
x1_filled <- ifelse(is.na(x1_ev), mean_1, x1)

# Örnek vektör oluşturma
vec <- c(5, 15, 25, NA, 40, NA, 54, NA, 66, NA, 78, NA)

# Eksik değerleri önceki gözlemle doldururken for döngüsünü kullandım.
for (i in 2:length(vec)) {
  if (is.na(vec[i])) {
    vec[i] <- vec[i - 1]
  }
}

filled_vec <- vec

#aykırı değer tespiti
# Örnek veri setini oluşturdum.
set.seed(50)
my_data <- rnorm(250)

# Kutu grafiği oluşturdum. Aykırı derğerleri tespit ederken boxplot grafiği oldukça kullanışlıdır.
boxplot(my_data) 
sort(my_data)

# Z-puanı hesaplama
z_scores <- scale(my_data)
z_scores
sort(z_scores)

# Aykırı değerleri belirleme
outliers <- abs(z_scores) > 3
outliers

# Önce veri setinin özet istatistiklerini aldım. Sonra bu özet istatistik içinden Q1 ve Q3 değerlerini veren başlıkları seçerek, alt ve üst çeyrekliklerinin bilgisine ulaştım.
summary_stats <- summary(my_data)
Q1 <- summary_stats["1st Qu."]
Q3 <- summary_stats["3rd Qu."]

# Veri setime ait five number summary bilgisine ulaştım ve bu sayede sırasıyla (min değer, Q1, medyan, Q3, max değer) bilgilerini elde ettim. Bu bilgilerle alt ve üst sınırı hesapladım.
fivenums <- fivenum(my_data)
Q1 <- fivenums[2]
Q3 <- fivenums[4]
IQR <- Q3 - Q1
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Aykırı değerleri belirledim.
outliers <- !(my_data >= lower_bound & my_data <= upper_bound)
outliers

# Aykırı değerler hangileri yazdırdım.
outlier_values <- my_data[outliers]
print(outlier_values)

#dağılımları keşif
# Örnek veri setini oluşturma
set.seed(50)
my_data <- rnorm(250)

# Histogram oluşturma
hist(my_data)

# Kutu grafiği oluşturma. Bu grafikte gözlemlediğim aykırı değerlerin varlığını yukarıdaki kodlarla ortaya koymuştum.
boxplot(my_data) 

# Q-Q plot oluşturma
qqnorm(my_data)
qqline(my_data)

# Kantillerden yararlanma
summary(my_data)

# Örnek veri setini oluşturma
set.seed(50)
x <- rnorm(250) # Bağımsız değişken
y <- 6 + 2.5*x + rnorm(250, mean = 0, sd = 3) # Bağımlı değişken

# Korelasyon katsayısını hesaplama
correlation <- cor(x, y)

# Korelasyon katsayısını ekrana yazdırma
print(correlation)

#multicollinearity
# Örnek veri setini oluşturma
set.seed(50)
x1 <- rnorm(250) # Bağımsız değişken 1
x2 <- rnorm(250) # Bağımsız değişken 2
x3 <- rnorm(250) # Bağımsız değişken 3
y <- 6 + 2.5*x1 + 0.5*x2 - 3.5*x3 + rnorm(250, mean = 0, sd = 3) # Bağımlı değişken

# Çoklu doğrusal regresyon modelini oluşturma
model <- lm(y ~ x1 + x2 + x3)

# Model özetini alma
summary(model)

# Bağımsız değişkenler arasındaki korelasyonu hesaplama
correlation_matrix <- cor(data.frame(x1, x2, x3))
print(correlation_matrix)

# corrplot paketini yükleme
install.packages("corrplot")
library(corrplot)

# Korelasyon matrisini görselleştirme
corrplot(correlation_matrix, method = "color")

# Bağımsız değişkenlerin grafiğini çizme
par(mfrow=c(1,3)) # Grafiklerin yan yana yerleştirilmesi için ayar
plot(x1, y, main = "x1 vs. y", xlab = "x1", ylab = "y", col = "blue", pch = 16)
plot(x2, y, main = "x2 vs. y", xlab = "x2", ylab = "y", col = "red", pch = 16)
plot(x3, y, main = "x3 vs. y", xlab = "x3", ylab = "y", col = "green", pch = 16)

# Örnek veri setini oluşturma
set.seed(50)
x1 <- rnorm(250) # Bağımsız değişken 1
x2 <- rnorm(250) # Bağımsız değişken 2
x3 <- rnorm(250) # Bağımsız değişken 3
y <- 6 + 2.5*x1 + 0.5*x2 - 3.5*x3 + rnorm(250, mean = 0, sd = 3) # Bağımlı değişken

# Veriyi standartlaştırma
x1_standardized <- scale(x1)
x2_standardized <- scale(x2)
x3_standardized <- scale(x3)
y_standardized <- scale(y)

# Standartlaştırılmış veriyi kontrol etme
summary(x1_standardized)
summary(x2_standardized)
summary(x3_standardized)
summary(y_standardized)

# caTools paketini yükleme
install.packages("caTools")
library(caTools)

# Veri setini oluşturma (örnek veri)
set.seed(50)
x1 <- rnorm(250) # Bağımsız değişken 1
x2 <- rnorm(250) # Bağımsız değişken 2
x3 <- rnorm(250) # Bağımsız değişken 3
y <- 6 + 2.5*x1 + 0.5*x2 - 3.5*x3 + rnorm(250, mean = 0, sd = 3) # Bağımlı değişken

# Veriyi test ve eğitim alt kümelerine böleme
split <- sample.split(y, SplitRatio = 0.7) # 70% eğitim, 30% test
train_data <- subset(data.frame(x1, x2, x3, y), split == TRUE)
test_data <- subset(data.frame(x1, x2, x3, y), split == FALSE)

# Verinin boyutlarını kontrol etme
dim(train_data)
dim(test_data)


######################################### VERİ ÇEKEREK KEŞİFSEL VERİ ANALİZİ #########################################

#Verinin açık bir kaynaktan teminini yaptım.
install.packages("readxl")
library(readxl)

#Covid-19 oncesi gida fiyatlari.csv isimli veri setini seçerek import ettim. Bu veri seti 2013 ve 2019 yılları arasını kapsamaktadır.
covid_oncesi_fiyatlar <- read.csv("Covid-19 oncesi gida fiyatlari.csv") 

##Covid-19 sonrasi gida fiyatlari.csv isimli veri setini seçerek import ettim. Bu veri seti 2021 yılını kapsamaktadır.
covid_sonrasi_fiyatlar <- read.csv("Covid-19 sonrasi gida fiyatlari.csv") 

#Veri setine ait ilk 6 satıra göz attım.
head(covid_oncesi_fiyatlar) #verilere ait ilk 6 satıra göz attım.
head(covid_sonrasi_fiyatlar)

#Veri setlerindeki değişkenlerin türüne göz attım.
str(covid_oncesi_fiyatlar) 
str(covid_sonrasi_fiyatlar)

View(covid_oncesi_fiyatlar) 
View(covid_sonrasi_fiyatlar)

#Örneğin Month değişkenini numeric yapmak isteseydim bu şekilde yapabilirdim.
##covid_oncesi_fiyatlar$Month <- as.numeric(covid_oncesi_fiyatlar$Month) 

#Month değişkeni 1-12 arasında değer alabilir. Çünkü yılın 12 ayı var. Bu değişkene aralık dışında derğer verilip verilmediğini kontrol ediyorum. Eğer aralık dışında veri girişi yapıldıysa bu yanlış hangi satırda yapılmış onu bana gösterecek.Örneğin hata 5. satırda çıksın.
##which(!(covid_oncesi_fiyatlar$Month >= 1 & erkek_giyim$hourspw <= 12 )) 
#Hatalı veri girişinin 5. satırda olduğunu varsaymıştık. Month değişkeni de 6. sütundaydı. Bu değere doğru veri olan 6'yı bu şekilde atarım.
##covid_oncesi_fiyatlar[5,6] <- 6  

#Covid-19 öncesi ve sonrası gıda fiyatlarına ait iki ayrı veri seti vardı. Bunları birleştirdim.
all_dataset <- rbind(covid_oncesi_fiyatlar, covid_sonrasi_fiyatlar) 

#(Betimleyici İstatistiklerin incelenmesi sonucu ilk gözlemlerim) Fiyat değişkeni bazında 2013-2021 yılları arasında en küçük değer 0.2539, en büyük değer 166.9046'dır.
summary(all_dataset) 
str(all_dataset)

#(Veri temizleme, boş değerleri kontrol etme) sütunlardaki eksik değer var mı diye baktım hiç yok.
colSums(is.na(all_dataset)) 

#Üstteki kodun bir alternatifidir.na değer var ya da yok bilgisini verir.
any(is.na(all_dataset)) 

#NA kayıp gözlem sayısını verir direkt.
sum(is.na(all_dataset)) 

#(Veri temizleme, boş değerleri kontrol etme) yinelenen satır sayısına baktım hiç yok.
sum(duplicated(all_dataset)) 

#Değişkenler, türleri, na gözlem sayısı, değişkenlere verilen eşsiz değer sayısı gibi bilgilerin çıktısını verir.
install.packages("funModeling")
library(funModeling)
df_all_dataset <- data.frame(all_dataset)
df_status(df_all_dataset)  

#verisetindeki toplam satır ve sütun sayısına baktım.
dim(all_dataset)

#ProductName değişkenine ait sütundaki benzersiz değerleri ve bu değerlerin toplamda kaç kez tekrarlandığını gösterir.
table(all_dataset$ProductName) 

install.packages("expss")
library(expss)

# Place değişkenine ait benzersiz değerleri ve bu değerlerin verisetinde toplam kaç kez tekrarlandığını ve en önemlisi NA(kayıp gözlem) sayısını kontrol ettiğim ayrı bir kod.
fre(all_dataset$Place) 

#Örneğin Ankara yerine Ank şeklinde yazılmış veriler tespit ettim. Bu fonksiyon bana "Ank" şeklinde yanlış girilen veri/verilerin hangi x ekseninde yer aldığını belirtir. Sonrasında kolayca doğru değeri atayabilirim. 
which(all_dataset$place == "Ank") 

#Üstteki koddan 5,18 ve 55'nci satırlarda olduğunu bulduğumu varsayalım. Ayrıca Place değişkeni de 1. sütunda idi.  Bu kod ile "Ank" leri "Ankara" yaptım.
all_dataset[c(5,18,55),1] <- "Ankara" 

install.packages("plotly")
library(plotly)

#Ürünlerin tekrar sayısını hesapladım.
product_counts <- sort(table(all_dataset$ProductName))
product_counts

#Her bir ürünün tekrar sayısını gösteren bir çubuk grafik oluşturdum. Kırmızı elma, ekmek, bezelye,yeşilçay ve vasıfsız işgücünün maaşlarına ait gözlem sayısı diğerlerine oranla çok daha azdır. 
plot_ly(x = names(product_counts), y = product_counts, type = "bar") %>%
  layout(title = "Ürün Dağılımı", xaxis = list(title = "Ürün"), yaxis = list(title = "Sayı"))

#Place sütunundaki değerlerin sayısını aldım.
place <- sort(table(all_dataset$Place), decreasing = FALSE) 

#Place sütunundaki her bir yere ait toplam gözlem sayısını sayısını gösteren bir çubuk grafik oluşturdum.
plot_ly(x = names(place), y = place, type = "bar") %>%
  layout(title = "Yer Dağılımı", xaxis = list(title = "Yer"), yaxis = list(title = "Sayı"))  

#Year sütunundaki her bir yılın tekrar sayısını gösteren bir çubuk grafik oluşturdum. 2018 ve 2019 da data sayısı diğerlerine oranla çok daha fazla.
year_counts <- table(all_dataset$Year) 
plot_ly(x = names(year_counts), y = year_counts, type = "bar") %>%
  layout(title = "Yıl Dağılımı", xaxis = list(title = "Yıl"), yaxis = list(title = "Sayı")) 

install.packages("corrplot")
library(corrplot)

install.packages("dplyr")
library(dplyr)

#Korelasyon hesaplanabilmesi için korelasyon verilerinin numerik olması şarttır. Veri setindeki yalnızca numerik veri içeren sütunları seçtim.
numerik_sutunlar <- all_dataset %>% select_if(is.numeric) 
korelasyon <- cor(numerik_sutunlar)
corrplot(korelasyon, method = "color") #korelasyon matrisini görselleştirdim.

#Analize fayda sağlamadığını düşündüğüm productId ve UmId sütunlarını içermeyen yeni bir veri seti oluşturdum.
df1 <- subset(all_dataset, select = -c(ProductId, UmId)) 
df1

install.packages("caret")
library(caret)
library(ggplot2)
library(lattice)

#Place,ProductName ve umName sütunlarını dummy değişkenlere dönüştürdüm.
dummy_data <- dummyVars(~ Place + ProductName + UmName, data = df1)
df2 <- data.frame(predict(dummy_data, newdata = df1)) 

#Bu yeni veri seti df2 nin dummy değişkenlerini ve all_dataset'in month,year ve price sütunlarını içerir. Bunların birleşimidir.
df3 <- cbind(df2, all_dataset[, c("Month", "Year", "Price")])  
df3

#Price sütununu bağımlı değişken olarak atadım.
y <- df3$Price 

#Price sütunu haricindekileri bağımsız değişkenler olarak atadım. Bu işlemler sonucu veri seti analize hazır hale geldi.
x <- subset(df3, select = -c(Price)) 

# Bu fonksiyon ile, rastgele sayı üretimindeki tekrarlanabilirliği sağlamayı amaçladım.
set.seed(42) 

#Eğitim ve test kümelerini oluşturdum.Bu kod ile, veri setini bağımsız değişkenler (x) ve bağımlı değişken (y) olarak ayırdım ve ardından belirli bir oranda eğitim ve test kümelerine böldüm. Eğitim seti %80'ini ve test seti %20'sini içerecek şekilde bölme işlemi gerçekleştirdim.
index <- createDataPartition(y, times = 1, p = 0.8, list = FALSE)  
x_train <- x[index, ]
x_test <- x[-index, ]
y_train <- y[index]
y_test <- y[-index]

## Train ve test ölçeklendirilmesi
preproc <- preProcess(x_train, method = c("range")) 
x_train_scaled <- predict(preproc, x_train)
x_test_scaled <- predict(preproc, x_test)

#Veri boyutlarını kontrol ettim.
dim(x_train) 
dim(x_test)



















