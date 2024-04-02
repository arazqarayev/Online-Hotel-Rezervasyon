# Online-Hotel-Rezervasyon
Modelimiz, bağımsız değişkenler olarak online kanalların kullanımını kullanarak rezervasyonun iptal olup olmadığını tahmin eder. Bu tahminler, test setinde gerçekleştirilen tahminlerle karşılaştırılarak modelinizin performansı değerlendirilir. Sonuçlar, karmaşıklık matrisi gibi metrikler kullanılarak analiz edilir ve modelinizin ne kadar iyi çalıştığına dair bir fikir edinilir. Bu şekilde, hangi online kanalların iptal olma olasılığını artırdığı veya azalttığı gibi önemli bilgiler elde edilebilir. 




# İlk olarak, karmaşıklık matrisini oluşturmak için gerekli kodları çalıştırın
# Veri setinizden sadece belirli sütunları seçin ve eksik verileri görselleştirin
a <- aa[c(1, 2, 4, 15, 16)] # a değişkeni, karmaşıklık matrisi oluşturmadan önce kullanmak üzere veri setinizin belirli sütunlarını içerir.
library(DataExplorer)
plot_missing(a) # Veri setinizdeki eksik değerleri görselleştirin. Eksik değer yok.

# Verinin numerik olup olmadığını kontrol edin
is.numeric(a$is_canceled)

# Veri tipini faktör olarak değiştirin ve tekrar kontrol edin
a$is_canceled <- factor(a$is_canceled, levels = c(0, 1))
is.factor(a$is_canceled)

# Oluşturduğunuz veriyi CSV dosyası olarak kaydedin
write.csv(a, "ahotel.csv", row.names = FALSE)

# Şimdi, CSV dosyasını okuyun ve veriyi eğitim ve test setlerine bölmek için gerekli adımları izleyin
setwd('C:\\Users\\TOSHIBA\\Desktop') # Dosyanızın bulunduğu dizine gidin
aa <- read.csv('ahotel.csv') # CSV dosyasını okuyun
library(caTools)
set.seed(123)
split <- sample.split(aa$is_canceled, SplitRatio = 0.75) # Veriyi eğitim ve test setlerine bölmek için rastgele bir bölme oluşturun
training_set <- subset(aa, split == TRUE) # Eğitim setini oluşturun
test_set <- subset(aa, split == FALSE) # Test setini oluşturun

# Lojistik regresyon modelini eğitin
classifier <- glm(formula = is_canceled ~ ., # Modelinizi eğitmek için lojistik regresyon kullanın
                  family = binomial,
                  data = training_set)

# Test seti üzerinde tahmin yapın ve karmaşıklık matrisini oluşturun
prob_pred <- predict(classifier, type = 'response', newdata = test_set[-2]) # Test seti üzerinde tahmin yapın
y_pred <- test_set$is_canceled
cm <- table(y_pred, g) # Karmaşıklık matrisini oluşturun

# Karmaşıklık matrisini görüntüleyin
cm :   
1)y_pred
g       0     1
  0 18792     0
  1     0 11056
2) 18792+11056
[1] 29848
3) 29848-28848
[1] 1000


# Doğru tahmin edilen örnek sayısını hesaplayın
correct_predictions <- sum(diag(cm))

# Yanlış tahmin edilen örnek sayısını hesaplayın
wrong_predictions <- sum(cm) - correct_predictions

# Toplam örnek sayısını hesaplayın
total_samples <- sum(cm)

# Doğru tahmin oranını hesaplayın
accuracy <- correct_predictions / total_samples
