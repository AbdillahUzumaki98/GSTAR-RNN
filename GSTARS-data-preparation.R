library(tidyverse)
library(ggplot2)

data <- read.csv(file="../daftar_harga_pasar_jogja.csv", header =T)
View(data)

#ganti 'None' dengan NA
data$harga[data$harga == "None"] <- NA
View(data)
data[(is.na(data$harga)),]

#cek tipe data variabel
glimpse(data)
class(data$nama_pasar)
unique(data$nama_pasar)
unique(data$komoditas)

#ganti tipe data sesuai keperluan
data$nama_pasar <- as.factor(data$nama_pasar)
class(data$nama_pasar)
levels(data$nama_pasar)

data$komoditas <- as.factor(data$komoditas)

data$harga <- as.integer(data$harga)

data$waktu <- ymd(data$waktu)

glimpse(data)

#mengisi missing value
any(is.na(data))
data1 <- data %>% fill(harga, .direction = 'up')
data2 <- data1 %>% fill(nama_pasar, .direction = 'down')
data3 <- data2 %>% fill(komoditas, .direction = 'down')
data4 <- data3 %>% fill(harga, .direction = 'down')
View(data4)
glimpse(data4)
any(is.na(data4))

ggplot(data=data4)+
  geom_line(mapping = aes(data4$waktu,data4$harga, color=data4$komoditas))

write.csv(data4, "D:\\Safril\\data.csv", row.names=FALSE)
is.ts(data4)
  
