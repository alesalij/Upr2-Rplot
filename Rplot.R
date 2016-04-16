install.packages('data.table')
install.packages('lattice')
install.packages('RCurl')
install.packages('XML')
library('RCurl')
library('XML') 
library('data.table')         
library('lattice')  

# создаѐм директорию для данных, если она ещѐ не существует:
if (!file.exists('./data')) {
  dir.create('./data')
}
# создаѐм файл с логом загрузок, если он ещѐ не существует:
if (!file.exists('./data/download.log')) {
  file.create('./data/download.log')
}


# адрес файла
fileURL <- 'https://raw.githubusercontent.com/aksyuk/R-data/master/COMTRADE/040510-Imp-RF-comtrade.csv'
# загружаем файл, если он ещѐ не существует,
# и делаем запись о загрузке в лог:
if (!file.exists('./data/040510-Imp-RF-comtrade.csv')) {
  download.file(fileURL,
                './data/040510-Imp-RF-comtrade.csv')
  # сделать запись в лог
  write(paste('Файл "040510-Imp-RF-comtrade.csv" загружен',
              Sys.time()),
        file = './data/download.log', append = T)
}

# читаем данные из загруженного .csv во фрейм,
# если он ещѐ не существует
if (!exists('DT')){
  DT <- data.table(read.csv('./data/040510-Imp-RF-comtrade.csv', as.is = T))
}
# предварительный просмотр
#dim(DT) # размерность таблицы
#str(DT) # структура (характеристики столбцов)
#head(DT) # первые несколько строк таблицы




# сколько NA в каждом из оставшихся столбцов?
#na.num <- apply(DT, 2, function(x) length(which(is.na(x))))
# выводим только положительные и по убыванию
#sort(na.num[na.num > 0], decreasing = T)


# явное преобразование типа, чтобы избежать проблем
# при заполнении пропусков
DT[, Netweight.kg:=as.double(Netweight.kg)]
# считаем медианы и округляем до целого, как исходные данные
DT[, round(median(.SD$Netweight.kg, na.rm = T), 0), by = Year]

# заменяем пропуски на медианы
DT[, Netweight.kg.median:=round(median(.SD$Netweight.kg,na.rm = T), 0),by = Year]

# смотрим результат
#DT[is.na(Netweight.kg), Year, Netweight.kg.median]

# заменяем пропуски на медианы
for(i in seq(1,length(DT[[1]]))){
  if(!is.na(DT$Netweight.kg[i])){
    DT$Netweight.kg.median[i]<-DT$Netweight.kg[i]
  }
}
png(filename = "./data/Rplot.png")
# простой график разброса
xyplot(Netweight.kg.median ~ Trade.Value.USD | as.factor(Year), data = DT,
       ylab = 'Масса поставки',
       xlab = 'Стоимость поставки',
       main = 'График разброса стоимости поставки от массы поставки',
       panel = function(x, y, ...) {
         # вызов функции по умолчанию (график разброса)
         panel.xyplot(x, y, ...)
         # затем накладываем линии регрессии
         panel.lmline(x, y, col = 'red')
       })
dev.off()
