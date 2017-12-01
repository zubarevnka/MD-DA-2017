#Загрузите данные о землятресениях
anss <- readLines("https://raw.githubusercontent.com/SergeyMirvoda/MD-DA-2017/master/data/earthquakes_2011.html", warn=FALSE)

#Выберите строки, которые содержат данные с помощью регулярных выражений и функции grep
expstr <- "\\d{4}\\/\\d{2}\\/\\d{2} \\d{2}:\\d{2}:\\d{2}.\\d{2},\\W?\\d*.\\d{4},\\W?\\d*.\\d{4},\\d+.\\d{2},\\d+.\\d{2},\\w*,\\d+,\\d*,\\d*,\\d*.\\d{2},\\w+,\\d*"
data <- grep(expstr, anss)

#Проверьте, что все строки (all.equal) в результирующем векторе подходят под шаблон.
pre <- grep("PRE", anss)
all.equal(length(data), pre[2] - pre[1] - 2)
