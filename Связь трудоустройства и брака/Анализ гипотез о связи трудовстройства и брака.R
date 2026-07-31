
library(haven)

path = file.path("D:/", "ВУЗ/R/дз3", "данные2020_индивиды.sav")
dataset = read_sav(path)

# Создаем новый data.frame только с нужными переменными
selected_data <- data.frame(
  indiv = dataset$idind,       # уникальный номер индивида
  age = dataset$y_age,       # возраст
  sex = dataset$yh5,           # пол
  fam = dataset$yj322,       # семейное положение 
  job = dataset$yj1            # занятость 
)

# Оставляем индивидов от 25 до 45 лет и убираем NA из переменной age
filtered_data <- subset(selected_data, age >= 25 & age <= 45 & !is.na(age))


#Расчёт долей 
# Расчет количества людей в браке (fam = 2, 3 или 6)
married <- filtered_data[filtered_data$fam %in% c(2, 3, 6), ]

# Подсчет общего числа людей в браке
total_married <- nrow(married)

# Подсчет числа трудоустроенных людей в браке (job = 1)
employed_married <- sum(married$job == 1)

# Расчет доли трудоустроенных среди людей в браке (p1)
p1 <- employed_married / total_married


#Расчет количества людей не в браке (fam = 1, 4 или 5)
not_married <- filtered_data[filtered_data$fam %in% c(1, 4, 5), ]
total_not_married <- nrow(not_married)

#Расчет числа трудоустроенных людей не в браке 
employed_not_married <- sum(not_married$job == 1)

# Расчет доли трудоустроенных среди людей не в браке (p1)
p2 <- employed_not_married/ total_not_married

#Построение доверительных интервалов для разности долей

n1 <- total_married #размер выборки людей в браке  
n2 <- total_not_married #размер выборки людей не в браке 
ESE <- sqrt((p1*(1-p1))/n1 + (p2*(1-p2))/n2) # cтандартная ошибка разности долей
lower10 <- (p1-p2)- 1.64 * ESE # нижняя граница доверительного интервала для a = 0.1
upper10 <- (p1-p2) + 1.64 * ESE # верхняя граница доверительного интервала для а = 0.1
paste0("[", round(lower10, 4), ", ", round(upper10, 4), "]") # Доверительный интервал для 90% уровня значимости 

lower5 <- (p1-p2)- 1.96 * ESE # нижняя граница доверительного интервала для a = 0.05
upper5 <- (p1-p2) + 1.96 * ESE # верхняя граница доверительного интервала для а = 0.05
paste0("[", round(lower5, 4), ", ", round(upper5, 4), "]") # Доверительный интервал для 95% уровня значимости

lower1 <- (p1-p2)- 2.5758 * ESE # нижняя граница доверительного интервала для a = 0.01
upper1 <- (p1-p2) + 2.5758 * ESE # верхняя граница доверительного интервала для а = 0.01
paste0("[", round(lower1, 4), ", ", round(upper1, 4), "]") # Доверительный интервал для 99% уровня значимости

# №5 Проверка гипотезы о равенстве долей

p<-(employed_married+employed_not_married)/(total_married+total_not_married)
Z<- ((p1-p2)-0)/sqrt((p*(1-p)*(1/n1+1/n2))) 
Z
Z_critical<-c(
  "10%" = qnorm(1 - 0.10/2),
  "5%" = qnorm(1 - 0.05/2), 
  "1%" = qnorm(1 - 0.01/2)
)


results<-data.frame(
  Уровень = names(Z_critical),
  Z_критическое = Z_critical,
  Z_статистика=Z,
  Отклоняем_H0 = abs(Z)>Z_critical
)



#визуализация
library(ggplot2)
#параметры текста

Z_critical<-c(1.645, 1.96, 2.576)
#создаем график
ggplot(data.frame(x=c(-3.5, 3.5)), aes(x)) +
  #Основное распределение
  stat_function(fun=dnorm, args=list(mean=0, sd=1),
                size=1.2, color="gray40")+
  #критические области закрашены красным
  geom_area(stat="function", fun=dnorm,
            args=list(mean=0, sd=1),
            xlim=c(Z_critical[1], 3.5), fill="red", 
            alpha=0.3
            )+
  geom_area(stat="function", fun=dnorm,
            args=list(mean=0, sd=1),
            xlim=c(-3.5, -Z_critical[1]), fill="red", 
            alpha=0.3
  )+
  #вертикальные линии для критических значений
  geom_vline(xintercept=c(-Z_critical, Z_critical), 
             color="red", linetype="dashed", size=0.8)+

  #линия Z-статистики
  geom_vline(xintercept=Z, color="blue", size=1) +
    
  #подписи
  annotate("text", x=0, y=0.2, label="π1= π2", size=5)+
  geom_text(aes(x=Z, y=0.1, label =paste0("Z = ", round(Z, 3))),
            hjust = -0.1, vjust=-1, color ="blue", size =4)+
  
  #Подписи уровней значимости
  geom_text(aes(x=Z_critical[1]+0.1, y = 0.3, label="10%"),
            color="red", size=3.5, hjust=1)+
  geom_text(aes(x=Z_critical[2]+0.1, y = 0.3, label="5%"),
              color="red", size=3.5, hjust=1)+
  geom_text(aes(x=Z_critical[3]+0.1, y = 0.3, label="1%"),
              color="red", size=3.5, hjust=1)+
  
  #оформление
  labs(title = "Проверка гипотезы π1=π2",
       subtitle ="Красные зоны - критические области, синяя линия - наблюдаемое значение",
       x="Z-статистика"
       )+
  theme_minimal(base_size = 14)+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank())
  
  #вывод результатов
  print(results)

# Разделим выборку на мужчин и женщин

# Количество замужних женщин
  female_married<-sum(married$sex == 2) # значение женского пола = 2
# Количество незамужних женщин
  female_not_married<-sum(not_married$sex == 2)    
# Количество женатых мужчин
  male_married<-sum(married$sex == 1) # значение мужского пола = 1
# Количество неженатых мужчин
  male_not_married<-sum(not_married$sex == 1) 
    
    
  #замужние трудоустроенные женщины
female_employed_married<-sum(married$job == 1&married$sex == 2)
  #незамужние трудоустроенные женщины
female_employed_not_married<-sum(not_married$job == 1&not_married$sex == 2) 
  #женатые трудоустроенные мужчины
male_employed_married<-sum(married$job == 1&married$sex == 1)
  #неженатые трудоустроенные мужчины
male_employed_not_married<-sum(not_married$job == 1&not_married$sex == 1)

# проверка гипотезы для подгруппы женщины 

# π1 - доля трудоустроенных женщин среди замужних женщин
# π2 - доля трудоустроенных женщин среди незамужних женщин

#H0: π1 = π2 
#H1: π1 != π2 

# Расчет доли трудоустроенных женщин среди замужних женщин (p1)
p1<-female_employed_married/female_married
# Расчет доли трудоустроенных женщин среди незамужних женщин (p2)
p2<-female_employed_not_married/female_not_married

# Расчет Z-статистики
# π1-π2=0
p<-(female_employed_married+female_employed_not_married)/(female_married+female_not_married)

Z_stat<- ((p1-p2)-0)/sqrt((p*(1-p)*(1/female_married+1/female_not_married))) 
Z_stat
Z_critical<-c(
  "10%" = qnorm(1 - 0.10/2),
  "5%" = qnorm(1 - 0.05/2), 
  "1%" = qnorm(1 - 0.01/2)
)


results<-data.frame(
  Уровень = names(Z_critical),
  Z_критическое = Z_critical,
  Z_статистика=Z_stat,
  Отклоняем_H0 = abs(Z_stat)>Z_critical
)

print(results)

#визуализация
library(ggplot2)
#параметры текста

Z_critical<-c(1.645, 1.96, 2.576)
#создаем график
ggplot(data.frame(x=c(-3.5, 3.5)), aes(x)) +
  #Основное распределение
  stat_function(fun=dnorm, args=list(mean=0, sd=1),
                size=1.2, color="gray40")+
  #критические области закрашены красным
  geom_area(stat="function", fun=dnorm,
            args=list(mean=0, sd=1),
            xlim=c(Z_critical[1], 3.5), fill="red", 
            alpha=0.3
  )+
  geom_area(stat="function", fun=dnorm,
            args=list(mean=0, sd=1),
            xlim=c(-3.5, -Z_critical[1]), fill="red", 
            alpha=0.3
  )+
  #вертикальные линии для критических значений
  geom_vline(xintercept=c(-Z_critical, Z_critical), 
             color="red", linetype="dashed", size=0.8)+
  
  #линия Z-статистики
  geom_vline(xintercept=Z_stat, color="blue", size=1) +
  
  #подписи
  annotate("text", x=0, y=0.2, label="π1=π2", size=5)+
  geom_text(aes(x=Z_stat, y=0.1, label =paste0("Z_stat = ", round(Z_stat, 3))),
            hjust = -0.1, vjust=-1, color ="blue", size =4)+
  
  #Подписи уровней значимости
  geom_text(aes(x=Z_critical[1]+0.1, y = 0.3, label="10%"),
            color="red", size=3.5, hjust=1)+
  geom_text(aes(x=Z_critical[2]+0.1, y = 0.3, label="5%"),
            color="red", size=3.5, hjust=1)+
  geom_text(aes(x=Z_critical[3]+0.1, y = 0.3, label="1%"),
            color="red", size=3.5, hjust=1)+
  
  #оформление
  labs(title = "Проверка гипотезы π1=π2",
       subtitle ="Красные зоны - критические области, синяя линия - наблюдаемое значение",
       x="Z-статистика"
  )+
  theme_minimal(base_size = 14)+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank())

# проверка гипотезы для подгруппы мужчины 

# π1 - доля трудоустроенных мужчин среди женатых мужчин
# π2 - доля трудоустроенных мужчин среди неженатых мужчин

#H0: π1 = π2 
#H1: π1 != π2 

# Расчет доли трудоустроенных мужчин среди женатых мужчин (p1)
p1<-male_employed_married/male_married
# Расчет доли трудоустроенных мужчин среди неженатых мужчин (p2)
p2<-male_employed_not_married/male_not_married

# Расчет Z-статистики
# π1-π2=0
p<-(male_employed_married+male_employed_not_married)/(male_married+male_not_married)

Z_stat<- ((p1-p2)-0)/sqrt((p*(1-p)*(1/male_married+1/male_not_married))) 
Z_stat
Z_critical<-c(
  "10%" = qnorm(1 - 0.10/2),
  "5%" = qnorm(1 - 0.05/2), 
  "1%" = qnorm(1 - 0.01/2)
)


results<-data.frame(
  Уровень = names(Z_critical),
  Z_критическое = Z_critical,
  Z_статистика=Z_stat,
  Отклоняем_H0 = abs(Z_stat)>Z_critical
)

print(results)

#визуализация
library(ggplot2)
#параметры текста

Z_critical<-c(1.645, 1.96, 2.576)
#создаем график
ggplot(data.frame(x=c(-3.5, 3.5)), aes(x)) +
  #Основное распределение
  stat_function(fun=dnorm, args=list(mean=0, sd=1),
                size=1.2, color="gray40")+
  #критические области закрашены красным
  geom_area(stat="function", fun=dnorm,
            args=list(mean=0, sd=1),
            xlim=c(Z_critical[1], 3.5), fill="red", 
            alpha=0.3
  )+
  geom_area(stat="function", fun=dnorm,
            args=list(mean=0, sd=1),
            xlim=c(-3.5, -Z_critical[1]), fill="red", 
            alpha=0.3
  )+
  #вертикальные линии для критических значений
  geom_vline(xintercept=c(-Z_critical, Z_critical), 
             color="red", linetype="dashed", size=0.8)+
  
  #линия Z-статистики
  geom_vline(xintercept=Z_stat, color="blue", size=1) +
  
  #подписи
  annotate("text", x=0, y=0.2, label="π1=π2", size=5)+
  geom_text(aes(x=Z_stat, y=0.1, label =paste0("Z_stat = ", round(Z_stat, 3))),
            hjust = 1, vjust=1, color ="blue", size =4)+
  
  #Подписи уровней значимости
  geom_text(aes(x=Z_critical[1]+0.1, y = 0.3, label="10%"),
            color="red", size=3.5, hjust=1)+
  geom_text(aes(x=Z_critical[2]+0.1, y = 0.3, label="5%"),
            color="red", size=3.5, hjust=1)+
  geom_text(aes(x=Z_critical[3]+0.1, y = 0.3, label="1%"),
            color="red", size=3.5, hjust=1)+
  
  #оформление
  labs(title = "Проверка гипотезы π1=π2",
       subtitle ="Красные зоны - критические области, синяя линия - наблюдаемое значение",
       x="Z-статистика"
  )+
  theme_minimal(base_size = 14)+
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank())







