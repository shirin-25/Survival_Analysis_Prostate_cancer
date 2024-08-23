install.packages("asaur")
library(asaur)

data("prostateSurvival")
prostateSurvival

# on supprime les 2 (colonne statut)

prostateSurvival <- prostateSurvival[prostateSurvival$status != 2, ]

# cet ensemble de données contients 4 variables qualitatives et une variables quantitatives

# Résumé des données
summary(prostateSurvival)

# status
table_status <- table(prostateSurvival$status)
table_status

# Calcul des pourcentages
percentages <- prop.table(table_status) * 100
percentages

#histogramme pour la variable 'survTime'
hist(prostateSurvival$survTime, 
     main="Histogramme du temps de survie",
     xlab="Temps de survie (jours)", 
     ylab="Fréquence", 
     col="lightblue", 
     border="black")

# avec courbe de densité
hist(prostateSurvival$survTime, 
     main="Histogramme du temps de survie avec courbe de densité",
     xlab="Temps de survie (jours)", 
     ylab="Fréquence", 
     col="lightblue", 
     border="black", 
     prob=TRUE)
lines(density(prostateSurvival$survTime), col="red", lwd=2)

table_grade <- table(prostateSurvival$grade)
table_grade

# diagramme circulaire pour 'grade'
pie(table_grade, 
    main="Répartition des grades", 
    col=rainbow(length(table_grade)),
    labels=paste(names(table_grade), "\n", round(prop.table(table_grade) * 100, 1), "%"))

table_stage <- table(prostateSurvival$stage)
table_stage
# diagramme circulaire pour 'stage'
pie(table_stage, 
    main="Répartition des stades", 
    col=rainbow(length(table_stage)),
    labels=paste(names(table_stage), "\n", round(prop.table(table_stage) * 100, 1), "%"))

table_ageGroup <- table(prostateSurvival$ageGroup)
table_ageGroup
# diagramme circulaire pour 'ageGroup'
pie(table_ageGroup, 
    main="Répartition des groupes d'âge", 
    col=rainbow(length(table_ageGroup)),
    labels=paste(names(table_ageGroup), "\n", round(prop.table(table_ageGroup) * 100, 1), "%"))

table_status <- table(prostateSurvival$status)
table_status
#diagramme circulaire pour 'status'
pie(table_status, 
    main="Répartition des statuts de censure", 
    col=c("blue", "salmon", "lightgreen"), 
    labels=paste(names(table_status), "\n", round(prop.table(table_status) * 100, 1), "%"))

# Calcul des stats descriptives pour SurvTime 
summary(prostateSurvival$survTime)

# Autres statistiques descriptives
mean_survTime <- mean(prostateSurvival$survTime)
sd_survTime <- sd(prostateSurvival$survTime)
var_survTime <- var(prostateSurvival$survTime)
cat("Moyenne du temps de survie : ", mean_survTime, "\n")
cat("Écart-type du temps de survie : ", sd_survTime, "\n")
cat("Variance du temps de survie : ", var_survTime, "\n")
iqr_survTime <- IQR(prostateSurvival$survTime)
cat("Écart interquartile du temps de survie : ", iqr_survTime, "\n")

boxplot(prostateSurvival$survTime, 
        main="Boxplot du temps de survie", 
        ylab="Temps de survie (jours)", 
        col="lightgreen")

# Tableau croisé entre 'grade' et 'stage'
table_grade_stage <- table(prostateSurvival$grade, prostateSurvival$stage)
print(table_grade_stage)


# Boxplot du temps de survie par groupe d'âge
boxplot(survTime ~ ageGroup, data=prostateSurvival, 
        main="Temps de survie par groupe d'âge", 
        xlab="Groupe d'âge", 
        ylab="Temps de survie (jours)", 
        col="lightblue")

# Boxplot du temps de survie par status
boxplot(survTime ~ status, data=prostateSurvival, 
        main="Temps de survie par status", 
        xlab="status", 
        ylab="Temps de survie (jours)", 
        col="blue")

# Boxplot du temps de survie par  grade
boxplot(survTime ~ grade, data=prostateSurvival, 
        main="Temps de survie par grade", 
        xlab="grade", 
        ylab="Temps de survie (jours)", 
        col="green")

# Boxplot du temps de survie par stage
boxplot(survTime ~ stage, data=prostateSurvival, 
        main="Temps de survie par stage", 
        xlab="stage", 
        ylab="Temps de survie (jours)", 
        col="red")

pairs(prostateSurvival, col="darkorchid3")

install.packages("ggplot2")
library(ggplot2)

ggplot(prostateSurvival, aes(x=stage, fill=grade)) +
  geom_bar(position="fill") +
  labs(title="Répartition du Grade par Stage",
       x="Stage",
       y="Proportion",
       fill="Grade") +
  theme_minimal()

ggplot(prostateSurvival, aes(x=ageGroup, fill=grade)) +
  geom_bar(position="fill") +
  labs(title="Répartition du Grade par groupe age ",
       x="groupe age",
       y="Proportion",
       fill="Grade") +
  theme_minimal()

library(survival)
#objet de survie
surv_obj <- Surv(time = prostateSurvival$survTime, event = prostateSurvival$status)
#modèle de régression de Cox
cox_model <- coxph(surv_obj ~ grade + stage + ageGroup , data = prostateSurvival)

# Résumé du modèle
summary(cox_model)
cox.zph(cox_model)

# courbe de survie
surv_fit <- survfit(surv_obj ~ grade, data = prostateSurvival)
plot(surv_fit, col=rainbow(length(levels(prostateSurvival$grade))), main="Courbe de survie par Grade")

