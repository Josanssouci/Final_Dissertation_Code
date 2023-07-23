
install.packages(c("dplyr", "stringr", "tidyr", "lubridate", "readr"))

library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)
library(readr)

# Lire le fichier CSV
survey <- read_csv('df_vf.csv')

# Vérifier les premières lignes du dataset
head(survey)

# Renommer les colonnes en utilisant leur numéro
colnames(survey)[1] <- "Timestamp"
colnames(survey)[2] <- "Role"
colnames(survey)[3] <- "Experience"
colnames(survey)[4] <- "AI_Exposure"
colnames(survey)[5] <- "Data_Quality"
colnames(survey)[6] <- "Bias_Concerns"
colnames(survey)[7] <- "AI_Transparency"
colnames(survey)[8] <- "AI_Trust"
colnames(survey)[9] <- "Collaboration_Importance"
colnames(survey)[10] <- "Collaboration_Experience"
colnames(survey)[11] <- "AI_Training"
colnames(survey)[12] <- "Training_Importance"

# Vérifier les premières lignes du dataset
head(survey)

# Regrouper par Role et compter le nombre d'occurrences
most_common_roles <- survey %>%
  group_by(Role) %>%
  summarize(n_occurrences = n()) %>%
  arrange(desc(n_occurrences))

# Trouver les différentes orthographes de "Dentist"
all_dentists <- most_common_roles %>%
  filter(str_detect(Role,"(Dentiste|Chirurgie dentaire|Chirurgien dentiste|assistant dentaire|Chirurgien dentaire|Chirurgienne-dentiste)"))

# Créer un vecteur de ces orthographes alternatives
dentist_vector <- unique(all_dentists$Role)

# Remplacer toutes les orthographes alternatives par "Dentist"
for (val in dentist_vector) {
  survey$Role <- replace(survey$Role, survey$Role == val, "Dentist")
}

# Trouver les différentes orthographes de "Pharmacist"
all_pharmacist <- most_common_roles %>%
  filter(str_detect(Role,"(Pharmacien|Marketing pharmaceutique|Marketing pharma|chef de produit pharmaceutique|Etudiant en pharmacie|Formateur et medical reviewer|Interne pharmacie|marketing|Marketing|Markrting pharmaceutique|Medical advisor|Pharmacie dans les affaires médicales|Pharmacien d’industrie|Pharmacien marketing|Consultant en stratégie|Consulting)"))

# Créer un vecteur de ces orthographes alternatives
pharmacist_vector <- unique(all_pharmacist$Role)

# Remplacer toutes les orthographes alternatives par "Pharmacist"
for (val in pharmacist_vector) {
  survey$Role <- replace(survey$Role, survey$Role == val, "Pharmacist")
}


# Trouver les différentes orthographes de "Physiotherapist"
all_physiotherapist <- most_common_roles %>%
  filter(str_detect(Role,"(Kine|Kinésithérapeute|Masseur kinésithérapeute)"))

# Créer un vecteur de ces orthographes alternatives
physiotherapist_vector <- unique(all_physiotherapist$Role)

# Remplacer toutes les orthographes alternatives par "Physiotherapist"
for (val in physiotherapist_vector) {
  survey$Role <- replace(survey$Role, survey$Role == val, "Physiotherapist")
}


# Trouver les différentes orthographes de "Doctor"
all_doctor <- most_common_roles %>%
  filter(str_detect(Role,"(Médecin|Medecin|Médecin produit|Chirurgien)"))

# Créer un vecteur de ces orthographes alternatives
doctor_vector <- unique(all_doctor$Role)

# Remplacer toutes les orthographes alternatives par "Doctor"
for (val in doctor_vector) {
  survey$Role <- replace(survey$Role, survey$Role == val, "Doctor")
}


# Trouver les différentes orthographes de "Biologist"
all_biologist <- most_common_roles %>%
  filter(str_detect(Role,"(Chef de produit|Chef de produit oncologie|	
Chef de produit|Ingénieur biomédical)"))

# Créer un vecteur de ces orthographes alternatives
biologist_vector <- unique(all_biologist$Role)

# Remplacer toutes les orthographes alternatives par "Biologist"
for (val in biologist_vector) {
  survey$Role <- replace(survey$Role, survey$Role == val, "Biologist")
}


# Charger le package tidyr
library(tidyr)

# Remplacer les valeurs NA par "Doctor"
survey <- survey %>%
  mutate(Role = replace_na(Role, "Doctor"))

# Remplacer "Infirmière libérale" par "Nurse"
survey <- survey %>%
  mutate(Role = ifelse(Role == "Infirmière libérale", "Nurse", Role))

# Remplacer "Psychomotricienne D.E (rééducation)" par "Psychomotrician"
survey <- survey %>%
  mutate(Role = ifelse(Role == "Psychomotricienne D.E (rééducation)", "Psychomotrician", Role))

# Supprimer la colonne "Collaboration_Experience"
survey <- select(survey, -Collaboration_Experience)


# Remplacez les slashs par des tirets
survey$Timestamp <- gsub("/", "-", survey$Timestamp)

# Convertissez la chaîne de caractères en date/heure
survey$Timestamp <- dmy_hms(survey$Timestamp, tz = "Europe/Paris")


# Convertir le champ 'Experience' en facteur ordonné
levels_experience <- c("Moins d'un an", "1 à 3 ans", "4 à 6 ans", "7 à 10 ans", "Plus de 10 ans")
survey$Experience <- factor(survey$Experience, levels = levels_experience, ordered = TRUE)

# Convertir les réponses 'Oui', 'Non' et 'Préfère ne pas répondre' en facteurs
columns_to_factor <- c("AI_Exposure", "Bias_Concerns", "AI_Training", "Training_Importance")
survey[columns_to_factor] <- lapply(survey[columns_to_factor], function(x) {
  factor(x, levels = c("Non", "Oui", "Préfère ne pas répondre"))
})


#Remplacement de la seule valeur NA par la moyenne de la colonne. 
survey$AI_Transparency[is.na(survey$AI_Transparency)] <- trunc(mean(survey$AI_Transparency, na.rm = TRUE))
survey$AI_Transparency <- trunc(survey$AI_Transparency)


# Vérifier si des valeurs manquantes sont présentes dans le jeu de données
if(any(is.na(survey))) {
  print("Il y a des valeurs manquantes dans le jeu de données.")
  # Vous pouvez décider d'exclure ces lignes ou de les remplir avec d'autres valeurs
  # Exemple de comment supprimer les lignes avec des NA
  # survey <- na.omit(survey)
} else {
  print("Il n'y a pas de valeurs manquantes dans le jeu de données.")
}

# Supprimer la ligne 72
survey <- survey %>% slice(-72)


write_csv(survey, "df_clean.csv")


