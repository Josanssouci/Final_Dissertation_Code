library(ggplot2)
library(ggthemes)
library(dplyr)
library(kableExtra)

#Import the data : 
df <- read.csv('df_clean.csv')

## FOR Hypothesis 1: Data quality and algorithmic bias

#Creation of the graph on AI exposure

# Filter the data to eliminate "Préfère ne pas répondre"
df_filtered_exposure <- df %>%
  filter(AI_Exposure != "Préfère ne pas répondre")

# Calculate percentages
df_percentages_exposure <- df_filtered_exposure %>%
  group_by(AI_Exposure) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n))

# Create the graph
ggplot(df_percentages_exposure, aes(x="", y=percentage, fill=AI_Exposure)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(percentage*100), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "AI_Exposure",
       title = "Exposure to AI") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# J'ai repéré 4 graphs où la répartition est plutot équilibré. L'objectif est de pousser l'analyse plus dans le détail pour voir si une catégorier entre l'Experience et le Role se démarque.


#Table of AI exposure by Experience
exposure_table <- table(df_filtered_exposure$Experience, df_filtered_exposure$AI_Exposure)

exposure_df <- as.data.frame.matrix(exposure_table)

exposure_df$Ratio <- round(exposure_df$Oui / exposure_df$Non,2)

exposure_table <- as.table(as.matrix(exposure_df))

print(exposure_table)

kable(exposure_df, "html", caption = "AI Exposure by Experience") %>%
  kable_styling("striped", full_width = F)

#Table of AI exposure by Role

role_response_table <- table(df_filtered_exposure$Role, df_filtered_exposure$AI_Exposure)

role_response_df <- as.data.frame.matrix(role_response_table)

role_response_df$Ratio <- round(role_response_df$Oui / role_response_df$Non,2)

role_response_table <- as.table(as.matrix(role_response_df))

print(role_response_table)

kable(role_response_df, "html", caption = "AI Exposure by Role") %>%
  kable_styling("striped", full_width = F)

#Table of AI adoption by Bias Concern

df_filtered_exposure_bias <- df %>%
  filter(AI_Exposure != "Préfère ne pas répondre",
         Bias_Concerns != "Préfère ne pas répondre")

table_bias_exposure <- table(df_filtered_exposure_bias$AI_Exposure, df_filtered_exposure_bias$Bias_Concerns)
print(table_bias_exposure)

kable(table_bias_exposure, "html", caption = "AI Exposure by Bias Concern") %>%
  kable_styling("striped", full_width = F)

# Transformer le tableau en format 'long'
table_bias_exposure_df <- as.data.frame.table(table_bias_exposure)
names(table_bias_exposure_df) <- c("AI_Exposure", "Bias_Concerns", "Count")

# Créer le graphique
library(ggplot2)
ggplot(table_bias_exposure_df, aes(fill=Bias_Concerns, y=Count, x=AI_Exposure)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Pastel1") +
  labs(x = "Exposure to AI", y = "Count", fill = "Bias Concerns") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label= round(Count, 1)), position = position_stack(vjust = 0.5), size = 3)


#Creation of the graph on Data Quality

# Calculate percentages
df_percentages_data_quality <- df %>%
  group_by(Data_Quality) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n))

# Data are factor type
df_percentages_data_quality$Data_Quality <- factor(df_percentages_data_quality$Data_Quality, levels = c(1, 2, 3, 4, 5))

# Define a color palette
colors <- c("skyblue", "gold", "orange", "darkred")

# Create the graph
ggplot(df_percentages_data_quality, aes(x=Data_Quality, y=n, fill=Data_Quality)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(round(percentage*100, 1), "%")), vjust=-0.5) +
  theme_minimal() +
  labs(x="Data_Quality", y="Count",
       title="The quality of data in AI for Healthcare professional ") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 1)
  ) +
  scale_fill_manual(values = colors)







# Creation of the graph on Bias Concerns

# Filter the data to eliminate "Préfère ne pas répondre"
df_filtered_bias <- df %>%
  filter(Bias_Concerns != "Préfère ne pas répondre")

# Calculate percentages
df_percentages_bias <- df_filtered_bias %>%
  group_by(Bias_Concerns) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n))

# Create the graph
ggplot(df_percentages_bias, aes(x="", y=percentage, fill=Bias_Concerns)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(percentage*100), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Bias_Concerns",
       title = "Bias Concerns to AI") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

#Table of Bias Concern by Experience
bias_table_xp <- table(df_filtered_bias$Experience, df_filtered_bias$Bias_Concerns)

bias_df_xp <- as.data.frame.matrix(bias_table_xp)

bias_df_xp$Ratio <- round(bias_df_xp$Oui / bias_df_xp$Non,2)

bias_table_xp <- as.table(as.matrix(bias_df_xp))

print(bias_table_xp)

kable(bias_table_xp, "html", caption = "Bias Concerns by Experience") %>%
  kable_styling("striped", full_width = F)

#Table of Bias Concerns by Role

bias_table_role <- table(df_filtered_bias$Role, df_filtered_bias$Bias_Concerns)

bias_df_role <- as.data.frame.matrix(bias_table_role)

bias_df_role$Ratio <- round(bias_df_role$Oui / bias_df_role$Non,2)

bias_table_role <- as.table(as.matrix(bias_df_role))

print(bias_table_role)

kable(bias_table_role, "html", caption = "Bias Concerns by Role") %>%
  kable_styling("striped", full_width = F)





## FOR Hypothesis 2: Transparency and trust

# Creation of the graph on AI_Transparency

# Calculate percentages
df_percentages_transparency <- df %>%
  group_by(AI_Transparency) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n))

# Data are factor type
df_percentages_transparency$AI_Transparency <- factor(df_percentages_transparency$AI_Transparency, levels = c(1, 2, 3, 4, 5))

colors2 <- c("lightblue", "skyblue", "gold", "orange", "darkred")

# Create the graph
ggplot(df_percentages_transparency, aes(x=AI_Transparency, y=n, fill=AI_Transparency)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(round(percentage*100, 1), "%")), vjust=-0.5) +
  theme_minimal() +
  labs(x="AI_Transparency", y="Count",
       title="Transparency in AI Decision-Making") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_manual(values = colors2)


table1 <- table(df_filtered_exposure$AI_Transparency, df_filtered_exposure$AI_Exposure)
print(table)

# Transformer le tableau en format 'long'
table1 <- as.data.frame.table(table1)
names(table1) <- c("AI_Exposure", "AI_Transparency", "Count")

# Créer le graphique
ggplot(table1, aes(fill=AI_Transparency, y=Count, x=AI_Exposure)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Pastel1") +
  labs(x = "AI_Exposure", y = "Count", fill = "AI_Transparency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label= round(Count, 1)), position = position_stack(vjust = 0.5), size = 3)

table2 <- table(df$Experience, df$AI_Transparency)
print(table2)

# Transformer le tableau en format 'long'
table2 <- as.data.frame.table(table2)
names(table2) <- c("Experience", "AI_Transparency", "Count")

# Créer le graphique
ggplot(table2, aes(fill=AI_Transparency, y=Count, x=Experience)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Pastel1") +
  labs(x = "Experience", y = "Count", fill = "AI_Transparency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label= round(Count, 1)), position = position_stack(vjust = 0.5), size = 3)

table3 <- table(df$Role, df$AI_Transparency)
print(table3)

#Adjust data for the graph
table3 <- as.data.frame.table(table3)
names(table3) <- c("Role", "AI_Transparency", "Count")

#Create graph
ggplot(table3, aes(fill=AI_Transparency, y=Count, x=Role)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Pastel1") +
  labs(x = "Role", y = "Count", fill = "AI_Transparency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label= round(Count, 1)), position = position_stack(vjust = 0.5), size = 3)







# Creation of the graph on AI_Trust

# Calculate percentages
df_percentages_trust <- df %>%
  group_by(AI_Trust) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n))

# Data are factor type
df_percentages_trust$AI_Trust <- factor(df_percentages_trust$AI_Trust, levels = c(1, 2, 3, 4, 5))

# Create the graph
ggplot(df_percentages_trust, aes(x=AI_Trust, y=n, fill=AI_Trust)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(round(percentage*100, 1), "%")), vjust=-0.5) +
  theme_minimal() +
  labs(x="AI_Trust", y="Count",
       title="Healthcare professionals' confidence in AI") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 0.5)
  ) +
  scale_fill_manual(values = colors2)

#Table of AI trust by Experience

trust_table <- table(df$Experience, df$AI_Trust)
trust_df <- as.data.frame.matrix(trust_table)
print(trust_df)

#Adjust data for the graph
trust_table <- as.data.frame.table(trust_table)
names(trust_table) <- c("Experience", "AI_Trust", "Count")

#Create graph
ggplot(trust_table, aes(fill=AI_Trust, y=Count, x=Experience)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Pastel1") +
  labs(x = "Experience", y = "Count", fill = "AI_Trust") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label= round(Count, 1)), position = position_stack(vjust = 0.5), size = 3)


#Table of AI trust by Role

trust_table_role <- table(df$Role, df$AI_Trust)
trust_df_role <- as.data.frame.matrix(trust_table_role)
print(trust_df_role)

table1 <- table(df_filtered_exposure$AI_Trust, df_filtered_exposure$AI_Exposure)
print(table)

#Adjust data for the graph
table1 <- as.data.frame.table(table1)
names(table1) <- c("AI_Exposure", "AI_Trust", "Count")

#Create graph
ggplot(table1, aes(fill=AI_Trust, y=Count, x=AI_Exposure)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Pastel1") +
  labs(x = "AI_Trust", y = "Count", fill = "AI_Exposure") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label= round(Count, 1)), position = position_stack(vjust = 0.5), size = 3)

table2 <- table(df$Experience, df$AI_Trust)
print(table2)

table3 <- table(df$Role, df$AI_Trust)
print(table3)

#Adjust data for the graph
table3 <- as.data.frame.table(table3)
names(table3) <- c("Role", "AI_Trust", "Count")

#Create graph
ggplot(table3, aes(fill=AI_Trust, y=Count, x=Role)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_brewer(palette="Pastel1") +
  labs(x = "Role", y = "Count", fill = "AI_Trust") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label= round(Count, 1)), position = position_stack(vjust = 0.5), size = 3)




# Creation of the graph on Collaboration Importance

# Calculate percentages
df_percentages_colab <- df %>%
  group_by(Collaboration_Importance) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n))

# Data are factor type
df_percentages_colab$Collaboration_Importance <- factor(df_percentages_colab$Collaboration_Importance, levels = c(1, 2, 3, 4, 5))

# Create the graph
ggplot(df_percentages_colab, aes(x=Collaboration_Importance, y=n, fill=Collaboration_Importance)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(round(percentage*100, 1), "%")), vjust=-0.5) +
  theme_minimal() +
  labs(x="Collaboration_Importance", y="Count",
       title="The importance of working with AI") +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_text(hjust = 1)
  ) +
  scale_fill_manual(values = colors2)



table1 <- table(df_filtered_exposure$Collaboration_Importance, df_filtered_exposure$AI_Exposure)
print(table)

kable(table1, "html", caption = "Collaboration_Importance by AI_Exposure") %>%
  kable_styling("striped", full_width = F)

table2 <- table(df$Experience, df$Collaboration_Importance)
print(table2)

kable(table2, "html", caption = "Collaboration_Importance by Experience") %>%
  kable_styling("striped", full_width = F)

table3 <- table(df$Role, df$Collaboration_Importance)
print(table3)

kable(table3, "html", caption = "Collaboration_Importance by Role") %>%
  kable_styling("striped", full_width = F)










# Creation of the graph on AI training

# Filter the data to eliminate "Préfère ne pas répondre"
df_filtered_training <- df %>%
  filter(AI_Training != "Préfère ne pas répondre")

# Calculate percentages
df_percentages_training <- df_filtered_training %>%
  group_by(AI_Training) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n))

# Create the graph
ggplot(df_percentages_training, aes(x="", y=percentage, fill=AI_Training)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(percentage*100), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "AI_Training",
       title = "Healthcare professional trained in AI") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.7)
  )






# Creation of the graph on Training Importance

# Filter the data to eliminate "Préfère ne pas répondre"
df_filtered_importance <- df %>%
  filter(Training_Importance != "Préfère ne pas répondre")

# Calculate percentages
df_percentages_importance <- df_filtered_importance %>%
  group_by(Training_Importance) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / sum(n))

# Create the graph
ggplot(df_percentages_importance, aes(x="", y=percentage, fill=Training_Importance)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(percentage*100), "%")), position = position_stack(vjust = 0.5)) +
  labs(x = NULL, y = NULL, fill = "Training_Importance",
       title = "Considers AI training important") +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(hjust = 0.9)
  )

# TEST
# Créer un nouveau dataframe qui exclut "Préfère ne pas répondre" pour les deux colonnes
df_filtered <- subset(df, !(Bias_Concerns == "Préfère ne pas répondre" | AI_Exposure == "Préfère ne pas répondre"))

# Créer une nouvelle table de contingence avec le dataframe filtré
table_contingence_filtered <- table(df_filtered$Bias_Concerns, df_filtered$AI_Exposure)

# Créer un nouveau mosaic plot avec le dataframe filtré
mosaicplot(table_contingence_filtered, main="Correlation between Bias_Concerns and AI_Exposure",
           xlab="Bias_Concerns", ylab="AI_Exposure", color = c("#B3E2CD", "#FDCDAC"))


# TEST TABLEAU
library(DT)

datatable(exposure_df, caption = "AI Adoption by Experience", options = list(pageLength = 5))

# Installer le package si ce n'est pas encore fait
install.packages("kableExtra")

# Charger le package
library(kableExtra)

kable(exposure_df, "html", caption = "AI Adoption by Experience") %>%
  kable_styling("striped", full_width = F)

table(df$Experience)



