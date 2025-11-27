install.packages('ggplot2')
install.packages('dplyr')
install.packages('gridExtra')
install.packages('cowplot')


library(ggplot2)
library(dplyr)
library(cowplot)
library(scales)
library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)

summary(Students.Social.Media.Addiction$Age)


# ==============================
# Pie chart para Academic_Level
# ==============================
academic_data <- Students.Social.Media.Addiction %>%
  mutate(Academic_Level = recode(Academic_Level,
                                 "Graduate" = "Graduado",
                                 "High School" = "Secundaria",
                                 "Undergraduate" = "Pregrado")) %>%
  count(Academic_Level) %>%
  mutate(prop = n / sum(n),
         label = paste0(Academic_Level, " (", scales::percent(prop, accuracy = 0.1), ")"))

g1 <- ggplot(academic_data, aes(x = "", y = prop, fill = Academic_Level)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_void() +
  theme(legend.position = "none")

# ===========================================
# Pie chart para Affects_Academic_Performance
# ===========================================
performance_data <- Students.Social.Media.Addiction %>%
  mutate(Affects_Academic_Performance = recode(Affects_Academic_Performance,
                                               "Yes" = "Sí",
                                               "No" = "No")) %>%
  count(Affects_Academic_Performance) %>%
  mutate(prop = n / sum(n),
         label = paste0(Affects_Academic_Performance, " (", scales::percent(prop, accuracy = 0.1), ")"))

g2 <- ggplot(performance_data, aes(x = "", y = prop, fill = Affects_Academic_Performance)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            size = 4) +
  scale_fill_brewer(palette = "Pastel2") +
  theme_void() +
  theme(legend.position = "none")


# Concatenar en disposición horizontal
grid.arrange(g1, g2, ncol = 2)


# Poner A y B arriba a la izquierda
final_plot <- plot_grid(g1, g2, labels = c("A", "B"), label_size = 16, ncol = 2)
print(final_plot)


# ===========================================
# Bar chart para Affects_Academic_Performance
# ===========================================
platform_data <- Students.Social.Media.Addiction %>%
  filter(!is.na(Most_Used_Platform)) %>%
  count(Most_Used_Platform) %>%
  arrange(desc(n)) %>%
  rename(Platform = Most_Used_Platform, Count = n) %>%
  mutate(prop = Count / sum(Count),
         label = ifelse(prop < 0.01, "", paste0(Count, " (", percent(prop, accuracy = 0.1), ")")))

# ============================
# 1) Barras
# ============================

ggplot(platform_data, aes(x = reorder(Platform, -Count), y = Count, fill = Platform)) +
  geom_col(color = "black", width = 0.9, show.legend = FALSE) +
  geom_text(aes(label = label), vjust = -0.3, size = 3) +
  scale_fill_brewer(palette = "Pastel2") +
  labs(x = NULL, y = "Número de estudiantes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(top10, aes(x = reorder(Country_es, n), y = n)) +
  geom_bar(stat = "identity", color = "black", width = 0.9,) +
  scale_fill_brewer(palette = "Pastel2") +
  theme_minimal() +
  labs(x = "País",
       y = "Número de usuarios")

# =============================
# Boxplot Academic vs Uso horas
# =============================

academic_data <- Students.Social.Media.Addiction %>%
  mutate(Academic_Level = recode(Academic_Level,
                                 "Graduate" = "Graduado",
                                 "High School" = "Secundaria",
                                 "Undergraduate" = "Pregrado"),
         Academic_Level = factor(Academic_Level, 
                                 levels = c("Secundaria", "Pregrado", "Graduado")))

summary(academic_data$Academic_Level)

ggplot(academic_data, 
       aes(x = Academic_Level, y = Avg_Daily_Usage_Hours, fill = Academic_Level)) +
  geom_boxplot(outlier.shape = 16, outlier.size = 2) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = NULL, 
       y = "Horas promedio de uso diario") +
  theme_minimal() +
  theme(legend.position = "none")


aggregate(Avg_Daily_Usage_Hours ~ Academic_Level,
          data = Students.Social.Media.Addiction,
          FUN = summary)

Students.Social.Media.Addiction %>%
  mutate(Academic_Level = recode(Academic_Level,
                                 "Graduate" = "Graduado",
                                 "High School" = "Secundaria",
                                 "Undergraduate" = "Pregrado")) %>%
  group_by(Academic_Level) %>%
  summarise(
    n       = n(),
    mean    = mean(Avg_Daily_Usage_Hours, na.rm = TRUE),
    median  = median(Avg_Daily_Usage_Hours, na.rm = TRUE),
    sd      = sd(Avg_Daily_Usage_Hours, na.rm = TRUE),
    min     = min(Avg_Daily_Usage_Hours, na.rm = TRUE),
    max     = max(Avg_Daily_Usage_Hours, na.rm = TRUE)
  )









# Crear etiquetas con conteos
Students.Social.Media.Addiction <- Students.Social.Media.Addiction %>%
  group_by(Gender) %>%
  mutate(Gender_Label = paste0(Gender, " (", n(), ")")) %>%
  ungroup()

# Pirámide de población con intervalos de 2 años y colores pasteles
ggplot(Students.Social.Media.Addiction, aes(x = Age, fill = Gender_Label)) +
  geom_histogram(data = subset(Students.Social.Media.Addiction, Gender == "Female"),
                 aes(y = after_stat(count), fill = Gender_Label), 
                 binwidth = 1, alpha = 0.8, color = "black", linewidth = 0.5) +
  geom_histogram(data = subset(Students.Social.Media.Addiction, Gender == "Male"),
                 aes(y = -after_stat(count), fill = Gender_Label), 
                 binwidth = 1, alpha = 0.8, color = "black", linewidth = 0.5) +
  coord_flip() +
  scale_fill_manual(values = c("Female (353)" = "#FFB3D9", "Male (352)" = "#B3D9FF")) +
  scale_y_continuous(labels = abs, name = "Frecuencia") +
  labs(
       x = "Edad",
       fill = "Género") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray40"),
        legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"))

# ======================================================
# Grafico de cajas Most_Used_Platform vs Addicted_Score
# ======================================================
datos_estudiantes = read.table("datos_estudiantes.csv", header= TRUE, sep= ",", dec=".", stringsAsFactors = TRUE)
nuevosDatos = filter(datos_estudiantes, Most_Used_Platform %in% c("Instagram", "Facebook", "YouTube", "WhatsApp", "Twitter", "TikTok", "Snapchat", "LinkedIn", "WeChat"))
ggplot(nuevosDatos, aes(x=Most_Used_Platform, y=Addicted_Score, fill= Most_Used_Platform)) +
  geom_boxplot() +
  labs(x = "", y ="Puntaje de adiccion") + 
  theme_minimal() +
  scale_fill_brewer(palette = "Pastel2") +
  theme(legend.position = "none") +
  theme(axis.line = element_line(color = "grey90")) 


# ======================================================
# Grafico de cajas Gender vs Mental_Health_Score
# ======================================================
datos_estudiantes = read.table("datos_estudiantes.csv", header= TRUE, sep= ",", dec=".", stringsAsFactors = TRUE)
nuevosDatos = filter(datos_estudiantes, Gender %in% c("Female", "Male"))
ggplot(datos_estudiantes, aes(x=Gender, y=Mental_Health_Score, fill= Gender)) +
  geom_boxplot() +
  labs(x = "", y ="Auto-percepción de la Salud Mental") + 
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey90"),  # líneas principales gris claro
    panel.grid.minor = element_line(color = "grey90"),
    panel.background = element_rect(fill = "white") # líneas menores más tenues
    # fondo blanco
  ) + 
  guides(fill = "none")