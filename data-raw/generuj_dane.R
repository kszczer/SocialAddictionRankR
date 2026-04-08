# --- PRZYGOTOWANIE DANYCH DLA PAKIETU SocialMediaAddictionRankR ---

# 1. Wczytujemy dane surowe
# Ścieżka dostosowana do struktury projektu R (data-raw/)
if (!file.exists("data-raw/Students_Social_Media_Addiction.csv")) {
  stop("Nie znaleziono pliku w 'data-raw/'. Sprawdź ścieżkę!")
}

raw_data <- read.csv("data-raw/Students_Social_Media_Addiction.csv")

# 2. Filtrowanie i obróbka (Inżynieria Danych)
# - Wybieramy tylko studentów (Graduate i Undergraduate)
# - Wybieramy platformy: Facebook, TikTok, Instagram, YouTube, WhatsApp
social_media_data <- raw_data |>
  subset(
    Academic_Level %in% c("Graduate", "Undergraduate") &
      Most_Used_Platform %in% c("Facebook", "TikTok", "Instagram", "WhatsApp")
  )
# 3. Zmiana nazw kolumn na docelowe (mapowanie pod model MCDA)
colnames(social_media_data)[colnames(social_media_data) == "Student_ID"] <- "studentID"
colnames(social_media_data)[colnames(social_media_data) == "Most_Used_Platform"] <- "most_used_platform"
colnames(social_media_data)[colnames(social_media_data) == "Avg_Daily_Usage_Hours"] <- "avg_daily_usage"
colnames(social_media_data)[colnames(social_media_data) == "Mental_Health_Score"] <- "mental_health"
colnames(social_media_data)[colnames(social_media_data) == "Addicted_Score"] <- "addicted_score"
colnames(social_media_data)[colnames(social_media_data) == "Sleep_Hours_Per_Night"] <- "sleep"
colnames(social_media_data)[colnames(social_media_data) == "Conflicts_Over_Social_Media"] <- "conflicts"

# 4. Selekcja kolumn końcowych
social_media_data <- social_media_data[, c(
  "studentID",
  "most_used_platform",
  "avg_daily_usage",
  "mental_health",
  "addicted_score",
  "sleep",
  "conflicts"
)]

# 5. Zapisanie danych w strukturze pakietu (data/social_media_data.rda)
usethis::use_data(social_media_data, overwrite = TRUE)

# Komunikat potwierdzający (do logów kompilacji)
message("---")
message("Sukces! Obiekt 'social_media_data' został zaktualizowany.")
message("Grupa docelowa: Studenci (Undergraduate/Graduate)")
message("Platformy: Facebook, TikTok, Instagram, YouTube, WhatsApp")
message("Liczba wierszy: ", nrow(social_media_data))

