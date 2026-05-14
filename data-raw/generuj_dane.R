# --- PRZYGOTOWANIE DANYCH DLA PAKIETU SocialMediaAddictionRankR ---
# Cel: Stworzenie zbalansowanego zbioru danych do analizy MCDA (N=200)

# 1. Wczytujemy dane surowe
# Ścieżka dostosowana do struktury projektu R (data-raw/)
if (!file.exists("data-raw/Students_Social_Media_Addiction.csv")) {
  stop("Nie znaleziono pliku w 'data-raw/'. Sprawdź ścieżkę!")
}

raw_data <- read.csv("data-raw/Students_Social_Media_Addiction.csv")

# 2. Filtrowanie wstępne
# Wybieramy tylko studentów oraz konkretne platformy
social_media_data <- raw_data |>
  subset(
    Academic_Level %in% c("Graduate", "Undergraduate") &
      Most_Used_Platform %in% c("Facebook", "TikTok", "Instagram", "WhatsApp")
  )

# 3. Stratified Sampling (Losowanie warstwowe)
# Wybieramy dokładnie 50 respondentów dla każdej platformy
set.seed(123) # Gwarantuje powtarzalność wyników w pracy licencjackiej

social_media_data <- do.call(rbind, lapply(split(social_media_data, social_media_data$Most_Used_Platform), function(x) {
  if(nrow(x) < 50) {
    stop(paste("Błąd: Platforma", unique(x$Most_Used_Platform), "ma mniej niż 50 rekordów!"))
  }
  # Losowanie bez zwracania
  x[sample(1:nrow(x), 50), ]
}))

# 4. Zmiana nazw kolumn na docelowe (mapowanie pod model MCDA)
# Dostosowanie nazw z Kaggle do nazw wymaganych przez funkcje pakietu
colnames(social_media_data)[colnames(social_media_data) == "Student_ID"] <- "studentID"
colnames(social_media_data)[colnames(social_media_data) == "Most_Used_Platform"] <- "most_used_platform"
colnames(social_media_data)[colnames(social_media_data) == "Avg_Daily_Usage_Hours"] <- "avg_daily_usage"
colnames(social_media_data)[colnames(social_media_data) == "Mental_Health_Score"] <- "mental_health"
colnames(social_media_data)[colnames(social_media_data) == "Addicted_Score"] <- "addicted_score"
colnames(social_media_data)[colnames(social_media_data) == "Sleep_Hours_Per_Night"] <- "sleep"
colnames(social_media_data)[colnames(social_media_data) == "Conflicts_Over_Social_Media"] <- "conflicts"

# 5. Selekcja kolumn końcowych
# Usuwamy niepotrzebne zmienne demograficzne, zostawiamy te do analizy
social_media_data <- social_media_data[, c(
  "studentID",
  "most_used_platform",
  "avg_daily_usage",
  "mental_health",
  "addicted_score",
  "sleep",
  "conflicts"
)]

# 6. Czyszczenie poziomów czynnika (opcjonalne, porządkuje dane)
social_media_data$most_used_platform <- as.character(social_media_data$most_used_platform)

# 7. Zapisanie danych w strukturze pakietu (data/social_media_data.rda)
# Dzięki temu dane będą dostępne po wydaniu komendy data("social_media_data")
usethis::use_data(social_media_data, overwrite = TRUE)

# --- KOMUNIKATY KONTROLNE ---
message("---")
message("Sukces! Obiekt 'social_media_data' został poprawnie przygotowany.")
message("Metodologia: Losowanie warstwowe (Stratified Sampling), N=50 na platformę.")
message("Łączna liczba obserwacji: ", nrow(social_media_data))
message("Platformy w zbiorze: ", paste(unique(social_media_data$most_used_platform), collapse = ", "))
message("---")

