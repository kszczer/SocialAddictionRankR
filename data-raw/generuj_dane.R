# --- PRZYGOTOWANIE DANYCH DLA PAKIETU ---

# 1. Wczytujemy dane surowe z pliku CSV
# Upewnij się, że plik znajduje się w Twoim folderze roboczym (Working Directory)
if (!file.exists("Students_Social_Media_Addiction.csv")) {
  stop("Nie znaleziono pliku 'Students_Social_Media_Addiction.csv'. Sprawdź ścieżkę!")
}

raw_data <- read.csv("data-raw/Students_Social_Media_Addiction.csv")

# 2. Filtrowanie i wstępna obróbka
# Wybieramy tylko interesujące nas platformy i ograniczamy do 70 rekordów
social_media_data <- raw_data |>
  subset(Most_Used_Platform %in% c("Facebook", "TikTok", "Instagram", "YouTube", "Twitter")) |>
  head(70)

# 3. Zmiana nazw kolumn na docelowe (zgodnie z Twoim schematem)
# Robimy to masowo, aby uniknąć pomyłek
colnames(social_media_data)[colnames(social_media_data) == "Student_ID"] <- "studentID"
colnames(social_media_data)[colnames(social_media_data) == "Most_Used_Platform"] <- "most_used_platform"
colnames(social_media_data)[colnames(social_media_data) == "Avg_Daily_Usage_Hours"] <- "avg_daily_usage"
colnames(social_media_data)[colnames(social_media_data) == "Mental_Health_Score"] <- "mental_health"
colnames(social_media_data)[colnames(social_media_data) == "Addicted_Score"] <- "addicted_score"
colnames(social_media_data)[colnames(social_media_data) == "Sleep_Hours_Per_Night"] <- "sleep"
colnames(social_media_data)[colnames(social_media_data) == "Conflicts_Over_Social_Media"] <- "conflicts"

# 4. Wybieramy tylko te kolumny, które mają wejść do zbioru danych
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
# Funkcja automatycznie skompresuje dane
usethis::use_data(social_media_data, overwrite = TRUE)

# Komunikat potwierdzający
message("---")
message("Sukces! Obiekt 'social_media_data' został utworzony.")
message("Liczba wierszy: ", nrow(social_media_data))
message("Kolumny: ", paste(colnames(social_media_data), collapse = ", "))

