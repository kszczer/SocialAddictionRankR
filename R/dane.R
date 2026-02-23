#' Dane dotyczące użycia mediów społecznościowych przez studentów (MCDA)
#'
#' Zbiór danych zawierający rzeczywiste rekordy (ograniczone do 70) dotyczące
#' wpływu najpopularniejszych platform społecznościowych na życie studentów.
#' Dane obejmują 5 głównych alternatyw (platform) oraz kryteria zdrowotne,
#' czasowe i społeczne. Przeznaczony do testowania metod MCDA.
#'
#' @format Ramka danych (data frame) z 70 wierszami i 7 zmiennymi:
#' \describe{
#'   \item{studentID}{Unikalny identyfikator studenta (z pliku źródłowego)}
#'   \item{most_used_platform}{Alternatywa podlegająca ocenie (Facebook, TikTok, Instagram, YouTube, Twitter)}
#'   \item{avg_daily_usage}{Średni dzienny czas spędzony na platformie (w godzinach)}
#'   \item{mental_health}{Subiektywna ocena zdrowia psychicznego (skala 1-10, gdzie 10 to stan najlepszy)}
#'   \item{addicted_score}{Wskaźnik poziomu uzależnienia (skala 1-10)}
#'   \item{sleep}{Średnia liczba godzin snu na dobę}
#'   \item{conflicts}{Liczba konfliktów interpersonalnych wynikających z użycia social mediów (0-10)}
#' }
#'
#' @details
#' Dane zostały przefiltrowane z większego zbioru "Students Social Media Addiction",
#' aby skupić się na pięciu najpopularniejszych platformach. Zbiór idealnie nadaje się
#' do analizy wielokryterialnej, gdzie alternatywami są platformy, a kryteriami
#' zmienne wpływające na dobrostan studenta.
#'
#' @usage data(social_media_data)
#' @name social_media_data
NULL
