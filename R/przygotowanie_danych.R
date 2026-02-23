#' @title Wewnętrzny parser składni MCDA
#' @description Funkcja pomocnicza do interpretowania modelu MCDA
#' zadanego jako ciąg znaków przez użytkownika.
#' Zamienia zapis typu "Kryterium =~ zmienna1 + zmienna2"
#' na listę mapującą kryteria na zmienne składowe.
#' @keywords internal
.parsuj_skladnie_mcda <- function(skladnia) {

  # Usuwamy znaki nowej linii
  czysta_skladnia <- gsub("\n", "", skladnia)

  # Dzielimy po średniku (oddzielne kryteria)
  linie <- strsplit(czysta_skladnia, ";")[[1]]
  mapowanie <- list()

  for (linia in linie) {
    if (trimws(linia) == "") next

    # Dzielimy wg operatora "=~"
    czesci <- strsplit(linia, "=~")[[1]]

    if (length(czesci) == 2) {
      nazwa_kryterium <- trimws(czesci[1])
      elementy <- trimws(strsplit(czesci[2], "\\+")[[1]])
      mapowanie[[nazwa_kryterium]] <- elementy
    }
  }

  return(mapowanie)
}

#' @title Wewnętrzny skaler do skali Saaty'ego (1–9)
#' @description Przekształca zmienne wejściowe
#' (ciągłe lub porządkowe, np. Likert 1–5)
#' na skalę porównań Saaty'ego 1–9.
#' @keywords internal
.skaluj_do_saaty <- function(wektor) {

  # Zabezpieczenie przed wartościami ujemnymi
  if (any(wektor < 0, na.rm = TRUE)) {
    stop("Wykryto wartości ujemne w danych wejściowych.")
  }

  # Obsługa braków danych i kodów błędów (np. 99)
  wektor[is.na(wektor) | wektor == 99] <- 0

  # Skalujemy tylko poprawne wartości (>0)
  maska <- wektor > 0
  wartosci <- wektor[maska]

  if (length(wartosci) == 0) return(wektor)

  min_v <- min(wartosci)
  max_v <- max(wartosci)

  if (min_v == max_v) {
    wektor[maska] <- 1
  } else {
    wektor[maska] <- 1 + (wartosci - min_v) * (8 / (max_v - min_v))
  }

  return(wektor)
}

#' @title Wewnętrzna funkcja rozmywająca (fuzzifier)
#' @description Zamienia wektor liczb rzeczywistych
#' na trójkątne liczby rozmyte (TFN).
#' Każda wartość x jest mapowana na (x-1, x, x+1).
#' @keywords internal
.rozmyj_wektor <- function(wektor) {

  l <- pmax(1, wektor - 1)
  m <- wektor
  u <- pmin(9, wektor + 1)

  # Zera (braki danych) pozostają zerami
  zerowe <- wektor == 0
  l[zerowe] <- 0
  m[zerowe] <- 0
  u[zerowe] <- 0

  return(cbind(l, m, u))
}

#' Przygotowanie danych do rozmytej analizy MCDA
#'
#' @description Funkcja przekształca surowe dane wejściowe
#' w rozmytą macierz decyzyjną MCDA.
#' Tworzy kryteria kompozytowe na podstawie składni,
#' skaluje dane do skali Saaty'ego (1–9),
#' agreguje opinie ekspertów (jeśli dotyczy)
#' oraz dokonuje rozmycia (fuzzification).
#'
#' @param dane Ramka danych (data frame) z danymi wejściowymi.
#' @param skladnia Ciąg znaków definiujący strukturę kryteriów,
#'   np. "Koszt =~ k1 + k2; Jakosc =~ j1 + j2".
#' @param kolumna_alternatyw Nazwa kolumny identyfikującej alternatywy
#'   (np. dostawców, typy ataków). Jeśli NULL, każdy wiersz
#'   traktowany jest jako osobna alternatywa.
#' @param funkcja_agregacji Funkcja agregująca oceny ekspertów
#'   (domyślnie mean).
#'
#' @return Rozmyta macierz decyzyjna MCDA
#' o wymiarach (m × 3n), gdzie m to liczba alternatyw,
#' a n to liczba kryteriów.
#'
#' @export
przygotuj_dane_mcda <- function(dane,
                                skladnia,
                                kolumna_alternatyw = NULL,
                                funkcja_agregacji = mean) {

  if (!is.data.frame(dane)) {
    stop("Argument 'dane' musi być ramką danych (data frame).")
  }

  # 1. Parsowanie składni kryteriów
  mapowanie <- .parsuj_skladnie_mcda(skladnia)
  nazwy_kryteriow <- names(mapowanie)

  # 2. Obliczanie wyników kompozytowych i skalowanie
  tmp <- data.frame(row_id = seq_len(nrow(dane)))

  for (kryt in nazwy_kryteriow) {
    zmienne <- mapowanie[[kryt]]

    brakujace <- zmienne[!zmienne %in% names(dane)]
    if (length(brakujace) > 0) {
      stop(paste(
        "Brakuje zmiennych w danych:",
        paste(brakujace, collapse = ", ")
      ))
    }

    if (length(zmienne) > 1) {
      surowy <- rowMeans(dane[, zmienne, drop = FALSE], na.rm = TRUE)
    } else {
      surowy <- dane[[zmienne]]
    }

    tmp[[kryt]] <- .skaluj_do_saaty(surowy)
  }

  # 3. Agregacja ekspertów do alternatyw
  if (!is.null(kolumna_alternatyw)) {

    if (!kolumna_alternatyw %in% names(dane)) {
      stop("Nie znaleziono kolumny alternatyw w danych.")
    }

    tmp$ID_Alternatywy <- dane[[kolumna_alternatyw]]

    zagregowane <- aggregate(
      . ~ ID_Alternatywy,
      data = tmp[, -1],
      FUN = funkcja_agregacji
    )

    zagregowane <- zagregowane[order(zagregowane$ID_Alternatywy), ]
    nazwy_wierszy <- zagregowane$ID_Alternatywy
    macierz <- as.matrix(zagregowane[, nazwy_kryteriow])

  } else {

    macierz <- as.matrix(tmp[, nazwy_kryteriow])
    nazwy_wierszy <- seq_len(nrow(macierz))
  }

  # 4. Rozmywanie (crisp → fuzzy)
  lista <- list()
  for (i in seq_along(nazwy_kryteriow)) {
    lista[[nazwy_kryteriow[i]]] <- .rozmyj_wektor(macierz[, i])
  }

  wynik <- do.call(cbind, lista)
  rownames(wynik) <- nazwy_wierszy
  attr(wynik, "nazwy_kryteriow") <- nazwy_kryteriow

  return(wynik)
}
