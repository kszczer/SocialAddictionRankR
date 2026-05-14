#' @title Teoria Dominacji dla Rankingu (VIKOR & TOPSIS)
#' @description
#' Funkcja pomocnicza. Wyznacza ranking konsensusu na podstawie reguły większości dla dwóch metod.
#' Iteracyjnie sprawdza, która alternatywa najczęściej wygrywa na danej pozycji.
#'
#' @param r1 Wektor numeryczny rang metody 1.
#' @param r2 Wektor numeryczny rang metody 2.
#' @return Wektor numeryczny z finalnym rankingiem.
#' @keywords internal
.oblicz_ranking_dominacji <- function(r1, r2) {
  n <- length(r1)
  finalny_ranking <- rep(0, n)

  # Macierz rang (wiersze = alternatywy, kolumny = metody)
  macierz_rang <- cbind(r1, r2)

  # Maska dostepnych alternatyw (na początku wszystkie są dostępne)
  dostepne <- rep(TRUE, n)

  for (obecna_pozycja in 1:n) {
    # Pobieramy rangi tylko dla dostepnych alternatyw (reszte zamieniamy na Inf)
    obecna_macierz <- macierz_rang
    obecna_macierz[!dostepne, ] <- Inf

    # Kto ma najlepszą (najniższą) rangę w każdej metodzie?
    najlepszy_r1 <- which.min(obecna_macierz[, 1])
    najlepszy_r2 <- which.min(obecna_macierz[, 2])

    kandydaci <- c(najlepszy_r1, najlepszy_r2)

    # Głosowanie większościowe
    tabela_czestosci <- table(kandydaci)

    if (length(tabela_czestosci) == 1) {
      # Zgoda obu metod
      zwyciezca_idx <- as.numeric(names(tabela_czestosci))
    } else {
      # Remis (każda metoda wskazała kogo innego) - Head-to-Head
      c1 <- najlepszy_r1
      c2 <- najlepszy_r2

      # Kto był częściej lepszy od kogo w bezpośrednim starciu?
      if (sum(macierz_rang[c1, ] < macierz_rang[c2, ]) >= sum(macierz_rang[c2, ] < macierz_rang[c1, ])) {
        zwyciezca_idx <- c1
      } else {
        zwyciezca_idx <- c2
      }
    }

    # Przypisz pozycje i oznacz jako niedostepnego
    finalny_ranking[zwyciezca_idx] <- obecna_pozycja
    dostepne[zwyciezca_idx] <- FALSE
  }

  return(finalny_ranking)
}

#' @title Rozmyty Meta-Ranking (VIKOR & TOPSIS)
#' @description
#' Agreguje wyniki z metod Fuzzy VIKOR oraz TOPSIS, aby stworzyć
#' jeden, robustny ranking konsensusu.
#'
#' @param macierz_decyzyjna Rozmyta macierz danych.
#' @param typy_kryteriow Wektor typów ("min", "max").
#' @param wagi (Opcjonalnie) Wagi kryteriów.
#' @param bwm_najlepsze (Opcjonalnie) Wektor BWM Best-to-Others.
#' @param bwm_najgorsze (Opcjonalnie) Wektor BWM Others-to-Worst.
#' @param v Parametr dla VIKOR (domyślnie 0.5).
#'
#' @return Lista zawierająca ramkę danych z porównaniem rankingów oraz macierz korelacji.
#' @importFrom RankAggreg BruteAggreg RankAggreg
#' @importFrom stats cor
#' @export
rozmyty_meta_ranking <- function(macierz_decyzyjna,
                                 typy_kryteriow,
                                 wagi = NULL,
                                 bwm_najlepsze = NULL,
                                 bwm_najgorsze = NULL,
                                 v = 0.5) {

  # 1. Sprawdzenie wag
  if (is.null(wagi) && (is.null(bwm_najlepsze) || is.null(bwm_najgorsze))) {
    message("Brak wag i parametrów BWM. Obliczam wagi metodą Entropii...")
    wagi_surowe <- oblicz_wagi_entropii(macierz_decyzyjna)
    wagi <- rep(wagi_surowe, each = 3)
  }

  # 2. Uruchomienie metod (VIKOR i TOPSIS)
  args_baza <- list(macierz_decyzyjna = macierz_decyzyjna, typy_kryteriow = typy_kryteriow)
  if (!is.null(wagi)) args_baza$wagi <- wagi
  if (!is.null(bwm_najlepsze)) {
    args_baza$bwm_najlepsze <- bwm_najlepsze
    args_baza$bwm_najgorsze <- bwm_najgorsze
    args_baza$bwm_kryteria <- attr(macierz_decyzyjna, "nazwy_kryteriow")
  }

  # Obliczenia
  res_vikor <- do.call(rozmyty_vikor, c(args_baza, list(v = v)))
  res_topsis <- do.call(rozmyty_topsis, args_baza)

  # 3. Ekstrakcja Rankingów
  r_vikor <- res_vikor$wyniki$Ranking
  r_topsis <- res_topsis$wyniki$Ranking

  # 4. Agregacja Rankingów

  # A. Suma Rang
  suma_pkt <- r_vikor + r_topsis
  ranking_suma <- rank(suma_pkt, ties.method = "first")

  # B. Teoria Dominacji (poprawiona na 2 metody)
  ranking_dominacja <- .oblicz_ranking_dominacji(r_vikor, r_topsis)

  # C. RankAggreg
  macierz_dla_ra <- rbind(order(r_vikor), order(r_topsis))
  n_alt <- nrow(macierz_decyzyjna)

  if (n_alt <= 10) {
    ra_wynik <- RankAggreg::BruteAggreg(macierz_dla_ra, n_alt, distance = "Spearman")
  } else {
    ra_wynik <- RankAggreg::RankAggreg(macierz_dla_ra, n_alt, method = "GA", distance = "Spearman", verbose = FALSE)
  }

  # Konwersja RankAggreg na wektor rang
  top_lista <- ra_wynik$top.list
  wektor_ra <- numeric(n_alt)
  for(pozycja in 1:n_alt) {
    indeks_alternatywy <- as.numeric(top_lista[pozycja])
    wektor_ra[indeks_alternatywy] <- pozycja
  }

  # 5. Zestawienie wyników
  porownanie_df <- data.frame(
    Alternatywa = rownames(macierz_decyzyjna),
    R_VIKOR = r_vikor,
    R_TOPSIS = r_topsis,
    Meta_Suma = ranking_suma,
    Meta_Dominacja = ranking_dominacja,
    Meta_Agregacja = wektor_ra
  )

  # Macierz korelacji
  macierz_kor <- cor(porownanie_df[,-1], method = "spearman")

  return(list(
    porownanie = porownanie_df,
    korelacje = macierz_kor,
    detale_vikor = res_vikor,
    detale_topsis = res_topsis
  ))
}
