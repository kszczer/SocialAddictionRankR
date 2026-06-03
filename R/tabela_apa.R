#' Generowanie tabeli w standardzie APA
#'
#' @description Konwertuje wyniki zbiorcze z procedury rozmytego meta-rankingu
#' do sformatowanej tabeli zgodnej z wymogami manuala APA (7th edition), wzorem z pracy
#' oraz czcionką Times New Roman.
#'
#' @param meta_wynik Obiekt listy zwracany przez funkcję `rozmyty_meta_ranking`.
#' @return Obiekt klasy `flextable` sformatowany zgodnie z zasadami APA.
#' @importFrom flextable flextable theme_apa autofit add_footer_lines align as_paragraph as_chunk font
#' @importFrom officer fp_text
#' @export
tabela_apa <- function(meta_wynik) {

  if (is.null(meta_wynik$porownanie)) {
    stop("Przekazany obiekt nie zawiera ramki '$porownanie'.")
  }

  # Pobranie danych do tabeli
  dane_tab <- meta_wynik$porownanie

  # Mapowanie nazw kolumn z kodu źródłowego na dokładne nazwy z Twojego obrazka
  nowe_nazwy <- c(
    "Alternatywa"     = "Alternatywa",
    "Ranking_VIKOR"   = "R VIKOR",
    "Ranking_TOPSIS"  = "R TOPSIS",
    "Meta_Suma"       = "Meta Suma",
    "Meta_Dominacja"  = "Meta Dominacja",
    "Meta_Agregacja"  = "Meta Agregacja"
  )

  # Automatyczne dopasowanie nazw istniejących kolumn
  istniejace_kolumny <- names(dane_tab)
  for (i in seq_along(istniejace_kolumny)) {
    stara_nazwa <- istniejace_kolumny[i]
    if (stara_nazwa %in% names(nowe_nazwy)) {
      names(dane_tab)[i] <- nowe_nazwy[stara_nazwa]
    }
  }

  # Tworzenie obiektu flextable
  tab_flextable <- flextable::flextable(dane_tab)

  # Nałożenie standardowego, czystego motywu APA (linie poziome góra/dół)
  tab_flextable <- flextable::theme_apa(tab_flextable)

  # Wyrównanie zawartości do środka dla kolumn numerycznych (oprócz pierwszej)
  tab_flextable <- flextable::align(tab_flextable, j = 2:ncol(dane_tab), align = "center", part = "all")
  tab_flextable <- flextable::align(tab_flextable, j = 1, align = "left", part = "all")

  # Dodanie podpisu pod dolną linią: "Note." kursywą, reszta zwykłym tekstem
  # W fp_text również definiujemy Times New Roman dla spójności notatki
  tab_flextable <- flextable::add_footer_lines(
    tab_flextable,
    values = flextable::as_paragraph(
      flextable::as_chunk("Note. ", props = officer::fp_text(italic = TRUE, font.family = "Times New Roman")),
      flextable::as_chunk("Zestawienie rang uzyskanych metodami Fuzzy VIKOR i Fuzzy TOPSIS oraz ostateczny ranking konsensusu (Meta).", props = officer::fp_text(font.family = "Times New Roman"))
    )
  )

  # Ustawienie czcionki Times New Roman dla całej tabeli (nagłówki, body, stopka)
  tab_flextable <- flextable::font(tab_flextable, fontname = "Times New Roman", part = "all")

  # Automatyczne dopasowanie szerokości kolumn do tekstu
  tab_flextable <- flextable::autofit(tab_flextable)

  return(tab_flextable)
}
