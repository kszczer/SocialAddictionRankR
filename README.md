SocialAddictionRankR
================

# SocialAddictionRankR

Pakiet SocialAddictionRankR to specjalistyczne narzędzie do
wielokryterialnej analizy decyzyjnej (MCDA) w środowisku rozmytym,
zaprojektowane do badania wpływu mediów społecznościowych na
użytkowników.

## Umożliwia realizację pełnej ścieżki analitycznej: od surowych danych, przez agregację zmiennych ukrytych, aż po zaawansowane rankingi oparte na trójkątnych liczbach rozmytych (TFN).

\#Instalacja Możesz zainstalować wersję deweloperską bezpośrednio z
serwisu GitHub:

``` r
# Instalacja narzędzi deweloperskich (jeśli nie są zainstalowane)
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Instalacja pakietu
devtools::install_github("kszczer/SocialAddictionRankR")
#> xfun       (0.57     -> 0.58  ) [CRAN]
#> openssl    (2.4.0    -> 2.4.1 ) [CRAN]
#> gdtools    (0.5.0    -> 0.5.1 ) [CRAN]
#> data.table (1.18.2.1 -> 1.18.4) [CRAN]
#> package 'xfun' successfully unpacked and MD5 sums checked
#> package 'openssl' successfully unpacked and MD5 sums checked
#> package 'gdtools' successfully unpacked and MD5 sums checked
#> package 'data.table' successfully unpacked and MD5 sums checked
#> 
#> Pobrane pakiety binarne są w
#>  C:\Users\Użytkownik\AppData\Local\Temp\RtmpcHsqup\downloaded_packages
#> ── R CMD build ─────────────────────────────────────────────────────────────────
#>          checking for file 'C:\Users\Użytkownik\AppData\Local\Temp\RtmpcHsqup\remotesc3a82d604e34\kszczer-SocialAddictionRankR-672c1fa/DESCRIPTION' ...     checking for file 'C:\Users\Użytkownik\AppData\Local\Temp\RtmpcHsqup\remotesc3a82d604e34\kszczer-SocialAddictionRankR-672c1fa/DESCRIPTION' ...   ✔  checking for file 'C:\Users\Użytkownik\AppData\Local\Temp\RtmpcHsqup\remotesc3a82d604e34\kszczer-SocialAddictionRankR-672c1fa/DESCRIPTION' (824ms)
#>       ─  preparing 'SocialMediaAddictionRankR':
#>    checking DESCRIPTION meta-information ...     checking DESCRIPTION meta-information ...   ✔  checking DESCRIPTION meta-information
#>   Ostrzeżenie:     Ostrzeżenie: C:/Users/Użytkownik/AppData/Local/Temp/Rtmp2BPEZO/Rbuildb3a479663ca4/SocialMediaAddictionRankR/man/rozmyty_topsis.Rd:26: unknown macro '\times'
#>   Ostrzeżenie:     Ostrzeżenie: C:/Users/Użytkownik/AppData/Local/Temp/Rtmp2BPEZO/Rbuildb3a479663ca4/SocialMediaAddictionRankR/man/rozmyty_vikor.Rd:28: unknown macro '\times'
#>       ─  checking for LF line-endings in source and make files and shell scripts (416ms)
#>       ─  checking for empty or unneeded directories
#>   Ostrzeżenie:     Ostrzeżenie: C:/Users/Użytkownik/AppData/Local/Temp/Rtmp2BPEZO/Rbuildb3a479663ca4/SocialMediaAddictionRankR/man/rozmyty_topsis.Rd:26: unknown macro '\times'
#>   Ostrzeżenie:     Ostrzeżenie: C:/Users/Użytkownik/AppData/Local/Temp/Rtmp2BPEZO/Rbuildb3a479663ca4/SocialMediaAddictionRankR/man/rozmyty_vikor.Rd:28: unknown macro '\times'
#>       ─  building 'SocialMediaAddictionRankR_0.1.0.tar.gz'
#>      
#> 
```

\#Szybki Start

Oto podstawowy przykład użycia pakietu z wykorzystaniem wbudowanych
danych.

\#1. Przygotowanie macierzy rozmytej Pakiet automatycznie agreguje
zmienne surowe i rozmywa je w skali 1-9.

``` r
library(SocialAddictionRankR)
data("social_media_data")

# Definicja modelu (składnia wzorowana na SEM)
skladnia <- "
  Uzytkowanie =~ avg_daily_usage; 
  Dobrostan   =~ mental_health + sleep; 
  Negatywy    =~ addicted_score + conflicts
"

macierz_rozmyta <- przygotuj_dane_mcda(
  dane = social_media_data,
  skladnia = skladnia,
  kolumna_alternatyw = "most_used_platform"
)
```

\#2. Wyznaczanie wag (Metoda BWM) Wyznaczamy wagę każdego kryterium na
podstawie preferencji eksperta.

``` r
nazwy <- c("Uzytkowanie", "Dobrostan", "Negatywy")

# Dobrostan (najlepsze), Uzytkowanie (najgorsze)
bwm_best <- c(4, 1, 2)
bwm_worst <- c(1, 4, 3)

wagi_sm <- oblicz_wagi_bwm(
  nazwy_kryteriow = nazwy,
  najlepsze_do_innych = bwm_best,
  inne_do_najgorszego = bwm_worst
)
```

\#3. Ranking końcowy (Meta-Ranking)

Agregujemy wyniki z trzech metod, aby uzyskać najbardziej stabilny
ranking platform.

``` r
typy <- c("min", "max", "min")

meta_wynik <- rozmyty_meta_ranking(
  macierz_decyzyjna = macierz_rozmyta,
  typy_kryteriow = typy,
  bwm_najlepsze = bwm_best,
  bwm_najgorsze = bwm_worst
)
```

# Tabela porównawcza

knitr::kable(meta_wynik\$porownanie, caption = “Zestawienie wyników i
ostateczny Meta-Ranking”)

\#Dokumentacja

Pełny poradnik krok-po-kroku dostępny jest w winiecie pakietu:

``` r
vignette("poradnik_mcda", package = "SocialMediaAddictionRankR")
```
