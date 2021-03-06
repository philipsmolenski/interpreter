Uruchamianie:
Aby skompilować interpreter należy użyć polecenia make. Jeżeli skompilować interpreter na maszynie students, należy zmienić następujące 3 linie w pliku Makefile:
bnfc --haskell -p Big Big.cf  --->  /home/students/inf/PUBLIC/MRJP/bin/students/bnfc --haskell -p Big Big.cf
ghc --make $< -o $@           --->  /home/students/inf/PUBLIC/MRJP/ghc-8.2.2/bin/ghc --make $< -o $@
ghc --make $< -o interpreter  --->  /home/students/inf/PUBLIC/MRJP/ghc-8.2.2/bin/ghc --make $< -o interpreter

oraz wykonać polecenia:
cabal update
cabal install mtl
cabal install transformers

Opis rozwiązania:

Rozwiązanie składa się z następujących modułów:

1.Main.hs - główny moduł uruchamiający interpreter

2.Interpreter.hs - moduł interpretujący instrukcje języka. Używa on modnady InterpreterMonad, która składa się z:

-Monady reader przechowującej środowisko (a w tym środowisku pamiętane są mapowania identyfikatorów na miejsca zmiennych w pamięci, funkcje i wyjątki oraz mapowanie sprawdzające, czy dany identyfikator reprezentuje zmienną read-only).

-Monady state przechowującej stan programu (a w stanie pamiętane jest mapowanie miejsc zmiennych w pamięci na wartości oraz stan deklaracji funkcji, tzn. czy włączone są deklaracje dynamiczne, czy statyczne, patrz dodatek2).

-Monady Either, obsługującej błędy programu

-Monady IO, wypisującej komunikaty na wyjście

3.EvalExpr.hs - moduł obliczający wartości wyrażeń (operacje arytmetyczne, porównania itd.) za pomocą opisanej wyżej monady.

4.TypeCheck.hs - moduł obsługujący statyczną kontrolę typów (i parę innych błędów łatwych do wykrycia statycznie) przed wykonaniem programu. Została w nim użyta monada TCM, w której skład wchodzi:

-Monada reader przechowująca mapowania zmiennych na ich typy, wyjątków na fakt, czy zostały one zadeklarowane oraz funkcji na ich typ oraz typy ich argumentów.

-Monada Either, obsługująca błędy

-Monada IO, wypisująca komunikaty na wyjście

Statyczna kontrola typów zawsze terminuje. Dzieje się tak, bo ciała pętli for i while są wykonywane raz, funkcje statyczne są sprawdzane w momencie deklaracji, a funkcjie dynamiczne są sprawdzane do głębokości rekurencji 2 (wyszło mi, że jeśli wszystkie odgałęzienia ciała funkcji się dobrze typują po dwóch zejściach w rekurencji, to przy kolejnych też będą się typowały dobrze).

5.DataStructures.hs - moduł przechowujący wszystkie struktury danych użyte w rozwiązaniu.

W rozwiązaniu zrealizowano wszystkie funkcjonalności zadeklarowane w deklaracja.txt oprócz tablic, to znaczy:

Na 15 punktów
01 (trzy typy)
02 (literały, arytmetyka, porównania)
03 (zmienne, przypisanie)
04 (print)
05 (while, if)
06 (funkcje lub procedury, rekurencja)
07 (przez zmienną i przez wartość)
Na 20 punktów
09 (przesłanianie i statyczne wiązanie)
10 (obsługa błędów wykonania)
11 (funkcje zwracające wartość)
Na 30 punktów
12 (4) (statyczne typowanie)
13 (2) (funkcje zagnieżdżone ze statycznym wiązaniem)
16 (1) (break, continue)
ponadto:
(1) drugi z pary punktów 7-8, czyli w tym wypadku zmienne read-only + pętla for
(2) opcja dynamic on / dynamic off <- ale to czy obecnie deklarujemy w trybie statycznym czy dynamicznym decyduje się w sposób "środowiskowy" a nie "stanowy" (zob. good/dodatek1-dynamiczne-deklaracje.prog
(2) argumenty z defaultowymi wartościami <- to chyba miało nie być punktowane (zgodnie z odpowiedzą na deklarację) ale napisałem to przed tą odpowiedzią i już zostawiłem.
(3) wyjątki z wagami

Razem więc zrealizowane podpunkty za 32 punkty: zaoferowane 33 - 1 za brak tablic
