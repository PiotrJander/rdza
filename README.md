# rdza

Język imperatywny
Język imperatywny, najlepiej w składni opartej o Pascala lub C.
Na 8 punktów
1. Jeden typ wartości, np. int.
=> tak
2. Zmienne, operacja przypisania.
=> tak
3. if.
=> tak
4. while lub goto.
=> tak, while
5. Wyrażenia z arytmetyką + - * / ( ).
=> tak
6. Porównania (dopuszczalne tylko w warunkach lub z interpretacją liczbową 0/1 jak w C).
=> porownania z uzyciem wartości logicznych
Wykonanie może polegać na wykonaniu ciągu instrukcji i wypisaniu stanu końcowego.
Na 12 punktów
J.w., a dodatkowo:
7. Funkcje lub procedury z parametrami przez wartość, rekurencja.
=> tak
Na 16 punktów
1. Co najmniej dwa typy wartości w wyrażeniach: int i bool
=> tak
2. Arytmetyka, porównania.
=> tak
3. while, if (z else i bez, może być też składnia if _ elif _ else _ endif).
=> tak
4. Funkcje lub procedury (bez zagnieżdżania), rekurencja.
=> tak
5. Jawne wypisywanie wartości na wyjście (instrukcja lub wbudowana procedura print).
=> tak
6. Dwie wybrane rzeczy z poniższej listy lub coś o porównywalnej trudności:
a) dwa sposoby przekazywania parametrów (przez zmienną / przez wartość),
=> tylko przez wartość
b) pętla for w stylu Pascala,
=> nie
c) typ string, literały napisowe, wbudowane funkcje pozwalające na rzutowanie między
napisami a liczbami,
=> tak
d) wyrażenia z efektami ubocznymi (przypisania, operatory języka C ++, += itd).
=> nie
Na 20 punktów
J.w., a ponadto:
1. Przesłanianie identyfikatorów ze statycznym ich wiązaniem (np. zmienne globalne i lokalne w
funkcjach lub lokalne w blokach).
=> nie
2. Statyczne typowanie (tj. zawsze terminująca faza kontroli typów przed rozpoczęciem wykonania
programu). (Wymaganie nie dotyczy nietrywialnych projektów, w których dynamiczne
typowanie jest istotną cechą wybranego języka, np. Smalltalk, JavaScript).
=> tak, za wyjątkiem wczesnego return, ktory nie jest sprawdzany statycznie
3. Jawnie obsłużone dynamiczne błędy wykonania, np. dzielenie przez zero.
=> tak
4. Funkcje zwracające wartość (tzn. nie tylko procedury; za to mogą być tylko funkcje – jak w C).
=> tak
5. Dwie dodatkowe rzeczy z poniższej listy lub coś o porównywalnej trudności:
=> brak; funkcje teoretycznie mogłyby być zwracane i brane jako argumenty,
    ale nie zaimplementowałem składni dla funkcji zagniezdzonych
    i anonimowych
a) rekordy,
b) tablice indeksowane int lub coś à la listy,
c) tablice/słowniki indeksowane dowolnymi porównywalnymi wartościami; typ klucza należy
uwzględnić w typie słownika,
d) dowolnie zagnieżdżone krotki z przypisaniem jak w Pythonie (składnia wedle uznania),
e) funkcje jako parametry,
f) zwracanie funkcji w wyniku, domknięcia à la JavaScript.
g) funkcje anonimowe (szczególnie sensowne w połączeniu z punktem e).