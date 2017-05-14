# rdza

notes:
1) we have static typechecking
2) but can't check early returns

simple:
* 

todo:
* funcje anonimowe, zwracanie funkcji, funkcje jako parametry, ale bez domknięcia
* wyjaśnić konflikty!
* lista: wymaganie -> przykład



simple examples:
early return
euclid alg
simple recursion: factorial
printing, strconcat
arithmetic
conditionals
arithmetic, boolean logic



Język imperatywny
Język imperatywny, najlepiej w składni opartej o Pascala lub C.
Na 8 punktów
1. Jeden typ wartości, np. int.
=> todo
2. Zmienne, operacja przypisania.
=> todo
3. if.
4. while lub goto.
5. Wyrażenia z arytmetyką + - * / ( ).
6. Porównania (dopuszczalne tylko w warunkach lub z interpretacją liczbową 0/1 jak w C).
Wykonanie może polegać na wykonaniu ciągu instrukcji i wypisaniu stanu końcowego.
Na 12 punktów
J.w., a dodatkowo:
7. Funkcje lub procedury z parametrami przez wartość, rekurencja.
Na 16 punktów
1. Co najmniej dwa typy wartości w wyrażeniach: int i bool
(to znaczy if 2+2 then _ parsuje się, ale wyrażenie ma niepoprawny typ).
2. Arytmetyka, porównania.
3. while, if (z else i bez, może być też składnia if _ elif _ else _ endif).
4. Funkcje lub procedury (bez zagnieżdżania), rekurencja.
5. Jawne wypisywanie wartości na wyjście (instrukcja lub wbudowana procedura print).
6. Dwie wybrane rzeczy z poniższej listy lub coś o porównywalnej trudności:
a) dwa sposoby przekazywania parametrów (przez zmienną / przez wartość),
b) pętla for w stylu Pascala,
c) typ string, literały napisowe, wbudowane funkcje pozwalające na rzutowanie między
napisami a liczbami,
d) wyrażenia z efektami ubocznymi (przypisania, operatory języka C ++, += itd).
Na 20 punktów
J.w., a ponadto:
1. Przesłanianie identyfikatorów ze statycznym ich wiązaniem (np. zmienne globalne i lokalne w
funkcjach lub lokalne w blokach).
2. Statyczne typowanie (tj. zawsze terminująca faza kontroli typów przed rozpoczęciem wykonania
programu). (Wymaganie nie dotyczy nietrywialnych projektów, w których dynamiczne
typowanie jest istotną cechą wybranego języka, np. Smalltalk, JavaScript).
3. Jawnie obsłużone dynamiczne błędy wykonania, np. dzielenie przez zero.
4. Funkcje zwracające wartość (tzn. nie tylko procedury; za to mogą być tylko funkcje – jak w C).
5. Dwie dodatkowe rzeczy z poniższej listy lub coś o porównywalnej trudności:
a) rekordy,
b) tablice indeksowane int lub coś à la listy,
c) tablice/słowniki indeksowane dowolnymi porównywalnymi wartościami; typ klucza należy
uwzględnić w typie słownika,
d) dowolnie zagnieżdżone krotki z przypisaniem jak w Pythonie (składnia wedle uznania),
e) funkcje jako parametry,
f) zwracanie funkcji w wyniku, domknięcia à la JavaScript.
g) funkcje anonimowe (szczególnie sensowne w połączeniu z punktem e).