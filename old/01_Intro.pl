% FAKTEN
animal(dog).
animal(cat).
dog(baru).
cat(kimba).
cat(bobby).
cat(marly).
cat(garfield).

% Fakten mit Beziehungen
mother(kimba, bobby).
mother(kimba, marly).
mother(marly, garfield).

% REGEL mit einem Regeloperator ":-", gesprochen: "folgt aus ..." (umgedrehte Implikation).
% X ist Großmutter von Z, wenn es eine Mutter Y dazwischen gibt.
% "," definiert eine Konjunktur (Schnittmenge, "und").
grandmother(X,Z) :-
    mother(X,Y),
    mother(Y,Z).
% Head/Konsequenz folgt aus der Bedingung 1 UND der Bedingung 2.

% Oder-Verknüpfung  
four_legs(X) :-
    dog(X); 
    cat(X).

% Logisches UND: ","
% Logisches ODER: ";"

% Vergleiche:
%% > größer (numerisch).
%% < kleiner (numerisch).
%% >= größer gleich (numerisch).
%% =< kleiner gleich (numerisch). "=" kommt immer auf die Seite des zeigenden Brackets.
%% =:= gleich (numerisch). Nicht "=="!
%% =\= ungleich (numerisch). 

%% == Übereinstimmende Muster.
%% \== Nicht übereinstimmende Muster.

% Mathematische Operationen:
%% + - * / mod
%% Zuweisung mit "is"


% Listen:
% Rekursive Datenstrukturen mit Head (Kopf) und Tail (Rest). Der Rest kann auch wieder aus Listen bestehen.
% Head ist immer der erste Eintrag, Tail ist der Rest.
% [1, 2] % Head Zahl 1, Tail Liste 2
%['one', 'two'] % Head Zeichenkette 'one', Tail Liste 'two'
% [1, 'two', 3] % Head Zahl 1, Tail Liste ['two', 3]

member(X, [X|_]).
member(X, [_|T]):-
    member(X, T).

% member(2, ['anton', 'berta', 'caesar']).

% Queries:
%% mother(kimba, charles). -> false
%% mother(kimba, marly). -> true

%% grandmother(kimba, garfield). -> true
%% grandmother(kimba, marly). -> false

%% cat(baru); dog(baru). -> true
%% four_legs(baru). -> true

%% Mathematische Operationen:
%% 1 =:= (3-2) -> true





