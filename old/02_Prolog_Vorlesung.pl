brother(Person, Bruder) :- parent(Elternteil, Person), parent(Elternteil, Bruder), female(Bruder).
sister(Person, Schwester) :- parent(Elternteil, Person), parent(Elternteil, Schwester), female(Schwester).
aunt(Person, Tante) :- parent(Elternteil, Person), sister(Elternteil, Person).

has_siblings(Person) :- sister(Person, Schwester); brother(Person, Bruder). %?