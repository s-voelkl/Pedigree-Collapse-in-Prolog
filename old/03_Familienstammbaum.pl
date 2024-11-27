% Student exercise profile
:- set_prolog_flag(occurs_check, error).        % disallow cyclic terms
%:- set_prolog_stack(global, limit(8 000 000)).  % limit term space (8Mb)
%:- set_prolog_stack(local,  limit(2 000 000)).  % limit environment space

% ----- LIBRARIES -----
%:- use_module(library(clpfd)).
%  library clpfd for #= (replacing "is" -> more general), see here: swi-prolog.org/pldoc/man?predicate=%23%3D/2

% ----- IDEEN -----
% Inzest in Familien herausfinden. 
% Dozent schlug "Ahnenverlust" als Problem vor. de.wikipedia.org/wiki/Ahnenverlust
% z.B. Prozentualer Ahnenverlust
% Ahnenverlustkoeffizient: AVK = TatsöächlicheAhnen / InsgesamtMöglicheAhnen, in %
% 	hoher AVK -> wenig Ahnenverlust -> wenig Inzucht. Hoch ist gut.
% 	Problem: Wird in Wissenschaft nicht verwendet!
% Inzuchtkoeffizient: de.wikipedia.org/wiki/Inzuchtkoeffizient
% 	n[1]: Anzahl der Generationen vom Vater zum gemeinsamen Vorfahren
%	n[2]: Anzahl der Generationen von der Mutter zum gemeinsamen Vorfahren
% 	F[Ai]: Inzuchtkoeffizient des gemeinsamen Vorfahren

% https://en.wikipedia.org/wiki/Coefficient_of_inbreeding

% ----- FACTS -----
% Family Tree from this graphic: i.pinimg.com/originals/a6/9e/a7/a69ea7c6c262aeaa4058204c33fc533c.jpg

% GENDER
males([
      philip, charles, william, harry, george, 
      louis, archie_harrison, andrew, mark_p, 
      timothy_l, edward, peter_p, mike, james]).
females([elisabeth, diana, camilla_pb, kate_m, 
        meghan_m, charlotte,  sarah_f, eugenie, 
        beatrice, anne, autumn_p, zara_t, sophie_rj, 
        louise_w]).
diverses([]).

% PARENTS
% REDO? maybe add "married/not" as a status for getting additional information!
% structure: parents([List of all parents], [List of children of these parents]).

% 1st generation
parents([philip, elisabeth], [charles, andrew, edward, anne]).

% 2nd generation
parents([charles, diana], [william, harry]).
parents([charles, camilla_pb], []).
parents([andrew, sarah_f], [eugenie, beatrice]).
parents([mark_p, anne], [peter_p, zara_t]).
parents([anne, timothy_l], []).
parents([edward, sophie_rj], [louise_w, james]).

% 3rd generation
parents([william, kate_m], [george, charlotte, louis]).
parents([harry, maghan_m], [archie_harrison]).
parents([zara_t, mike_t], []).
parents([peter_p, autumn_p], []).

% REDO: incest testing fact:
parents([george, zara_t], [incest_child]).

% ----- RULES -----
% Derive atomar facts from the given fact lists by using rules
% TEST: female(diana) -> true
% TEST: parent(elisabeth, edward) -> true
% TEST: family_member(X) -> X = philip, charles, william, harry, ...
male(Person) :- males(List), member(Person, List).
female(Person) :- females(List), member(Person, List).
diverse(Person) :- diverses(List), member(Person, List).

parent(Parent, Child) :- 
    parents(Parents, Children), 
    member(Parent, Parents), 
    member(Child, Children).

family_member(Person) :- 
    males(Males), females(Females), diverses(Diverses),
    append(Males, Females, Temporary),
    append(Temporary, Diverses, Family),
    member(Person, Family).

% ----- DEFINE DIRECT RELATIONSHIPS -----
% TEST: father(philip, charles) -> true
% TEST: mother(X, charles) -> X=elisabeth
mother(Mother, Child) :- parent(Mother, Child), female(Mother).
father(Father, Child) :- parent(Father, Child), male(Father).
child(Child, Parent) :- parent(Parent, Child).

% ----- DEFINE INDIRECT RELATIONSHIPS ----- 
% TEST: grandfather(X, Y) -> X = philip, Y = william, ...
% TEST: uncle(X, george) -> X = harry
% TEST: sibling(charles, X) -> X = andrew, anne, edward
grandfather(Grandfather, Grandchild) :- 
    parent(Grandfather, Parent), 
    parent(Parent, Grandchild), 
    male(Grandfather).
grandmother(Grandmother, Grandchild) :- 
    parent(Grandmother, Parent), 
    parent(Parent, Grandchild), 
    female(Grandmother).

% for an setof-explanation, see: alsprolog.com/docs/ref/setof.html
sibling(Sibling, Person) :- 
    % get unique entries of the returning siblings by creating a set
    % ^ for not generating seperate lists, see here: 
    % cs.union.edu/~striegnk/learn-prolog-now/html/node98.html
    setof(
        % template:
        Sibling, 
        % goal (without seperate lists):
        Parent^(
          parent(Parent, Sibling), 
          parent(Parent, Person), 
          Sibling \== Person %inequality
    	), 
        % list output
        Siblings),
    % tries to find sibling in the list
    member(Sibling, Siblings).


uncle(Uncle, Person) :- 
    male(Uncle), 
    sibling(Uncle, ParentOfPerson), 
    parent(ParentOfPerson, Person).
aunt(Aunt, Person) :- 
    female(Aunt), 
    sibling(Aunt, ParentOfPerson), 
    parent(ParentOfPerson, Person).

% ----- DEFINE MORE COMPLICATED RELATIONSHIPS -----
% TEST: descendent(archie_harrison, elisabeth) -> true
% TEST: common_ancestor(X, george, zara_t) -> X = elisabeth, philip
% TEST: incest_child(X) -> X = incest_child

% compare a similar solution on StackOverflow
% Basisfall (falls Tiefensuche gescheitert ist)
descendent(Descendent, Person) :-
    child(Descendent, Person).
% Rekursion (ruft ggf. Basisfall auf, falls kein descendent gefunden wurde, siehe Tiefensuche)
descendent(Descendent, Person) :-
    child(Descendent, Parent),
    descendent(Parent, Person).
ancestor(Ancestor, Person) :- descendent(Person, Ancestor).

common_ancestor(CommonAncestor, Person1, Person2) :-
	setof(
        CommonAncestor, 
        (
        	ancestor(CommonAncestor, Person1),
            ancestor(CommonAncestor, Person2),
            Person1 \== Person2
        ), 
        CommonAncestors),
    member(CommonAncestor, CommonAncestors).

incest_child(Person) :-
    % use ^ParentN as existencial quantifier (as ParentN is not needed for the result).
    % equivalent to: "there is at least one parent for which applies ..."
    setof(CommonAncestor, ParentA^ParentB^(
        % get distinct parents of a child, then see if they have common ancestors.
        parent(ParentA, Person),
        parent(ParentB, Person),
        ParentA \== ParentB,
        common_ancestor(CommonAncestor, ParentA, ParentB)
    ), CommonAncestor).


% ----- DEFINE PEDIGREE COLLAPSE FUNCTIONS -----
% coefficient of imbreeding (COI) -> number measuring how imbred an individual is. Low is genetically better.
% Calculation: sum(0.5^(n-1) * (1 + fA)
%	n: number of inividuals
%	fA: COI of the common ancestor of the individual
% --> Value of the COI between 0 (genetically good) and 1 (clone to itself, only logarithmically possible)

% Base case: If a person does not have ancestors, the COI is 0, see here:
%   Wright, Sewall (1922), "Coefficients of Inbreeding and Relationship", The American Naturalist, vol. 56, page 333
%   ( journals.uchicago.edu/doi/10.1086/279872 )
% Other sources claim to create a virtual Person that takes the average of the COI of the same generation, see NOTABLE SOURCES below.
% "_" is used as a placeholder here.
coi(_, 0).

% Recursive calculation of the COI
coi(Person, COI) :-
    % get COI of parents. COI is 0 if base case, else between 0 and 1. (runs recursively until base case).
    % Note: parents are not "mother" and "father", but taken here without a designated gender for free gender compatability.
    parent(Parent1, Person),
    parent(Parent2, Person),
    Parent1 \== Parent2,
    
    % Ensure unique results by using setof %redo
    %setof(COIParent1, coi(Parent1, COIParent1), [COIParent1]),
    %setof(COIParent2, coi(Parent2, COIParent2), [COIParent2]),
    coi(Parent1, COIParent1),
    coi(Parent2, COIParent2),
    
    % calculate the coefficient
    common_ancestor_coi(Parent1, Parent2, CommonAncestorCOI),
    
    % "is" is used for numeric equality (compare to "=" that only checks the structure), see here:
   	%	stackoverflow.com/questions/16027449/what-is-the-difference-between-is-and
   
    COI is (COIParent1 + COIParent2) / 2 + (1 + CommonAncestorCOI) / 2.
   	% solved problem with everybody having a COI of 0.5 ???
    %COI is (COIParent1 + COIParent2) / 2 + CommonAncestorCOI.


% maybe test with just random values like 0.12 and try to generate outputs step by step this way.

% START OF TESTING -----
% Calculate the common ancestor coefficient
common_ancestor_coi(Parent1, Parent2, CommonAncestorCOI) :-
    findall(Ancestor, (
            	ancestor(Ancestor, Parent1), 
            	ancestor(Ancestor, Parent2)%, 
            	%Parent1 \== Parent2
                ), 
            CommonAncestors),
    list_sum(COIs, CommonAncestorCOI).

% Calculate coefficients for each common ancestor
calc_common_ancestor_coi([], []).
calc_common_ancestor_coi([Ancestor|Rest], [COI|COIs]) :-
    coi(Ancestor, COIAncestor),
    COI is 0.5 * (1 + COIAncestor), % bad solution
    calc_common_ancestor_coi(Rest, COIs).
% END OF TESTING -----




% Sum a list of numbers using arithmetic operations.
%  Answer based on StackOverflow here: stackoverflow.com/questions/11520621/how-do-i-get-the-sum-of-given-numbers-in-prolog
list_sum(List, Sum) :-
    list_sum(List, 0, Sum).
% Note the Accumulator Acc for optimizing the recursion calls onto the list, fastening the process.
list_sum([], Acc, Acc).
list_sum([Head|Tail], Acc, Result) :-
    NewAcc is Acc + Head,
    list_sum(Tail, NewAcc, Result).



% DEFINE ADDITIONAL RELATIONSHIPS
is_not_parent(Parent, Child) :- not(parent(Parent, Child)).

:- debug. % redo!

% NOTABLE SOURCES
% - risks of imbreeding, threshold for health and reproduction problems, understanding inbreeding of endangered species,
%	data of generations to include for determining the COI, override missing objects with the averages COI of a generation:
%  	instituteofcaninebiology.org/blog/coi-faqs-understanding-the-coefficient-of-inbreeding