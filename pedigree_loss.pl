% Student exercise profile
:- set_prolog_flag(occurs_check, error).        % disallow cyclic terms
%:- set_prolog_stack(global, limit(8 000 000)).  % limit term space (8Mb)
%:- set_prolog_stack(local,  limit(2 000 000)).  % limit environment space

% ----- LIBRARIES -----
:- use_module(library(clpfd)).
%  library clpfd for #= (replacing "is" -> more general), see here: swi-prolog.org/pldoc/man?predicate=%23%3D/2
%  "is" is used for numeric equality (compare to "=" that only checks the structure), see here:
%	stackoverflow.com/questions/16027449/what-is-the-difference-between-is-and

:- use_module(library(tabling)).
% Library tabling for a better alternative to memoization, see calculation of the coi below.

% ----- IDEEN ----- [REDO]
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


% ----- FACTS -----
% GENDER REDO!
males([]).
females([]).
diverses([]).

% PARENTS
% structure: parents([List of all parents], [List of children of these parents]).
% Pedigree chart about the House of Habsburg, see SOURCES [5,6] below.


% 1st generation
parents([philip_I_kingOfCastile, joanna_queenOfCastileAndAragon], [charles_V_holyRomanEmperor, ferdinand_I_holyRomanEmperor, isabella_ofAustria]).
% 2nd generation
parents([isabella_ofPortugal, charles_V_holyRomanEmperor], [philip_II_kingOfSpain, maria_ofAustria]).
parents([ferdinand_I_holyRomanEmperor, anna_ofBohemiaAndHungary], [maximilian_II_holyRomanEmperor, charles_II_archdukeOfAustria, anna_ofAustria1528]).
parents([isabella_ofAustria, christian_II_kingOfDenmark], [christina_ofDenmark]).
% 3rd generation
parents([maria_ofAustria, maximilian_II_holyRomanEmperor], [anna_ofAustria1549]).
parents([anna_ofAustria1528, albert_V_dukeOfBavaria], [maria_anna_ofBavaria1551, william_V_dukeOfBavaria]).
parents([christina_ofDenmark, francis_I_dukeOfLorraine], [renata_ofLorraine]).
% 4th generation
parents([philip_II_kingOfSpain, anna_ofAustria1549], [philip_III_kingOfSpain]).
parents([charles_II_archdukeOfAustria, maria_anna_ofBavaria1551], [margaret_ofAustria, ferdinand_II_holyRomanEmperor]).
parents([william_V_dukeOfBavaria, renata_ofLorraine], [maria_anna_ofBavaria1574]).       
% 5th generation
parents([philip_III_kingOfSpain, margaret_ofAustria], [philip_IV_kingOfSpain, maria_anna_ofSpain]).       
parents([ferdinand_II_holyRomanEmperor, maria_anna_ofBavaria1574], [ferdinand_III_holyRomanEmperor]).       
% 6th generation
parents([maria_anna_ofSpain, ferdinand_III_holyRomanEmperor], [mariana_ofAustria]).       
% 7th generation
parents([philip_IV_kingOfSpain, mariana_ofAustria], [charles_II_kingOfSpain]).       


% REDO: TESTING
%parents([grosseltern1, grosseltern2], [eltern1, eltern2]).
%parents([eltern1, anderer1], [kind1, kind2]).
%parents([kind1, kind2], [bruderSchwester]). % 25%
%parents([eltern1, anderer2], [kind3]).
%parents([kind1, kind3], [cousins]). % 6.25%
%parents([grosseltern1, kind1], [grosselternEnkelkind]). % 12.50%

% TESTS with cows, from SOURCES [3] below.
% 1. Gen 
parents([hassan, lina], [loner]).
parents([streif, gerlind], [gerda]).
parents([streif, helga], [streitl]).
parents([hassan, luxi], [liesa]).
% 2. Gen
parents([loner, gerda], [lotus]).
parents([harko, senta], [stutzi]).
parents([harko, lerche], [happ]).
parents([streitl, liesa], [lilli]).
% 3. Gen
parents([lotus, stutzi], [lock]).
parents([happ, lilli], [loti]).
% 4. Gen
parents([lock, loti], [leo]).


% Testing animalrelationships REDO
parents([animalC, animalD], [animalA]).
parents([animalC, animalE], [animalB]).
parents([animalA, animalB], [animalP]). % 0.125


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
% Base case (if depth search fails)
descendent(Descendent, Person) :-
    child(Descendent, Person).
% Recursion (calls base case if no descendent was found, see depth search)
descendent(Descendent, Person) :-
    child(Descendent, Parent),
    descendent(Parent, Person).
ancestor(Ancestor, Person) :- descendent(Person, Ancestor).

% Common ancestor determination function
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
% --> Value of the COI between 0 (genetically good) and 1 (clone to itself, only logarithmically possible)
% 
% Base case: If a person does not have ancestors, the COI is 0, see SOURCES [4] below.
% Other sources claim to create a virtual Person that takes the average of the COI of the same generation, see SOURCES [1] below.
% Also it is claimed that the base risk is 3% for genetic similarity, see SOURCES [2] below.

% USEFUL PREDICATES: 
% find_common_ancestor(CommonAncestor, Parent1, Parent2): 
% 	Get all common ancestors of a person's parents
% generation_difference(CalculatedDifference, Descendant, Ancestor): 
% 	Get generation difference between two people
% list_sum(Sum, List): 
% 	Sums all numeric elements of a list.


% TABLING: 
% Computation takes too long and is sometimes inconsistent --> Saving the predicate results is needed.
% Solution: Using tabling or memoization: 
% - Memoization: Requires manual implementation, offers more control, but can be error-prone due to own errors in the code.
% - Tabling: Automatically managed by Prolog, easier to use, ensures consistency, nice syntactic sugar compared to memoization.
% see here: metalevel.at/prolog/memoization 
% see here: swi-prolog.org/pldoc/man?section=tabling-memoize
:- table coi/2.
    
% CALCULATING THE COI (Coefficient Of Imbreeding)
% TEST: coi(COI, leo) -> 0.015625 (correct, see SOURCES [3], page 150)
% TEST: coi(COI, leo) -> 0.015625 (correct, see SOURCES [3], page 151)

% number of parents == 0
coi(COI, Person) :-
    not(any_parent_existing(Person)),
    COI is 0,
    write('-------------- COI is 0 as num parents == 0'), nl.

% number of parents == 1 -> simplified [REDO]
coi(COI, Person) :-
    any_parent_existing(Person),
    not(both_parents_existing(Person)),
    COI is 0.

% number of parents == 2
coi(COI, Person) :- 
    % Summe aller Coefs der CommonAncestors der Eltern der Person. --> Summenfunktion, appende die Coefs an die Liste	
   	% dabei gilt pro Coef eines CommonAncestors: 0,5^(n_gens1 + n_gens2 + 1) * (...)
    
    write('coi/Person: '), write(Person), nl, 
    % get common ancestors of this person as a list
    common_ancestors(ListCommonAncestors, Person),
    write('coi/ListCommonAncestors: '), write(ListCommonAncestors), nl, 
    
    % get all CommonAncestorCOIs
    calc_multiple_cois(CommonAncestorCOIs, ListCommonAncestors, Person),
    write('coi/CommonAncestorCOIs: '), write(CommonAncestorCOIs), nl, 
    
    % COI is the sum of all CommonAncestorCOIs
    %list_sum(COI, CommonAncestorCOIs),
    list_sum(COI, CommonAncestorCOIs),
    write('coi/COI: '), write(COI), nl, nl.
    

% user-friendly predicate for accessing the COIs of CommonAncestors from a person
calc_multiple_cois(CommonAncestorCOIs, ListCommonAncestors, Person) :-
    calc_single_coi([], CommonAncestorCOIs, ListCommonAncestors, Person).

% base case for accessing the CommonAncestors COIs of a person
% This predicate is using Accumulators for faster and guranteed output at every time, see here:
% 	stackoverflow.com/questions/19944969/prolog-accumulators-are-they-really-a-different-concept
calc_single_coi(Acc, Acc, [], _).

% Recursive calculation of a Person's CommonAncestors' COIs.
calc_single_coi(Acc, CommonAncestorCOIs, [CurrentCommonAncestor | RemainingCommonAncestors], Person) :-    
    % get parents and generation differences
    parents_of_child(Parent1, Parent2, Person),
    generation_difference(GenDiffParent1, Parent1, CurrentCommonAncestor),
    generation_difference(GenDiffParent2, Parent2, CurrentCommonAncestor),

    % calculate from CurrentCommonAncestor to probable other CommonAncestors => OwnCOI
    % (Relationship between this CommonAncestor and those CommonAncestors of itself)
    coi(OwnCOI, CurrentCommonAncestor),
    
    % calculate from Person to CurrentCommonAncestor
    PersonCOI is (0.5 ** (GenDiffParent1 + GenDiffParent2 + 1)) * (1 + OwnCOI),
    
    %debug
    write('calc_single_coi/OwnCOI of: '), write(CurrentCommonAncestor), write(' is: '), write(OwnCOI), nl,
    write('calc_single_coi/CurrentCOI: '), write(PersonCOI), nl,
    
    calc_single_coi([PersonCOI |Acc], CommonAncestorCOIs, RemainingCommonAncestors, Person).


% find all common ancestors with find_common_ancestor that a person's parents have.
% Predicate tries to eliminate duplicates with "member" and "Visited"
% TEST: common_ancestors(CommonAncestors, leo) -> CommonAncestors = [harko, hassan, streif]

common_ancestors(CommonAncestors, Person) :-
    parents_of_child(Parent1, Parent2, Person),
    % using findall to return an empty list if no CommonAncestors could have been found. -> []
    % also returns a set of all found CommonAncestors if one was found multiple times (no duplicates).
    findall(CommonAncestor, common_ancestor(CommonAncestor, Parent1, Parent2), CommonAncestors).

% Get the count of all common ancestors of a person.
% TEST: common_ancestor_count(AncestorCount, leo) -> AncestorCount = 3
common_ancestor_count(Count, Person) :-
    common_ancestors(Ancestors, Person),
    list_length(Count, Ancestors).
    
    


% Number of generations to a specific ancestor. 
% TEST: generation_difference(Diff, charles, elisabeth) -> DIFF = 1
% TEST: generation_difference(Diff, george, elisabeth) -> DIFF = 3
generation_difference(CalculatedDifference, Descendant, Ancestor) :-
    setof(CalculatedDiff, 
        calc_generation_difference(CalculatedDiff, Descendant, Ancestor), 
        [CalculatedDifference|_]).

% Base case: no generation in between (same person)
calc_generation_difference(0, Person, Person).
% Base case: only one generation in between -> return 1.
calc_generation_difference(1, Descendant, Ancestor) :-
    parent(Ancestor, Descendant).
% Recursive call: Backtracking until first generation was found (each time +1).
calc_generation_difference(Difference, Descendant, Ancestor) :-
    parent(Intermediate, Descendant),
    calc_generation_difference(IntermediateDifference, Intermediate, Ancestor),
    Difference #= IntermediateDifference + 1.



% Sum a list of numbers using arithmetic operations.
%  Answer based on StackOverflow here: stackoverflow.com/questions/11520621/how-do-i-get-the-sum-of-given-numbers-in-prolog
list_sum(Sum, List) :-
    list_sum(List, 0, Sum).
% Note the Accumulator Acc for optimizing the recursion calls onto the list, fastening the process.
list_sum([], Acc, Acc).
list_sum([Head|Tail], Acc, Result) :-
    NewAcc is Acc + Head,
    list_sum(Tail, NewAcc, Result).

% Get the total elements of a list. [equivalent of len(list) function of python]
% TEST: list_length([a,b,c,d], Length) -> Length = 4
% Base case: empty list-count is 0
list_length(0, []).
% Recursion: Recursivly call the predicate with a length of + 1 and the elements minus the first one until base case.
list_length(Count, [_|Tail]) :-
    list_length(TailCount, Tail),
    Count #= (TailCount + 1).


% DEFINE ADDITIONAL RELATIONSHIPS
% TEST: parents_of_child(Parent1, Parent2, charles_V_holyRomanEmperor) -> 
% 		Parent1 = joanna_queenOfCastileAndAragon, Parent2 = philip_I_kingOfCastile
parents_of_child(Parent1, Parent2, Child) :-
    % Problem: returns 2x true.
    % Solution: Get the parents as a set and place them in a sorted order, 
    %  then only take one combination -> only one result is true.
    setof(
        (Parent1, Parent2), 
          (parent(Parent1, Child), 
          parent(Parent2, Child), 
          Parent1 \== Parent2), 
        ParentPairs),
    % take only the first list element with the given structure.
    Parent1 = FirstParent1,
    Parent2 = FirstParent2,
    ParentPairs = [(FirstParent1, FirstParent2) | _].


% TEST: both_parents_existing(anna_ofAustria1549) -> true
% TEST: both_parents_existing(isabella_ofPortugal) -> false
both_parents_existing(Child) :-
    % can be any parents here.
	parents_of_child(_, _, Child).

% TEST: any_parent_existing(anna_ofAustria1549) -> true
% TEST: <a person with only one parent> -> true
any_parent_existing(Child) :-
	% Placeholder, as there can be any parent. "!" cut for stopping after the first parent, not returning 2 times true.
	parent(_, Child), !.
    

    
    
:- debug. % redo!

% SOURCES
% [1] risks of imbreeding, threshold for health and reproduction problems, understanding inbreeding of endangered species,
%	data of generations to include for determining the COI, override missing objects with the averages COI of a generation:
%  	instituteofcaninebiology.org/blog/coi-faqs-understanding-the-coefficient-of-inbreeding
% [2] base risk in the population for genetic risks is 3%:
%	Hansjakob Müller u. a.: Medizinische Genetik: Familienplanung und Genetik. In: Schweizer Medizin Forum. Basel, 2005
%	web.archive.org/web/20180329120812/https://medicalforum.ch/de/resource/jf/journal/file/view/article/smf/de/smf.2005.05576/2005-24-398.pdf/#expand
% [3] Tierzucht von Alfons William: https://www.utb.de/doi/book/10.36198/9783838548050
% [4] Wright, Sewall (1922), "Coefficients of Inbreeding and Relationship", The American Naturalist, vol. 56, page 333
% 	(journals.uchicago.edu/doi/10.1086/279872)
% [5] House of Habsburg: en.wikipedia.org/wiki/Template:Ancestors_of_Charles_II_of_Spain
% [6] House of Habsburg: de.wikipedia.org/wiki/Ahnenverlust#/media/Datei:Carlos_segundo80.png 
%	as there are many different pedigree charts of the House of Habsburg, the most accessible is being taken here (from wikipedia).


/** <examples>
?- generation_difference(Diff, Desc, elisabeth)
?- common_ancestors(Ancestors, lock, loti)
*/
