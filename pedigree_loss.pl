% Pedigree Collapse Prolog Program. 
:- set_prolog_flag(occurs_check, error). % disallow cyclic terms

% ----- STRUCTURE -----
% define families with genders and relationships
% define easy and intermediate relationship predicates
% specify on pedigree collapse determination using the Coefficient of Imbreeding (Wright, 1922) 
%   (see SOURCES [4]) by using helper predicates and bundling them into an advanced logic.
% define helper functions for the COI calculation
% Sources

% ----- GOAL OF THIS PROGRAM -----
% This program sets itself the goal to find out the degree of pedigree collapse (incest) in families.
% The coefficient of imbreeding (COI) is being measured with the formula by Wright, from 1922 (see SOURCES [4]).
% Note: the pedigree collapse coefficient is normally not being used in science, due to misleading approximations.
% For scientific purposes, the COI is normally being used.
% The formula works as following:

% ----- FORMULA -----
% FP = SUM[ 0.5 ^ (N1 + N2 + 1) * (1 + FCA)]
% FP: Coefficient of Imbreeding of a Person P.
% SUM: Sum of all results for each common ancestor of P.
% N1: Number of generations between P and a found common ancestor of P's father (Parent1).
% N2: same like N1, for the mother (Parent2).
% FCA: Coefficient of Imbreeding for the found common ancetor himself.

% ----- EXAMPLE EQUATION -----
% Example: see the simple animal relationship example (see SOURCES [3], page 150):
% animal_P has the same grandfather animal_c.
% The equation results as following:
% FP = 0.5 ^ (1 + 1 + 1) * (1 + 0) = 0.125
% N1: is 1, as one generation-difference of grandfather and father.
% N1: is 1, as one generation-difference of grandfather and mother.
% FCA: is 0, as no information about the ancestors of the grandfather was given.
% Result: 0.125 --> 12.5% of the genes are duplicates

% ----- LIBRARIES -----
:- use_module(library(clpfd)).
%  library clpfd for #= (replacing "is" -> more general), see SOURCES [8]
%  "is" is used for numeric equality (compare to "=" that only checks the structure), see SOURCES [9]

:- use_module(library(tabling)).
% Library tabling for a better alternative to memoization, see calculation of the coi below.


% ----- FACTS -----
% Pedigree chart about the House of Habsburg, see SOURCES [5,6] below.
% GENDERS
males([
    philip_I_kingOfCastile, charles_V_holyRomanEmperor, ferdinand_I_holyRomanEmperor, 
    philip_II_kingOfSpain, maximilian_II_holyRomanEmperor, william_V_dukeOfBavaria, philip_III_kingOfSpain, 
    ferdinand_II_holyRomanEmperor, philip_IV_kingOfSpain, ferdinand_III_holyRomanEmperor, 
    charles_II_kingOfSpain, christian_II_kingOfDenmark, albert_V_dukeOfBavaria, francis_I_dukeOfLorraine
]).
females([
    joanna_queenOfCastileAndAragon, isabella_ofAustria, maria_ofAustria, isabella_ofPortugal, 
    charles_II_archdukeOfAustria, anna_ofAustria1528, christina_ofDenmark, anna_ofAustria1549, 
    maria_anna_ofBavaria1551, renata_ofLorraine, margaret_ofAustria, maria_anna_ofBavaria1574, 
    maria_anna_ofSpain, mariana_ofAustria, anna_ofBohemiaAndHungary
]).
diverses([ ]).


% PARENTS
% structure: parents([List of all parents], [List of children of these parents]).
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


% TESTS with cows, from SOURCES [3] below.
% 1. Gen 
parents([cow_hassan, cow_lina], [cow_loner]).
parents([cow_streif, cow_gerlind], [cow_gerda]).
parents([cow_streif, cow_helga], [cow_streitl]).
parents([cow_hassan, cow_luxi], [cow_liesa]).
% 2. Gen
parents([cow_loner, cow_gerda], [cow_lotus]).
parents([cow_harko, cow_senta], [cow_stutzi]).
parents([cow_harko, cow_lerche], [cow_happ]).
parents([cow_streitl, cow_liesa], [cow_lilli]).
% 3. Gen
parents([cow_lotus, cow_stutzi], [cow_lock]).
parents([cow_happ, cow_lilli], [cow_loti]).
% 4. Gen
parents([cow_lock, cow_loti], [cow_leo]).


% Testing simple animal relationship (Example)
parents([animal_C, animal_D], [animal_A]).
parents([animal_C, animal_E], [animal_B]).
parents([animal_A, animal_B], [animal_P]). % animal_P has 0.125 (SOURCES [3], page 150)


% ----- RULES -----
% Derive atomar facts from the given fact lists by using rules
% TEST: female(maria_anna_ofSpain) -> true
% TEST: parent(philip_I_kingOfCastile, charles_V_holyRomanEmperor) -> true
% TEST: family_member(X) -> X = philip_I_kingOfCastile, charles_V_holyRomanEmperor, ...
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
% TEST: father(philip_I_kingOfCastile, charles_V_holyRomanEmperor) -> true
% TEST: mother(X, charles_V_holyRomanEmperor) -> X=joanna_queenOfCastileAndAragon
mother(Mother, Child) :- parent(Mother, Child), female(Mother).
father(Father, Child) :- parent(Father, Child), male(Father).
child(Child, Parent) :- parent(Parent, Child).

% ----- DEFINE INDIRECT RELATIONSHIPS ----- 
% TEST: grandfather(X, Y) -> X = philip_I_kingOfCastile, Y = philip_II_kingOfSpain, ...
% TEST: sibling(philip_II_kingOfSpain, X) -> X = maria_ofAustria
grandfather(Grandfather, Grandchild) :- 
    parent(Grandfather, Parent), 
    parent(Parent, Grandchild), 
    male(Grandfather).
grandmother(Grandmother, Grandchild) :- 
    parent(Grandmother, Parent), 
    parent(Parent, Grandchild), 
    female(Grandmother).

% for an setof-explanation, see SOURCES [10]
sibling(Sibling, Person) :- 
    % get unique entries of the returning siblings by creating a set
    % "^" for not generating seperate lists, see SOURCES [11]
    setof(
        Sibling, 
        Parent^(
          parent(Parent, Sibling), 
          parent(Parent, Person), 
          Sibling \== Person %inequality -> not the same parent again.
    	), 
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
% TEST: descendent(charles_V_holyRomanEmperor, philip_I_kingOfCastile) -> true
% TEST: common_ancestor(X, ferdinand_I_holyRomanEmperor, charles_V_holyRomanEmperor) -> X = philip_I_kingOfCastile, ...
% TEST: incest_child(X) -> X = charles_II_kingOfSpain, ...

% Base case (if no further descendent was found)
descendent(Descendent, Person) :-
    child(Descendent, Person).
% Recursion (depth search for further descendents)
descendent(Descendent, Person) :-
    child(Descendent, Parent),
    descendent(Parent, Person).
ancestor(Ancestor, Person) :- descendent(Person, Ancestor).

% Common ancestor (single) 
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
    %  equivalent to: "there is at least one parent for which applies ..."
    setof(CommonAncestor, ParentA^ParentB^(
        % get distinct parents of a child, then see if they have common ancestors.
        parent(ParentA, Person),
        parent(ParentB, Person),
        ParentA \== ParentB,
        common_ancestor(CommonAncestor, ParentA, ParentB)
    ), CommonAncestor).


% ----- DEFINE PEDIGREE COLLAPSE FUNCTIONS -----
% coefficient of imbreeding (COI) -> number, measuring how imbred an individual is. Low is genetically better.
% --> Value of the COI between 0 (genetically good) and 1 (clone to itself, only logarithmically possible).


% TABLING: 
% Computation takes too long and is sometimes inconsistent --> Saving the predicate results is needed.
% Solution: Using tabling or memoization: 
% - Memoization: Requires manual implementation, offers more control, 
%   but can be error-prone due to own errors in the code. see SOURCES [12]
% - Tabling: Automatically managed by Prolog, easier to use, ensures consistency, 
%   nice syntactic sugar compared to memoization. see SOURCES [13]
:- table coi/3.
    
% CALCULATING THE COI (Coefficient Of Imbreeding)
% TEST: coi(COI, animal_P) -> 0.125 (correct, see SOURCES [3], page 150)
% TEST: coi(COI, leo) -> 0.015625 (correct, see SOURCES [3], page 151)
% TEST: coi(COI, charles_II_kingOfSpain) -> 0.2412109375 

% number of parents == 0
% Base case: If a person does not have any ancestors, a constant value must be used.
%   There are different approximations, claiming a base risk of 0% (see SOURCES) [4], 3% [2], 
%       or a base risk equal to the average COI of the people in the same generation as the 
%       person, determined virtually [1].
% For testing, 0% should be taken, to compare the values with scientificly deducted values.
% For production, 3% are a good measurement, to map reality. 
coi(COI, Person, BaseRiskRate) :-
    not(any_parent_existing(Person)),
    COI is BaseRiskRate.

% number of parents == 1
% This case is being ignored by Wright (SOURCES [4]). 
% --> Deeper look into some formula-applying papers. [redo later?]
coi(COI, Person, BaseRiskRate) :-
    any_parent_existing(Person),
    not(both_parents_existing(Person)),
    COI is BaseRiskRate.

% number of parents == 2
coi(COI, Person, BaseRiskRate) :-     
    % get the common ancestors of this person as a list
    common_ancestors(ListCommonAncestors, Person),
    
    % get all CommonAncestorCOIs by recursive calculation, also using tabling
    calc_multiple_cois(CommonAncestorCOIs, ListCommonAncestors, Person, BaseRiskRate),
    
    % the resulting person's COI is the sum of all CommonAncestorCOIs
    list_sum(COI, CommonAncestorCOIs). %revert to TempCOI in case of BaseRiskRate application
    
    % if the COI is smaller than the BaseRiskRate, apply the BaseRiskRate as a value, see SOURCES [18].
    % (TempCOI < BaseRiskRate -> COI = BaseRiskRate ; COI = TempCOI).
    
    % debug:
    % write('Person: '), write(Person), write(' has the COI: '), write(COI), nl.
    

% ----- USEFUL PREDICATES: -----
% common_ancestors(CommonAncestor, Parent1, Parent2): 
% 	Get all common ancestors of a person's parents
% generation_difference(CalculatedDifference, Descendant, Ancestor): 
% 	Get generation difference between two people
% list_sum(Sum, List): 
% 	Sums all numeric elements of a list.
% calc_multiple_cois(CommonAncestorCOIs, ListCommonAncestors, Person, BaseRiskRate):
%   calculate all COIs for the common ancestors of a person, needed for the sum function.
% coi(COI, Person, BaseRiskRate):
%   get the COI of a Person with a given BaseRiskRate in the population.


% user-friendly predicate for accessing the COIs of CommonAncestors from a person
calc_multiple_cois(CommonAncestorCOIs, ListCommonAncestors, Person, BaseRiskRate) :-
    calc_single_coi([], CommonAncestorCOIs, ListCommonAncestors, Person, BaseRiskRate).

% base case for accessing the CommonAncestors COIs of a person
% This predicate is using Accumulators for faster and guranteed output, see SOURCES [14].
calc_single_coi(Acc, Acc, [], _, _).

% Recursive calculation of a Person's CommonAncestors' COIs.
calc_single_coi(Acc, CommonAncestorCOIs, [CurrentCommonAncestor | RemainingCommonAncestors], Person, BaseRiskRate) :-    
    % get parents and generation differences
    parents_of_child(Parent1, Parent2, Person),
    generation_difference(GenDiffParent1, Parent1, CurrentCommonAncestor),
    generation_difference(GenDiffParent2, Parent2, CurrentCommonAncestor),

    % calculate COI from CurrentCommonAncestor to probable other CommonAncestors => OwnCOI
    % (Relationship between this CommonAncestor and those CommonAncestors of him-/herself)
    coi(OwnCOI, CurrentCommonAncestor, BaseRiskRate),
    
    % calculate COI from Person to CurrentCommonAncestor
    PersonCOI is (0.5 ** (GenDiffParent1 + GenDiffParent2 + 1)) * (1 + OwnCOI),
    
    % recursion 
    % - with adding the PersonCOI to the CommonAncestorCOIs
    % - with removing the CurrentCommonAncestor from the RemainingCommonAncestors
    calc_single_coi([PersonCOI |Acc], CommonAncestorCOIs, RemainingCommonAncestors, Person, BaseRiskRate).


% find all common ancestors that a person's parents have.
% TEST: common_ancestors(CommonAncestors, leo) -> CommonAncestors = [harko, hassan, streif] 
%   (correct, see SOURCES [3], page 151)

common_ancestors(CommonAncestors, Person) :-
    parents_of_child(Parent1, Parent2, Person),
    % using findall to return an empty list if no CommonAncestors could have been found. -> []
    %   or else a list without duplicates, see SOURCES [15]
    findall(CommonAncestor, common_ancestor(CommonAncestor, Parent1, Parent2), CommonAncestors).

% Get the count of all common ancestors of a person.
% TEST: common_ancestor_count(AncestorCount, leo) -> AncestorCount = 3 (correct, see above)
common_ancestor_count(Count, Person) :-
    common_ancestors(Ancestors, Person),
    list_length(Count, Ancestors).
    

% Number of generations to a specific ancestor. 
% TEST: generation_difference(Diff, philip_IV_kingOfSpain, philip_I_kingOfCastile) -> Diff = 4
% TEST: generation_difference(Diff, mariana_ofAustria, philip_I_kingOfCastile) -> Diff = 5
% TEST: generation_difference(Diff, charles_II_kingOfSpain, philip_I_kingOfCastile) -> Diff = 5

% Interesting Problem: CharlesII has a Diff of 5, but his parents Diffs of 4 and 5 
%   -> min of 4+1 and 5+1 is taken?
%   How is this conflict being solved in the formula?
% Solution: not known yet, have to look deeper into formula-application papers. [redo later?]

% callable function to determine the number of generation difference betwenn a descendant and ancestor.
generation_difference(CalculatedDifference, Descendant, Ancestor) :-
    setof(CalculatedDiff, 
        calc_generation_difference(CalculatedDiff, Descendant, Ancestor), 
        [CalculatedDifference|_]).

% Base case: no generation in between (same person)
calc_generation_difference(0, Person, Person).
% Base case: only one generation in between -> return 1.
calc_generation_difference(1, Descendant, Ancestor) :-
    parent(Ancestor, Descendant).
% Recursive call: Backtracking until first generation was found (each time +1 deeper).
calc_generation_difference(Difference, Descendant, Ancestor) :-
    parent(Intermediate, Descendant),
    calc_generation_difference(IntermediateDifference, Intermediate, Ancestor),
    % performance-boost with "#=", see SOURCES [8]
    Difference #= IntermediateDifference + 1.


% Sum a list of numbers using arithmetic operations, see SOURCES [16]
list_sum(Sum, List) :-
    list_sum(List, 0, Sum).
% using accumulator, see SOURCES [14]
list_sum([], Acc, Acc).
list_sum([Head|Tail], Acc, Result) :-
    NewAcc is Acc + Head,
    list_sum(Tail, NewAcc, Result).


% Get the total elements of a list. [equivalent to len(list) function in Python]
% TEST: list_length([a,b,c,d], Length) -> Length = 4

% Base case: empty list-count is 0
list_length(0, []).
% Recursion: Recursively call the predicate with a length of length+1 
%   and the elements without the first element until base case.
list_length(Count, [_|Tail]) :-
    list_length(TailCount, Tail),
    Count #= (TailCount + 1).


% ----- DEFINE ADDITIONAL RELATIONSHIPS -----
% TEST: parents_of_child(Parent1, Parent2, charles_V_holyRomanEmperor) -> 
% 		    Parent1 = joanna_queenOfCastileAndAragon, Parent2 = philip_I_kingOfCastile
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
	% Placeholder, as there can be any parent. 
    %   "!" cut for stopping after the first parent, not returning 2 times true, see SOURCES [17]
	parent(_, Child), !.
    



% ----- SOURCES -----
% [1] risks of imbreeding, threshold for health and reproduction problems, understanding inbreeding of endangered species,
%	data of generations to include for determining the COI, override missing objects with the averages COI of a generation:
%  	instituteofcaninebiology.org/blog/coi-faqs-understanding-the-coefficient-of-inbreeding
% [2] base risk in the population for genetic risks is 3%:
%	Hansjakob Müller u. a.: Medizinische Genetik: Familienplanung und Genetik. In: Schweizer Medizin Forum. Basel, 2005
%	web.archive.org/web/20180329120812/https://medicalforum.ch/de/resource/jf/journal/file/view/article/smf/de/smf.2005.05576/2005-24-398.pdf/#expand
% [3] Tierzucht von Alfons William: utb.de/doi/book/10.36198/9783838548050
% [4] Wright, Sewall (1922), "Coefficients of Inbreeding and Relationship", The American Naturalist, vol. 56, page 333
% 	(journals.uchicago.edu/doi/10.1086/279872)
% [5] House of Habsburg: en.wikipedia.org/wiki/Template:Ancestors_of_Charles_II_of_Spain
% [6] House of Habsburg: de.wikipedia.org/wiki/Ahnenverlust#/media/Datei:Carlos_segundo80.png 
%	as there are many different pedigree charts of the House of Habsburg, the most accessible is being taken here (from wikipedia).
% [7] nature.com/articles/hdy201325 INTERESTING!
% [8] Prolog "#=" Predicate (from library clpfd) instead of is: swi-prolog.org/pldoc/man?predicate=%23%3D/2
% [9] Prolog "=" vs is: stackoverflow.com/questions/16027449/what-is-the-difference-between-is-and
% [10] Prolog setof/3 explanation: alsprolog.com/docs/ref/setof.html
% [11] Prolog existencial quantifier "^": cs.union.edu/~striegnk/learn-prolog-now/html/node98.html
% [12] Prolog Memoization: metalevel.at/prolog/memoization
% [13] Prolog Tabling: swi-prolog.org/pldoc/man?section=tabling-memoize
% [14] Prolog Accumulators: stackoverflow.com/questions/19944969/prolog-accumulators-are-they-really-a-different-concept
% [15] Prolog findall/3: swi-prolog.org/pldoc/man?predicate=findall%2f3
% [16] Prolog sum predicate: stackoverflow.com/questions/11520621/how-do-i-get-the-sum-of-given-numbers-in-prolog
% [17] Prolog cut: stackoverflow.com/questions/14541164/knowing-when-to-use-cut-in-prolog
% [18] Prolog if-statement: learnxbyexample.com/prolog/if-else/

/** <examples>
?- generation_difference(Diff, Desc, elisabeth)
?- common_ancestors(Ancestors, lock, loti)
*/
