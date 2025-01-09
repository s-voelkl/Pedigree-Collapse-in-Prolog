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

% ----- USAGE - coi -----
% ?- coi(<COI>, <Person>, <BaseRiskRate>).
% <COI> should not be set with a value.
% <Person> could be any Person in the knowledge base like charles_II_kingOfSpain or a variable.
% <BaseRiskRate> with a float value in [0, 1].
% Recommended BaseRiskRate: 0, 0.03, 0.05.
% --> coi(COI, charles_II_kingOfSpain, 0.03).

% ----- USAGE - cois to json -----
% ?- cois_to_json(<BaseRiskRate>, <JSON>), write_jsoned_cois(<JSON>).
% give <BaseRiskRate> a value, like 0.03
% let <JSON> be the variable JSON
% --> writes JSONed COIs into the console.

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

% ----- POSSIBLE FURTHER STEPS -----
- plot the solution for a given person or the average of many people with different base rates?
  - ask mates if anybody does grafviz in Prolog (visualization)
  - one person in dependency of the base rate [0, 10].
  - all people of a population, averaged. base rates [0, 10]?
- check out other methods of evaluating pedigree collapse
