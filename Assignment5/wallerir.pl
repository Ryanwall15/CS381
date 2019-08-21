
% This program defines the simpson family tree.
% these are the same do not change them
% defining the female names as given in the file.
% Ryan Wallerius : wallerir
% Daniyal Abbas : abbasd 

female(mona).
female(jackie).
female(marge).
female(patty).
female(selma).
female(lisa).
female(maggie).
female(ling).

% defining the male names of the files

male(abe).
male(clancy).
male(herb).
male(homer).
male(bart).

% define the marriage relationship of the male and female

marriedDefine(abe,mona).
marriedDefine(clancy,jackie).
marriedDefine(homer,marge).

% married predicate definition

married(A,B) :- marriedDefine(A,B).
married(A,B) :- marriedDefine(B,A).

% parent predicate defining

parent(abe,homer).
parent(abe,herb).
parent(clancy,marge).

parent(mona,homer).
parent(clancy,patty).
parent(jackie,patty).

parent(jackie,selma).
parent(homer,lisa).
parent(marge,lisa).

parent(homer,bart).
parent(jackie,marge).
parent(clancy,selma).

parent(marge,bart).
parent(homer,maggie).
parent(marge,maggie).
parent(selma,ling).



%%
% Part 1. Family relations
%%

% 1. Define a predicate `child/2` that inverts the parent relationship.
%1. answer..
child(A,B) :- parent(B,A).

% 2. Define two predicates `isMother/1` and `isFather/1`.
%2. answer..
isMother(A) :- female(A),parent(A,_).
isFather(A) :- male(A),parent(A,_).

% 3. Define a predicate `grandparent/2`.
%1. answer..
grandparent(A,C) :- parent(A,B), parent(B,C).
%
% As due to the use of duplicates in this solution, to get solution setof/3 is used.
% where setof/3 is used in otder to get input variables, function parameters and also the output
% In a list, the arguments are defined by the second argument
%
% 4. Define a predicate `sibling/2`. Siblings share at least one parent.
%1. answer..

sibling(A,B) :- setof((A,B), C^(parent(C,A) ,parent(C,B), \+A=B), Sibs),
               member((A,B), Sibs).

% 5. Define two predicates `brother/2` and `sister/2`.
%1. answer..
brother(A,B) :- male(A), sibling(A,B).
sister(A,B) :- female(A), sibling(A,B).

% 6. Define a predicate `siblingInLaw/2`. A sibling-in-law is either married to
%    a sibling or the sibling of a spouse.
%1. answer..

siblingInLaw(A,B) :- sibling(A,C), married(C,B).
siblingInLaw(A,B) :- married(C,A), sibling(B,C).

% 7. Define two predicates `aunt/2` and `uncle/2`. Your definitions of these
%    predicates should include aunts and uncles by marriage.
%1. answer..

aunt(A,B) :- female(A), sibling(A,C), parent(C,B).
aunt(A,B) :- female(A), married(A,C), sibling(C,B), parent(B,B).
uncle(A,B) :- male(A), sibling(A,C), parent(C,B).
uncle(A,B) :- male(A), married(A,C), sibling(C,A), parent(A,B).

% 8. Define the predicate `cousin/2`.
%1. answer..

cousin(A,B) :- parent(C,A), parent(O, B), sibling(C,O).

% 9. Define the predicate `ancestor/2`.
%1. answer..

ancestor(A,B) :- parent(A,B).
ancestor(A,B) :- parent(C,B), ancestor(A,C).

% Extra credit: Define the predicate `related/2`.
%1. answer..

related(A,B) :- ancestor(A,B).
related(A,B) :- ancestor(B,A).
related(A,B) :- aunt(A,B).
related(A,B) :- aunt(B,A).
related(A,B) :- uncle(A,B).
related(A,B) :- uncle(B,A).
related(A,B) :- cousin(A,B).
related(A,B) :- cousin(B,A).
related(A,B) :- siblingInLaw(A,B).
related(A,B) :- siblingInLaw(B,A).
related(A,B) :- sibling(A,B).
related(A,B) :- sibling(B,A).


%%
% Part 2. Language implementation
%%

% 1. Define the predicate `cmd/3`, which describes the effect of executing a
%    command on the stack.


% the structure of the lists is [Head, value | Tail] which further contains atomics.

% Also the symbol J represents the evaluation definition
cmd(add,[H,B|T],S2) :- J is (H+B), S2 = [J|T].

% Here lte causes two values to pop and pushe value as a boolean true or false.
%
% condition for head value to be true or false

cmd(lte,[H,B|T], S2) :- J = (H =< B ->C=t;C=f), call(J), S2 = [C|T].

cmd(if(R,_),[t|T], S2) :- prog(R,T,S2).
cmd(if(_,W),[f|T], S2) :- prog(W,T,S2).
cmd(J,T,S2) :- S2 = [J|T].

% 2. Define the predicate `prog/3`, which describes the effect of executing a
%    program on the stack.

% if the value is empty for the first variable now equate with stack

prog([], S1, S2) :- S2 = S1.

% run command or prog in case it is not empty, always depending on type
prog([C|T], S1, S2) :- cmd(C, S1, S3), prog(T,S3,S2).
