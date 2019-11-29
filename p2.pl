%Interface
start():-
  use_module(library(http/http_open)),
  use_module(library(sgml)),
  use_module(library(option)),
  write('Welcome to the UBC Course Requirement Navigator.\n'),
  write('Please input the course you are planning to take: '),
  read_string(C),
  write('\nPlease input courses you have taken:\n'),
  write('(you can press enter to input another course, or leave the field empty to finish inputting.)\n'),
  courses_reader(L),
  nav_message(C,L),
  !.

%Helper functions for the interface
read_string(String) :-
    current_input(Input),
    read_line_to_codes(Input, Codes),
    atom_codes(String, Codes).

courses_reader([H|T]):-
  read_string(H),
  dif(H,''),
  courses_reader(T).

courses_reader([]).

nav_message(C,L):-
  navigator(C,L),
  atom_concat('You have all the prerequisites for ', C, X1),
  atom_concat(X1, '. You can now take the course.', X2),
  write(X2).

nav_message(C,L):-
  atom_concat('You do not have all the prerequisites for ', C, X1),
  atom_concat(X1, ' yet. You cannot take the course.', X2),
  write(X2).
%-------------

% test inputs
% navigator("BIOL 112",["BIOL 111","CHEM 100"]) -> should return true
% navigator("BIOL 111",["BIOL 111","CHEM 100"]) -> should return true
% navigator("BIOL 111",[])                      -> same
% navigator("BIOL 112",["CPSC 312"])            -> false
% navigator("CPSC 110",[])                      -> true (no prerequisites)



% case: no prerequisites
handle_prereqs([],_).
handle_prereqs(([],[]),_).

handle_prereqs(([],AllOf),T) :-
    all_are_members(AllOf,T).

handle_prereqs((OneOf,[]),T) :-
    one_is_member(OneOf,T).

% else
handle_prereqs((OneOf, AllOf),T) :-
    one_is_member(OneOf,T),
    all_are_members(AllOf,T).


checkprereqs(Course,Taken) :-
    nth0(0,CourseSplit,Subj),
    nth0(1,CourseSplit,Num),
    split_string(Course," ","",CourseSplit),
    getprereqs(Subj,Num,PCS),
    handle_prereqs(PCS, Taken).

    % maybe not done



% navigator (main function) -- navigator(C,T) requires a desired course C and a list of courses taken T
% input course as a string, eg. 'CPSC 312'

% obvious case - if a person has previously taken a course, they are able to take the course
navigator(C,T) :- member(C,T),!.
% case where student has not taken course yet
navigator(C,T) :-  checkprereqs(C,T),!.




% define function member(E,L), return true if E is in list L
% member(E,[E|T]).
% member(E,[H|T]) :- member(E,T).

% access fns for course tuples
getfirst((X,Y),X).
getsecond((X,Y),Y).

% ---

% #empty list (will have to move this case to handle_prereqs)
% #one_is_member([],_). this will return false truths, do not uncomment. reimplemented at handle_prereqs level

% list of courses
one_is_member([H], L2) :- member(H,L2).
one_is_member([H|T], L2) :- member(H,L2).
one_is_member([H|T], L2) :- one_is_member(T, L2).

% list of tuples
one_is_member([(H_one,H_all)], L2) :-
    handle_prereqs((H_one,H_all), L2).

one_is_member([(H_one,H_all)|T], L2) :-
    handle_prereqs((H_one,H_all), L2).

one_is_member([(H_one,H_all)|T], L2) :-
    one_is_member(T,L2).


% #empty list (may have to move this case to handle_prereqs)
% #all_are_members([],_). commented out for case consistency, reimplemented at handle_prereqs level

% list of courses
all_are_members([H],L2):- member(H,L2).
all_are_members([H|T], L2) :- member(H,L2), all_are_members(T,L2).

% list of tuples
all_are_members([(H_one,H_all)], L2) :-
    handle_prereqs((H_one,H_all),L2).
all_are_members([(H_one,H_all)|T], L2) :-
    handle_prereqs((H_one,H_all),L2),
    all_are_members(T,L2).



% --- below here Harys code ---

getbody([element(html,_,[element(head,_,_),element(body,_,L)])],L).
getcontainer([element(div,[class=container],L)|_], L).
getcontainer([H|T],L):-
  getcontainer(T,L).

getcontent([element(div,[class='content expand',role=main],L)|_], L).
getcontent([H|T],L):-
  getcontent(T,L).

gettable([element(table,[class='sortable table table-striped',id=mainTable],L)|_], L).
gettable([H|T],L):-
  gettable(T,L).

gettbody([element(thead,_,_),element(tbody,[],L)|_],L).

getSUBJ(element(a,[_],[Subj]), Subj).

getSUBJs([],[]).

getSUBJs([element(tr,_,[element(td,[],[S])|_])|T], [H|R]):-
  getSUBJ(S,H),
  getSUBJs(T,R).

getSUBJs([E|T],R):-
  getSUBJs(T,R).

get_list(Term, L6):-
  getbody(Term, L1),
  getcontainer(L1,L2),
  getcontent(L2,L3),
  gettable(L3,L4),
  gettbody(L4,L5),
  getSUBJs(L5,L6).

get_courses([],[]).
get_courses([S|T], [subj(S,LC)|R]):-
  atom_concat('https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-department&dept=',S, Html_C),
  load_html(Html_C, Term_C, []),
  get_list(Term_C, LSC),
  trim_courses(LSC, S, LC),
  get_courses(T,R).

trim_courses([],_,[]).
trim_courses([SC|T], S, [C|R]):-
  atom_concat(S, ' ', SS),
  atom_concat(SS, C, SC),
  trim_courses(T,S,R).

% Parse prereqs of a course from the website
getprereqs(S,C,PCS):-
  atom_concat('https://courses.students.ubc.ca/cs/courseschedule?pname=subjarea&tname=subj-course&dept=',S,P1),
  atom_concat(P1,'&course=', P2),
  atom_concat(P2, C, Html_P),
  load_html(Html_P, Term_P, []),
  getbody(Term_P,L1),
  getcontainer(L1,L2),
  getcontent(L2,L3),
  getprereqc(L3,PS),
  getcoreqc(L3,PS,PCS),
  !.

% Reading the prereqs
getprereqc([element(p,[],[H|T])|_],PREREQ):-
  atom_concat('Pre-reqs:',_,H),
  read_prereqs([H|T],PREREQ).
getprereqc([H|T],L):-
  getprereqc(T,L).
getprereqc([],[]).

% Reading the coreqs
getcoreqc([element(p,[],[H|T])|_],PS,[[],[PS,COREQ]]):-
  atom_concat('Co-reqs:',_,H),
  read_prereqs([H|T],COREQ).
getcoreqc([H|T],PS,L):-
  getcoreqc(T,PS,L).
getcoreqc([],PS,PS).

% Putting the prereqs into our structure of representation
% A Prereq is one of:
%     A course
%     (AllOf,[]) where AllOf is a list of Prereq contain a list of prereqs that must all be satisfied
%     ([],OneOf) where OneOf is a list of Prereq contain a list of prereqs where at least one must be satisfied
%CASE: Prereqs = either
read_prereqs([X,H|T],(LC,[])):-
  atom(X),isStr('Either',X),
  split([H|T],'or (',LL),
  read_sublists(LL,LC).
  % read_sublists(LL,LC).
%CASE: Prereqs = _and_
read_prereqs([H|T],([],LC)):-
  hasStr(T,'and'),
  split([H|T],'and',LL),
  read_sublists(LL,LC).
%CASE: Prereq = AllOf
read_prereqs([AllOf,H|T],([],LC)):-
  atom_concat(_, 'All of ', AllOf),
  read_courses([H|T],LC).

read_prereqs([AllOf,H|T],([],LC)):-
  atom_concat(_, 'all of ', AllOf),
  read_courses([H|T],LC).
%CASE: Prereq = OneOf
read_prereqs([OneOf,H|T],(LC,[])):-
  atom_concat(_, 'One of ', OneOf),
  read_courses([H|T],LC).

read_prereqs([OneOf,H|T],(LC,[])):-
  atom_concat(_, 'one of ', OneOf),
  read_courses([H|T],LC).
%CASE: Prereq = Course
read_prereqs([_,element(a,_,[PC])|_],([],[PC])).
read_prereqs([element(a,_,[PC])|_],([],[PC])).

read_prereqs([_|_],([],[])).
%---------

%Helper functions
take_until([],_,[],[],' ').
take_until([X|T],Str,[],T,X):-
  atom(X),isStr(Str,X).
take_until([H|T],Str,[H|T1],T2,K):-
  take_until(T,Str,T1,T2,K).

split([],_,[]).
split([H|T],Str,[H1|T2]):-
  take_until([H|T],Str,H1,R,K),
  split(R,Str,T1),
  add_head(T1,K,T2).

add_head([],_,[]).
add_head([H|T],K,[[K|H]|T]).


head_tail([],[],[]).
head_tail([H|T],H,T).

isStr(Str, X):-
  atom_concat(Or1,_,X),
  atom_concat(_,Str,Or1).

hasStr([H|T],Str):-
  atom(H),isStr(Str,H).
hasStr([H|T],Str):-
  hasStr(T,Str).

read_sublists([],[]).
read_sublists([H|T],[LC|R]):-
  read_prereqs(H,LC),
  read_sublists(T,R).

read_courses([],[]).
read_courses([element(a,_,[PC])|T],[PC|R]):-
  read_courses(T,R).

read_courses([H|T],R):-
  read_courses(T,R).
%---------
