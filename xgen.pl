:- use_module(utils).
:- use_module(parser).
:- use_module(tokenize).

:- dynamic fresh_vars/1.
:- dynamic verbose/0.

main :- 
    prolog_flag(argv,ArgV),
    get_options(ArgV,Options,_RemArgV), !,
    %(member(verbose,Options) -> (assert_verbose, print(Options),nl)
    %  ; (member(very_verbose,Options) -> (assert_verbose,assert_very_verbose, print(Options),nl) ; true)),
    ((member(format(F),Options), assert(cli_format(F)),fail)
     ; true),
    ((member(file(File),Options), assert(cli_initial_file(File)),fail)
     ; true),
    main_cli.

:- dynamic cli_initial_file/1.
:- dynamic cli_format/1.
:- dynamic cli_option/1.

main_cli :-
  cli_initial_file(File),
  %cli_format(Format),
  !,
  xgen(File).

get_options([],Rec,Rem) :- !,Rec=[],Rem=[].
get_options(Inputs,RecognisedOptions,RemOptions) :-
   (recognise_option(Inputs,Flag,RemInputs)
     -> (RecognisedOptions = [Flag|RecO2], 
         assert(cli_option(Flag)), %%print(Flag),nl,
         RemO2 = RemOptions)
     ;  (Inputs = [H|RemInputs], RemOptions = [H|RemO2], RecO2 = RecognisedOptions)
   ),
   get_options(RemInputs,RecO2,RemO2).

recognise_option(Inputs,Flag,RemInputs) :-
   recognised_option(Heads,Flag),
   append(Heads,RemInputs,Inputs).
   
recognised_option(['-file',NT],file(NT)).
%recognised_option(['-format',F],format(F)).

/*
xgen(filename)
*/

:- dynamic cl/3.
:- dynamic comment/1.
:- dynamic visible/1.
:- dynamic unsafe/1.
:- dynamic explanations/1.
:- dynamic filename/1.
:- dynamic query/1.
:- dynamic counter/1.

counter(0).

% empty set of preds: no renaming is required
pred_renaming([],Prob,Atom,[(Prob,Pred,N)],Atom) :- !,functor(Atom,Pred,N).
% a derived atom is renamed when it already belongs to Preds (duplication is not a problem)
pred_renaming(Preds,1,Atom,Preds,RAtom) :- 
  functor(Atom,Pred,N),
  member((1,Pred,N),Preds),!,
  counter(I),retractall(counter(I)),J is I+1,assertz(counter(J)),
  Atom =.. [_|Args],
  atom_concat(Pred,I,PredN),
  RAtom =.. [PredN|Args].
% a probabilistic atom is renamed when the predicate belongs to Preds with a DIFFERENT probability
% (this is needed to avoid confusing the probabilities...)
pred_renaming(Preds,Prob,Atom,Preds,RAtom) :-  %% no need to add the new one to Preds...
  Prob < 1,
  functor(Atom,Pred,N),
  member((Prob2,Pred,N),Preds),
  Prob \== Prob2,!,
  counter(I),retractall(counter(I)),J is I+1,assertz(counter(J)),
  Atom =.. [_|Args],
  atom_concat(Pred,I,PredN),
  RAtom =.. [PredN|Args].
% in the remaining cases, renaming is not needed...
% (note that we don't want to duplicate probabilistic predicates)
pred_renaming(Preds,Prob,Atom,[(Prob,Pred,N)|Preds],Atom) :- 
  functor(Atom,Pred,N).

xgen(File) :-
  retractall(cl(_,_,_)),
  retractall(visible(_)),
  retractall(unsafe(_)),
  retractall(comment(_)),
  retractall(query(_)),
  retractall(filename(_)),
  retractall(explanations(_)),
  retractall(counter(_)),assertz(counter(0)),
  read_problog_program(File,Program),
  file_base_name(File,FileNameExt),
  string_concat(FileName,'.pl',FileNameExt),
  assertz(filename(FileName)),
  %nl,print(read_problog_program(File,Program)),nl,
  replace_vars(Program,ProgramVars),
  %nl,print(replace_vars(Program,ProgramVars)),nl,
  assert_clauses(ProgramVars),
  query(Atom),
  findall(SortedExplanation,
          (cl(P,Atom,Body),
           add_empty_ancestors(Body,BodyAn),
           explanation([cl(P,Atom,[],[],BodyAn)],[],Explanation,[]),
           sort(Explanation,SortedExplanation)
          ),List),
  %cl(Probability,Head,NotUnfoldable,Variants,Unfoldable)
  %nl,print(Explanation),nl, 
  assertz(explanations(List)),
  print_explanations(List),
  format("Output files can be found in folder \"explanations\"."),!.

explanation(Pending,_,_,_) :-
  printv(explanation(Pending)),
  fail.

% no more clauses: we found an explanation!
explanation([],Explanation,Explanation,_) :-
  true.

% a visible call: move it to the variants set (even if not a variant) since it is not unfoldable
explanation([cl(P,Head,Visited,Variants,[res(H)|T])|R],Explanation,NewExp,Preds) :-
  append(Variants,[res(H)],NewVariants),
  explanation([cl(P,Head,Visited,NewVariants,T)|R],Explanation,NewExp,Preds).
% an unsafe call: move it to the variants set (even if not a variant) since it is not unfoldable
explanation([cl(P,Head,Visited,Variants,[nounf(H)|T])|R],Explanation,NewExp,Preds) :-
  append(Variants,[nounf(H)],NewVariants),
  explanation([cl(P,Head,Visited,NewVariants,T)|R],Explanation,NewExp,Preds).

% no more unfoldable calls (all are res/nounf): move clause to the current explanation
explanation([cl(P,Head,Visited,Variants,[])|R],Explanation,NewExp,Preds) :-
  no_call(Variants),
  append(Visited,Variants,Temp),
  remove_marks(Temp,Body), 
  explanation(R,[cl(P,Head,Body)|Explanation],NewExp,Preds).

% trying to unfold a non-probabilistic predicate: a variant of an ancestor 
% (attention! it has priority over the visible case)
explanation([cl(P,Head,Visited,Variants,[call(H,Ancestors)|T])|R],Explanation,NewExp,Preds) :-
  member_variant(H,Ancestors),!,
  append(Variants,[call(H,Ancestors)],NewVariants),
  explanation([cl(P,Head,Visited,NewVariants,T)|R],Explanation,NewExp,Preds).

% grounding a nonprobabilistic, visible predicate in an arbitrary clause (**last change!) 
explanation([cl(P,Head,Visited,Variants,[call(H,Ancestors)|T])|R],Explanation,NewExp,Preds) :-
  visible(H),!, 
  copy_term(H,H2),
  cl(1,H,Body), %% must be a derived predicate (probablistic predicates are all visible by default) 
  add_ancestors(Body,[H2|Ancestors],BodyAn), %% ancestors must be kept even for visible calls
  pred_renaming(Preds,1,H,NewPreds,HR),
  append(Variants,[res(HR)|T],NewBody),
  explanation([cl(1,HR,[],[],BodyAn),cl(P,Head,Visited,[],NewBody)|R],Explanation,NewExp,NewPreds).

% grounding a nonprobabilistic, visible predicate in a probabilistic clause 
explanation([cl(P,Head,Visited,Variants,[call(H,Ancestors)|T])|R],Explanation,NewExp,Preds) :-
  P<1,unsafe(H),!, 
  copy_term(H,H2),
  cl(1,H,Body), %% must be a derived predicate from a probababilistic clause 
  add_ancestors(Body,[H2|Ancestors],BodyAn), %% ancestors must be kept even for visible calls
  pred_renaming(Preds,1,H,NewPreds,HR),
  append(Variants,[nounf(HR)|T],NewBody),
  explanation([cl(1,HR,[],[],BodyAn),cl(P,Head,Visited,[],NewBody)|R],Explanation,NewExp,NewPreds).

% unfolding a non-probabilistic predicate: not a variant of an ancestor
explanation([cl(P,Head,Visited,Variants,[call(H,Ancestors)|T])|R],Explanation,NewExp,Preds) :-
  %\+visible(H),\+ unsafe(H)  %% this is assumed
  copy_term(H,H2),
  cl(1,H,B),
  add_ancestors(B,[H2|Ancestors],BA),
  append(BA,T,BT),
  append(Variants,BT,Body),
  explanation([cl(P,Head,Visited,[],Body)|R],Explanation,NewExp,Preds).

% grounding a probabilistic atom using a probabilistic fact 
% (this rule is redundant if the next rule is used; it is just a shortcut)
explanation([cl(Prob,Head,Visited,Variants,[call(H,_)|T])|R],Explanation,NewExp,Preds) :-
  cl(P,H,[]),P<1,
  %pred_renaming(Preds,P,H,NewPreds,HR),
  H=HR, Preds=NewPreds,
  append(Variants,[res(HR)|T],Body), % probabilitic predicates are all considered visible by default
%  append(Visited,[H],NewVisited),
  %Temp is Prob*P,
  explanation([cl(Prob,Head,Visited,[],Body)|R],[cl(P,HR,[])|Explanation],NewExp,NewPreds).

% grounding a probabilistic atom using a probabilistic rule 
explanation([cl(Prob,Head,Visited,Variants,[call(H,Ancestors)|T])|R],Explanation,NewExp,Preds) :-
  cl(P,H,[B1|Body]),P<1,
  copy_term(H,H2),
  add_ancestors([B1|Body],[H2|Ancestors],BodyAn), %% ancestors must be kept even for visible calls
  %pred_renaming(Preds,P,H,NewPreds,HR),
  H=HR, Preds=NewPreds,
  append(Variants,[res(HR)|T],NewBody), % probabilistic atoms are always considered visible
  explanation([cl(P,HR,[],[],BodyAn),cl(Prob,Head,Visited,[],NewBody)|R],Explanation,NewExp,NewPreds).

no_call([]).
no_call([res(_)|R]) :- no_call(R).
no_call([nounf(_)|R]) :- no_call(R).

remove_marks([],[]).
remove_marks([call(H,_)|R],[H|RR]) :- remove_marks(R,RR).
remove_marks([res(H)|R],[H|RR]) :- remove_marks(R,RR).
remove_marks([nounf(H)|R],[H|RR]) :- remove_marks(R,RR).

add_empty_ancestors([],[]).
add_empty_ancestors([H|R],[call(H,[])|RR]) :- add_empty_ancestors(R,RR).

add_ancestors([],_,[]).
add_ancestors([B|R],Ancestors,[call(B,Ancestors)|RA]) :-
  add_ancestors(R,Ancestors,RA).

member_variant(H,[A|_]) :- variant(H,A),!.
member_variant(H,[_|R]) :- member_variant(H,R).

%% read_problog_program(file,program)
%% reads and processes the structure of the ProBlog program from a given file:
%%   * extracts the list of tokens (Tokens) from the file
%%     (with help from tokenizer.pl)
%%   * removes unwanted tokens from Tokens (CleanTokens)
%%   * generates the data structure CleanProg by parsing (phrase)
%%     the list of tokens with the DCG specified in parser.pl
%%   * performs a post-processing ...

read_problog_program(File,Program) :-
  tokenize_file(File,Tokens,[cased(true),spaces(false),to(strings)]),
  %lists:subtract(Tokens,[cntrl("\n")],CleanTokens),
  %print(Tokens),nl,
  phrase(program(Program),Tokens),
  %nl,print(Program),nl,
  %vars_ctrs(CleanCtrs,Vars),
  %funs_ctrs(CleanCtrs,Funs),
  %post(CleanCtrs,Vars,Funs,PostCtrs),
  !.

assert_clauses([]).
assert_clauses([visible(Preds)|R]) :-
  !,assert_visible_preds(Preds),
  assert_clauses(R).
assert_clauses([unsafe(Preds)|R]) :-
  !,assert_unsafe_preds(Preds),
  assert_clauses(R).
assert_clauses([Cl|R]) :-
  assertz(Cl),
  assert_clauses(R).

assert_visible_preds([]).
assert_visible_preds([(Pred,N)|R]) :-
  functor(Atom,Pred,N),
  assertz(visible(Atom)),
  assert_visible_preds(R).

assert_unsafe_preds([]).
assert_unsafe_preds([(Pred,N)|R]) :-
  functor(Atom,Pred,N),
  assertz(unsafe(Atom)),
  assert_unsafe_preds(R).

replace_vars([],[]).
replace_vars([visible(L)|R],[visible(L)|RT]) :- 
  replace_vars(R,RT).
replace_vars([unsafe(L)|R],[unsafe(L)|RT]) :- 
  replace_vars(R,RT).
replace_vars([comment(C)|R],[comment(C)|RT]) :- 
  replace_vars(R,RT).
replace_vars([query(Q)|R],[query(Q3)|RT]) :- 
  rvars([Q],[Q2],[],-1),
  varnumbers([Q2],[Q3]),
  replace_vars(R,RT).
replace_vars([cl(Prob,Head,Body)|R],[cl(Prob,Head3,Body3)|RT]) :- 
  rvars([Head|Body],[Head2|Body2],[],-1),
  varnumbers([Head2|Body2],[Head3|Body3]),
  replace_vars(R,RT).

rvars([],[],_,_).
rvars([Atom|R],[NAtom|RR],VarList,N) :-
  Atom=..[Pred|Args],
  rvars_args(Args,VarList,N,ArgsVars,NVarList,NN),
  NAtom =.. [Pred|ArgsVars],
  rvars(R,RR,NVarList,NN).

rvars_args([],VarList,N,[],VarList,N).
rvars_args([var(X)|R],VarList,N,['$VAR'(Num)|RR],NVarList,NN) :-
  member((X,Num),VarList),!,
  rvars_args(R,VarList,N,RR,NVarList,NN).
rvars_args([var(X)|R],VarList,N,['$VAR'(M)|RR],NVarList,NN) :-
  !, M is N+1,
  rvars_args(R,[(X,M)|VarList],M,RR,NVarList,NN).
rvars_args([T|R],VarList,N,[T|RR],NVarList,NN) :-
  rvars_args(R,VarList,N,RR,NVarList,NN).



