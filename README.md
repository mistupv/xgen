## xgen: explanation generator for ProbLog

The tool **xgen** takes a probabilistic logic program 
and a query following the ProbLog syntax, and
generates a number of ProbLog programs representing the different explanations of this query. The output is both shown on the screen and saved in the folder
"explanations".

### Instalation

You only need to download the files to a local folder.
The tool requires 
[SWI Prolog](https://www.swi-prolog.org/)
and 
[ProbLog](https://dtai.cs.kuleuven.be/problog/).

### Use 

Each generated  explanation corresponds to a proof of the query. The probability of each explanation is also shown. For this purpuse, a running implementation of ProbLog is required.

Consider, for instance, the following program (it can be found in ``examples/smokes3.pl``):

```
% visible: 
% unsafe: smokes/1

0.3::stress(X) :- person(X).
0.2::influences(X,Y) :- person(X), person(Y).

smokes(X) :- stress(X).
smokes(X) :- friend(X,Y), influences(Y,X), smokes(Y).

0.4::asthma(X) :- smokes(X).

person(john).
person(rose).
person(peter).
person(anna).

friend(john,rose).
friend(rose,john).
friend(rose,anna).
friend(peter,rose).
friend(anna,rose).

query(asthma(john)).
```

Here, we might want to know the possible explanations of the query ``asthma(john)``. In order to use the tool, you only need
to load **xgen** into SWI Prolog and proceed as follows:

```
$ swipl
Welcome to SWI-Prolog (threaded, 64 bits, version 8.2.4)
SWI-Prolog comes with ABSOLUTELY NO WARRANTY. This is free software.
Please run ?- license. for legal details.

For online help and background, visit https://www.swi-prolog.org
For built-in help, use ?- help(Topic). or ?- apropos(Word).

?- [xgen].
true.

?- xgen('examples/smokes3.pl').
% Explanation #1:

0.3::stress(john).
0.4::asthma(john) :- smokes(john).
smokes(john) :- stress(john).

query(asthma(john)).

% Success probability: 0.12

% Explanation #2:

0.2::influences(rose,john).
0.3::stress(rose).
0.4::asthma(john) :- smokes(john).
smokes(john) :- influences(rose,john),stress(rose).

query(asthma(john)).

% Success probability: 0.024

% Explanation #3:

0.2::influences(anna,rose).
0.2::influences(rose,john).
0.3::stress(anna).
0.4::asthma(john) :- smokes(john).
smokes(john) :- influences(rose,john),influences(anna,rose),stress(anna).

query(asthma(john)).

% Success probability: 0.0048

% No more explanations...
```

For instance, the first explanation can be read as follows "John has asthma because he smokes (with probability 0.4), 
and he smokes because he is stressed (with probability 0.3)". The probability of this explanation is 0.12. Note that this is the most likely explanation for John having asthma.

The tool **xgen** accepts ProbLog programs including definite clauses and (possibly nonground) probabilistic facts and rules. Negation or disjunction has not been yet considered.

Moreover, programs may include two types of annotations:

* ``% visible: pred1/n1, pred2/n2,...`` Explanations are parametric regarding the visible predicates. These predicates will show up in the explanations while all other predicates will be unfolded.
* ``% unsafe: pred1/n1, pred2/n2,...`` This is a more technical matter, which is related to the possibility of unfolding calls in probabilistic rules. For this to preserve the probability of a query, unfolded atoms cannot have several successful derivations computing the same (ground) answer. Predicates for which this property does not hold can be specified here as unsafe. This is the case of predicate
```smokes/1``` in the example above.

The tool also generates a combined program that includes all the explanations. This program can be seen as a specialized version of the original one tailored for the considered query. The marginal probability is the same in the original program and in the combination of the generated explanations:

```
$ problog examples/smokes3.pl 
asthma(john):	0.139152  
(True, {asthma(john): 0.13915200000000003})

$ problog explanations/smokes3_all.pl 
asthma(john):	0.139152  
(True, {asthma(john): 0.13915200000000003})
```

You can assert the atom ```verbose``` to see a trace of the explanation generation.