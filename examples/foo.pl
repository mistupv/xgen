% visible: 
% unsafe: p/0

%0.4::b.
%0.3::c.

0.7::b.
0.8::c.

0.1::p :- b.
0.2::p :- c.

foo :- p,p.

query(foo).

%worlds:
%a      a
%0.1   0.2  = 0.02
%0.1   0.8  = 0.08
%0.9   0.2  = 0.18
%           = 0.28

%successful derivations:

% foo --> a,a -->_0.1 a --> true     (0.1)
% foo --> a,a -->_0.1 a -->_0.2 true (0.02)
% foo --> a,a -->_0.2 a --> true     (0.2)
% foo --> a,a -->_0.2 a -->_0.1 true (0.02)