membro(X, [X|_]).
membro(R, [_|S]):- membro(R,S).

prefixo(_,_).
prefixo([X|R],[X|S]):- prefixo(R,S).

sufixo(X,X).
sufixo(X,[_|S]):-sufixo(X,S).

sublista(Xs, Ys) :- prefixo(Xs, Ys).
sublista(Xs, [_|Ys]) :- sublista(Xs, Ys).

junta([],L,L).
junta([R|S],L,[R|Z]):- junta(S,L,Z).

seguidos(X,Y,Z):- junta(_,[X,Y|_],Z).

ultimo(X,Y):- junta(_,[X],Y).

comp([],0).
comp([P|R],C):- comp(R, C+1).

comp1(L,C):- comp1(L,0,C).
comp1([],X,X).
comp1([_|Li],X,C):- X1 is X+1, comp1(Li,X1,C).

%Aula 7

insere_ordenado(El,Lst1,Lst2):-
    findall(El2,(member(El2,Lst1),El2<El),Men),
    append(Men,Mai,Lst1),
    append([Menores,[El],Maiores],Lst2).

repete_el(El,N,L):-
    length(L,N),
    maplist(elementos(El),Lst).
elementos(El,El1):- El=El1.

duplica_elementos(Lst1,Lst2):-
    findall([El,El],member(El,Lst1),Lst3),
    append(Lst3,Lst2).

num_occ(Lst,El,N):-
    findall(El1,(member(El1,Lst),El1=:=El),Lst2),
    length(Lst2,N).

substitui_maiores_N(N,Subst,Lst1,Lst2):-
    maplist(subst_aux(N,Subst),L1,L2).
subst_aux(N,Subst,El,Subst):- El>N.
subst_aux(N,Subst,El,El):- El=<N.

%Aula8

suc(N,M):- M is N+1.
ant(N,M):- M is N-1.

perimetro(R,P):- P is (R+R)*pi.

divisor(D,N):- (N mod D) =:= 0.

aplica_op(+,Val1,Val2,R):- R is (Val1 + Val2).
aplica_op(-,Val1,Val2,R):- R is (Val1 - Val2).
aplica_op(*,Val1,Val2,R):- R is (Val1 * Val2).
aplica_op(/,Val1,Val2,R):- R is (Val1 / Val2).

soma_digitos_rec(0,0).
soma_digitos_rec(N,So):-
    Nu is N // 10,
    soma_digitos_rec(Nu,S),
    So is S + N mod 10.

soma_digitos_it(N,S):- soma_digitos_it(N,0,S).
soma_digitos_it(0,Res,Res).
soma_digitos_it(N,Ac,S):-
    NAc is Ac + (N mod 10),
    NN is N // 10,
    soma_digitos_it(NN, NAc, S).

inverte_r(0,0).
inverte_r(N,AddInv):-
    NewN is N//10,
    inverte_r(NewN,Inv),
    AddInv is (Inv*10) + (N mod 10).

inverte(0, 0).

inverte(N, NewI) :-
  AuxI is N mod 10,
  NewN is N // 10,
  inverte(NewN, I),
  NewI is (I * 10) + AuxI.


