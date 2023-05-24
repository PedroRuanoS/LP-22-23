inverte_r(0,0).
inverte_r(N,AddInv):-
    NewN is N//10,
    inverte_r(NewN,Inv),
    AddInv is (Inv*10) + (N mod 10).

inverte_i(N,Inv):- inverte_i(N,0,Inv).
inverte_i(0,Acc,Acc).
inverte_i(N,Acc,Inv):-
    NewN is N//10,
    NewAcc is (Acc*10) + (N mod 10),
    inverte_i(NewN, NewAcc, Inv).


insere_ordenado(El,Lst1,Lst2):-
    findall(X,(member(X,Lst1),X<El),Menores),
    findall(X,(member(X,Lst1),X>El),Maiores),
    append(Menores,[El|Maiores],Lst2).

repete_el(El,N,L):-
    length(L,N),
    maplist(iguala(El),L).
iguala(El,X):-El=X.

duplica_elementos(Lst1, Lst2):-
    merge(Lst1,Lst1,Lst2).

num_occ(Lst,El,N):-
    findall(X,(member(X,Lst),El=:=X),LstAux),
    length(LstAux,N).

substitui_maiores_N(N, Subst, Lst1, Lst2):-
    maplist(aux(N,Subst),Lst1,Lst2).
aux(N,Subst,El,El):- El=<N.
aux(N,Subst,El,Subst):- El>N.


