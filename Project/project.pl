%106642 Pedro Silveira
:- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- ['dados.pl'], ['keywords.pl'].

    %%%%%
    %3.1%
    %%%%%

/*E verdade se EventosSemSala e uma lista (ordenada e sem elementos repetidos)
composta por IDs de eventos sem sala. */
eventosSemSalas(EventosSemSala):-
    findall(ID, evento(ID,_,_,_,semSala), EventosSemSala1),
    sort(EventosSemSala1, EventosSemSala).

/*E verdade se EventosSemSala e uma lista (ordenada e sem elementos repetidos)
composta por IDs de eventos sem sala que ocorrem no dia da semana DiaDaSemana.*/
eventosSemSalasDiaSemana(DiaDaSemana, EventosSemSala):- 
    findall(ID,(evento(ID,_,_,_,semSala), horario(ID,DiaDaSemana,_,_,_,_)), EventosSemSala1),
    sort(EventosSemSala1, EventosSemSala).

/*E verdade se EventosSemSala e uma lista (ordenada e sem elementos repetidos)
composta por IDs de eventos sem sala nos periodos da ListaPeriodos (lista de periodos).*/
eventosSemSalasPeriodo([], []):- !.
eventosSemSalasPeriodo([N|R], EventosSemSala):-
    eventosSemSalas(ListaEventosSemSala),  
    include(getPeriodo(N),ListaEventosSemSala,EventosSemSala1), %EventosSemSala1 e uma lista com os elementos da lista ListaEventosSemSala com o periodo N.
    eventosSemSalasPeriodo(R,EventosSemSala2), 
    append(EventosSemSala1,EventosSemSala2,EventosSemSala3), 
    sort(EventosSemSala3,EventosSemSala). %EventosSemSala3 e a juncao das listas EventosSemSala1 e EventosSemSala2 - sort ordena a mesma e retira os elementos repetidos.

/*E verdade se Periodo for o periodo do evento com o identificador ID.*/
getPeriodo(Periodo, ID):- 
    horario(ID,_,_,_,_,Periodo), !. %Evento periodal (p1, p2, p3, p4).
getPeriodo(Periodo, ID):-
    horario(ID,_,_,_,_,p1_2),   %Evento semestral (p1_2).
    (Periodo = p1; Periodo = p2), !.
getPeriodo(Periodo, ID):-
    horario(ID,_,_,_,_,p3_4), %Evento semestral (p3_4).
    (Periodo = p3; Periodo = p4), !.

    %%%%%
    %3.2%
    %%%%%

/*(Caso terminal) E verdade se EventosNoPeriodo e uma lista (ordena e sem elementos repetidos)
 de IDs (da lista ListaEventos) de eventos que ocorrem no periodo Periodo. */
organizaEventos([],_,[]):- !.
organizaEventos([N|R],Periodo,EventosNoPeriodo):- %Caso de ID ocorrer no periodo.
    getPeriodo(Periodo,N), !,
    organizaEventos(R,Periodo,EventosNoPeriodo1),
    append(EventosNoPeriodo1,[N], EventosNoPeriodo2),
    sort(EventosNoPeriodo2,EventosNoPeriodo). 
organizaEventos([_|R],Periodo,EventosNoPeriodo):- %Caso de ID nao ocorrer no periodo.
    organizaEventos(R,Periodo,EventosNoPeriodo).

/*E verdade se ListaEventosMenoresQue e uma lista (ordenada e sem elementos repetidos)
dos IDs dos eventos com duracao menor ou igual a Duracao.*/
eventosMenoresQue(Duracao, ListaEventosMenoresQue):- 
    findall(ID, (horario(ID,_,_,_,DuracaoMenor,_),DuracaoMenor =< Duracao), ListaEventosMenoresQue1),
    sort(ListaEventosMenoresQue1, ListaEventosMenoresQue).

/*E verdade se o evento com identificador ID tiver duracao menor ou igual a Duracao.*/
eventosMenoresQueBool(ID, Duracao):- 
    horario(ID,_,_,_,DuracaoMenor,_),
    DuracaoMenor =< Duracao.

/*E verdade se ListaDisciplinas e a lista (ordenada alfabeticamente) do nome de todas
as disciplinas do curso Curso.*/
procuraDisciplinas(Curso, ListaDisciplinas):-
    findall(NomeDisciplina,(turno(ID,Curso,_,_),evento(ID,NomeDisciplina,_,_,_)),ListaDisciplinas1),
    sort(ListaDisciplinas1, ListaDisciplinas).

/*E verdade se Semestres e uma lista composta por duas listas (ordenadas e sem elementos repetidos),
a primeira contem as disciplinas da lista ListaDisciplinas que sao do primeiro semestre e a segunda
as disciplinas do segundo (tambem ordenada e sem elementos repetidos). O predicado falha se alguma 
das disciplinas nao existir no curso Curso.*/
organizaDisciplinas(ListaDisciplinas, Curso, Semestres):-
    sort(ListaDisciplinas, ListaDisciplinasSorted),
    organizaDisciplinas_aux(ListaDisciplinasSorted, Curso, Semestres). %Predicado auxiliar recebe ListaDisciplinas ordenada e sem elementos repetidos.

/*(Caso terminal) Predicado auxiliar ao predicado OrganizaDisciplinas (tambem e verdade se acontecer
o mesmo que em OrganizaDisciplinas).*/
organizaDisciplinas_aux([],_,[[],[]]):- !.
organizaDisciplinas_aux([P|R], Curso, [[P|S],T]):- %Caso de disciplina P ocorrer no primeiro semestre.
    turno(ID, Curso, _,_),
    evento(ID, P, _,_,_),
    (getPeriodo(p1, ID); getPeriodo(p2, ID)),
    organizaDisciplinas_aux(R, Curso, [S,T]), !.   
organizaDisciplinas_aux([P|R], Curso, [T,[P|S]]):- %Caso de disciplina P ocorrer no segundo semestre.
    turno(ID, Curso, _,_),
    evento(ID, P, _,_,_),
    (getPeriodo(p3, ID); getPeriodo(p4, ID)),
    organizaDisciplinas_aux(R, Curso, [T,S]), !.

/*E verdade se TotalHoras e o total de horas dos eventos do curso Curso, que ocorrem no ano Ano e no periodo Periodo.*/
horasCurso(Periodo, Curso, Ano, TotalHoras):-
    findall(ID, turno(ID,Curso,Ano,_), ListaIDs), %ListaIDs e uma lista com IDs dos eventos do curso Curso e do ano Ano.
    include(getPeriodo(Periodo), ListaIDs, ListaIDsPeriodo1),
    sort(ListaIDsPeriodo1, ListaIDsPeriodo),
    maplist(substitui_id_duracao, ListaIDsPeriodo, ListaDuracoes), %ListaDuracoes e a lista com as duracoes de todos os IDs da lista ListaIDsPeriodo.
    sumlist(ListaDuracoes, TotalHoras).

/*Predicado auxiliar ao predicado horasCurso (maplist) que e verdade se ID e o identificador do evento com duracao Duracao.*/
substitui_id_duracao(ID, Duracao):- horario(ID,_,_,_,Duracao,_).

/*E verdade se Evolucao for uma lista com tuplos ((Ano, Periodo, NumHoras)) em que NumHoras
e o total de horas do curso Curso, no ano Ano e no periodo Periodo.*/
evolucaoHorasCurso(Curso, Evolucao):-
    evolucaoHorasCurso_aux(Curso, Evolucao, [1,1,1,1,2,2,2,2,3,3,3,3], [p1,p2,p3,p4,p1,p2,p3,p4,p1,p2,p3,p4]). 
/*(Caso terminal) Predicado auxiliar ao predicado evolucaoHorasCurso.*/
evolucaoHorasCurso_aux(_, [], [], []):- !. 
/*E verdade se Evolucao for o mesmo que e no predicado evolucaoHorasCurso, porem este predicado recebe duas listas,
uma com os anos e outra com os periodos, que vao pertencer aos tuplos da lista Evolucao. */
evolucaoHorasCurso_aux(Curso, Evolucao, [P|R], [S|T]):-
    horasCurso(S, Curso, P, TotalHoras), %Utilizacao do predicado horasCurso para calcular o TotalHoras.
    TupEvolucao = (P,S,TotalHoras),
    Evolucao = [TupEvolucao|TupEvolucao2],
    evolucaoHorasCurso_aux(Curso, TupEvolucao2, R, T).

    %%%%%
    %3.3%
    %%%%%

/*E verdade se Horas e o numero de horas sobrepostas entre o evento (HoraInicioEvento, HoraFimEvento)
e o slot (HoraInicioDada, HoraFimDada), se nao houver sobreposicao o predicado falha.*/
ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas):- 
    Maior_Iniciais is max(HoraInicioDada, HoraInicioEvento),
    Menor_Finais is min(HoraFimDada, HoraFimEvento),
    not(Menor_Finais - Maior_Iniciais =< 0), %horas sobrepostas nao sao zero.
    Horas is Menor_Finais - Maior_Iniciais.

/*E verdade se SomaHoras e o total de horas ocupadas nas salas do tipo TipoSala no intervalo
de tempo definido pela HoraInicio e HoraFim, no dia de semana DiaSemana e no periodo Periodo.*/
numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras):-
    salas(TipoSala, Salas),
    evento_aux(Salas, IDsTipoSala),
    include(getPeriodo(Periodo), IDsTipoSala, IDsPeriodo), %IDsPeriodo e uma lista com os IDs cuja sala e do tipo TipoSala e cujo periodo e Periodo.
    findall(IDs, (member(IDs, IDsPeriodo), horario(IDs, DiaSemana,_,_,_,_)), IDsDiaSemana),
    horasocupadas_aux(IDsDiaSemana, HoraInicio, HoraFim, ListaSomaHoras),
    sumlist(ListaSomaHoras, SomaHoras), !.

/*(Caso terminal) Predicado auxiliar ao predicado numHorasOcupadas - E verdade se IDsTipoSala 
(presente no predicado numHorasOcupadas) e uma lista com todos os IDs cuja sala e do tipo TipoSala
(presente no predicado numHorasOcupadas).*/
evento_aux([], []):- !. 
evento_aux([P|R], IDs_evento):-
    findall(ID, evento(ID,_,_,_,P), IDs_evento1),
    evento_aux(R, IDs_evento2),
    append(IDs_evento1, IDs_evento2, IDs_evento).

/*(Caso terminal) Predicado auxiliar ao predicado numHorasOcupadas - E verdade se ListaSomaHoras
(presente no predicado numHorasOcupadas) for uma lista com as sobreposicoes entre os eventos com
ID presente na lista IDsDiaSemana (presente no predicado numHorasOcupadas) e o intervalo de tempo
definido por HoraInicio e HoraFim (presentes no predicado numHorasOcupadas).*/
horasocupadas_aux([],_,_,[]):- !. 
horasocupadas_aux([P|R], HoraInicio, HoraFim, [HorasOcupadas|S]):- %Caso de haver sobreposicao
    horario(P, _, Inicio, Fim, _,_),
    ocupaSlot(Inicio, Fim, HoraInicio, HoraFim, HorasOcupadas),
    horasocupadas_aux(R, HoraInicio, HoraFim, S).
horasocupadas_aux([P|R], HoraInicio, HoraFim, S):- %Caso de nao haver sobreposicao
    horario(P, _, Inicio, Fim, _,_),
    not(ocupaSlot(Inicio, Fim, HoraInicio, HoraFim, _)),
    horasocupadas_aux(R, HoraInicio, HoraFim, S).

/*E verdade se Max e o numero de horas possiveis a ser ocupadas por salas do tipo TipoSala
no intervalo de tempo definido por HoraInicio e HoraFim.*/
ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max):- 
    salas(TipoSala, TotalSalas),
    length(TotalSalas, NumeroSalas),
    Max is (HoraFim - HoraInicio)*NumeroSalas.

/*E verdade se Percentagem e a divisao de SomaHoras por Max, multiplicada por 100.*/
percentagem(SomaHoras, Max, Percentagem):-
    Percentagem is (SomaHoras/Max)*100.

/*E verdade se Resultados e uma lista ordenada de tuplos (casosCriticos(DiaSemana, TipoSala, Percentgem)
em que DiaSemana e um dia da semana, TipoSala e um tipo de sala e Percentagem e uma percentagem de ocupacao,
no intervalo de tempo definido por HoraInicio e HoraFim, que e maior qu Threshold.*/
ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados):-
    findall(ID, evento(ID,_,_,_,_), IDs),
    ocupacaoCritica_aux(IDs, HoraInicio, HoraFim, Threshold, Resultados1),
    sort(Resultados1, Resultados).

/*(Caso terminal) Predicado auxiliar ao predicado ocupacaoCritica - E verdade se acontecer
o mesmo do que no predicado ocupacaoCritica (excepto no facto de IDs ser uma lista composta
por todos os IDs), (Resultados1). Este predicado e recursivo pois verifica a percentagem de
cada ID pertencente a lista IDs (presente no predicado ocupacaoCritica).
ocupacaoCritica_aux([P|R], HoraInicio, HoraFim, Threshold, S):- %Caso da percentagem ser maior.*/
ocupacaoCritica_aux([],_,_,_,[]):- !.
    horario(P, DiaSemana,_,_,_, Periodo),
    evento(P,_,_,_, Sala),
    (salas(TipoSala, [_|Sala]); salas(TipoSala, [Sala|_])), %Identificar o tipo de sala a partir do nome da sala com ID P.
    numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras),
    ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max),
    percentagem(SomaHoras, Max, Percentagem),
    Percentagem > Threshold, !,
    PercentagemArredondada is ceiling(Percentagem),
    S = [casosCriticos(DiaSemana, TipoSala, PercentagemArredondada)|T],
    ocupacaoCritica_aux(R, HoraInicio, HoraFim, Threshold, T).
ocupacaoCritica_aux([_|R], HoraInicio, HoraFim, Threshold, S):- %Caso da percentagem ser menor ou igual.
    ocupacaoCritica_aux(R, HoraInicio, HoraFim, Threshold, S).
    
    %%%%%
    %3.4%
    %%%%%
    
/*E verdade se OcupacaoMesa e uma lista com 3 listas (a primeira contem um lado da mesa - 3 lugares,
a segunda as cabeceiras - 2 lugares, e o terceiro o outro lado - 3 lugares), ListaPessoas e a lista
com o nome das pessoas que se vao sentar na mesa e ListaRestricoes e a lista de restricoes a verificar
para os lugares onde as pessoas se sentam ser unica.*/
ocupacaoMesa([A,B,C,D,E,F,G,H], ListaRestricoes, OcupacaoMesa):-
    sentado(A, OcupacaoMesa), %sentar todas as pessoas na mesa
    sentado(B, OcupacaoMesa),
    sentado(C, OcupacaoMesa),
    sentado(D, OcupacaoMesa),
    sentado(E, OcupacaoMesa),
    sentado(F, OcupacaoMesa),
    sentado(G, OcupacaoMesa),
    sentado(H, OcupacaoMesa),
    ocupacaoMesa_aux(ListaRestricoes, OcupacaoMesa), !.

/*(Caso terminal) Predicado auxiliar ao predicado ocupacaoMesa - E verdade pela mesma
razao do predicado ocupacaoMesa, porem as pessoas ja estam sentadas na mesa. 
Assim, este predicado aplica as restricoes a verificar.*/
ocupacaoMesa_aux([], _).
ocupacaoMesa_aux([P|R], OcupacaoMesa):- %P e a restricao em conta.
    P =.. [A|B], %A sera o functor e B (lista) o argumento.
    append([A], B, Literal), %Literal sera a lista que contem o functor A e o argumento B.
    append(Literal, [OcupacaoMesa], NovoLiteral), %NovoLiteral sera uma lista que contem A, B e OcupacaoMesa.
    Novo_P =.. NovoLiteral, 
    Novo_P, %Executar Novo_P
    ocupacaoMesa_aux(R, OcupacaoMesa).

sentado(NomePessoa, [[NomePessoa,_,_],[_,_],[_,_,_]]). %E verdade se NomePessoa estiver sentado na mesa.
sentado(NomePessoa, [[_,NomePessoa,_],[_,_],[_,_,_]]).
sentado(NomePessoa, [[_,_,NomePessoa],[_,_],[_,_,_]]).
sentado(NomePessoa, [[_,_,_],[NomePessoa,_],[_,_,_]]).
sentado(NomePessoa, [[_,_,_],[_,NomePessoa],[_,_,_]]).
sentado(NomePessoa, [[_,_,_],[_,_],[NomePessoa,_,_]]).
sentado(NomePessoa, [[_,_,_],[_,_],[_,NomePessoa,_]]).
sentado(NomePessoa, [[_,_,_],[_,_],[_,_,NomePessoa]]).
cab1(NomePessoa, [[_,_,_],[NomePessoa,_],[_,_,_]]). %E verdade se NomePessoa for a pessoa que fica na cabeceira 1 (lareira)
cab2(NomePessoa, [[_,_,_],[_,NomePessoa],[_,_,_]]). %E verdade se NomePessoa for a pessoa que fica na cabeceira 2.
honra(NomePessoa1, NomePessoa2, [[_,_,NomePessoa2],[_,NomePessoa1],[_,_,_]]). %E verdade se NomePessoa1 estiver numa das cabeceiras e NomePessoa2 estiver noutra.
honra(NomePessoa1, NomePessoa2, [[_,_,_],[NomePessoa1,_],[NomePessoa2,_,_]]).
lado(NomePessoa1, NomePessoa2, [[NomePessoa1,NomePessoa2,_],[_,_],[_,_,_]]). %E verdade se NomePessoa1 e NomePessoa2 estiverem lado a lado na mesa.
lado(NomePessoa1, NomePessoa2, [[_,NomePessoa1,NomePessoa2],[_,_],[_,_,_]]).
lado(NomePessoa1, NomePessoa2, [[_,_,_],[_,_],[NomePessoa1,NomePessoa2,_]]).
lado(NomePessoa1, NomePessoa2, [[_,_,_],[_,_],[_,NomePessoa1,NomePessoa2]]).
lado(NomePessoa1, NomePessoa2, [[NomePessoa2,NomePessoa1,_],[_,_],[_,_,_]]).
lado(NomePessoa1, NomePessoa2, [[_,NomePessoa2,NomePessoa1],[_,_],[_,_,_]]).
lado(NomePessoa1, NomePessoa2, [[_,_,_],[_,_],[NomePessoa2,NomePessoa1,_]]).
lado(NomePessoa1, NomePessoa2, [[_,_,_],[_,_],[_,NomePessoa2,NomePessoa1]]).
naoLado(NomePessoa1, NomePessoa2, OcupacaoMesa):- not(lado(NomePessoa1, NomePessoa2, OcupacaoMesa)). %E verdade se NomePessoa1 e NomePessoa2 nao estiverem lado a lado na mesa.
frente(NomePessoa1, NomePessoa2, [[NomePessoa1,_,_],[_,_],[NomePessoa2,_,_]]). %E verdade se NomePessoa1 e NomePessoa2 estiverem frente a frente na mesa (cabeceiras nao contam).
frente(NomePessoa1, NomePessoa2, [[_,NomePessoa1,_],[_,_],[_,NomePessoa2,_]]).
frente(NomePessoa1, NomePessoa2, [[_,_,NomePessoa1],[_,_],[_,_,NomePessoa2]]).
frente(NomePessoa1, NomePessoa2, [[NomePessoa2,_,_],[_,_],[NomePessoa1,_,_]]).
frente(NomePessoa1, NomePessoa2, [[_,NomePessoa2,_],[_,_],[_,NomePessoa1,_]]).
frente(NomePessoa1, NomePessoa2, [[_,_,NomePessoa2],[_,_],[_,_,NomePessoa1]]).
naoFrente(NomePessoa1, NomePessoa2, OcupacaoMesa):- not(frente(NomePessoa1, NomePessoa2, OcupacaoMesa)). %E verdade se NomePessoa1 e NomePessoa2 nao estiverem frente a frente na mesa (cabeceiras nao contam).













