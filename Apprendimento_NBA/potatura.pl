:- ensure_loaded(attributi).
:- ensure_loaded(test_set).
:- ensure_loaded(training_set).
:- ensure_loaded(albero).
:- op(300, xfx, <==).
:- dynamic alb/1.

% Programma Prolog che effettua la potatura di un albero di decisione
% per valutare il cambiamento delle sue prestazioni.
%
% NOTA: per eseguire correttamente il programma e' necessario aumentare
% lo stack size disponibile (3 GB in questo caso) con il seguente
% comando: set_prolog_stack(global, limit(3*10**9)).
% Potrebbe essere necessario modificare in base alla propria
% configurazione. Assicurarsi di avere il file albero.pl che viene
% generato dopo aver lanciato l'induzione dell'albero con il criterio di
% gini o dell'entropia, nella propria workspace.
%
% Il programma prima calcola tutti i sotto-alberi procedendo in
% profondita'. Successivamente, il programma scorre la lista dei
% sotto-alberi e rimuove ognuno di questi dall'albero originale,
% mantenendo la potatura solo se questa ha migliorato le prestazioni,
% valutate con il test del chi-quadro. Infine, viene mostrata la matrice
% di confusione finale dell'albero potato.

% Lancia la potatura dell'albero originale
lancia(MigliorAlbero, Punteggio) :-
    findall(t(Att,Val), t(Att,Val), Albero),
    lancia_sottoalberi(Albero, [], [], ListaFinale),
    length(ListaFinale, Length),
    inverti(ListaFinale, ListaPotatura,[]),
    lancia_potatura(Albero, ListaPotatura, MigliorAlbero, 100,Punteggio,Length),
    assert(alb(MigliorAlbero)),
    stampa_matrice_di_confusione(MigliorAlbero).

% inverte una lista
inverti([],Z,Z).
inverti([H|T],Z,Acc) :- inverti(T,Z,[H|Acc]).


% Prova la potatura per ogni sottoalbero mantenendola solo se questa ha
% migliorato le predizione (utilizzando il test del chi-quadro)
lancia_potatura(Albero,_,AlberoVincente,PunteggioCorrente, PunteggioFinale, 0) :-
    AlberoVincente = Albero,
    PunteggioFinale = PunteggioCorrente.
lancia_potatura([Albero|_], [SubAlbero|Resto], MigliorAlbero, PunteggioCorrente, PunteggioFinale, Limite) :-
    Limite > 0,
    writeln(Limite),
    rimuovi_sottoalbero(Albero, SubAlbero, AlberoPotato), % pota l'albero rimuovendo il sottoalbero
    valuta_chi_quadro(Albero, AlberoPotato, AlberoVincente, Punteggio), % vede se la potatura ha migliorato l'accuratezza
    Punteggio =< PunteggioCorrente,
    NuovoLimite is Limite - 1,
    lancia_potatura([AlberoVincente], Resto, MigliorAlbero, Punteggio, PunteggioFinale, NuovoLimite).

% Aggiunge i sottoalberi ad una lista procedendo per livelli di
% profondita' nell'albero originale
lancia_sottoalberi([], [], ListaFinale, ListaFinale).
lancia_sottoalberi([], _, ListaCorrente, ListaFinale) :-
    lancia_sottoalberi(ListaCorrente, [], ListaCorrente, ListaFinale).
lancia_sottoalberi([Albero|Resto], ListaLivelloCorrente, ListaCorrente, ListaFinale) :-
    ricava_sottoalberi(Albero, [], SottoAlberi),
    (SottoAlberi = [[]] -> (ListaLivello = [],
                            ListaRisultante = ListaCorrente);
    append(ListaLivelloCorrente, SottoAlberi, ListaLivello),
    append(ListaCorrente, SottoAlberi, ListaRisultante)),
    lancia_sottoalberi(Resto, ListaLivello, ListaRisultante, ListaFinale).

% Predicati per ricavare i sottoalberi
ricava_sottoalberi(t(_,[]),ListaCorrente,ListaFinale) :-
    reverse(ListaCorrente, ListaFinale).
ricava_sottoalberi(t(Att, [Val|Resto]), ListaCorrente, ListaFinale) :-
    (Resto = [], ListaCorrente = [] -> (controlla_se_lista(Val, ValAggiustato),
                                        (rimuovi_valore(ValAggiustato, t(Att2,[Val2|Resto2])),
                                         costruisci_albero(Att2, [Val2], Risultato),
                                         ricava_sottoalberi(t(Att2,Resto2), [Risultato|ListaCorrente], ListaFinale))
                                        ;
                                        (rimuovi_valore([Val], [l(_)|Resto]);rimuovi_valore([Val],l(_));rimuovi_valore([Val],null)),
                                        ricava_sottoalberi(t(Att,Resto),[Resto|ListaCorrente], ListaFinale))
    ;
    controlla_se_lista(Val, ValAggiustato),
    costruisci_albero(Att, ValAggiustato, Risultato),
    ricava_sottoalberi(t(Att,Resto), [Risultato|ListaCorrente], ListaFinale)).

% Costruisce la sintassi per un albero o sottoalbero partendo da
% attributo e valore
costruisci_albero(Att, Valore, AlberoRisultante) :-
    AlberoRisultante = t(Att,Valore).

% Predicato ausiliario per la rimozione del valore dalla struttura di un
% sottoalbero
rimuovi_valore([Valore|_], SubAlb) :-
    compound(Valore),
    arg(2, Valore, Arg1),
    SubAlb = Arg1.

% Predicato ausiliario che controlla se Valore è una lista, in caso
% contrario lo converte in una lista
controlla_se_lista(Valore, NuovoValore) :-
    \+is_list(Valore) -> NuovoValore = [Valore] ;
    NuovoValore = Valore.

% Predicato per la potatura effettiva di un sottoalbero da Albero
rimuovi_sottoalbero(Albero, SottoalberoDaRimuovere, AlberoRisultante) :-
    rimuovi_sottoalbero_helper(Albero, SottoalberoDaRimuovere, AlberoRisultante), !.

rimuovi_sottoalbero_helper(t(Attributo, Valori), SottoalberoDaRimuovere, t(Attributo, ValoriAggiornati)) :-
    remove_subtree_from_list(Valori, Attributo, SottoalberoDaRimuovere, ValoriAggiornati), !.

rimuovi_sottoalbero_helper(l(Classe), _, l(Classe)) :- !.

rimuovi_sottoalbero_helper(null, _, null) :- !.

remove_subtree_from_list([],_, _, []).
remove_subtree_from_list([H|T], Att, SottoalberoDaRimuovere, T1) :-
    (t(Att,[H]) == SottoalberoDaRimuovere
    ->  remove_subtree_from_list(T, Att,SottoalberoDaRimuovere, T1)
    ;   H = Tier:SubTree,
        rimuovi_sottoalbero_helper(SubTree, SottoalberoDaRimuovere, SubTreeAggiornato),
        T1 = [Tier:SubTreeAggiornato|T2],
        remove_subtree_from_list(T, Att, SottoalberoDaRimuovere, T2)
    ).
remove_subtree_from_list([H|T], Att ,SottoalberoDaRimuovere, [H|T1]) :-
    H \= _:_,
    remove_subtree_from_list(T, Att, SottoalberoDaRimuovere, T1).

% Predicato che decide qual e' l'albero "migliore" in base al punteggio
% ottenuto con il test del chi-quadro
valuta_chi_quadro(Albero, AlberoPotato, AlberoVincente, MigliorCosto) :-
    costo_albero(Albero,CostoOriginale),
    costo_albero(AlberoPotato,CostoPotato),
    (	CostoOriginale < CostoPotato -> AlberoVincente = Albero, MigliorCosto = CostoOriginale
    ;
    (
     AlberoVincente = AlberoPotato, MigliorCosto = CostoPotato)
    ).

% Valuta un albero con il test del chi-quadro
costo_albero(Albero, Costo) :-
    findall(Classe/Oggetto,s(Classe,Oggetto),TestSet),
    length(TestSet,N),
    findall(sano/Oggetto,s(sano,Oggetto),SaniAttesi),
    length(SaniAttesi, Ns),
    InfAttesi is N - Ns,
    valuta(Albero,TestSet,VN,0,VP,0,FN,0,FP,0,_,0),
    SaniOsservati is VN + FN,
    InfOsservati is VP + FP,
    chi_quadro(InfOsservati, SaniOsservati, InfAttesi, Ns, Costo).

% Predicato che effettua il test del chi-quadro
chi_quadro(InfOss, SaniOss, InfAtt, SaniAtt, Costo) :-
    CostoInf is ((InfOss - InfAtt) ** 2) / InfAtt,
    CostoSani is ((SaniOss - SaniAtt) ** 2) / SaniAtt,
    Costo is CostoInf + CostoSani.

% Predicatii per calcolare vari positivi, falsi positivi, veri negativi
% e falsi negativi
valuta(_,[],VN,VN,VP,VP,FN,FN,FP,FP,NC,NC).
valuta(Albero,[sano/Oggetto|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
	(classifica(Oggetto,sano,Albero)-> VNA1 is VNA + 1, FPA1 = FNA, NCA1 = NCA ;
	(classifica(Oggetto,infortunato,Albero)-> FPA1 is FPA + 1, VNA1 = VNA, NCA1 = NCA ;
	NCA1 is NCA + 1, VNA1 = VNA, FPA1 = FPA)),
	valuta(Albero,Coda,VN,VNA1,VP,VPA,FN,FNA,FP,FPA1,NC,NCA1).
valuta(Albero,[infortunato/Oggetto|Coda],VN,VNA,VP,VPA,FN,FNA,FP,FPA,NC,NCA) :-
	(classifica(Oggetto,infortunato,Albero)-> VPA1 is VPA + 1, FNA1 = FNA, NCA1 = NCA ;
	(classifica(Oggetto,sano,Albero)-> FNA1 is FNA + 1, VPA1 = VPA, NCA1 = NCA ;
	NCA1 is NCA + 1, VPA1 = VPA, FNA1 = FNA)),
	valuta(Albero,Coda,VN,VNA,VP,VPA1,FN,FNA1,FP,FPA,NC,NCA1).

classifica(Oggetto,nc,t(Att,Valori)) :-
	member(Att=Val,Oggetto),
        member(Val:null,Valori).

classifica(Oggetto,Classe,t(Att,Valori)) :-
	member(Att=Val,Oggetto),
        member(Val:l(Classe),Valori).

classifica(Oggetto,Classe,t(Att,Valori)) :-
	member(Att=Val,Oggetto),
	delete(Oggetto,Att=Val,Resto),
	member(Val:t(AttFiglio,ValoriFiglio),Valori),
	classifica(Resto,Classe,t(AttFiglio,ValoriFiglio)).

% Predicato per mostrare la matrice di confusione
stampa_matrice_di_confusione([Albero|_]) :-
	findall(Classe/Oggetto,s(Classe,Oggetto),TestSet),
	length(TestSet,N),
	valuta(Albero,TestSet,VN,0,VP,0,FN,0,FP,0,NC,0),
	A is (VP + VN) / (VP+VN+FP+FN), % Accuratezza,
        arrotonda(A, Accuratezza),
	E is 1 - Accuratezza, % Errore
        arrotonda(E, Errore),
        P is VP / (VP + FN), % Precisione
        arrotonda(P, Precisione),
	write('Test effettuati :'),  writeln(N),
	write('Test non classificati :'),  writeln(NC),
	write('Veri sani  '), write(VN), write('   Falsi ifortuni '), writeln(FP),
	write('Falsi sani '), write(FN), write('   Veri infortuni  '), writeln(VP),
	write('Accuratezza: '), writeln(Accuratezza),
	write('Errore: '), writeln(Errore),
        write('Precisione: '), writeln(Precisione).

% Predicato ausiliario per arrotondare un numero decimale
arrotonda(Numero, Arrotondamento) :-
    Temp is round(Numero * 100),
    Arrotondamento is Temp / 100.
