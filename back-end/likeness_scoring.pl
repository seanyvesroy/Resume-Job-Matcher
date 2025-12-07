:- use_module(library(lists)).
:- use_module(library(aggregate)).

% -------------------------
% 1) Knowledge Graph
% -------------------------
related('azure', '.net', 0.85).
related('azure', 'aws', 0.65).
related('.net', 'csharp', 0.9).
related('machine_learning', 'data_science', 0.85).
related('sql', 'mssql', 0.9).

edge(X,Y,W) :- related(X,Y,W).
edge(X,Y,W) :- related(Y,X,W).

% -------------------------
% 2) Aliases
% -------------------------
alias('c#','csharp').
alias('microsoft_sql_server','mssql').

canonical(T, C) :-
    ( alias(T, A) -> C = A ; C = T ).

% -------------------------
% 3) Resume / Job Facts — NOW FULLY DYNAMIC
% -------------------------
:- dynamic resume_term/3.
:- dynamic job_term/4.

% -------------------------
% 4) Temporal Utilities
% -------------------------
ym_to_ym(YM, Y, M) :- Y is YM // 100, M is YM mod 100.

months_between(YM1, YM2, Months) :-
    ym_to_ym(YM1, Y1, M1),
    ym_to_ym(YM2, Y2, M2),
    Months is (Y2 - Y1) * 12 + (M2 - M1) + 1.

months_intersection(Start1, End1, Start2, End2, 0) :-
    ( End1 < Start2 ; End2 < Start1 ), !.
months_intersection(Start1, End1, Start2, End2, Months) :-
    MaxStart is max(Start1, Start2),
    MinEnd is min(End1, End2),
    months_between(MaxStart, MinEnd, Months).

temporal_factor(RStart, REnd, none, none, Factor) :-
    months_between(RStart, REnd, M),
    Factor0 is M / 24,
    Factor is min(1.0, Factor0), !.

temporal_factor(RStart, REnd, JStart, JEnd, Factor) :-
    JStart \= none, JEnd \= none, !,
    months_intersection(RStart, REnd, JStart, JEnd, Overlap),
    (Overlap = 0 -> Factor = 0.0 ;
     months_between(JStart, JEnd, JDur),
     Factor is Overlap / JDur).

temporal_factor(RStart, REnd, JStart, none, Factor) :-
    JStart \= none,
    months_intersection(RStart, REnd, JStart, 999912, Overlap),
    (Overlap = 0 -> Factor = 0.0 ;
     Factor0 is Overlap / 36,
     Factor is min(1.0, Factor0)).

% -------------------------
% 5) Similarity Measure
% -------------------------
graph_similarity(A, B, S) :-
    canonical(A, Ac),
    canonical(B, Bc),
    ( Ac == Bc -> S = 1.0
    ; edge(Ac, Bc, W) -> S = W
    ; S = 0.0 ).

% -------------------------
% 6) Pair Score
% -------------------------
pair_score(JTerm, RTerm, Importance, PairScore, details{sim:Sim, temp:TF}) :-
    graph_similarity(JTerm, RTerm, Sim),
    resume_term(RTerm, RStart, REnd),
    job_term(JTerm, Importance, JStart, JEnd),
    temporal_factor(RStart, REnd, JStart, JEnd, TF),
    Raw is Sim * TF,
    PairScore is Importance * Raw.

best_match_for_job(JTerm, BestRTerm, BestScore, Details) :-
    findall( pair(RTerm,Score,Det),
             ( resume_term(RTerm,_,_),
               pair_score(JTerm,RTerm,_,Score,Det) ),
             Pairs),
    Pairs \= [], !,
    sort(2, @>=, Pairs, Sorted),
    Sorted = [ pair(BestRTerm, BestScore, Details) | _ ].
best_match_for_job(_, none, 0.0, _{}).

% -------------------------
% 7) Final Aggregate Score
% -------------------------
final_score(Score, Breakdown) :-
    findall(Importance, job_term(_, Importance, _, _), Imps),
    sum_list(Imps, Den),
    Den > 0,
    findall(entry(JTerm,Importance,BestRTerm,PairScore,Sim,TF),
      ( job_term(JTerm, Importance, JStart, JEnd),
        best_match_for_job(JTerm, BestRTerm, PairScore,
                           details{sim:Sim,temp:TF})
      ),
      Breakdown),
    findall(PS, member(entry(_,_,_,PS,_,_),Breakdown), PSs),
    sum_list(PSs, Num),
    Score is Num / Den.
