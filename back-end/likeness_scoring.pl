% === likeness_scoring.pl ===
% Run with SWI-Prolog: swipl -s likeness_scoring.pl
% Example query at bottom: ?- demo_score(S).

:- use_module(library(lists)).
:- use_module(library(aggregate)).

% -------------------------
% 1) Knowledge graph edges
% Format: related(TermA, TermB, Weight).
% Terms should be lowercase and canonical.
% Weight range: 0..1 (we assume you've normalized externally or use large numbers and normalize in code).
% -------------------------
related('azure', '.net', 0.85).
related('azure', 'aws', 0.65).
related('.net', 'csharp', 0.9).
related('machine_learning', 'data_science', 0.85).
related('sql', 'mssql', 0.9).
%related('csharp', '.net', 0.9).
%related('.net', 'csharp', 0.9).
%related('azure', 'csharp', 0.8).

% symmetric view (make edges undirected for lookup)
edge(X,Y,W) :- related(X,Y,W).
edge(X,Y,W) :- related(Y,X,W).

% -------------------------
% 2) Aliases (optional)
% -------------------------
alias('c#','csharp').
alias('microsoft_sql_server','mssql').

% canonicalize term: apply alias map or lowercase (for demo we assume lower-case input)
canonical(T, C) :-
    ( alias(T, A) -> C = A ; C = T ).

% -------------------------
% 3) Example resume and job facts
% resume_term(Term, StartYYYYMM, EndYYYYMM).
% job_term(Term, Importance, ReqStartOpt, ReqEndOpt).
% Use ReqStartOpt = none if job doesn't require timeframe.
% -------------------------
resume_term('csharp', 202103, 202107).  % March 2018 - July 2021
resume_term('azure', 201901, 202305).   % Jan 2019 - May 2023
resume_term('sql', 201701, 201812).     % Jan 2017 - Dec 2018
%resume_term('python', 201605, 202305).

% Job posting wants:
job_term('azure', 1.0, none, none).
job_term('.net', 1.0, 201801, none).    % prefers experience since Jan 2018
job_term('mssql', 0.7, none, none).
%job_term('machine_learning', 0.5, none, none).

% -------------------------
% 4) Temporal utilities
% Months encoded as YYYYMM integer
% months_between(YYYYMM1, YYYYMM2, Months)
% -------------------------
ym_to_ym(YM, Y, M) :- Y is YM // 100, M is YM mod 100.

months_between(YM1, YM2, Months) :-
    ym_to_ym(YM1, Y1, M1), ym_to_ym(YM2, Y2, M2),
    Months is (Y2 - Y1) * 12 + (M2 - M1) + 1.  % inclusive months

months_intersection(Start1, End1, Start2, End2, 0) :-
    ( End1 < Start2 ; End2 < Start1 ), !.
months_intersection(Start1, End1, Start2, End2, Months) :-
    MaxStart is max(Start1, Start2),
    MinEnd is min(End1, End2),
    months_between(MaxStart, MinEnd, Months).

% temporal_factor: fraction of overlap relative to job's expected duration OR recent decay
% If job has none timeframe, we use a recency-weighted factor based on how recent the resume item ended.
% For simplicity we implement:
% - If job req has range: temporal = overlap_months / job_req_months (clamped 0..1)
% - If job req none: temporal = min(1, overlap_months / 24)  (experience counts up to 24 months)
temporal_factor(RStart, REnd, none, none, Factor) :-
    % treat total experience months, capped
    months_between(RStart, REnd, M), Factor0 is M / 24, Factor is min(1.0, Factor0), !.
temporal_factor(RStart, REnd, JStart, JEnd, Factor) :-
    JStart \= none, JEnd \= none, !,
    months_intersection(RStart, REnd, JStart, JEnd, Overlap),
    (Overlap = 0 -> Factor = 0.0 ; months_between(JStart, JEnd, JDur), Factor is Overlap / JDur).
temporal_factor(RStart, REnd, JStart, none, Factor) :-
    JStart \= none, JEnd = none, !,
    months_intersection(RStart, REnd, JStart, 999912, Overlap),
    (Overlap = 0 -> Factor = 0.0 ; % if job started requirement but no end, normalize by overlap/(years since job start ???)
     % simple heuristic: normalize by 36 months window
     Factor0 is Overlap / 36, Factor is min(1.0, Factor0)).

% -------------------------
% 5) Similarity measure
% If direct edge exists, use it. Otherwise zero (or you could implement path search or precompute node2vec then call it)
% -------------------------


graph_similarity(A, B, S) :-
    canonical(A, Ac), canonical(B, Bc),
    ( edge(Ac, Bc, W) -> S = W ; S = 0.0 ).

% -------------------------
% 6) For a given job term, find best resume term and its pair score
% pair_score(JobTerm, ResumeTerm, Importance, Score, Details)
% Score = Importance * (alpha * sim + (1-alpha) * embed_sim) * temporal_factor
% For this demo alpha=1 and no embed.
% -------------------------
pair_score(JTerm, RTerm, Importance, PairScore, details{sim:Sim, temp:TF}) :-
    graph_similarity(JTerm, RTerm, Sim),
    resume_term(RTerm, RStart, REnd),
    job_term(JTerm, Importance, JStart, JEnd),
    temporal_factor(RStart, REnd, JStart, JEnd, TF),
    % alpha = 1 -> sim only
    Raw is Sim * TF,
    PairScore is Importance * Raw.

% best_match_for_job(JobTerm, BestResumeTerm, BestScore, Details)
best_match_for_job(JTerm, BestRTerm, BestScore, Details) :-
    findall( pair(RTerm,Score,Det),
             ( resume_term(RTerm,_,_), pair_score(JTerm,RTerm,_,Score,Det) ),
             Pairs),
    Pairs \= [], !,
    sort(2, @>=, Pairs, Sorted),       % sort by Score desc
    Sorted = [ pair(BestRTerm, BestScore, Details) | _ ].
best_match_for_job(_JTerm, none, 0.0, _{}) :- true.

% -------------------------
% 7) Aggregate across job terms to final normalized score
% final_score(Score, BreakdownList)
% BreakdownList = [ job_term(Term, Importance, BestRTerm, PairScore, Sim, TF), ... ]
% -------------------------
final_score(Score, Breakdown) :-
    findall( Importance, job_term(_, Importance, _, _), Imps ),
    sum_list(Imps, Den),
    Den > 0,
    findall( entry(JTerm,Importance,BestRTerm,PairScore,Sim,TF),
             ( job_term(JTerm, Importance, JStart, JEnd),
               best_match_for_job(JTerm, BestRTerm, PairScore, details{sim:Sim, temp:TF})
             ),
             Breakdown),
    % sum pair scores
    findall(PS, member(entry(_,_,_,PS,_,_),Breakdown), PSs),
    sum_list(PSs, Num),
    Score is Num / Den.

% -------------------------
% 8) Demo helper
% -------------------------
demo_score(Score) :-
    final_score(Score, Breakdown),
    format("Final normalized score: ~2f~n", [Score]),
    format("Breakdown:~n"),
    forall(member(entry(J,Imp,BR,PS,Sim,TF), Breakdown),
           ( format(" job_term=~w importance=~w best_resume=~w pair_score=~4f sim=~4f temp=~4f~n",
                    [J,Imp,BR,PS,Sim,TF]) )),
    true.

% Allow running demo on load:
:- initialization(demo_on_load).

demo_on_load :-
    ( current_prolog_flag(argv, []) -> true ; true ),  % don't fail if CLI args
    writeln('--- Running demo_score ---'),
    demo_score(_),
    writeln('--- Demo complete ---').
