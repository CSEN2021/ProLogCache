convertBinToDec(Bin,Dec):-
    convertBinToDec(Bin,Dec,0).
convertBinToDec(0, 0, _).
convertBinToDec(Bin,Dec,P):-
    Bin > 0,
    Bit is Bin mod 10,
    BinNew is Bin // 10,
    PNew is P + 1,
    convertBinToDec(BinNew,DecNew,PNew),
    Dec is DecNew + Bit *(2**P).

replaceIthItem(Item,[_|T],0,[Item|T]).
replaceIthItem(Item,[H|T],I,Result):-
    I \= 0,
    I1 is I-1,
    replaceIthItem(Item,T,I1,T1),
    Result=[H|T1].

splitEvery(N, List, R):-
    splitEvery(N, N, List, R).
splitEvery(N, N, List, [List]):-
    length(List, Ln),
    Ln > 0,
    N > Ln.
splitEvery(_, _, [], []).
splitEvery(N, 0, List, [[]|T]):-
    splitEvery(N, N, List, T).
splitEvery(N, Acc, [H|T], [[H|T1]|T2]):-
    Acc > 0,
    NewAcc is Acc - 1,
    splitEvery(N, NewAcc, T, [T1|T2]).


logBase2(1, 0).
logBase2(Num,Res):-
    Num > 1,
    Num2 is Num // 2,
    logBase2(Num2, Res2),
    Res is Res2 + 1.

getNumBits(_,fullyAssoc,_,0).
getNumBits(NumOfSets,setAssoc,_,BitsNum):-
    logBase2(NumOfSets, BitsNum).
getNumBits(_,directMap,Cache,BitsNum):-
    length(Cache, N),
    logBase2(N, BitsNum).

fillZeros(R, 0, R).
fillZeros(String,N,R):-
    N > 0,
    string_concat("0", String, StringNew),
    N1 is N - 1,
    fillZeros(StringNew,N1,R).
