%general part
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

%component 1

%component 2

%component 3

tagToString(MemoryWord,BitsNum,Tag,StrTag):-
    atom_length(MemoryWord, StrLen),
    atom_number(ShortTag, Tag),
    atom_length(ShortTag, ShortTagLen),
    TagLen is StrLen - ShortTagLen - BitsNum,
    fillZeros(ShortTag, TagLen, StrTag).

searchCache(OldCache,Indx):-
    searchCache(OldCache,0, Indx);
    searchCache(OldCache,-1,-1,0,Indx).


searchCache([item(tag(_), data(_), 0, _)|_],Acc,Acc).
searchCache([item(tag(_), data(_), Vbit, _)|T],Acc,Indx):-
    Vbit \= 0,
    NewAcc is Acc + 1,
    searchCache(T, NewAcc, Indx).

searchCache([],_,BgstIndx,_,BgstIndx).

searchCache([item(tag(_), data(_), Vbit, Obit)|T],Bgst,_,Acc,Indx):-
    Vbit \= 0,
    Obit > Bgst,
    NewAcc is Acc + 1,
    searchCache(T,Obit,Acc,NewAcc,Indx).

searchCache([item(tag(_), data(_), Vbit, Obit)|T],Bgst,BgstIndx,Acc,Indx):-
    Vbit \= 0,
    Obit < Bgst,
    NewAcc is Acc + 1,
    searchCache(T,Bgst,BgstIndx,NewAcc,Indx).

incrementCache([],[]).
incrementCache([item(Tag, Data, 1, Obit)|T], [item(Tag, Data, 1, ObitNew)|Tn]):-
    ObitNew is Obit + 1,
    incrementCache(T, Tn).
incrementCache([item(Tag, Data, 0, Obit)|T], [item(Tag, Data, 0, Obit)|Tn]):-
    incrementCache(T, Tn).

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,BitsNum):-
    convertBinToDec(Idx,I),
    MemAdrsBin is Tag * (10**BitsNum) + Idx,
    convertBinToDec(MemAdrsBin,MemAdrs),
    nth0(MemAdrs, Mem, ItemData),
    tagToString(ItemData,BitsNum,Tag,FinalTag),
    replaceIthItem(item(tag(FinalTag),data(ItemData),1,0),OldCache,I,NewCache).

replaceInCache(Tag,0,Mem,OldCache,NewCache,ItemData,fullyAssoc,_):-
    convertBinToDec(Tag, MemAdrs),
    nth0(MemAdrs, Mem, ItemData),
    tagToString(ItemData,0,Tag,FinalTag),
    searchCache(OldCache,Indx),
    incrementCache(OldCache, MidCache),
    replaceIthItem(item(tag(FinalTag),data(ItemData),1,0),MidCache,Indx,NewCache).

/*
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
    convertBinToDec(Idx,I),
    MemAdrsBin is Tag * (10**BitsNum) + Idx,
    convertBinToDec(MemAdrsBin,MemAdrs),
    nth0(MemAdrs, Mem, ItemData),
    tagToString(ItemData,BitsNum,Tag,FinalTag),*/
