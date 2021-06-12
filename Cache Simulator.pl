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

%component 1 & 2

getDataFromCache(StringAddress,Cache,Data,0,directMap,BitsNum):-
    getIndexTag(StringAddress,BitsNum,Index,Tag),
    atom_number(Index,Idx),
    convertBinToDec(Idx,Ind),
    nth0(Ind, Cache, Item),
    Item=item(tag(Tag),data(Data),1,_).

getDataFromCache(StringAddress,Cache,Data,HopsNum,setAssoc,SetsNum):-
    getNumBits(SetsNum,setAssoc,_,BitsNum),
    getIndexTag(StringAddress,BitsNum,Index,Tag),
    atom_number(Index, IndexBin),
    convertBinToDec(IndexBin,IndexDecimal),
    length(Cache,CacheLength),
    AddressPerSet is CacheLength // SetsNum,
    splitEvery(AddressPerSet, Cache, Sets),
    nth0(IndexDecimal, Sets, Set),
    findTagInSet(Tag, Set, Data, 0, HopsNum).

convertAddress(Bin,BitsNum,Tag,Idx,directMap):-
    Idx is Bin mod (10 ** BitsNum),
    Tag is Bin // (10 ** BitsNum).

convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):-
    getNumBits(SetsNum,setAssoc,_,BitsNum),
    Idx is Bin mod (10 ** BitsNum),
    Tag is Bin // (10 ** BitsNum).

getIndexTag(StringAddress,BitsNum,Index,Tag):-
    string_length(StringAddress,StringLength),
    atom_number(StringAddress, Number),
    
    Ind is Number mod (10 ** BitsNum),
    atom_number(StringIndex, Ind),
    string_length(StringIndex,LenIndex),
    Diff is BitsNum - LenIndex,
    
    T is Number // (10 ** BitsNum),
    atom_number(StringTag,T),
    string_length(StringTag,LenTag),
    DiffTag is StringLength - LenTag - BitsNum,
    fillZeros(StringTag,DiffTag,Tag),
    
    fillZeros(StringIndex,Diff,Index).
    
findTagInSet(Tag, [item(tag(Tag), data(Data), 1, _)|_], Data, Acc, Acc).
    
findTagInSet(Tag,[item(tag(Tag2),_,_,_)|T], Data, Acc, HopsNum):-
    Tag \= Tag2,
    Acc1 is Acc + 1,
    findTagInSet(Tag,T, Data, Acc1, HopsNum).

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

sumListedList([],Acc,Acc).
sumListedList([H|T],Acc,R):-
    append(Acc, H, NewAcc),
    sumListedList(T, NewAcc, R).

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,BitsNum):-
    convertBinToDec(Idx,I),
    MemAdrsBin is Tag * (10**BitsNum) + Idx,
    convertBinToDec(MemAdrsBin,MemAdrs),
    nth0(MemAdrs, Mem, ItemData),
    tagToString("123456",BitsNum,Tag,FinalTag),
    replaceIthItem(item(tag(FinalTag),data(ItemData),1,0),OldCache,I,NewCache).

replaceInCache(Tag,0,Mem,OldCache,NewCache,ItemData,fullyAssoc,_):-
    convertBinToDec(Tag, MemAdrs),
    nth0(MemAdrs, Mem, ItemData),
    tagToString("123456",0,Tag,FinalTag),
    searchCache(OldCache,Indx),
    incrementCache(OldCache, MidCache),
    replaceIthItem(item(tag(FinalTag),data(ItemData),1,0),MidCache,Indx,NewCache).

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
    convertBinToDec(Idx,I),
    logBase2(SetsNum, BitsNum),
    MemAdrsBin is Tag * (10**BitsNum) + Idx,
    convertBinToDec(MemAdrsBin,MemAdrs),
    nth0(MemAdrs, Mem, ItemData),
    tagToString("123456",BitsNum,Tag,FinalTag),
    length(OldCache, CacheLen),
    SetSize is CacheLen // SetsNum,
    splitEvery(SetSize, OldCache, ListedCache),
    nth0(I, ListedCache, TheSet),
    searchCache(TheSet, Indx),
    incrementCache(TheSet, IncSet),
    replaceIthItem(item(tag(FinalTag),data(ItemData),1,0),IncSet,Indx,NewSet),
    replaceIthItem(NewSet,ListedCache,I,NewListedCache),
    sumListedList(NewListedCache, [], NewCache).

% Pre-Implemented Predicates

getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,hit):-
    getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
    NewCache = OldCache.
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
    \+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
    atom_number(StringAddress,Address),
    convertAddress(Address,BitsNum,Tag,Idx,Type),
    replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).

runProgram([],OldCache,_,OldCache,[],[],Type,_).
runProgram([Address|AdressList],OldCache,Mem,FinalCache,[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
    getNumBits(NumOfSets,Type,OldCache,BitsNum),
    (Type = setAssoc, Num = NumOfSets; Type \= setAssoc, Num = BitsNum),
    getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,Num,Status),
    runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,Type,NumOfSets).


