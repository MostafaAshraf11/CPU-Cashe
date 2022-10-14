convertBinToDec(A,D):- convertHelper(A,D,0).
convertHelper(0,0,Y).
convertHelper(A,D,Y):-  A > 0,
						X is (A mod 10),
						X=0,
						Y1 is (Y+1),
						A1 is (A//10),
						convertHelper(A1,D,Y1).

convertHelper(A,D,Y):- 	A > 0,
						X is (A mod 10),
						X=1,
						D2 is (2**Y),
						Y1 is (Y+1),
						A1 is (A//10),
						convertHelper(A1,D1,Y1),
						D is D1 + D2.

replaceIthItem(_,[],_,[]).
replaceIthItem(Item,[H|T],0,[Item|T]).
replaceIthItem(Item,[H|T],I,[H|T1]):- I>0, I1 is I-1,replaceIthItem(Item,T,I1,T1).

logBase2(1,0).
logBase2(Num,Res):- Num>1, N1 is Num/2,logBase2(N1,K) ,Res is K+1.


getNumBits(_,fullyAssoc,_,0).
getNumBits(_,setAssoc,_,1).
getNumBits(_,directMap,[H|T],BitsNum):- length([H|T],N), logBase2(N,BitsNum).

fillZeros(String,0,String).
fillZeros(String,N,R):- N>0 ,N1 is (N-1),string_concat("0", String, String1),fillZeros(String1,N1,R).


getDataFromCache(StringAddress,List,Data1,0,directMap,BitNum):-
												atom_number(StringAddress,Address),
												getTag(Address,BitNum,Tag1),
												getIdx(Address,BitNum,Idx),
												convertBinToDec(Idx,D),
												getDataHelper(List,D,Res),
												Res = item(tag(Tag),data(Data),ValidBit,Order),
												ValidBit=1,
												atom_number(Tag,T),
												Tag1=T,
												Data1 is Data. 
getDataHelper([H|T],0,H).
getDataHelper([H|T],Idx1,R):- Idx1 >0 , Idx is Idx1-1, getDataHelper(T,Idx,R). 

numLength(0,0).
numLength(Address,N):- Address>0,(A is Address//10) ,numLength(A ,N1), N is N1+1.


splitEvery(N,List,Res):-	
					splitHelper(N,N,List,[],[],Res).
splitHelper(N,0,List,Acc1,Acc2,Res):-
					append(Acc2,[Acc1],Acc2new),
				splitHelper(N,N,List,[],Acc2new,Res).

splitHelper(_,_,[],[],Acc,Acc).

splitHelper(N,N1,[H|T],Acc1,Acc2,Res):-	
					N1>0,
					N2 is N1-1,
					append(Acc1,[H],Acc3),
					splitHelper(N,N2,T,Acc3,Acc2,Res).

convertAddress(0,_,_,_,directMap).
convertAddress(Bin,0,Bin,_,directMap).
convertAddress(Bin,BitNum,Tag,Idx,directMap):- getTag(Bin,BitNum,Tag), getIdx(Bin,BitNum,Idx).
getTag(Bin,0,Bin).
getTag(Bin,BitNum,Tag):- BitNum>0, Bin1 is (Bin//10) ,Bit1 is (BitNum-1), getTag(Bin1,Bit1,Tag). 

getIdx(_,0,_).
getIdx(Bin,BitNum,R):- BitNum > 0,getIdx1(BitNum,D), R is Bin mod D.

getIdx1(1,10).
getIdx1(BitNum,D):- BitNum >1 , BitNum1 is BitNum-1, getIdx1(BitNum1,D1), D is D1*10.

getTagPos(Tag,0,Tag).
getTagPos(Tag,BitsNum,Address):- BitsNum>0, Tag1 is Tag*10, BitsNum1 is BitsNum-1,getTagPos(Tag1,BitsNum1,Address).


replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,directMap,BitsNum) :-    
										convertBinToDec(Idx,I),
										getTagPos(Tag,BitsNum,Tag1),
										convertBinToDec(Tag1,T),
										Address is I+T,
										getDataHelper(Mem,Address,M1),
										atom_number(M1,M2),
										getDataHelper(OldCache,I,Res),
										Res = item(tag(Tag2),data(Data),ValidBit,Order),
										NewRes = item(tag(Tag2),data(M2),1,0),
										replaceIthItem(NewRes,OldCache,I,NewCache),
										ItemData = M1.
										
										
convertAddress(0,_,_,_,setAssoc).
convertAddress(Bin,0,Bin,_,setAssoc).
convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):- logBase2(SetsNum,Num), getTag(Bin,Num,Tag),Num=0, Idx is 0.
convertAddress(Bin,SetsNum,Tag,Idx,setAssoc):- logBase2(SetsNum,Num), getTag(Bin,Num,Tag), Num>0, getIdx(Bin,Num,Idx).



getDataFromCache(StringAddress,Cache,Data,HopsNum,setAssoc,SetsNum):-
											logBase2(SetsNum,Num),
											atom_number(StringAddress,Address),
											getTag(Address,Num,Tag),
											getIdx(Address,Num,Idx),
											convertBinToDec(Idx,D),
											splitEvery(SetsNum,Cache,R),
											getDataHelper(R,D,Result),
											getData_set(Tag,D,Result,Data,HopsNum,setAssoc,SetsNum).
		
		
getData_set(Tag,D,Result,Data,HopsNum,setAssoc,SetsNum):- 
												getDataHelper(Result,0,R2),
												R2 = item(tag(Tag1),data(Data1),ValidBit,Order),
												ValidBit=1,
												atom_number(Tag1,T),
												Tag=T,
												HopsNum is D,
												Data = Data1.

getData_set(Tag,D,[H|T],Data,HopsNum,setAssoc,SetsNum):- 
												getDataHelper(Result,0,R2),
												R2 = item(tag(Tag1),data(Data1),ValidBit,Order),
												(ValidBit\=1;Tag\=T),
												getData_set(Tag,D,T,Data,HopsNum,setAssoc,SetsNum).
	






replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,fullyAssoc,BitsNum):- 
											length(OldCache,N),
											N>0,
											addOrder(OldCache,N,OldCache1),
											OldCache2 = OldCache1,
											replaceInCacheHelper(Tag,Idx,Mem,OldCache1,NewCache,ItemData,fullyAssoc,BitsNum,N,0,OldCache2).
											
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,fullyAssoc,BitsNum):- 
											length(OldCache,N),
											N>0,
											addOrder(OldCache,N,OldCache1),
											orderMax(OldCache1,0,R),
											OldCache2 = OldCache1,
											replaceInCacheHelper1(Tag,Idx,Mem,OldCache1,NewCache,ItemData,fullyAssoc,BitsNum,R,0,OldCache2).
											

replaceInCacheHelper(_,_,_,[],[],_,_,_,_,_,_).				
replaceInCacheHelper(_,_,_,_,_,_,_,_,0,_,_).	
replaceInCacheHelper(Tag,Idx,Mem,[H|T],NewCache,ItemData,fullyAssoc,BitsNum,N,Pos,OldCache2):- 
											N>0,
											[H|T] \= [],
											H = item(tag(Tag1),data(Data1),0,Order),
											atom_length(Tag1,R),
											numLength(Tag,R1),
											R2 is R-R1,
											number_string(Tag,X),
											fillZeros(X,R2,Tag2),
											getDataHelper(Mem,Tag,M1),
											Res = item(tag(Tag2),data(M1),1,0),
											replaceIthItem(Res,OldCache2,Pos,NewCache),
											ItemData = M1.												
replaceInCacheHelper(Tag,Idx,Mem,[H|T],NewCache,ItemData,fullyAssoc,BitsNum,Pos,OldCache2):-
											N>0,
											[H|T] \= [],
											H = item(tag(Tag1),data(Data1),1,Order),
											Pos1 is Pos+1,
											N1 is N-1,
											replaceInCacheHelper(Tag,Idx,Mem,T,NewCache,ItemData,fullyAssoc,BitsNum,N1,Pos1,OldCache2).
											

addOrder(Cache,0,Cache).
addOrder([H|T],N,[H1|T1]):- 
					N>0,
					H = item(tag(Tag1),data(Data1),ValidBit,Order),
					ValidBit=1,
					Order1 is Order+1,
					NewH = item(tag(Tag1),data(Data1),ValidBit,Order1),
					replaceIthItem(NewH,[H|T],0,[H1|T2]),
					N1 is N-1,
					addOrder(T2,N1,T1).
addOrder([H|T],N,[H|T1]):- 
					N>0,
					H = item(tag(Tag1),data(Data1),0,Order),
					ValidBit=0,
					N1 is N-1,
					addOrder(T,N1,T1).
					


orderMax([],Order,Order).
orderMax([H|T],Order,R):- H = item(tag(Tag1),data(Data1),1,Order1), Order1 > Order, orderMax(T, Order1, R). 
orderMax([H|T],Order,R):- H = item(tag(Tag1),data(Data1),1,Order1), Order1 =< Order, orderMax(T,Order, R).	


replaceInCacheHelper1(_,_,_,[],_,_,_,_,_,_,_).
replaceInCacheHelper1(Tag,Idx,Mem,[H|T],NewCache,ItemData,fullyAssoc,BitsNum,R,Pos,OldCache1):-
									H = item(tag(Tag1),data(Data1),1,R),
									atom_length(Tag1,R4),
									numLength(Tag,R1),
									R2 is R4-R1,
									number_string(Tag,X),
									fillZeros(X,R2,Tag2),
									getDataHelper(Mem,Tag,M1),
									Res = item(tag(Tag2),data(M1),1,0),
									replaceIthItem(Res,OldCache1,Pos,NewCache),
									ItemData = M1.

replaceInCacheHelper1(Tag,Idx,Mem,[H|T],NewCache,ItemData,fullyAssoc,BitsNum,R,Pos,OldCache1):-
									Pos1 is Pos+1,
									H \= item(tag(Tag1),data(Data1),1,R),
									replaceInCacheHelper1(Tag,Idx,Mem,T,NewCache,ItemData,fullyAssoc,BitsNum,R,Pos1,OldCache1).


replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):-
								convertBinToDec(Idx,I),
								logBase2(SetsNum,Set),
								getTagPos(Tag,Set,Tag1),
								convertBinToDec(Tag1,T),
								Address is I+T,
								splitEvery(SetsNum,OldCache,R),
								getDataHelper(R,I,Result),
								length(Result,N),
								N>0,
								addOrder(Result,N,Result1),
								OldCache2 = Result1,
								replaceInCacheHelper(Tag,Idx,Address,Mem,Result1,NewCache1,ItemData,setAssoc,setsNum,N,0,OldCache2),
								replaceIthItem(NewCache1,R,I,NewCache3),
								flatten(NewCache3,NewCache).
								
replaceInCacheHelper(Tag,Idx,Address,Mem,[H|T],NewCache1,ItemData,setAssoc,setsNum,N,Pos,OldCache2):- 
											N>0,
											[H|T] \= [],
											H = item(tag(Tag1),data(Data1),0,Order),
											atom_length(Tag1,R),
											numLength(Tag,R1),
											R2 is R-R1,
											number_string(Tag,X),
											fillZeros(X,R2,Tag2),
											getDataHelper(Mem,Address,M1),
											Res = item(tag(Tag2),data(M1),1,0),
											replaceIthItem(Res,OldCache2,Pos,NewCache1),
											ItemData = M1.												
replaceInCacheHelper(Tag,Idx,Address,Mem,[H|T],NewCache1,ItemData,setAssoc,SetsNum,N,Pos,OldCache2):-
											N>0,
											[H|T] \= [],
											H = item(tag(Tag1),data(Data1),1,Order),
											Pos1 is Pos+1,
											N1 is N-1,
											replaceInCacheHelper(Tag,Idx,Address,Mem,T,NewCache1,ItemData,SetAssoc,SetsNum,N1,Pos1,OldCache2).
								

replaceInCache(Tag,Idx,Mem,OldCache,NewCache,ItemData,setAssoc,SetsNum):- 
											convertBinToDec(Idx,I),
											logBase2(SetsNum,Set),
											getTagPos(Tag,Set,Tag1),
											convertBinToDec(Tag1,T),
											Address is I+T,
											splitEvery(SetsNum,OldCache,R),
											getDataHelper(R,I,Result),
											length(Result,N),
											N>0,
											addOrder(Result,N,Result1),
											orderMax(Result1,0,Max),
											OldCache2 = Result1,
											replaceInCacheHelper1(Tag,Idx,Address,Mem,Result1,NewCache1,ItemData,setAssoc,SetsNum,Max,0,OldCache2),
											replaceIthItem(NewCache1,R,I,NewCache3),
											flatten(NewCache3,NewCache).
		
replaceInCacheHelper1(Tag,Idx,Address,Mem,[H|T],NewCache,ItemData,setAssoc,SetsNum,Max,Pos,OldCache1):-
									H = item(tag(Tag1),data(Data1),1,Max),
									atom_length(Tag1,R4),
									numLength(Tag,R1),
									R2 is R4-R1,
									number_string(Tag,X),
									fillZeros(X,R2,Tag2),
									getDataHelper(Mem,Address,M1),
									Res = item(tag(Tag2),data(M1),1,0),
									replaceIthItem(Res,OldCache1,Pos,NewCache),
									ItemData = M1.

replaceInCacheHelper1(Tag,Idx,Address,Mem,[H|T],NewCache,ItemData,setAssoc,SetsNum,Max,Pos,OldCache1):-
									Pos1 is Pos+1,
									H \= item(tag(Tag1),data(Data1),1,Max),
									replaceInCacheHelper1(Tag,Idx,Address,Mem,T,NewCache,ItemData,setAssoc,SetsNum,Max,Pos1,OldCache1).









getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,hit):-
getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
NewCache = OldCache.
getData(StringAddress,OldCache,Mem,NewCache,Data,HopsNum,Type,BitsNum,miss):-
\+getDataFromCache(StringAddress,OldCache,Data,HopsNum,Type,BitsNum),
atom_number(StringAddress,Address),
convertAddress(Address,BitsNum,Tag,Idx,Type),
replaceInCache(Tag,Idx,Mem,OldCache,NewCache,Data,Type,BitsNum).




runProgram([],OldCache,_,OldCache,[],[],Type,_).
runProgram([Address|AdressList],OldCache,Mem,FinalCache,
[Data|OutputDataList],[Status|StatusList],Type,NumOfSets):-
getNumBits(NumOfSets,Type,OldCache,BitsNum),
(Type = setAssoc, Num = NumOfSets; Type \= setAssoc, Num = BitsNum),
getData(Address,OldCache,Mem,NewCache,Data,HopsNum,Type,Num,Status),
runProgram(AdressList,NewCache,Mem,FinalCache,OutputDataList,StatusList,
Type,NumOfSets).
