import Data.Foldable
data Item a = It Tag (Data a) Bool Int | NotPresent deriving (Show, Eq)
data Tag = T Int deriving (Show, Eq)
data Data a = D a deriving (Show, Eq)
data Output a = Out (a, Int) | NoOutput deriving (Show, Eq)
convertStringToInt x = (read :: String -> Int) x

replaceIthItem :: t -> [t] -> Int -> [t]
replaceIthItem a (x:xs) 0 = a: xs 
replaceIthItem a (x:xs) n = [x] ++ replaceIthItem a xs (n-1)

logBase2 :: Floating a => a -> a
logBase2 0 = 0.0
logBase2 1 = 0.0
logBase2 a = 1+ logBase2 (a/2)

fillZeros :: [Char] -> Int -> [Char]
fillZeros a 0 = a
fillZeros a n = '0':fillZeros a (n-1)

convertBinToDec :: Integral a => a -> a
convertBinToDec 0  = 0
convertBinToDec a = convertBinToDecHelper a 0
convertBinToDecHelper 0 _ = 0
convertBinToDecHelper a p = if mod a 2 == 1 
							then (2^p)+ convertBinToDecHelper (div a 10) (p+1)
							else convertBinToDecHelper (div a 10) (p+1) 
						
						
getNumBits :: (Integral a, RealFloat a1) =>a1 -> [Char] -> [c] -> a		
getNumBits _ "fullyAssoc" (x:xs) =  0 
getNumBits _  "setAssoc" (x:xs)= 1
getNumBits _ "directMap" (x:xs) = round (logBase2 (fromIntegral (length (x:xs)))) 

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []	
splitEvery n l | n > 0 = (take n l) : (splitEvery n (drop n l))
			   | otherwise = error ""


convertAddress a b "directMap" = (getTag a b, mod a (getIdx  b))
convertAddress a b "setAssoc" = (getTag a b, mod a (getIdx  b))
getTag a 0 = a
getTag a b = getTag (div a 10) (b-1)

getIdx 0 = 1
getIdx b = 10* getIdx (b-1)


getDataFromCache:: (Integral b, Eq a) => [Char] -> [Item a] -> [Char] -> b -> Output a

getDataFromCache stringAddress cache "directMap" bitsNum =  getDataFromCacheHelper (convertAddress (convertStringToInt stringAddress) bitsNum "directMap" ) cache
																				
getDataFromCacheHelper (tag , idx) cache = getDataFromCacheHelper1 (cache!!(convertBinToDec idx)) tag
getDataFromCacheHelper1 (It (T tag) (D itemData) v o) tag1 = if (tag1 == tag && v) 
															 then Out (itemData,0)
															 else NoOutput
															
															
replaceInCache:: Integral b => Int -> Int -> [a] -> [Item a] -> [Char] -> b -> (a, [Item a])													

replaceInCache tag idx memory oldCache "directMap" bitsNum = replaceInCacheHelper tag idx (memory!!(convertBinToDec idx)) (oldCache!!(convertBinToDec idx)) oldCache
replaceInCacheHelper tag1 idx memory cashe oldCache = (memory,  replaceIthItem ((It(T tag1) (D memory ) True 0)) oldCache (convertBinToDec idx) )


--getDataFromCache1 :: (Integral b, Eq a) => [Char] -> [Item a] -> [Char] -> b -> Output a																				

getDataFromCache1 stringAddress cache "setAssoc" bitsNum = getDataFromCacheSet (convertAddress (convertStringToInt stringAddress) bitsNum "setAssoc") cache bitsNum

getDataFromCacheSet (tag , idx) cache bitsNum = getDataFromCacheSet1 (tag,idx) ((splitEvery (2^bitsNum) cache)!!(convertBinToDec idx)) bitsNum


getDataFromCacheSet1 (tag1, idx) [] bitsNum = NoOutput

getDataFromCacheSet1 (tag1, idx) ((It (T tag) (D itemData) v o):xs) bitsNum  = if (tag1 == tag && v) 
																						then Out (itemData, convertBinToDec idx)
																						else getDataFromCacheSet1 (tag1, idx) xs bitsNum
			
			
incrementTrues [] = []				
incrementTrues ((It (T tag) (D itemData) v o):xs) =  if (v == True)
													then (It (T tag) (D itemData) v (o + 1)): incrementTrues xs
													else (It (T tag) (D itemData) v o):incrementTrues xs
		
getNeededIndex [] _ = 0		
getNeededIndex ((It (T tag) (D itemData) v o):xs) p = if (v == False)
													then p
													else getNeededIndex xs (p + 1)
													
