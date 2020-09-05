import System.Random
import System.IO.Unsafe
import Data.List

 
randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))
users = ["user1","user2","user3","user4"]
items = ["item1","item2","item3","item4","item5","item6"]
purchasesHistory = [("user1",[["item1","item2","item3"],["item1","item2","item4"]]),("user2",[["item2","item5"],["item4","item5"]]),("user3",[["item3","item2"]]),("user4",[])]


--Create Empty FreqList
createEmptyFreqList [] = []
createEmptyFreqList (h : t) = (h,[]) : createEmptyFreqList t 


--purchasesIntersection
		
getItemsOfUser user [] = [] 
getItemsOfUser user ((u,carts):xs) -- get purchased item of user1
                                 | user == u = (concat carts)
                                 | otherwise = (getItemsOfUser user xs)

removeDuplicate list = removeDuplicate2 list [] -- remover dups from list
removeDuplicate2 [] acc = acc
removeDuplicate2 (x:xs) acc
                           | elem x acc = removeDuplicate2 xs acc
						   | otherwise = removeDuplicate2 xs (acc ++ [x])
						   
intersect1 :: Eq a => [a] -> [a] -> [a]
intersect1 [] _ =[] -- get intersect
intersect1 (x:xs) list
					| elem x list = x:intersect1 xs list
					|otherwise = intersect1 xs list
					
getCountFrom _ [] =0
getCountFrom item ((i,n):xs) -- get frequency of item
                           | item == i =n
                           | otherwise = getCountFrom item xs
exist _ [] =False   -- check if exist
exist item ((i,n):xs)
					| item == i =True
					|otherwise  = exist item xs
					
createIntersection firstUser secondUser = intersect1 (removeDuplicate (getItemsOfUser firstUser purchasesHistory)) (removeDuplicate (getItemsOfUser secondUser purchasesHistory))

getFreqsOfIntersection firstUser secondUser = purchasesIntersectionOfTwoUsers(createIntersection firstUser secondUser) firstUser secondUser

purchasesIntersectionOfTwoUsers [] _ _ = []
purchasesIntersectionOfTwoUsers (i:is) firstUser secondUser = (createFreqs (getFreqs firstUser i (getAllUsersStats purchasesHistory)) (getFreqs secondUser i (getAllUsersStats purchasesHistory))):(purchasesIntersectionOfTwoUsers is firstUser secondUser)

traverse1 [] _ = []
traverse1 ((i,n):xs) secondFreqList
								| exist i secondFreqList = (i,n+(getCountFrom i secondFreqList)):(traverse1 xs secondFreqList)
								|otherwise = (i,n):(traverse1 xs secondFreqList)
								
traverse2 _ [] = []
traverse2 firstFreqList ((i,n):xs)
								|exist i firstFreqList = (traverse2 firstFreqList xs)
								|otherwise = (i,n):(traverse2 firstFreqList xs)
								
createFreqs firstFreqList secondFreqList = (traverse1 firstFreqList secondFreqList)++(traverse2 firstFreqList secondFreqList)

collectUsers [] = []
collectUsers ((u,freq):ufs) = u:(collectUsers ufs)

getUser [] _ ="null"
getUser (u:us) listOfUsers
						| elem u listOfUsers = (getUser us listOfUsers)
						| otherwise = u
						
purchasesIntersection list listOfFrequences =purchasesIntersection2 (getUser users (collectUsers listOfFrequences)) listOfFrequences

purchasesIntersection2 _ [] = [] 
purchasesIntersection2 user ((u,l):uls) = (zip (createIntersection user  u) (getFreqsOfIntersection user u)):(purchasesIntersection2 user uls)

-- Get All Users Stats

getAllUsersStats []=[]
getAllUsersStats ((name,l):xs) =(name,innerloop (createEmptyFreqList items) l):(getAllUsersStats (xs))
innerloop [] _ = []
innerloop ((name,l):xs) l2=(name,removesame(innerinnerloop name (deepestloop name l2)  (deepestloop name l2))):(innerloop xs l2)
innerinnerloop  _ [] _ = []
innerinnerloop name (x:xs) list
                |name==x = innerinnerloop name xs list
                |otherwise=(x,frequency x list 0):(innerinnerloop name xs list)
deepestloop  _ []=[]
deepestloop name (x:xs)
            |isinside name x ==True =x++deepestloop name xs
            |otherwise = deepestloop name xs
isinside _ []=False
isinside name (x:xs)
            |name==x =True
            |otherwise= isinside name xs
frequency _ [] y=y
frequency x (y:ys) z =if x==y
              then frequency x ys (z+1)
              else frequency x ys z
removesame []=[]
removesame (x:xs)
              |isinside x xs ==True =removesame xs
              |otherwise=x:removesame xs

-- Recommend based on users
createTheList [] =[]
createTheList ((i,n):ins)
                        | n==0 = createTheList ins
                        | otherwise = i:(createTheList ((i,n-1):ins))

recommendBasedOnUsers user 
							|(freqListUsers user) == [] =[]
							|otherwise = recommendBasedOnUsers2 (createTheList(freqListUsers user))
recommendBasedOnUsers2 list = list !! (randomZeroToX ((length list)-1))

--Freq List Users
getAllUsersStatsWithout _ [] = [] 
getAllUsersStatsWithout user ((u,f):ufs)
							|user == u = ufs
							| otherwise = (u,f):getAllUsersStatsWithout user ufs

sendOutput user = getOutput (purchasesIntersection2 user (getAllUsersStatsWithout user (getAllUsersStats purchasesHistory)))

getOutput [] = []
getOutput (l:ls) = (getPair l)++(getOutput ls)
getPair []=[]
getPair ((i,freqs):ifs) = (getTheFreqs freqs)++(getPair ifs)
getTheFreqs [] = []
getTheFreqs (x:xs) = x:getTheFreqs xs


helpme1 set= helpme2(nub (set)) 
helpme2 [] =[]
helpme2 ((name,h):t) = (name,0):helpme2 t
helpme3 x=(nub(helpme1 x))


freqListUsers x=freqListUsers1 (helpme3 (sendOutput x)) (sendOutput x)

freqListUsers1 [] y=[]
freqListUsers1 (h:t) y= freqListUsers3 h y :freqListUsers1 t y

freqListUsers3 x []=x
freqListUsers3 (name,h) ((name1,h1):t1) = if name1==name then  freqListUsers3((name1,(h+h1))) t1 else freqListUsers3  (name,h) t1

--Recommend Based On Items and Cart

getHistory user [] = []
getHistory user ((u,h):uhs)
							| user == u = h
							| otherwise = (getHistory user uhs)
recommendBasedOnItemsInCart user cart
                                    |(getHistory user purchasesHistory) == [] =[]
                                    |otherwise = recommendBasedOnItemsInCart2 (createTheList (freqListCartAndItems user cart))
recommendBasedOnItemsInCart2 list = list !! (randomZeroToX ((length list)-1))

--Recommend Based Empty Cart

recommendEmptyCart user = recommendBasedOnItemsInCart user []


--freqListCartAndItems
freqListCartAndItems a x =freqListCartAndItemshelper (freqListItems a) (freqListCart a x)

freqListCartAndItemshelper [] x = x
freqListCartAndItemshelper x [] = x
freqListCartAndItemshelper ((name1,l1):xs1) ((name2,l2):xs2) 
	|name1==name2 =(name1,l1+l2):freqListCartAndItemshelper xs1 xs2
	|search name1 xs2 ==True =(name1,(getfreqnum name1 xs2 )+l1):freqListCartAndItemshelper xs1 ((name2,l2):(removeitemv1 name1 xs2))
	|otherwise= (name1,l1):freqListCartAndItemshelper xs1 ((name2,l2):xs2) 



search _ []=False
search name ((name1,l):xs)
	|name==name1 =True
	|otherwise= search name xs
removeitemv1 _ []=[]
removeitemv1 name ((name1,l):xs)
	|name==name1 =removeitemv1 name xs
	|otherwise=(name1,l):removeitemv1 name xs

getfreqnum _ []=0
getfreqnum name ((name1,l):xs)
	|name==name1 =l
	|otherwise=getfreqnum name xs

	
--freqListItems

freqListItems:: String -> [(String, Int)]
freqListItems a =freqListHelper a (getAllUsersStats purchasesHistory)
freqListHelper _ []=[]
freqListHelper a ((name,l):xs)=
    if a==name 
    then innerfreqListHelper l l
    else freqListHelper a xs 
innerfreqListHelper [] _ =[]
innerfreqListHelper ((name,l):xs) list=removezeros((innerinnerfreqListHelper name list)++innerfreqListHelper xs list)
innerinnerfreqListHelper _ []=[]
innerinnerfreqListHelper name ((name1,l):xs)
    |name==name1 = innerinnerfreqListHelper name xs
    |otherwise = [(name,addall((deepestfreqListHelper name l) ++ (innerinnerfreqListHelper name xs)))]
deepestfreqListHelper _ []=[]
deepestfreqListHelper name ((name1,x):xs) 
    |name==name1 = (name,x):deepestfreqListHelper name xs
    |otherwise = deepestfreqListHelper name xs

addall []=0
addall ((name,l):xs)=l+addall(xs)

removezeros []=[]
removezeros ((name,l):xs)
    |l==0 =removezeros xs
    |otherwise =((name,l):removezeros xs)

	
--freqListCart

getFreqs user item ((u,freqList):xs)
                       |user == u = getItem item freqList 
                       |otherwise = getFreqs user item xs
getItem _ [] = []
getItem item ((currItem,freqs):xs)
                        | item == currItem = freqs
                        | otherwise = getItem item xs

freqListCartHelp _ [] = []   
freqListCartHelp user (item:xs)= (helper1 user item)++(freqListCartHelp user xs)
helper1 user item = helper2 (getFreqs user item (getAllUsersStats purchasesHistory))
helper2 [] = [] 
helper2 ((item,n):xs)= (addToList (item,n))++(helper2 xs)
addToList (item,n) = addToList1 (item,n) []                    
addToList1 ((item,n)) acc 
                        |n == 0 = acc
                        |otherwise = addToList1 ((item,(n-1))) (acc++[item])
getCount _ [] = 0
getCount item (i:is)
                   |item == i = 1+(getCount item is)
                   |otherwise = (getCount item is)
getAllCount user cart (i:is) = (getCount i (freqListCartHelp user cart)):(getAllCount user cart is)
freqListCartBeforeLast user cart = zip items (getAllCount user cart items)
filterFreqListCart [] = []
filterFreqListCart ((item,n):xs)
                              | n==0 = filterFreqListCart xs
                              | otherwise = (item,n):(filterFreqListCart xs)
freqListCart:: String ->[String] -> [(String, Int)]
freqListCart user cart = filterFreqListCart (freqListCartBeforeLast user cart)
	
-- Recommend
recommendHelper user cart = [(recommendBasedOnItemsInCart user cart),(recommendBasedOnUsers user)]
recommend user cart 
					| (recommendList !! 0) == "" && (recommendList !! 1) == "" = (items !! (randomZeroToX ((length items)-1)))
					| (recommendList !! 0) == "" = (recommendList !! 1) 
					| (recommendList !! 1) == "" = (recommendList !! 0)
					| otherwise = (recommendList !! (randomZeroToX 1))
					where recommendList = (recommendHelper user cart)