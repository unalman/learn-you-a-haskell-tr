Modüller
========


Modüller yükleniyor
-------------------

![modules](../img/modules.png)
Haskell modülü, ilgili fonksiyonların, türlerin ve tür sınıflarının bir koleksiyonudur. 
Haskell programı, ana modülün diğer modülleri yüklediği ve daha sonra bir şeyler yapmak için içlerinde tanımlanan fonksiyonları kullandığı bir modüller koleksiyonudur.
Birkaç modüle bölünmüş kodun oldukça fazla avantajı vardır. Bir modül yeterince genelse, dışa aktardığı fonksiyonlar çok sayıda farklı programda kullanılabilir.
Kendi kodunuz birbirine çok fazla güvenmeyen bağımsız modüllere ayrılırsa (gevşek bir şekilde bağlı olduklarını da söyleriz), bunları daha sonra yeniden kullanabilirsiniz.
Her biri bir çeşit amaca sahip olan birkaç parçaya bölünerek kod yazma işinin tamamını daha yönetilebilir hale getirir.

Haskell standart kütüphanesi modüllere ayrılmıştır, her biri bir şekilde ilişkili olan ve bazı ortak amaca hizmet eden fonksiyonlar ve türleri içerir.
Listeleri işlemek için bir modül, eşzamanlı programlama(concurrency programming) için bir modül, karmaşık sayılarla uğraşmak için bir modül vb.
Şimdiye kadar ele aldığımız tüm fonksiyonlar, türler ve tür sınıfları, varsayılan olarak içe aktarılan `Prelude` modülünün bir parçasıydı.
Bu bölümde, birkaç yararlı modülü ve sahip oldukları fonksiyonları inceleyeceğiz. Ama önce, modülleri nasıl içe aktaracağımızı(import) göreceğiz.

Haskell betiğindeki modülleri içe aktarmanın sözdizimi `import <module name>` şeklindedir. Bu, herhangi bir fonksiyonu tanımlamadan önce yapılmalıdır, 
bu nedenle içe aktarmalar genellikle dosyanın en üstünde yapılır. Elbette bir komut dosyası birkaç modülü içe aktarabilir.
Her import ifadesini ayrı bir satıra koyun. Listelerle çalışmak için bir dizi yararlı fonksiyonu olan Data.List modülünü içe aktaralım ve 
bir listenin kaç benzersiz öğeye sahip olduğunu bize söyleyen bir fonksiyon oluşturmak için dışa aktardığı bir fonksiyonu kullanalım.
`Data.List`'i içe aktardığınızda, Data.List'in dışa aktardığı tüm fonksiyonlar genel ad alanında(namespace) kullanılabilir hale gelir,
bu da onları komut dosyasının neresinden çağırabileceğiniz anlamına gelir.

~~~~ {.haskell: .ghci name="code"}
import Data.List  
  
numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub  
~~~~

`Data.List`'i içe aktardığınızda, `Data.List`'in dışa aktardığı tüm fonksiyonlar genel ad alanında(namespace) kullanılabilir hale gelir, 
yani bunları komut dosyasının her yerinden çağırabilirsiniz. `nub`, `Data.List`'te tanımlanan ve bir liste alan ve yinelenen öğeleri ayıklayan bir fonksiyondur.
`length . nub` komutunu yaparak `length` ve `nub`'u oluşturmak, `\xs -> length (nub xs)` ile eşdeğer olan bir fonksiyon üretir.

GHCI kullanırken modüllerin fonksiyonlarını global ad alanına da koyabilirsiniz. 
GHCI'daysanız ve `Data.List` tarafından dışa aktarılan fonksiyonları çağırabilmek istiyorsanız, şunu yapın:

~~~~ {.haskell: .ghci name="code"}
ghci> :m + Data.List   
~~~~

GHCI içindeki birkaç modülden isimleri yüklemek istiyorsak, birkaç kez `:m +` yapmak zorunda değiliz, sadece birkaç modülü aynı anda yükleyebiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> :m + Data.List Data.Map Data.Set   
~~~~

Ancak, zaten bir modülü içe aktaran bir komut dosyası yüklediyseniz, ona erişmek için `:m +` kullanmanız gerekmez.

Bir modülden yalnızca birkaç fonksiyona ihtiyacınız varsa, yalnızca bu fonksiyonları seçerek içe aktarabilirsiniz.
`Data.List`'ten sadece `nub` ve `sort` fonksiyonlarını içe aktarmak isteseydik, şunu yapardık:

~~~~ {.haskell: .ghci name="code"}
import Data.List (nub, sort)    
~~~~

Ayrıca bir modülün birkaç seçili olanlar dışında tüm fonksiyonlarını içe aktarmayı da seçebilirsiniz. 
Bu, genellikle birkaç modülün aynı adı taşıyan fonksiyonları dışa aktardığında ve rahatsız edici olanlardan kurtulmak istediğinizde yararlıdır.
Diyelim ki `nub` adında kendi fonksiyonlarımız var ve `nub` fonksiyonu dışındaki tüm fonksiyonları `Data.List`'ten içe aktarmak istiyoruz:

~~~~ {.haskell: .ghci name="code"}
import Data.List hiding (nub)   
~~~~

İsim çatışmalarıyla baş etmenin bir başka yolu da qualified import yapmaktır. Değerleri anahtara göre aramak için bir veri yapısı(data structure) sunan `Data.Map` modülü, 
`filter` veya `null` gibi `Prelude` fonksiyonlarıyla aynı adı taşıyan bir dizi fonksiyonu dışa aktarır. 
Dolayısıyla `Data.Map`'i içeri aktardığımızda ve ardından `filter` çağırdığımızda Haskell hangi fonksiyonu kullanacağını bilemeyecek. Bunu şu şekilde çözüyoruz:

~~~~ {.haskell: .ghci name="code"}
import qualified Data.Map  
~~~~

`Data.Map`'in `filter` fonksiyonuna referans vermek istiyorsak, `Data.Map.filter` yapmamız gerekir, oysa sadece `filter` hala hepimizin bildiği ve
sevdiği normal filter'ı ifade eder.
Ancak bu modüldeki her fonksiyonun önüne `Data.Map` yazmak biraz sıkıcıdır. Bu nedenle, qualified import daha kısa bir adla yeniden adlandırabiliriz:

~~~~ {.haskell: .ghci name="code"}
import qualified Data.Map as M  
~~~~

`Data.Map` `filter` fonksiyonuna nasıl referans verilir, sadece `M.filter` kullanıyoruz.

Standart kitaplıkta hangi modüllerin olduğunu görmek için bu [kullanışlı referansı kullanın](https://downloads.haskell.org/~ghc/latest/docs/html/libraries/).
Yeni Haskell bilgisi edinmenin harika bir yolu, standart kitaplık referansına tıklayıp modülleri ve fonksiyonlarını keşfetmektir.
Her modül için Haskell kaynak kodununu da görüntüleyebilirsiniz.
Bazı modüllerin kaynak kodunu okumak, Haskell'i öğrenmek ve sağlam bir fikir edinmek için gerçekten iyi bir yoldur.

Fonksiyonları aramak veya nerede bulunduklarını öğrenmek için [Hoogle'ı](http://haskell.org/hoogle) kullanın.
Gerçekten harika bir Haskell arama motoru, ada, modül adına veya hatta tür imzasını yazarak arama yapabilirsiniz.


Data.List
---------

`Data.List` modülü elbette listelerle ilgilidir. Onlarla başa çıkmak için çok faydalı bazı fonksiyonlar sağlar.
`Prelude` modülü, kolaylık sağlamak için `Data.List`'ten bazı fonksiyonları dışa aktardığı için, bazı fonksiyonlarıyla (`map` ve `filter` gibi) zaten tanıştık.
`Data.List`'i quelified bir import yoluyla içe aktarmanız gerekmez, çünkü `Prelude`'un `Data.List`'ten zaten çaldıkları dışında herhangi bir `Prelude` adıyla çakışmaz.
Daha önce karşılaşmadığımız bazı fonksiyonlara bir göz atalım.

`intersperse`, bir öğeyi ve bir listeyi alır ve ardından bu öğeyi listedeki her öğe çifti arasına yerleştirir. İşte bir gösterimi:

~~~~ {.haskell: .ghci name="code"}
ghci> intersperse '.' "MONKEY"  
"M.O.N.K.E.Y"  
ghci> intersperse 0 [1,2,3,4,5,6]  
[1,0,2,0,3,0,4,0,5,0,6]  
~~~~

`intercalate`, bir liste listesi ve bir liste alır. Daha sonra bu listeyi tüm bu listelerin arasına ekler ve ardından sonucu düzleştirir.

~~~~ {.haskell: .ghci name="code"}
ghci> intercalate " " ["hey","there","guys"]  
"hey there guys"  
ghci> intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]  
[1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]  
~~~~

`transpose` bir liste listesini transpoze eder. Listelerin bir listesine 2D matris olarak bakarsanız, sütunlar satırlar olur ve bunun tersi de geçerlidir.

~~~~ {.haskell: .ghci name="code"}
ghci> transpose [[1,2,3],[4,5,6],[7,8,9]]  
[[1,4,7],[2,5,8],[3,6,9]]  
ghci> transpose ["hey","there","guys"]  
["htg","ehu","yey","rs","e"]  
~~~~

Diyelim ki 3x2 + 5x + 9, 10x3 + 9 and 8x3 + 5x2 + x - 1 polinomları var ve bunları bir araya getirmek istiyoruz. Haskell'de temsil etmek için `[0,3,5,9]`, `[10,0,0,9]` ve
`[8,5,1,-1]` listelerini kullanabiliriz. Şimdi onları eklemek için tek yapmamız gereken şudur:

~~~~ {.haskell: .ghci name="code"}
ghci> map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]]  
[18,8,6,17]  
~~~~

Bu üç listeyi transpoze ettiğimizde, üçüncü kuvvetler ilk satırda, ikinci kuvvetler ikinci sırada vb. Buna `sum`'ı eşlemek, istediğimiz sonucu üretir.

![legolists](../img/legolists.png)
`foldl'` ve `foldl1'` ilgili tembel enkarnasyonlarının daha katı versiyonlarıdır. Gerçekten büyük listelerde lazy fold'lar kullanırken, 
genellikle bir yığın(stack) taşması hatası alabilirsiniz. Bunun suçlusu, fold'ların lazy doğası nedeniyle,
fold gerçekleşirken toplayıcı değerinin aslında güncellenmemesidir. Gerçekte olan şey, toplayıcı türünün,
sonucu gerçekten üretmesi istendiğinde değerini hesaplayacağına dair bir söz vermesidir (aynı zamanda thunk olarak da adlandırılır).
Bu, her ara toplayıcı için olur ve tüm bu thunks yığınınızdan taşar. 
Sıkı (strict) fold'lar lazy herifler değildir ve yığınınızı thunks ile doldurmak yerine aslında ara değerleri hesaplarlar.
Bu nedenle, lazy fold'lar yaparken yığın taşması (stack overflow) hataları alırsanız, Sıkı sürümlerine geçmeyi deneyin.

`concat`, bir liste listesini yalnızca bir öğe listesine düzleştirir.

~~~~ {.haskell: .ghci name="code"}
ghci> concat ["foo","bar","car"]  
"foobarcar"  
ghci> concat [[3,4,5],[2,3,4],[2,1,1]]  
[3,4,5,2,3,4,2,1,1]  
~~~~

Yalnızca bir seviye yuva kaldırır. Dolayısıyla, liste listelerinin bir listesi olan `[[[2,3],[3,4,5],[2]],[[2,3],[3,4]]]`'yi tamamen düzleştirmek istiyorsanız,
onu iki kez birleştirmeniz gerekir.

`concatMap` yapmak, önce bir fonksiyonu bir listeye eşlemek ve ardından listeyi `concat` ile birleştirmekle aynıdır.

~~~~ {.haskell: .ghci name="code"}
ghci> concatMap (replicate 4) [1..3]  
[1,1,1,1,2,2,2,2,3,3,3,3]  
~~~~

`and`, boolean değerlerinin bir listesini alır ve yalnızca listedeki tüm değerler `True` ise `True` döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> and $ map (>4) [5,6,7,8]  
True  
ghci> and $ map (==4) [4,4,4,3,4]  
False  
~~~~

`or`, `and` gibidir, yalnızca listedeki boole değerlerinden herhangi biri `True` ise `True` döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> or $ map (==4) [2,3,4,5,6,1]  
True  
ghci> or $ map (>4) [1,2,3]  
False  
~~~~

`any` ve `all` bir predicate alır ve ardından bir listedeki herhangi bir öğenin veya tüm öğelerin sırasıyla predicate'i karşılayıp karşılamadığını kontrol eder.
Genellikle bir listeyi eşleştirmek ve ardından `and` veya `or` yapmak yerine bu iki fonksiyonu kullanırız.

~~~~ {.haskell: .ghci name="code"}
ghci> any (==4) [2,3,5,6,1,4]  
True  
ghci> all (>4) [6,9,10]  
True  
ghci> all (`elem` ['A'..'Z']) "HEYGUYSwhatsup"  
False  
ghci> any (`elem` ['A'..'Z']) "HEYGUYSwhatsup"  
True   
~~~~

`iterate` bir fonksiyon ve bir başlangıç değeri alır. fonksiyonu başlangıç değerine uygular, sonra o fonksiyonu sonuca uygular, sonra fonksiyonu o sonuca tekrar uygular, vb.
Tüm sonuçları sonsuz bir liste biçiminde döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> take 10 $ iterate (*2) 1  
[1,2,4,8,16,32,64,128,256,512]  
ghci> take 3 $ iterate (++ "haha") "haha"  
["haha","hahahaha","hahahahahaha"]   
~~~~

`splitAt` bir sayı ve bir liste alır. Ardından, listeyi bu kadar çok öğeye böler ve ortaya çıkan iki listeyi bir tuple(demet) halinde döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> splitAt 3 "heyman"  
("hey","man")  
ghci> splitAt 100 "heyman"  
("heyman","")  
ghci> splitAt (-3) "heyman"  
("","heyman")  
ghci> let (a,b) = splitAt 3 "foobar" in b ++ a  
"barfoo"  
~~~~

`takeWhile` gerçekten yararlı küçük bir fonksiyondur. Predicate tutarken bir listeden öğeler alır ve ardından predicate'i karşılamayan bir öğe ile karşılaşıldığında kesilir. 
Görünüşe göre bu çok faydalı.

~~~~ {.haskell: .ghci name="code"}
ghci> takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1]  
[6,5,4]  
ghci> takeWhile (/=' ') "This is a sentence"  
"This" 
~~~~

10.000'in altındaki tüm üçüncü güçlerin toplamını bilmek istediğimizi varsayalım. `(^3)`'ü `[1..]` ile eşleyemeyiz,
bir filter uygulayamayız ve sonra bunu özetlemeye çalışamayız çünkü sonsuz bir listeyi filtrelemek asla bitmez.
Buradaki tüm unsurların yükseldiğini biliyor olabilirsiniz, ancak Haskell değil. Bu yüzden bunu yapabiliriz:

~~~~ {.haskell: .ghci name="code"}
ghci> sum $ takeWhile (<10000) $ map (^3) [1..]  
53361  
~~~~

Sonsuz bir listeye `(^3)` uygularız ve sonra 10.000'in üzerinde bir elemanla karşılaşıldığında liste kesilir. Şimdi kolayca toplayabilirsiniz.

`dropWhile` benzer olsa da, predicate yalnızca `True` ise tüm öğeleri bırakır. Predicate `False`'a eşitlendiğinde, listenin geri kalanını döndürür. 
Son derece kullanışlı ve hoş bir fonksiyon.

~~~~ {.haskell: .ghci name="code"}
ghci> dropWhile (/=' ') "This is a sentence"  
" is a sentence"  
ghci> dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]  
[3,4,5,4,3,2,1]   
~~~~

Bir hisse senedinin tarihe göre değerini temsil eden bir liste veriliyor. Liste, ilk bileşeni hisse senedi değeri,
ikincisi yıl, üçüncüsü ay ve dördüncüsü tarih olan demetlerden oluşur. Hisse senedi değerinin ilk bin doları ne zaman aştığını bilmek istiyoruz!

~~~~ {.haskell: .ghci name="code"}
ghci> let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]  
ghci> head (dropWhile (\(val,y,m,d) -> val < 1000) stock)  
(1001.4,2008,9,4)     
~~~~

`span`, `takeWhile` gibi bir tür, yalnızca bir çift liste döndürür. 
İlk liste, aynı predicate ve aynı liste ile çağrılsaydı `takeWhile`'dan elde edilen listenin içereceği her şeyi içerir. İkinci liste, listenin düşürülen kısmını içerir.

~~~~ {.haskell: .ghci name="code"}
ghci> let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest  
"First word: This, the rest: is a sentence"   
~~~~

`span`, predicate `True` olduğunda listeyi kapsarken, break, predicate ilk `True` olduğunda onu kırar. `break p` yapmak, `span (not . p)` yapmaya eşdeğerdir.

~~~~ {.haskell: .ghci name="code"}
ghci> break (==4) [1,2,3,4,5,6,7]  
([1,2,3],[4,5,6,7])  
ghci> span (/=4) [1,2,3,4,5,6,7]  
([1,2,3],[4,5,6,7])  
~~~~

`break` kullanılırken, sonuçtaki ikinci liste predicate'i karşılayan ilk öğe ile başlayacaktır.

`sort` basitçe bir listeyi sıralar. Listedeki elemanların türü `Ord` tür sınıfının bir parçası olmalıdır, çünkü bir listenin elemanları bir tür sıraya konulamazsa,
o zaman liste sıralanamaz.

~~~~ {.haskell: .ghci name="code"}
ghci> sort [8,5,3,2,1,6,4,2]  
[1,2,2,3,4,5,6,8]  
ghci> sort "This will be sorted soon"  
"    Tbdeehiillnooorssstw"  
~~~~

`group` bir listeyi alır ve bitişik öğeleri eşitlerse alt listelerde gruplandırır.

~~~~ {.haskell: .ghci name="code"}
ghci> group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]  
[[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]  
~~~~

Bir listeyi gruplamadan önce sıralarsak, her bir öğenin listede kaç kez göründüğünü öğrenebiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]  
[(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]  
~~~~

`inits` ve `tails`, `init` ve `tail` gibidir, ancak bunu geriye hiçbir şey kalmayana kadar yinelemeli olarak bir listeye uygularlar. Gözlemleyin.

~~~~ {.haskell: .ghci name="code"}
ghci> inits "w00t"  
["","w","w0","w00","w00t"]  
ghci> tails "w00t"  
["w00t","00t","0t","t",""]  
ghci> let w = "w00t" in zip (inits w) (tails w)  
[("","w00t"),("w","00t"),("w0","0t"),("w00","t"),("w00t","")]   
~~~~

Bir alt liste için bir search list uygulamak için bir fold kullanalım.

~~~~ {.haskell: .ghci name="code"}
search :: (Eq a) => [a] -> [a] -> Bool  
search needle haystack =   
    let nlen = length needle  
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)  
~~~~

Önce aradığımız liste ile `tails` diyoruz. Sonra her kuyruğun üzerinden geçip aradığımız şeyle başlayıp başlamadığına bakarız.

Bununla, aslında `isInfixOf` gibi davranan bir fonksiyon yaptık. 
`isInfixOf`, bir liste içinde bir alt liste arar ve aradığımız alt liste hedef listenin içinde bir yerdeyse `True` döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> "cat" `isInfixOf` "im a cat burglar"  
True  
ghci> "Cat" `isInfixOf` "im a cat burglar"  
False  
ghci> "cats" `isInfixOf` "im a cat burglar"  
False
~~~~

`isPrefixOf` ve `isSuffixOf` sırasıyla bir listenin başında ve sonunda bir alt liste arar.

~~~~ {.haskell: .ghci name="code"}
ghci> "hey" `isPrefixOf` "hey there!"  
True  
ghci> "hey" `isPrefixOf` "oh hey there!"  
False  
ghci> "there!" `isSuffixOf` "oh hey there!"  
True  
ghci> "there!" `isSuffixOf` "oh hey there"  
False  
~~~~

`elem` ve `notElem` bir elemanın listede olup olmadığını kontrol eder.

`partition` bir liste ve bir predicate alır ve bir çift liste döndürür. Sonuçtaki ilk liste predicate karşılayan tüm öğeleri içerir, ikincisi olmayanların hepsini içerir.

~~~~ {.haskell: .ghci name="code"}
ghci> partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"  
("BOBMORGAN","sidneyeddy")  
ghci> partition (>3) [1,3,5,6,3,2,1,0,3,7]  
([5,6,7],[1,3,3,2,1,0,3])    
~~~~

Bunun `span` ve `break`'dan ne kadar farklı olduğunu anlamak önemlidir:

~~~~ {.haskell: .ghci name="code"}
ghci> span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"  
("BOB","sidneyMORGANeddy")    
~~~~

`span` ve `break`, predicate'i karşılamayan ve karşılamayan ilk öğeyle karşılaştıklarında yapılırken, `partition` tüm listeyi gözden geçirir ve predicate'e göre böler.

`find` bir liste ve bir predicate alır ve predicate'i karşılayan ilk öğeyi döndürür. Ancak Maybe değeriyle sarılmış öğeyi döndürür.
Bir sonraki bölümde cebirsel veri türlerini daha derinlemesine ele alacağız, ancak şimdilik bilmeniz gereken: `Maybe` değeri `Just something` veya `Nothing` olabilir.
Bir listenin boş bir liste veya bazı öğeler içeren bir liste olabileceği gibi, Maybe değeri hiç öğe olmayabilir veya tek bir öğe olabilir.
Ve tamsayıların bir listesinin türü gibi, örneğin, `[Int]`, bir tam sayıya sahip olmanın türü de `Maybe Int` dir.
Her neyse, bir dönüş için `find` fonksiyonumuzu alalım.

~~~~ {.haskell: .ghci name="code"}
ghci> find (>4) [1,2,3,4,5,6]  
Just 5  
ghci> find (>9) [1,2,3,4,5,6]  
Nothing  
ghci> :t find  
find :: (a -> Bool) -> [a] -> Maybe a  
~~~~

`find` türüne dikkat edin. Sonucu `Maybe a`'dır. Bu, `[a]` türüne sahip olmaya benzer, yalnızca `Maybe` türündeki bir değer, hiçbir öğe veya bir öğe içerebilir, 
oysa bir liste hiçbir öğe, bir öğe veya birkaç öğe içeremez.

Stoklarımız 1000 doları aştı ilk kez aradığımızı hatırlayın. `head (dropWhile (\(val,y,m,d) -> val < 1000) stock)` yaptık. `head`'in gerçekten güvenli olmadığını unutmayın.
Stoklarımız 1000 doları geçmezse ne olur? `dropWhile` uygulamamız boş bir liste döndürür ve boş bir listenin başını almak bir hatayla sonuçlanır.
Ancak, bunu `find (\(val,y,m,d) -> val > 1000) stock` olarak yeniden yazarsak çok daha güvenli oluruz.
Eğer hissemiz 1000 $'ın üzerine çıkmasaydı (yani hiçbir öğe predicate'i karşılamazsa), `Nothing` alırdık.
Ama bu listede geçerli bir cevap vardı, diyelim ki, `Just (1001.4,2008,9,4)`.

`elemIndex` bir tür elem'dir, ancak boolean bir değer döndürmez. Belki aradığımız öğenin dizinini döndürür. Bu öğe listemizde yoksa `Nothing` döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> :t elemIndex  
elemIndex :: (Eq a) => a -> [a] -> Maybe Int  
ghci> 4 `elemIndex` [1,2,3,4,5,6]  
Just 3  
ghci> 10 `elemIndex` [1,2,3,4,5,6]  
Nothing  
~~~~

`elemIndices`, `elemIndex` gibidir, sadece aradığımız elemanın listemizde birkaç kez ekilmesi durumunda index'lerin bir listesini döndürür.
Index'leri temsil etmek için bir liste kullandığımız için, `Maybe` türüne ihtiyacımız yoktur, çünkü başarısızlık boş liste olarak temsil edilebilir, 
bu da `Nothing` ile eşanlamlıdır.

~~~~ {.haskell: .ghci name="code"}
ghci> ' ' `elemIndices` "Where are the spaces?"  
[5,9,13]  
~~~~

`findIndex`, `find` gibidir, ancak predicate'i karşılayan ilk öğenin dizinini döndürebilir. 
`findIndices`, predicate'i karşılayan tüm öğelerin dizinlerini bir liste biçiminde döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> findIndex (==4) [5,3,2,1,6,4]  
Just 5  
ghci> findIndex (==7) [5,3,2,1,6,4]  
Nothing  
ghci> findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"  
[0,6,10,14]  
~~~~

`zip` ve `zipWith`'i zaten ele aldık. Bir demet halinde veya binary fonksiyon (iki parametre alan böyle bir fonksiyon) iki listeyi bir araya getirdiklerini belirttik.
Peki ya üç listeyi bir araya getirmek istersek? Veya üç parametre alan bir fonksiyonla üç listeyi zip'leyin? Bunun için `zip3`, `zip4` vb. ve `zipWith3`, `zipWith4`, vb.
Bu varyantlar 7'ye kadar çıkar. Bu bir hack gibi görünse de gayet iyi çalışıyor çünkü 8 listeyi bir arada sıkıştırmak istediğinizde pek çok zaman yok.
Ayrıca sonsuz sayıda listeyi sıkıştırmanın çok akıllıca bir yolu var, ancak henüz bunu kapsayacak kadar gelişmiş değiliz.

~~~~ {.haskell: .ghci name="code"}
ghci> zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3]  
[7,9,8]  
ghci> zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]  
[(2,2,5,2),(3,2,5,2),(3,2,3,2)]  
~~~~

Normal sıkıştırmada olduğu gibi, sıkıştırılmakta olan en kısa listeden daha uzun olan listeler boyuta indirilir.

`lines`, dosyalar veya bir yerden girdilerle uğraşırken kullanışlı bir fonksiyondur. Bir string alır ve bu string'in her satırını ayrı bir listede döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> lines "first line\nsecond line\nthird line"  
["first line","second line","third line"]  
~~~~

`'\n'` bir unix satırsonu karakteridir. Ters eğik çizgilerin Haskell string'lerinde ve karakterlerinde özel bir anlamı vardır.

`unlines`, `lines`'ın ters fonksiyonudur. Stirng'lerin bir listesini alır ve onları bir `'\n'` kullanarak birleştirir.

~~~~ {.haskell: .ghci name="code"}
ghci> unlines ["first line", "second line", "third line"]  
"first line\nsecond line\nthird line\n"   
~~~~

`words` ve `unwords`'ler, bir metin satırını kelimelere ayırmak veya bir kelime listesini bir metne birleştirmek içindir. Çok kullanışlı.

~~~~ {.haskell: .ghci name="code"}
ghci> words "hey these are the words in this sentence"  
["hey","these","are","the","words","in","this","sentence"]  
ghci> words "hey these           are    the words in this\nsentence"  
["hey","these","are","the","words","in","this","sentence"]  
ghci> unwords ["hey","there","mate"]  
"hey there mate"    
~~~~

`nub`'dan daha önce bahsetmiştik. Bir liste alır ve yinelenen öğeleri ayıklayarak, her öğesi benzersiz bir kar tanesi olan bir liste döndürür!
fonksiyonun bir tür garip adı var. nub'ın küçük bir yumru veya bir şeyin temel parçası anlamına geldiği ortaya çıktı.
Bana göre fonksiyon isimleri için yaşlı kelimeleri yerine gerçek kelimeleri kullanmalılar.

~~~~ {.haskell: .ghci name="code"}
ghci> nub [1,2,3,4,3,2,1,2,3,4,3,2,1]  
[1,2,3,4]  
ghci> nub "Lots of words and stuff"  
"Lots fwrdanu"    
~~~~

`delete`, bir öğeyi ve bir listeyi alır ve bu öğenin listedeki ilk oluşumunu siler.

~~~~ {.haskell: .ghci name="code"}
ghci> delete 'h' "hey there ghang!"  
"ey there ghang!"  
ghci> delete 'h' . delete 'h' $ "hey there ghang!"  
"ey tere ghang!"  
ghci> delete 'h' . delete 'h' . delete 'h' $ "hey there ghang!"  
"ey tere gang!"  
~~~~

`\\` liste farkı fonksiyondur. Temelde belirli bir fark gibi davranır. Sağ taraftaki listedeki her öğe için, soldaki eşleşen bir öğeyi kaldırır.

~~~~ {.haskell: .ghci name="code"}
ghci> [1..10] \\ [2,5,9]  
[1,3,4,6,7,8,10]  
ghci> "Im a big baby" \\ "big"  
"Im a  baby"   
~~~~

`[1..10] \\ [2,5,9]` yapmak `delete 2 . delete 5 . delete 9 $ [1..10]` yapmak gibidir.

`union` aynı zamanda kümelerdeki bir fonksiyon gibi davranır. İki listenin birleşimini döndürür. 
İkinci listedeki her öğeyi hemen hemen gözden geçirir ve henüz girmemişse ilkine ekler. Yine de dikkatli olun, kopyalar ikinci listeden kaldırılır!

~~~~ {.haskell: .ghci name="code"}
ghci> "hey man" `union` "man what's up"  
"hey manwt'sup"  
ghci> [1..7] `union` [5..10]  
[1,2,3,4,5,6,7,8,9,10]     
~~~~

`intersect`, küme kesişimi gibi çalışır. Yalnızca her iki listede de bulunan öğeleri döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> [1..7] `intersect` [5..10]  
[5,6,7]   
~~~~

`insert`, bir elemanı ve sıralanabilen elemanların bir listesini alır ve onu bir sonraki elemandan küçük veya ona eşit olduğu son konuma ekler.
Başka bir deyişle, `insert` listenin başında başlayacak ve daha sonra eklediğimiz öğeye eşit veya ondan büyük bir öğe bulana kadar devam edecek
ve onu öğeden hemen önce ekleyecektir.

~~~~ {.haskell: .ghci name="code"}
ghci> insert 4 [3,5,1,2,8,2]  
[3,4,5,1,2,8,2]  
ghci> insert 4 [1,3,4,4,1]  
[1,3,4,4,4,1]  
~~~~

`4`, ilk örnekte `3`'ten hemen sonra ve `5`'ten önce ve ikinci örnekte `3` ile `4` arasına yerleştirilir.

Sıralı bir listeye eklemek için `insert` kullanırsak, ortaya çıkan liste sıralı tutulacaktır.

~~~~ {.haskell: .ghci name="code"}
ghci> insert 4 [1,2,3,5,6,7]  
[1,2,3,4,5,6,7]  
ghci> insert 'g' $ ['a'..'f'] ++ ['h'..'z']  
"abcdefghijklmnopqrstuvwxyz"  
ghci> insert 3 [1,2,4,3,2,1]  
[1,2,3,4,3,2,1]   
~~~~

`length`, `take`, `drop`, `splitAt`, `!!` ve `replicate`'in ortak yanı, 
`Integral` veya `Num` tür sınıfının bir parçası olan herhangi bir türü almış olsalar daha genel ve kullanılabilir olsalar bile, 
parametrelerinden biri olarak bir Int almaları (veya bir `Int` döndürmeleri) olmasıdır. fonksiyonlar hakkında). Bunu tarihsel nedenlerle yapıyorlar.
Ancak, bunu düzeltmek büyük olasılıkla mevcut birçok kodu bozacaktır.
Bu nedenle `Data.List`, `genericLength`, `genericTake`, `genericDrop`, `genericSplitAt`, `genericIndex` ve `genericReplicate` adlı daha genel eşdeğerlerine sahiptir.
Örneğin `length`'in tür imzası `length :: [a] -> Int`'dir. `let xs = [1..6] in sum xs / length xs` yaparak bir sayı listesinin ortalamasını almaya çalışırsak,
tür hatası alırdık, çünkü `/` sembolünü `Int`'lerde kullanamayız. Öte yandan `genericLength`, `genericLength :: (Num a) => [b] -> a` tür imzasına sahiptir.
Bir `Num` bir kayan noktalı sayı gibi davranabildiğinden, ortalamayı `let xs = [1..6] in sum xs / genericLength xs` yaparak elde etmek gayet iyi sonuç verir.

`nub`, `delete`, `union`, `intersect` ve `group` fonksiyonlarının tümü, `nubBy`, `deleteBy`, `unionBy`, `intersectBy` ve `groupBy` adında daha genel karşılıklarına sahiptir.
Aralarındaki fark, ilk fonksiyon kümesinin eşitliği test etmek için `==` kullanmasıdır, 
oysa *By* fonksiyonları bir eşitlik fonksiyonu alır ve ardından bu eşitlik fonksiyonunu kullanarak bunları karşılaştırır. `group`, `groupBy (==)` ile aynıdır.

Örneğin, her saniye için bir fonksiyonun değerini açıklayan bir listemiz olduğunu varsayalım.
Değerin ne zaman sıfırın altında olduğuna ve ne zaman üstüne çıktığına göre onu alt listelere ayırmak istiyoruz.
Normal bir `group` yapsaydık, sadece eşit bitişik değerleri bir araya getirirdi. Ama istediğimiz, onları olumsuz olup olmadıklarına göre gruplamaktır.
`groupBy` burada devreye girer! *By* fonksiyonlarına sağlanan eşitlik fonksiyona, aynı türden iki öğeyi almalı ve standartlarına göre eşit kabul ederse `True` döndürmelidir.

~~~~ {.haskell: .ghci name="code"}
ghci> let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]  
ghci> groupBy (\x y -> (x > 0) == (y > 0)) values  
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]] 
~~~~

Buradan hangi bölümlerin olumlu hangilerinin olumsuz olduğunu açıkça görüyoruz. 
Sağlanan eşitlik fonksiyona iki öğe alır ve ardından yalnızca ikisi de negatifse veya her ikisi de pozitifse True döndürür.
Bu eşitlik fonksiyona `\x y -> (x > 0) && (y > 0) || (x <= 0) && (y <= 0)` olarak da yazılabilir, ancak bence birinci yol daha okunabilir.
By fonksiyonları için eşitlik fonksiyonları yazmanın daha net bir yolu, `Data.Function`'dan `on` fonksiyonunu içe aktarmanızdır. `on` şu şekilde tanımlanır:

~~~~ {.haskell: .ghci name="code"}
on :: (b -> b -> c) -> (a -> b) -> a -> a -> c  
f `on` g = \x y -> f (g x) (g y)  
~~~~

Yani ``(==) `on` (> 0)`` yapıldığında `\x y -> (x > 0) == (y > 0)` gibi görünen bir eşitlik fonksiyona döndürülür. 
`on`, *By* fonksiyonlarıyla çokça kullanılır çünkü bununla şunları yapabiliriz:

~~~~ {.haskell: .ghci name="code"}
ghci> groupBy ((==) `on` (> 0)) values  
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]  
~~~~

Benzer şekilde, `sort`, `insert`, `maximum` ve `minimum` da daha genel eşdeğerlerine sahiptir. 
`groupBy` gibi fonksiyonlar, iki öğenin ne zaman eşit olduğunu belirleyen bir fonksiyon alır. 
`sortBy`, `insertBy`, `maximumBy` ve `minimumBy`, bir öğenin diğerinden büyük, küçük veya eşit olup olmadığını belirleyen bir fonksiyonu alır.
`sortBy`'ın tür imzası `sortBy :: (a -> a -> Ordering) -> [a] -> [a]` şeklindedir. Daha önce hatırlıyorsanız, `Ordering` türü `LT`, `EQ` veya `GT` değerine sahip olabilir.
`sort`, `sortBy compare`'in eşdeğeridir, çünkü karşılaştırma yalnızca türü `Ord` tür sınıfında olan iki öğeyi alır ve sıralama ilişkilerini döndürür.

Listeler karşılaştırılabilir, ancak olduklarında sözlükbilimsel(lexicographically) olarak karşılaştırılırlar.
Ya bir liste listemiz varsa ve bunu iç listelerin içeriğine göre değil, uzunluklarına göre sıralamak istiyorsak? 
Muhtemelen tahmin ettiğiniz gibi `sortBy` fonksiyonunu kullanacağız.

~~~~ {.haskell: .ghci name="code"}
ghci> let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]  
ghci> sortBy (compare `on` length) xs  
[[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]  
~~~~

``compare `on` length`` dostum, bu neredeyse gerçek İngilizceye benziyor. Burada tam olarak nasıl çalıştığından emin değilseniz, ``compare `on` length``, 
``\x y -> length x `compare` length y``'nin eşdeğeridir. Eşitlik fonksiyonu alan *By* fonksiyonlarıyla uğraşırken,
genellikle ``(==) `on` something`` yaparsınız ve bir sıralama fonksiyonu alan *By* fonksiyonlarıyla uğraşırken, genellikle ``compare `on` something`` yaparsınız.


Data.Char
---------

`Data.Char` modülü adından da anlaşılacağı gibi yapar. Karakterlerle ilgilenen fonksiyonları dışa aktarır.
String'ler üzerinde filtreleme ve eşleme yaparken de faydalıdır çünkü bunlar sadece karakter listeleridir.
![legochar](../img/legochar.png)

`Data.Char`, karakterler üzerinden bir grup predicate'i dışa aktarır. Yani, bir karakteri alan ve bize bu varsayımın doğru mu yanlış mı olduğunu söyleyen fonksiyonlar.

İşte bunlar:

`isControl`, bir karakterin bir kontrol karakteri olup olmadığını kontrol eder. 

`isSpace`, bir karakterin white-space karakteri olup olmadığını kontrol eder. Buna boşluklar, sekme karakterleri, yeni satırlar vb. içerir.

`isLower`, bir karakterin küçük harfli olup olmadığını kontrol eder.

`isUpper`, bir karakterin büyük harfli olup olmadığını kontrol eder.

`isAlpha`, bir karakterin harf olup olmadığını kontrol eder.

`isAlphaNum`, bir karakterin harf mi yoksa sayı mı olduğunu kontrol eder.

`isPrint`, bir karakterin yazdırılabilir olup olmadığını kontrol eder. Örneğin, kontrol karakterleri yazdırılamaz.

`isDigit`, bir karakterin rakam olup olmadığını kontrol eder.

`isOctDigit`, bir karakterin sekizlik(Octal) rakam olup olmadığını kontrol eder.

`isHexDigit`, bir karakterin onaltılık(hex) bir rakam olup olmadığını kontrol eder.

`isLetter` bir karakterin harf olup olmadığını kontrol eder.

`isMark`, Unicode işaret karakterlerini kontrol eder. Bunlar, aksanlı sonlar oluşturmak için önceki harflerle birleşen karakterlerdir. Fransızsanız bunu kullanın.

`isNumber`, bir karakterin sayısal olup olmadığını kontrol eder.

`isPunctuation`, bir karakterin noktalama işareti olup olmadığını kontrol eder.

`isSymbol`, bir karakterin süslü bir matematik veya para birimi simgesi olup olmadığını kontrol eder.

`isSeparator`, Unicode boşluklarını ve ayırıcıları denetler.

`isAscii`, bir karakterin Unicode karakter kümesinin ilk 128 karakterine girip girmediğini kontrol eder.

`isLatin1`, bir karakterin Unicode'un ilk 256 karakterine girip girmediğini kontrol eder.

`isAsciiUpper`, bir karakterin ASCII ve büyük harf olup olmadığını kontrol eder.

`isAsciiLower`, bir karakterin ASCII ve küçük harf olup olmadığını kontrol eder.

Tüm bu predicate'ler bir `Char -> Bool` tür imzasına sahiptir. Çoğu zaman bunu string'leri veya bunun gibi bir şeyi filtrelemek için kullanacaksınız.
Örneğin, bir kullanıcı adı alan bir program yaptığımızı varsayalım ve kullanıcı adı sadece alfasayısal karakterlerden oluşabilir.
Kullanıcı adının doğru olup olmadığını belirlemek için `Data.List` fonksiyonunu `all` ile `Data.Char` predicate'lerini birlikte kullanabiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> all isAlphaNum "bobby283"  
True  
ghci> all isAlphaNum "eddy the fish!"  
False  
~~~~

Kewl. Hatırlamazsanız, `all` bir predicate ve bir liste alır ve yalnızca bu predicate listedeki her öğe için tutuyorsa `True` döndürür.

`Data.List`'in `words` fonksiyonunu simüle etmek için `isSpace`'i de kullanabiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> words "hey guys its me"  
["hey","guys","its","me"]  
ghci> groupBy ((==) `on` isSpace) "hey guys its me"  
["hey"," ","guys"," ","its"," ","me"]  
ghci>    
~~~~

Hmmm, `words`'ün yaptığını yapıyor gibi ama sadece boşluk unsurlarıyla baş başa kalıyoruz. Hmm, ne yapalım? Biliyorum, şu salağı süzelim.

~~~~ {.haskell: .ghci name="code"}
ghci> filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey guys its me"  
["hey","guys","its","me"]     
~~~~

Ah.

`Data.Char` ayrıca `Ordering` gibi bir veri türünü dışa aktarır. `Ordering` türü `LT`, `EQ` veya `GT` değerine sahip olabilir. Bu bir çeşit sıralama.
İki öğenin karşılaştırılmasından doğabilecek birkaç olası sonucu açıklar. `GeneralCategory` türü ayrıca bir numaralandırmadır(enumaration).
Bize bir karakterin içine girebileceği birkaç olası kategori sunar. Bir tür `generalCategory :: Char -> GeneralCategory` vardır.
Yaklaşık 31 kategori var, bu yüzden hepsini burada listelemeyeceğiz, ancak fonksiyonla oynayalım.

~~~~ {.haskell: .ghci name="code"}
ghci> generalCategory ' '  
Space  
ghci> generalCategory 'A'  
UppercaseLetter  
ghci> generalCategory 'a'  
LowercaseLetter  
ghci> generalCategory '.'  
OtherPunctuation  
ghci> generalCategory '9'  
DecimalNumber  
ghci> map generalCategory " \t\nA9?|"  
[Space,Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol] 
~~~~

`GeneralCategory` türü `Eq` tür sınıfıının bir parçası olduğundan, `generalCategory c == Space` gibi şeyleri de test edebiliriz.

`toUpper`, bir karakteri büyük harfe dönüştürür. Boşluklar, sayılar ve benzerleri değişmeden kalır.

`toLower`, bir karakteri küçük harfe dönüştürür.

`toTitle`, bir karakteri büyük / küçük harfe dönüştürür. Çoğu karakter için, başlık-büyük / küçük harf aynıdır.

`digitToInt`, bir karakteri `Int`'e dönüştürür. Başarılı olmak için karakterin `'0'..'9'`, `'a'..'f'` veya `'A'..'F'` aralığında olması gerekir.

~~~~ {.haskell: .ghci name="code"}
ghci> map digitToInt "34538"  
[3,4,5,3,8]  
ghci> map digitToInt "FF85AB"  
[15,15,8,5,10,11]  
~~~~

`intToDigit`, `digitToInt`'in ters fonsiyonudur. `0..15` aralığında bir `Int` alır ve onu küçük harf karakterine dönüştürür.

~~~~ {.haskell: .ghci name="code"}
ghci> intToDigit 15  
'f'  
ghci> intToDigit 5  
'5'  
~~~~

`Ord` ve `chr` fonksiyonları, karakterleri karşılık gelen sayılara dönüştürür ve bunun tersi de geçerlidir:

~~~~ {.haskell: .ghci name="code"}
ghci> ord 'a'  
97  
ghci> chr 97  
'a'  
ghci> map ord "abcdefgh"  
[97,98,99,100,101,102,103,104]  
~~~~

İki karakterin `ord` değerleri arasındaki fark, Unicode tablosunda ne kadar uzakta olduklarına eşittir.

Sezar şifresi(Caesar cipher), mesajların her bir karakteri alfabede sabit sayıda konumla kaydırarak kodlamanın ilkel bir yöntemidir.
Kendimize ait bir tür Sezar şifresini kolayca yaratabiliriz, ancak kendimizi alfabeye sınırlamayız.

~~~~ {.haskell: .ghci name="code"}
encode :: Int -> String -> String  
encode shift msg = 
    let ords = map ord msg  
        shifted = map (+ shift) ords  
    in  map chr shifted   
~~~~

Burada önce string'i sayılar listesine dönüştürüyoruz. Ardından sayılar listesini tekrar karakterlere dönüştürmeden önce her sayıya kaydırma miktarını ekliyoruz.
Bir kompozisyon kovboyuysanız, bu fonksiyonun gövdesini `map (chr . (+ shift) . ord) msg` olarak yazabilirsiniz. Birkaç mesajı kodlamayı deneyelim.

~~~~ {.haskell: .ghci name="code"}
ghci> encode 3 "Heeeeey"  
"Khhhhh|"  
ghci> encode 4 "Heeeeey"  
"Liiiii}"  
ghci> encode 1 "abcd"  
"bcde"  
ghci> encode 5 "Marry Christmas! Ho ho ho!"  
"Rfww~%Hmwnxyrfx&%Mt%mt%mt&"  
~~~~

Pekala kodlanmış. Bir mesajın kodunu çözmek, temelde onu ilk başta kaydırıldığı yer sayısına göre geri kaydırmaktır.

~~~~ {.haskell: .ghci name="code"}
decode :: Int -> String -> String  
decode shift msg = encode (negate shift) msg  
~~~~

~~~~ {.haskell: .ghci name="code"}
ghci> encode 3 "Im a little teapot"  
"Lp#d#olwwoh#whdsrw"  
ghci> decode 3 "Lp#d#olwwoh#whdsrw"  
"Im a little teapot"  
ghci> decode 5 . encode 5 $ "This is a sentence"  
"This is a sentence"  
~~~~


Data.Map
--------

İlişkilendirme listeleri (sözlükler de denir), sıralamanın önemli olmadığı durumlarda key-value çiftlerini depolamak için kullanılan listelerdir.
Örneğin, telefon numaralarını saklamak için bir ilişkilendirme listesi kullanabiliriz; burada telefon numaraları değerler, kişilerin adları ise anahtarlar olabilir.
Hangi sırayla saklandıkları umurumuzda değil, sadece doğru kişi için doğru telefon numarasını almak istiyoruz.

Haskell'de dernek listelerini temsil etmenin en açık yolu, bir çiftler listesine sahip olmaktır. Çiftteki ilk bileşen anahtar, ikinci bileşen değer olacaktır.
Aşağıda, telefon numaraları içeren bir ilişkilendirme listesi örneği verilmiştir:

~~~~ {.haskell: .ghci name="code"}
phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ]  
~~~~

Görünüşte garip olan bu girintiye rağmen, bu sadece dizelerin bir listesidir. İlişkilendirme listeleri ile uğraşırken en yaygın görev,
anahtara göre bir değer aramaktır. Anahtar verilen bir değeri arayan bir fonksiyon yapalım.

~~~~ {.haskell: .ghci name="code"}
findKey :: (Eq k) => k -> [(k,v)] -> v  
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs  
~~~~

Bir anahtar ve bir liste alan fonksiyon, listeyi filtreler, böylece yalnızca eşleşen anahtarlar kalır, eşleşen ilk key-value çiftini alır ve değeri döndürür.
Peki aradığımız anahtar ilişkilendirme listesinde yoksa ne olur? Burada, ilişkilendirme listesinde bir anahtar yoksa, 
bir çalışma zamanı hatası veren boş bir listenin başını almaya çalışırız. Bununla birlikte, programlarımızı bu kadar kolay çökertmekten kaçınmalıyız, 
bu yüzden `Maybe` veri türünü kullanalım. Anahtarı bulamazsak `Nothing` döndürürüz. Onu bulursak, `Just something` döndürürüz, burada bir şey o anahtara karşılık gelen değerdir.

~~~~ {.haskell: .ghci name="code"}
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey key [] = Nothing  
findKey key ((k,v):xs) = if key == k  
                            then Just v  
                            else findKey key xs  
~~~~

Tür bildirimine bakın. Eşitlenebilen bir anahtar, bir ilişkilendirme listesi alır ve sonra belki bir değer üretir. Doğru gibi görünüyor.

Bu, bir liste üzerinde çalışan bir ders kitabı özyinelemeli fonksiyondur. Uç durum, bir listeyi bir başlığa ve bir kuyruğa bölmek, özyinelemeli çağrılar, hepsi orada.
Bu klasik fold modelidir, bu yüzden bunun bir fold olarak nasıl uygulanacağını görelim.

~~~~ {.haskell: .ghci name="code"}
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v  
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing 
~~~~

**Not**: Okunması ve tanımlanması daha kolay olduğundan, özyinelemeyi açıkça yazmak yerine, bu standart liste özyineleme modeli için fold kullanmak genellikle daha iyidir.
Herkes `foldr` çağrısını gördüklerinde bunun bir pas geçme olduğunu bilir, ancak açık özyinelemeyi okumak biraz daha düşünmeyi gerektirir.

~~~~ {.haskell: .ghci name="code"}
ghci> findKey "penny" phoneBook  
Just "853-2492"  
ghci> findKey "betty" phoneBook  
Just "555-2938"  
ghci> findKey "wilma" phoneBook  
Nothing  
~~~~

Tıkır tıkır çalışıyor! Kızın telefon numarası elimizde varsa, numarayı `Just`'tan alırız, aksi takdirde `Nothing` alırız.

`Data.List`'ten `lookup` fonksiyonunu uyguladık. Bir key'e karşılık gelen value'yu bulmak istiyorsak, onu bulana kadar listenin tüm öğelerini taramamız gerekir.
`Data.Map` modülü, çok daha hızlı ilişkilendirme listeleri sunar (çünkü ağaçlarla dahili olarak uygulanırlar) ve ayrıca birçok yardımcı fonksiyon sağlar.
Şu andan itibaren, ilişkilendirme listeleri yerine haritalarla çalıştığımızı söyleyeceğiz.

`Data.Map`, `Prelude` ve `Data.List` fonksiyonlarıyla çakışan fonksiyonları dışa aktardığı için, nitelikli bir içe aktarma yapacağız.

~~~~ {.haskell: .ghci name="code"}
import qualified Data.Map as Map  
~~~~

Bu import ifadesini bir komut dosyasına koyun ve ardından komut dosyasını GHCI aracılığıyla yükleyin.

Haydi gidelim ve `Data.Map`'in bizim için ne sakladığını görelim! İşte fonksiyonların temel özeti.

`fromList` fonksiyonu bir ilişkilendirme listesi alır (liste biçiminde) ve aynı ilişkilendirmelere sahip bir harita döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> Map.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]  
fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]  
ghci> Map.fromList [(1,2),(3,4),(3,2),(5,5)]  
fromList [(1,2),(3,2),(5,5)]  
~~~~

Orijinal ilişkilendirme listesinde yinelenen anahtarlar varsa, kopyalar yalnızca atılır. Bu, `fromList`'in tür imzasıdır.

~~~~ {.haskell: .ghci name="code"}
Map.fromList :: (Ord k) => [(k, v)] -> Map.Map k v   
~~~~

`k` ve `v` türü çiftlerin bir listesini aldığını ve `k` türü anahtarlardan `v` türüne eşleyen bir harita döndürdüğünü söylüyor.
Normal listelerle ilişkilendirme listeleri yaptığımızda, anahtarların yalnızca denkleştirilebilir olması gerektiğine (bunların türü `Eq` tür sınıfına aittir),
ancak şimdi sıralanabilir olmaları gerektiğine dikkat edin. Bu, `Data.Map` modülündeki önemli bir kısıtlamadır.
Anahtarların bir ağaçta düzenlenmesi için sıralanabilir olması gerekiyor.

`Ord` tür sınıfının parçası olmayan anahtarlara sahip olmadığınız sürece key-value ilişkilendirmeleri için her zaman `Data.Map` kullanmalısınız.

`empty` boş bir haritayı temsil eder. Hiçbir argüman almaz, sadece boş bir map döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> Map.empty  
fromList []  
~~~~

`insert` bir anahtar, bir değer ve bir harita alır ve sadece anahtar ve değer eklenmiş olarak eskisi gibi yeni bir harita verir. 

~~~~ {.haskell: .ghci name="code"}
ghci> Map.empty  
fromList []  
ghci> Map.insert 3 100 Map.empty  
fromList [(3,100)]  
ghci> Map.insert 5 600 (Map.insert 4 200 ( Map.insert 3 100  Map.empty))  
fromList [(3,100),(4,200),(5,600)]  
ghci> Map.insert 5 600 . Map.insert 4 200 . Map.insert 3 100 $ Map.empty  
fromList [(3,100),(4,200),(5,600)]  
~~~~

Boş harita, `insert` ve bir fold kullanarak kendi `fromList`'imizi uygulayabiliriz. izleyin:

~~~~ {.haskell: .ghci name="code"}
fromList' :: (Ord k) => [(k,v)] -> Map.Map k v  
fromList' = foldr (\(k,v) acc -> Map.insert k v acc) Map.empty
~~~~

Oldukça basit bir fold. Boş bir haritayla başlıyoruz ve ilerledikçe anahtar değer çiftlerini toplayıcıya yerleştirerek onu sağdan fold'luyoruz.

`null`, haritanın boş olup olmadığını kontrol eder.

~~~~ {.haskell: .ghci name="code"}
ghci> Map.null Map.empty  
True  
ghci> Map.null $ Map.fromList [(2,3),(5,5)]  
False  
~~~~

`size` bir haritanın boyutunu bildirir.

~~~~ {.haskell: .ghci name="code"}
ghci> Map.size Map.empty  
0  
ghci> Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]  
5  
~~~~

`singleton` bir anahtar ve bir değer alır ve tam olarak bir eşlemesi olan bir map oluşturur.

~~~~ {.haskell: .ghci name="code"}
ghci> Map.singleton 3 9  
fromList [(3,9)]  
ghci> Map.insert 5 9 $ Map.singleton 3 9  
fromList [(3,9),(5,9)]    
~~~~

`lookup` `Data.List` `lookup`'ı gibi çalışır, yalnızca haritada çalışır. Anahtar için bir şey bulursa `Just somethings`, bulamazsa `Nothing` döndürür.

`member` bir predicate, anahtar ve bir harita alır ve anahtarın haritada olup olmadığını bildirir.

~~~~ {.haskell: .ghci name="code"}
ghci> Map.member 3 $ Map.fromList [(3,6),(4,3),(6,9)]  
True  
ghci> Map.member 3 $ Map.fromList [(2,5),(4,5)]  
False    
~~~~

`map` ve `filter`, liste eşdeğerleri(list equivalents) gibi çalışır.

~~~~ {.haskell: .ghci name="code"}
ghci> Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]  
fromList [(1,100),(2,400),(3,900)]  
ghci> Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]  
fromList [(2,'A'),(4,'B')]  
~~~~

`toList`, `fromList`'in tersidir.

~~~~ {.haskell: .ghci name="code"}
ghci> Map.toList . Map.insert 9 2 $ Map.singleton 4 3  
[(4,3),(9,2)]  
~~~~

`keys` ve `elems` sırasıyla anahtarların ve değerlerin listelerini döndürür. `keys`, `map fst . Map.toList`'e eşdeğerdir ve `elems`, `map snd . Map.toList`'e eşdeğerdir.

`fromListWith` harika bir küçük fonksiyondur. `fromList` gibi davranır, yalnızca benzer anahtarları atmaz, 
ancak onlarla ne yapılacağına karar vermek için kendisine sağlanan bir fonksiyonu kullanır. 
Diyelim ki bir kız birkaç numaraya sahip olabilir ve buna benzer bir ilişki listemiz var.

~~~~ {.haskell: .ghci name="code"}
phoneBook =   
    [("betty","555-2938")  
    ,("betty","342-2492")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("patsy","943-2929")  
    ,("patsy","827-9162")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")  
    ,("penny","555-2111")  
    ]  
~~~~

Şimdi bunu bir haritaya koymak için `fromList`'i kullanırsak, birkaç rakamı kaybedeceğiz! İşte yapacağımız şey:

~~~~ {.haskell: .ghci name="code"}
phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String  
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs  
~~~~

~~~~ {.haskell: .ghci name="code"}
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook  
"827-9162, 943-2929, 493-2928"  
ghci> Map.lookup "wendy" $ phoneBookToMap phoneBook  
"939-8282"  
ghci> Map.lookup "betty" $ phoneBookToMap phoneBook  
"342-2492, 555-2938" 
~~~~

Benzer bir anahtar bulunursa, geçirdiğimiz fonksiyon bu anahtarların değerlerini başka bir değerde birleştirmek için kullanılır.
İlk olarak ilişkilendirme listesindeki tüm değerleri tekli olarak yapabiliriz ve sonra sayıları birleştirmek için `++` kullanabiliriz.

~~~~ {.haskell: .ghci name="code"}
phoneBookToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]  
phoneBookToMap xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs  
~~~~

~~~~ {.haskell: .ghci name="code"}
ghci> Map.lookup "patsy" $ phoneBookToMap phoneBook  
["827-9162","943-2929","493-2928"]  
~~~~

Oldukça temiz! Başka bir kullanım durumu, bir ilişkilendirme listesinden bir map oluşturuyorsak ve yinelenen bir anahtar bulunduğunda, 
anahtar için en büyük değerin korunmasını isteriz.

~~~~ {.haskell: .ghci name="code"}
ghci> Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]  
fromList [(2,100),(3,29),(4,22)]  
~~~~

Veya aynı anahtarlar üzerinde değerleri birbirine eklemeyi seçebiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> Map.fromListWith (+) [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]  
fromList [(2,108),(3,62),(4,37)]  
~~~~

`insertWith`, `fromListWith`'in `fromList`'e ne olduğunu eklemektir. Bir haritaya bir key-value çifti ekler, ancak bu harita zaten anahtarı içeriyorsa,
ne yapılacağını belirlemek için ona iletilen fonksiyonu kullanır.

~~~~ {.haskell: .ghci name="code"}
ghci> Map.insertWith (+) 3 100 $ Map.fromList [(3,4),(5,103),(6,339)]  
fromList [(3,104),(5,103),(6,339)]  
~~~~

Bunlar `Data.Map`'in sadece birkaç fonksiyonuydu. [Dokümantasyonda](https://downloads.haskell.org/ghc/latest/docs/html/libraries/containers-0.6.2.1/Data-Map.html) fonksiyonların tam listesini görebilirsiniz.



Data.Set
--------

`Data.Set` modülü bize kümeler sunar. Matematikteki kümeler gibi. Kümeler, listeler ve haritalar arasında bir çeşit geçiş gibidir. Bir kümedeki tüm öğeler benzersizdir.
Ve dahili olarak ağaçlarla uygulandıkları için (`Data.Map`'teki map'ler gibi), sıralanırlar. Üyeliği kontrol etmek, eklemek,
silmek vb. Listelerle aynı şeyi yapmaktan çok daha hızlıdır. Kümelerle uğraşırken en yaygın işlem bir kümeye ekleme, üyeliği kontrol etme ve bir kümeyi listeye dönüştürmedir.
![legosets](../img/legosets.png)

`Data.Set`'teki isimler bir çok `Prelude` ve `Data.List` isimleriyle çakıştığından, qualified import yapıyoruz.

Bu import ifadesini bir komut dosyasına koyun:


~~~~ {.haskell: .ghci name="code"}
import qualified Data.Set as Set  
~~~~

Ve sonra komut dosyasını GHCI aracılığıyla yükleyin.

Diyelim ki iki parça metnimiz var. Her ikisinde de hangi karakterlerin kullanıldığını bulmak istiyoruz.

~~~~ {.haskell: .ghci name="code"}
text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"  
~~~~

`fromList` fonksiyonu beklediğiniz gibi çalışır. Bir liste alır ve bir kümeye dönüştürür.

~~~~ {.haskell: .ghci name="code"}
text1 = "I just had an anime dream. Anime... Reality... Are they so different?"  
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"  
~~~~

Gördüğünüz gibi, öğeler sıralı ve her öğe benzersizdir. Şimdi, her ikisinin de hangi öğeleri paylaştığını görmek için `intersection` fonksiyonunu kullanalım.

~~~~ {.haskell: .ghci name="code"}
ghci> Set.intersection set1 set2  
fromList " adefhilmnorstuy"  
~~~~

`difference` fonksiyonu, hangi harflerin ilk kümede olduğunu, ancak ikinci kümede olmadığını ve tersini görmek için kullanabiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> Set.difference set1 set2  
fromList ".?AIRj"  
ghci> Set.difference set2 set1  
fromList "!Tbcgvw"  
~~~~

Ya da `union` kullanarak her iki cümlede de kullanılan tüm unique harfleri görebiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> Set.union set1 set2  
fromList " !.?AIRTabcdefghijlmnorstuvwy" 
~~~~

`null`, `size`, `member`, `empty`, `singleton`, `insert` ve `delete` fonksiyonlarının tümü beklediğiniz gibi çalışır.

~~~~ {.haskell: .ghci name="code"}
ghci> Set.null Set.empty  
True  
ghci> Set.null $ Set.fromList [3,4,5,5,4,3]  
False  
ghci> Set.size $ Set.fromList [3,4,5,3,4,5]  
3  
ghci> Set.singleton 9  
fromList [9]  
ghci> Set.insert 4 $ Set.fromList [9,3,8,1]  
fromList [1,3,4,8,9]  
ghci> Set.insert 8 $ Set.fromList [5..10]  
fromList [5,6,7,8,9,10]  
ghci> Set.delete 4 $ Set.fromList [3,4,5,4,3,4,5]  
fromList [3,5]  
~~~~

Ayrıca alt kümeleri veya uygun alt kümeleri de kontrol edebiliriz. B, A'nın yaptığı tüm öğeleri içeriyorsa, A kümesi B kümesinin bir alt kümesidir.
B, A'nın yaptığı ancak daha fazla öğeye sahip tüm öğeleri içeriyorsa, A kümesi, B kümesinin uygun bir alt kümesidir.

~~~~ {.haskell: .ghci name="code"}
ghci> Set.fromList [2,3,4] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
True  
ghci> Set.fromList [1,2,3,4,5] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
True  
ghci> Set.fromList [1,2,3,4,5] `Set.isProperSubsetOf` Set.fromList [1,2,3,4,5]  
False  
ghci> Set.fromList [2,3,4,8] `Set.isSubsetOf` Set.fromList [1,2,3,4,5]  
False  
~~~~

Ayrıca kümeler üzerinde `map` ve `filter` da yapabiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]  
fromList [3,5,7]  
ghci> Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4]  
fromList [3,4,5,6,7,8]  
~~~~

Kümeler genellikle, bir listedeki yinelenenlerin bir listesini önce fromList ile bir küme haline getirip ardından `toList` ile bir
listeye geri dönüştürerek ayıklamak için kullanılır. `Data.List` fonksiyonu `nub` zaten bunu yapıyor, ancak büyük listeler için kopyaları ayıklamak,
onları bir kümeye sığdırırsanız ve daha sonra bunları `nub` kullanmaya kıyasla bir listeye dönüştürürseniz çok daha hızlıdır.
Ancak `nub` kullanmak yalnızca liste öğelerinin türünün `Eq` tür sınıfının parçası olmasını gerektirir, oysa öğeleri bir kümeye sıkıştırmak istiyorsanız,
listenin türü `Ord` tür sınıfı olur.

~~~~ {.haskell: .ghci name="code"}
ghci> let setNub xs = Set.toList $ Set.fromList xs  
ghci> setNub "HEY WHATS CRACKALACKIN"  
" ACEHIKLNRSTWY"  
ghci> nub "HEY WHATS CRACKALACKIN"  
"HEY WATSCRKLIN" 
~~~~

`setNub` genellikle büyük listelerdeki `nub`'dan daha hızlıdır, ancak görebileceğiniz gibi, `nub` listenin öğelerinin sırasını korurken `setNub` bunu yapmaz.


Kendi modüllerinizi yapmak
--------------------------

Şimdiye kadar bazı harika modüllere baktık, ancak kendi modülümüzü nasıl oluşturabiliriz? Hemen hemen her programlama dili,
kodunuzu birkaç dosyaya bölmenize olanak tanır ve Haskell de farklı değildir. Programlar oluştururken, benzer bir amaca yönelik fonksiyon 
ve türleri alıp bunları bir modüle yerleştirmek iyi bir uygulamadır. Bu şekilde, modülünüzü içe aktararak bu fonksiyonları diğer programlarda
kolayca yeniden kullanabilirsiniz.
![making_modules](../img/making_modules.png)


Birkaç geometrik nesnenin hacmini ve alanını hesaplamak için bazı fonksiyonlar sağlayan küçük bir modül yaparak kendi modüllerimizi nasıl oluşturabileceğimize bir bakalım.
`Geometry.hs` adında bir dosya oluşturarak başlayacağız.

Bir modülün fonksiyonları export ettiğini söylüyoruz. Bunun anlamı, bir modülü import ettiğimde, export ettiğim fonksiyonları kullanabilirim.
Fonksiyonların dahili olarak çağırdığı fonksiyonları tanımlayabilir, ancak biz sadece export edilenleri görebilir ve kullanabiliriz.

Bir modülün başında modül adını belirtiriz. `Geometry.hs` adında bir dosyamız varsa, modülümüze `Geometry` adını vermeliyiz.
Daha sonra export edeceğimiz fonksiyonları belirliyoruz ve ardından fonksiyonları yazmaya başlayabiliriz. Öyleyse bununla başlayacağız.

~~~~ {.haskell: .ghci name="code"}
module Geometry  
( sphereVolume  
, sphereArea  
, cubeVolume  
, cubeArea  
, cuboidArea  
, cuboidVolume  
) where  
~~~~

Gördüğünüz gibi küreler, küpler ve küpler için alanlar ve hacimler yapacağız. Devam edelim ve fonksiyonlarımızı tanımlayalım o zaman:

~~~~ {.haskell: .ghci name="code"}
module Geometry  
( sphereVolume  
, sphereArea  
, cubeVolume  
, cubeArea  
, cuboidArea  
, cuboidVolume  
) where  
  
sphereVolume :: Float -> Float  
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
  
sphereArea :: Float -> Float  
sphereArea radius = 4 * pi * (radius ^ 2)  
  
cubeVolume :: Float -> Float  
cubeVolume side = cuboidVolume side side side  
  
cubeArea :: Float -> Float  
cubeArea side = cuboidArea side side side  
  
cuboidVolume :: Float -> Float -> Float -> Float  
cuboidVolume a b c = rectangleArea a b * c  
  
cuboidArea :: Float -> Float -> Float -> Float  
cuboidArea a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
  
rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b  
~~~~

Burada oldukça standart bir geometri var. Yine de dikkat edilmesi gereken birkaç nokta var. Bir küp, bir küboid için yalnızca özel bir durum olduğundan, alanını ve hacmini,
kenarları aynı uzunlukta olan bir küboid gibi ele alarak tanımladık. Ayrıca, bir dikdörtgenin alanını kenarlarının uzunluklarına göre hesaplayan `rectangleArea` adlı bir
yardımcı fonksiyon tanımladık. Oldukça önemsiz çünkü sadece çarpma. Modüldeki fonksiyonlarımızda (yani `cuboidArea` ve `cuboidVolume`) kullandığımıza dikkat edin, 
ancak export etmedik! Modülümüzün sadece üç boyutlu nesnelerle ilgilenmek için fonksiyonlar sunmasını istediğimiz için, `rectangleArea` kullandık ama dışarı aktarmadık.

Bir modül oluştururken, genellikle uygulamanın gizlenmesi için modülümüze sadece bir tür arayüz görevi gören fonksiyonları dışa aktarırız.
Birisi `Geometry` modülümüzü kullanıyorsa, export etmediğimiz fonksiyonlarla ilgilenmeleri gerekmez.
Bu fonksiyonları tamamen değiştirmeye veya daha yeni bir sürümde silmeye karar verebiliriz (`rectangleArea`'yı silebilir ve bunun yerine sadece `*` kullanabiliriz) 
ve ilk etapta bunları export etmediğimiz için kimse aldırmayacaktır.

Modülümüzü kullanmak için sadece şunları yapıyoruz:

~~~~ {.haskell: .ghci name="code"}
import Geometry    
~~~~

`Geometry.hs`, onu import eden programla aynı klasörde olmalıdır.

Modüllere ayrıca hiyerarşik yapılar da verilebilir. Her modülün bir takım alt modülleri olabilir ve kendilerine ait alt modülleri olabilir.
Bu fonksiyonu bölümlere ayıralım, böylece `Geometry`, her nesne türü için bir tane olmak üzere üç alt modülü olan bir modüldür.

Önce `Geometry` adında bir klasör oluşturacağız. Başkent G'ye dikkat edin. İçine üç dosya yerleştireceğiz: `Sphere.hs`, `Cuboid.hs` ve `Cube.hs`. İşte dosyalar içerecekler:

`Sphere.hs`

~~~~ {.haskell: .ghci name="code"}
module Geometry.Sphere  
( volume  
, area  
) where  
  
volume :: Float -> Float  
volume radius = (4.0 / 3.0) * pi * (radius ^ 3)  
  
area :: Float -> Float  
area radius = 4 * pi * (radius ^ 2)  
~~~~

`Cuboid.hs`

~~~~ {.haskell: .ghci name="code"}
module Geometry.Cuboid  
( volume  
, area  
) where  
  
volume :: Float -> Float -> Float -> Float  
volume a b c = rectangleArea a b * c  
  
area :: Float -> Float -> Float -> Float  
area a b c = rectangleArea a b * 2 + rectangleArea a c * 2 + rectangleArea c b * 2  
  
rectangleArea :: Float -> Float -> Float  
rectangleArea a b = a * b  
~~~~

`Cube.hs`

~~~~ {.haskell: .ghci name="code"}
module Geometry.Cube  
( volume  
, area  
) where  
  
import qualified Geometry.Cuboid as Cuboid  
  
volume :: Float -> Float  
volume side = Cuboid.volume side side side  
  
area :: Float -> Float  
area side = Cuboid.area side side side  
~~~~

İlk önce `Geometry.Sphere`. Onu `Geometry` adlı bir klasöre nasıl yerleştirdiğimize ve modül adını `Geometry.Sphere` olarak tanımladığımıza dikkat edin.
Cuboid için de aynısını yaptık. Ayrıca, her üç alt modülde de aynı adlarla fonksiyonları tanımladığımıza dikkat edin.
Bunu yapabiliriz çünkü bunlar ayrı modüllerdir. `Geometry.Cuboid`'deki fonksiyonları `Geometry.Cube`'de kullanmak istiyoruz,
ancak doğrudan `import Geometry.Cuboid` diyemeyiz çünkü fonksiyonları `Geometry.Cube` ile aynı isimlerle export eder.
Bu sebeple qualified import yapıyoruz ve her şey yolunda gidiyor.

`Geometry` klasörüyle aynı seviyede olan bir dosyadaysak şunu söyleyebiliriz:

~~~~ {.haskell: .ghci name="code"}
import Geometry.Sphere  
~~~~

Ve sonra `area` ve `volume` diyebiliriz ve bize bir kürenin alan (area) ve hacimini(volume) verirler. Ve bu modüllerden iki veya daha fazlasını idare etmek istiyorsak, 
aynı adlarla fonksiyonları export etmek için qualified import yapmalıyız. Bu yüzden şöyle bir şey yapıyoruz:

~~~~ {.haskell: .ghci name="code"}
import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube  
~~~~

Ve sonra `Sphere.area`, `Sphere.volume`, `Cuboid.area`, vb. diyerek çağırabiliriz. ve her biri karşılık gelen nesnenin area'sını veya volume'ünü hesaplayacaktır.

Bir dahaki sefere kendinizi gerçekten büyük ve birçok fonksiyonu olan bir dosya yazarken bulduğunuzda, 
hangi fonksiyonların bazı ortak amaca hizmet ettiğini görmeye çalışın ve sonra bunları kendi modüllerine yerleştirip koyamayacağınıza bakın.
Bir dahaki sefere aynı işlevselliğin(functionality) bir kısmını gerektiren bir program yazarken modülünüzü import edebileceksiniz.

