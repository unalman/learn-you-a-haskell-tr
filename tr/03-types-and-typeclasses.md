Türler ve Tür Sınıfları
=======================

Türe inanın
-------------
![cow](../img/cow.png)
Daha önce Haskell'in statik tür bir sisteme sahip olduğundan bahsetmiştik. Her ifadenin türü derleme sırasında bilinir ve bu da daha güvenli koda yol açar.
Bir boolean türünü bir sayı ile bölmeye çalıştığınız bir program yazarsanız, derlenmez bile. Bu iyidir çünkü programınızın çökmesi yerine
bu tür hataları derleme zamanında yakalamak daha iyidir. Haskell'deki her şeyin bir türü vardır, bu nedenle derleyici,
onu derlemeden önce programınız hakkında oldukça fazla mantık yürütebilir.

Java veya Pascal'dan farklı olarak Haskell, tür çıkarımına sahiptir. Bir sayı yazarsak, Haskell'e bunun bir sayı olduğunu söylemek zorunda değiliz.
Bunu kendi başına çıkarabilir, bu nedenle işleri halletmek için fonksiyonlarımızın ve ifadelerimizin türlerini açık bir şekilde yazmak zorunda kalmayız.
Haskell'in bazı temellerini türlere çok yüzeysel bir bakışla ele aldık. Ancak, tür sistemini anlamak Haskell öğrenmenin çok önemli bir parçasıdır.

Tür, her ifadenin sahip olduğu bir tür etikettir. Bize ifadenin hangi kategoriye uyduğunu söyler. `True` ifadesi bir boole, `"hello"` bir string'dir vb.

Şimdi GHCI'yi bazı ifadelerin türlerini incelemek için kullanacağız. Bunu, herhangi bir geçerli ifadenin izlediği `:t` komutunu kullanarak bize türünü söyleyeceğiz.
Hadi bir koşuşturma yapalım.

~~~~ {.haskell: .ghci name="code"}
ghci> :t 'a'  
'a' :: Char  
ghci> :t True  
True :: Bool  
ghci> :t "HELLO!"  
"HELLO!" :: [Char]  
ghci> :t (True, 'a')  
(True, 'a') :: (Bool, Char)  
ghci> :t 4 == 5  
4 == 5 :: Bool 
~~~~

Burada, bir ifadede `:t` yapıldığında, ifadenin ardından :: ve türünün yazdırıldığını görüyoruz. `::` "türü vardır" olarak okunur. 
Açık türler her zaman büyük harfle ilk harfle gösterilir. Göründüğü gibi `'a'` `Char` türüne sahiptir. Karakteri temsil ettiği sonucuna varmak zor değil. 
`True`, Bool türündedir. Bu mantıklı. Ama bu nedir? `"HELLO!"` bir `[Char]` verir. Köşeli parantezler bir listeyi belirtir. Bu yüzden bunu bir karakter listesi olarak okuyoruz.
Listelerden farklı olarak, her demet uzunluğunun kendi türü vardır. Yani (`True, 'a'`) ifadesinin bir türü (`Bool, Char`) varken, (`'a', 'b', 'c'`) gibi
bir ifade (`Char, Char, Char`). `4 == 5` her zaman `False` döndürecektir, bu nedenle türü `Bool`'dur.

Fonksiyonların da türleri vardır. Kendi fonksiyonlarımızı yazarken, onlara açık bir tür bildirimi vermeyi seçebiliriz. Bu, çok kısa fonksiyonlar yazmanın dışında
genellikle iyi bir uygulama olarak kabul edilir. Bundan sonra tür bildirimi yaptığımız tüm fonksiyonları vereceğiz. 
Bir string'i yalnızca büyük harf kalacak şekilde filtreleyen daha önce yaptığımız liste anlayışını hatırlıyor musunuz? Tür bildiriminde nasıl göründüğü aşağıda açıklanmıştır.

~~~~ {.haskell: .ghci name="code"}
removeNonUppercase :: [Char] -> [Char]  
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   
~~~~

`removeNonUppercase`, bir `[Char] -> [Char]` türüne sahiptir, yani bir string'ten bir string'e eşlenir. Bunun nedeni, bir string'i parametre olarak alması ve 
sonuç olarak başka bir string'i döndürmesidir. `[Char]` türü `String` ile eşanlamlıdır, bu nedenle `removeNonUppercase :: String -> String` yazarsak daha net olur.
Bu fonksiyona bir tür bildirimi vermek zorunda değildik çünkü derleyici kendi başına bunun bir string'ten string'e bir fonksiyon olduğunu çıkarabilir, ancak yine de yaptık.
Fakat birkaç parametre alan bir fonksiyonun türünü nasıl yazabiliriz? İşte üç tamsayı alan ve bunları birbirine ekleyen basit bir fonksiyon:

~~~~ {.haskell: .ghci name="code"}
addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  
~~~~

Parametreler `->` ile ayrılır ve parametreler ile dönüş türü arasında özel bir ayrım yoktur. Dönüş türü, bildirimdeki son maddedir ve parametreler ilk üçüdür.
Daha sonra, dönüş türleri ve `Int, Int, Int -> Int` veya başka bir şey gibi parametreler arasında daha açık bir ayrım yapmak yerine
neden hepsinin `->` ile ayrıldığını göreceğiz.

Fonksiyonunuza bir tür bildirimi vermek istiyorsanız, ancak ne olması gerektiğinden emin değilseniz, fonksiyonu her zaman onsuz yazabilir ve
ardından `:t` ile kontrol edebilirsiniz. Fonksiyonlar da ifadelerdir, bu yüzden `:t` onlar üzerinde sorunsuz çalışır.

İşte bazı yaygın türlere genel bir bakış.

`Int` tamsayı anlamına gelir. Tam sayılar için kullanılır. `7` bir `Int` olabilir ancak `7.2` olamaz. `Int` sınırlıdır, yani minimum ve maksimum değere sahip olduğu anlamına gelir.Genellikle 32 bit makinelerde olası maksimum `Int` 2147483647 ve minimum -2147483648'dir.

`Integer`, er… aynı zamanda tamsayı anlamına gelir. Temel fark, sınırlı olmamasıdır, bu yüzden gerçekten büyük sayıları temsil etmek için kullanılabilir.
Yani gerçekten büyük gibi. `Int`, ancak daha etkilidir.

~~~~ {.haskell: .ghci name="code"}
factorial :: Integer -> Integer  
factorial n = product [1..n]  
~~~~

~~~~ {.haskell: .ghci name="code"}
ghci> factorial 50  
30414093201713378043612608166064768844377641568960512000000000000  
~~~~

`Float`, tek hassasiyetli reel bir kayan noktadır.

~~~~ {.haskell: .ghci name="code"}
circumference :: Float -> Float  
circumference r = 2 * pi * r  
~~~~

~~~~ {.haskell: .ghci name="code"}
ghci> circumference 4.0  
25.132742  
~~~~

`Double`, çift hassasiyetli reel bir kayan noktadır!

~~~~ {.haskell: .ghci name="code"}
circumference' :: Double -> Double  
circumference' r = 2 * pi * r  
~~~~

~~~~ {.haskell: .ghci name="code"}
ghci> circumference' 4.0  
25.132741228718345  
~~~~

`Bool`, bir boolean türüdür. Yalnızca iki değeri olabilir: `True` ve `False`.

`Char` bir karakteri temsil eder. Tek tırnakla belirtilmiştir. Bir karakter listesi bir string'tir.

`Demetler(Tuples)'de` bir türdür ancak uzunluklarına ve bileşenlerin türlerine bağlıdırlar, bu nedenle sonsuz sayıda demet vardır.
Boş demetin () yalnızca tek bir değere sahip olabilen bir tür olduğunu unutmayın. Boş demet'in `()` aynı zamanda yalnızca tek bir değere sahip olabilen
bir tür olduğuna dikkat edin: `()`


Tür Değişkenleri
----------------

`head` fonksiyonunun türü sizce nedir? `head` herhangi bir türden bir liste aldığından ve ilk öğeyi döndürdüğünden, bu ne olabilir? Hadi kontrol edelim!

~~~~ {.haskell: .ghci name="code"}
ghci> :t head  
head :: [a] -> a  
~~~~

![box](../img/box.png)
Hmmm! Bu `a` nedir? Tür mü? Daha önce türlerin büyük harflerle yazıldığını belirttiğimizi unutmayın, bu nedenle tam olarak bir tür olamaz. 
Büyük harf olmadığı için aslında bir **tür değişkeni**. Bu, `a`'nın herhangi bir türde olabileceği anlamına gelir.
Bu, diğer dillerdeki jeneriklere(generics) çok benzer, yalnızca Haskell'de çok daha güçlüdür
çünkü içlerindeki türlerin herhangi bir özel davranışını kullanmazlarsa, çok genel fonksiyonları kolayca yazmamıza izin verir.
Tür değişkenleri olan fonksiyonlara **polimorfik fonksiyonlar** denir. `head`'in tür bildirimi, herhangi bir türden bir liste aldığını ve bu türden bir öğe döndürdüğünü belirtir.

Tür değişkenleri bir karakterden uzun isimlere sahip olabilmesine rağmen, genellikle onlara a, b, c, d ... isimleri veririz.

`fst`'yi hatırlıyor musun? Bir çiftin ilk bileşenini döndürür. Türünü inceleyelim.

~~~~ {.haskell: .ghci name="code"}
ghci> :t fst  
fst :: (a, b) -> a  
~~~~

`fst`'nin iki tür içeren bir demet aldığını ve çiftin ilk bileşeniyle aynı türden bir öğe döndürdüğünü görüyoruz. 
Bu nedenle, herhangi iki türü içeren bir çift üzerinde `fst` kullanabiliriz. Sadece `a` ve `b`'nin farklı tür değişkenleri olması nedeniyle
farklı türler olmaları gerekmediğini unutmayın. Yalnızca ilk bileşenin türü ile dönüş değerinin türünün aynı olduğunu belirtir.

Tür sınıfları 101
-----------------

![classes](../img/classes.png)
Tür sınıfı, bazı davranışları tanımlayan bir tür arayüzdür. Bir tür, bir tür sınıfının parçasıysa, bu, tür sınıfının tanımladığı davranışı desteklediği ve
uyguladığı anlamına gelir. OOP'den gelen birçok insan, nesne yönelimli dillerdeki sınıflar gibi olduklarını düşündükleri için tür sınıflarıyla karıştırılıyor.
Şey, değiller. Bunları bir nevi Java arayüzleri olarak düşünebilirsiniz, ancak daha iyisi.

`==` fonksiyonunun tür bildirimi nedir?

~~~~ {.haskell: .ghci name="code"}
ghci> :t (==)  
(==) :: (Eq a) => a -> a -> Bool  
~~~~

**Not**: eşitlik operatörü, `==` bir fonksiyondur. `+`, `*`, `-` ve hemen hemen tüm operatörler de öyle. Bir fonksiyon yalnızca özel karakterlerden oluşuyorsa,
varsayılan olarak bir infix fonksiyonu olarak kabul edilir. Türünü incelemek, başka bir fonksiyona geçirmek veya prefix fonksiyonu olarak adlandırmak istiyorsak,
onu parantez içine almalıyız.

İlginç. Burada yeni bir şey görüyoruz, `=>` sembolü. `=>` sembolünden önceki her şeye **sınıf kısıtlaması** denir. Önceki tür bildirimini şu şekilde okuyabiliriz:
eşitlik fonksiyonu aynı türden herhangi iki değeri alır ve bir `Bool` döndürür. Bu iki değerin türü, `Eq` sınıfının bir üyesi olmalıdır (bu, sınıf kısıtlamasıydı).

`Eq` tür sınıfı, eşitliği test etmek için bir arayüz sağlar. Bu türden iki değer arasındaki eşitliği test etmenin mantıklı olduğu her tür, `Eq` sınıfının bir üyesi olmalıdır.
IO (giriş ve çıkışla ilgilenme türü) ve fonksiyonlar hariç tüm standart Haskell türleri `Eq` tür sınıfının bir parçasıdır.

`elem` fonksiyonu `(Eq a) => a -> [a] -> Bool` türüne sahip çünkü aradığımız bir değerin içinde olup olmadığını kontrol etmek için bir liste üzerinde `==` kullanır.

Bazı temel tür sınıfları:

`Eq`, eşitlik testini destekleyen türler için kullanılır. Üyelerinin uyguladığı fonksiyonlar `==` ve `/=`'dir. Dolayısıyla, bir fonksiyondaki bir tür değişkeni için
bir `Eq` sınıfı kısıtlaması varsa, tanımının içinde bir yerde `==` veya `/=` kullanır. Fonksiyonlar dışında daha önce bahsettiğimiz tüm türler `Eq`'nun bir parçasıdır,
bu yüzden eşitlik açısından test edilebilirler.

~~~~ {.haskell: .ghci name="code"}
ghci> 5 == 5  
True  
ghci> 5 /= 5  
False  
ghci> 'a' == 'a'  
True  
ghci> "Ho Ho" == "Ho Ho"  
True  
ghci> 3.432 == 3.432  
True  
~~~~

`Ord`, sıralaması olan türler içindir.

~~~~ {.haskell: .ghci name="code"}
ghci> :t (>)  
(>) :: (Ord a) => a -> a -> Bool  
~~~~

Fonksiyonlar dışında şimdiye kadar ele aldığımız tüm türler `Ord`'un parçasıdır. `Ord`, `>`, `<`,`>=` ve `<=` gibi tüm standart karşılaştırma fonksiyonlarını kapsar.
`compare` fonksiyonu aynı türden iki `Ord` üyesini alır ve bir sıralama döndürür. `Ordering` `GT`, `LT` veya `EQ` olabilen bir türdür, yani sırasıyla *daha büyük*, *daha küçük* ve *eşittir*.

`Ord` üyesi olmak için, bir türün öncelikle prestijli ve `Eq` klübünün üyesi olması gerekir.

~~~~ {.haskell: .ghci name="code"}
ghci> "Abrakadabra" < "Zebra"  
True  
ghci> "Abrakadabra" `compare` "Zebra"  
LT  
ghci> 5 >= 2  
True  
ghci> 5 `compare` 3  
GT  
~~~~

`Show` üyeleri string'ler olarak sunulabilir. Fonksiyonlar dışında şimdiye kadar kapsanan tüm türler `Show`'un bir parçasıdır. `Show` tür sınıfı ile ilgilenen
en çok kullanılan fonksiyon `show`'dur. Türü `Show` üyesi olan bir değeri alır ve bize string olarak sunar.

~~~~ {.haskell: .ghci name="code"}
ghci> show 3  
"3"  
ghci> show 5.334  
"5.334"  
ghci> show True  
"True"
~~~~

`Read`, `Show`'un zıt tür sınıfıdır. `read` fonksiyonu bir string alır ve `Read`'ın üyesi olan bir tür döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> read "True" || False  
True  
ghci> read "8.2" + 3.8  
12.0  
ghci> read "5" - 2  
3  
ghci> read "[1,2,3,4]" ++ [3]  
[1,2,3,4,3]  
~~~~

Çok uzak çok iyi. Yine, şimdiye kadar kapsanan tüm türler bu tür sınıfındadır. Ama sadece `read "4"` yapmaya çalışırsak ne olur?

~~~~ {.haskell: .ghci name="code"}
ghci> read "4"  
<interactive>:1:0:  
    Ambiguous type variable `a' in the constraint:  
      `Read a' arising from a use of `read' at <interactive>:1:0-7  
    Probable fix: add a type signature that fixes these type variable(s)  
~~~~

GHCI'ın burada bize söylediği şey, karşılığında ne istediğimizi bilmediğidir. `read`'in önceki kullanımlarında, daha sonra sonuçla bir şeyler yaptığımıza dikkat edin.
Bu şekilde GHCI, bizim `read`'imizden ne tür bir sonuç istediğimizi çıkarabilirdi. Onu bir boolean olarak kullanırsak, bir `Bool` döndürmesi gerektiğini biliyordu.
Ama şimdi, `Read` class'ının bir parçası olan bir tür istediğimizi biliyor, hangisi olduğunu bilmiyor. `read`'in tür imzasına bir göz atalım.

~~~~ {.haskell: .ghci name="code"}
ghci> :t read  
read :: (Read a) => String -> a  
~~~~

Gördünüz mü? `Read` parçası olan bir tür döndürür, ancak onu daha sonra bir şekilde kullanmaya çalışmazsak, onun hangi tür olduğunu bilmenin
bir yolu yoktur. Bu yüzden açık **tür ek açıklamaları(type annotations)** kullanabiliriz. Tür ek açıklamaları, bir ifadenin türünün ne olması gerektiğini
açıkça söylemenin bir yoludur. Bunu ifadenin sonuna `::` ekleyerek ve ardından bir tür belirleyerek yapıyoruz. Gözlemleyin:

~~~~ {.haskell: .ghci name="code"}
ghci> read "5" :: Int  
5  
ghci> read "5" :: Float  
5.0  
ghci> (read "5" :: Float) * 4  
20.0  
ghci> read "[1,2,3,4]" :: [Int]  
[1,2,3,4]  
ghci> read "(3, 'a')" :: (Int, Char)  
(3, 'a')  
~~~~

Çoğu ifade, derleyicinin kendi türünün ne olduğunu çıkarabileceği şekildedir. Ancak bazen derleyici, `read "5"` gibi bir ifade için `Int` veya `Float` türünde
bir değer döndürüp döndürmeyeceğini bilemez. Türün ne olduğunu görmek için Haskell'in `read "5"` değerini değerlendirmesi gerekir.
Ancak Haskell statik olarak yazılmış bir dil olduğundan, kod derlenmeden (veya GHCI durumunda değerlendirilmeden) önce tüm türleri bilmesi gerekir.
Bu yüzden Haskell'e şunu söylemeliyiz: "Hey, bilmiyorsan diye bu ifade bu türde olmalı!".

`Enum` üyeleri sıralı türdedir - numaralandırılabilirler. `Enum` tür sınıfının asıl avantajı, türlerini liste aralıklarında kullanabilmemizdir.
`succ` ve `pred` fonksiyonlar elde edebileceğiniz ardılları ve öncüleri de tanımladılar. Bu sınıftaki türler: `()`, `Bool`, `Char`, `Ordering`, `Int`, `Integer`, `Float` ve `Double`.

~~~~ {.haskell: .ghci name="code"}
ghci> ['a'..'e']  
"abcde"  
ghci> [LT .. GT]  
[LT,EQ,GT]  
ghci> [3 .. 5]  
[3,4,5]  
ghci> succ 'B'  
'C'  
~~~~

`Bounded` üyelerin bir alt bir de üst sınırı vardır.

~~~~ {.haskell: .ghci name="code"}
ghci> minBound :: Int  
-2147483648  
ghci> maxBound :: Char  
'\1114111'  
ghci> maxBound :: Bool  
True  
ghci> minBound :: Bool  
False  
~~~~

`minBound` ve `maxBound` ilginçtir. Çünkü bir `(Bounded a) => a` türe sahiptirler. Bir anlamda polymorphic(polimorfik) sabitlerdir.

Bileşenler de içindeyse, tüm demetler de `Bounded`'ın parçasıdır.

~~~~ {.haskell: .ghci name="code"}
ghci> maxBound :: (Bool, Int, Char)  
(True,2147483647,'\1114111')  
~~~~

`Num` bir sayısal tür sınıftır. Üyeleri, sayılar gibi hareket etme özelliğine sahiptir. Bir sayının türünü inceleyelim.

~~~~ {.haskell: .ghci name="code"}
ghci> :t 20  
20 :: (Num t) => t  
~~~~

Tam sayıların aynı zamanda polimorfik sabitler olduğu görülmektedir. `Num` tür sınıfının bir üyesi olan herhangi bir tür gibi davranabilirler.

~~~~ {.haskell: .ghci name="code"}
ghci> 20 :: Int  
20  
ghci> 20 :: Integer  
20  
ghci> 20 :: Float  
20.0  
ghci> 20 :: Double  
20.0  
~~~~

Bunlar `Num` tür sınıfında bulunan türlerdir. `*` Türünü incelersek, tüm sayıları kabul ettiğini görürüz.

~~~~ {.haskell: .ghci name="code"}
ghci> :t (*)  
(*) :: (Num a) => a -> a -> a  
~~~~

Aynı türden iki sayı alır ve bu türden bir sayı döndürür. Bu nedenle `(5 :: Int) * (6 :: Integer)` bir tür hatasıyla sonuçlanırken,
`5 * (6 :: Integer)` iyi çalışır ve bir `Integer` üretir çünkü `5` bir `Integer` gibi davranabilir veya bir `Int`.

`Num`'a katılmak için, bir türün zaten `Show` ve `Eq` ile arkadaş olması gerekir.

`Integral` ayrıca sayısal bir tür sınıfıdır. `Num`, gerçek sayılar ve integral sayılar dahil tüm sayıları içerir, `Integral` yalnızca integral (tüm) sayıları içerir.
Bu tür sınıfında `Int` ve `Integer`'da vardır.

`Floating` yalnızca kayan nokta sayılarını içerir, bu nedenle `Float` ve `Double`.

Sayılarla uğraşmak için çok kullanışlı bir fonksiyon `fromIntegral`'dir. `fromIntegral :: (Num b, Integral a) => a -> b` şeklinde bir tür bildirimine sahiptir.
Tür imzasından, bir tamsayı aldığını ve onu daha genel bir sayıya dönüştürdüğünü görüyoruz. Bu, integral ve kayan nokta türlerinin birlikte
güzel bir şekilde çalışmasını istediğinizde kullanışlıdır. Örneğin, `length` fonksiyonu, daha genel bir `(Num b) => length :: [a] -> b` türüne sahip olmak yerine
`length :: [a] -> Int` türünde bir tür bildirimine sahiptir. Sanırım bu, tarihsel nedenlerle falan var, bence oldukça aptalca olsa da.
Her neyse, bir listenin uzunluğunu elde etmeye ve sonra onu `3.2`'ye eklemeye çalışırsak, bir `Int` ve bir kayan nokta sayısını birbirine eklemeye çalıştığımız için bir hata alacağız. Yani bunun üstesinden gelmek için `fromIntegral (length [1,2,3,4]) + 3.2` yapıyoruz ve her şey yolunda gidiyor.

`fromIntegral`'in tür bildiriminde birkaç class kısıtlaması olduğuna dikkat edin. Bu tamamen geçerlidir ve görebileceğiniz gibi,
sınıf kısıtlamaları parantez içinde virgülle ayrılmıştır.

