Birkaç Monad İçin Daha
======================

![clint](../img/clint.png)
Monad'ların bağlamlarla değerleri almak ve bunları fonksiyonlara uygulamak için nasıl kullanılabileceğini ve `>>=` veya `do n`otasyonu kullanmanın
bağlamı bizim için ele alınırken değerlere odaklanmamızı nasıl sağladığını gördük.

`Maybe` monadıyla tanıştık ve değerlere olası başarısızlık bağlamını nasıl eklediğini gördük. Liste monad'ını öğrendik ve
programlarımıza kolayca belirsizliği dahil etmemizi nasıl sağladığını gördük. Ayrıca `IO` monad'ında nasıl çalışılacağını da, monad'ın ne olduğunu bilmeden önce öğrendik!

Bu bölümde, diğer birkaç monad hakkında bilgi edineceğiz. Her tür değeri monadik değerler olarak ele almamıza izin vererek programlarımızı nasıl
daha net hale getirebileceklerini göreceğiz. Birkaç monad'ı daha fazla keşfetmek, monadlar için sezgimizi de sağlamlaştıracaktır.

Keşfedeceğimiz monadlar, `mtl` paketinin bir parçasıdır. Haskell paketi, modüller topluluğudur. `mtl` paketi Haskell Platformu ile birlikte gelir,
yani muhtemelen zaten ona sahipsiniz. Yapıp yapmadığınızı kontrol etmek için komut satırına `ghc-pkg list` yazın.
Bu, hangi Haskell paketlerini kurduğunuzu ve bunlardan birinin `mtl` olması gerektiğini ve ardından bir sürüm numarasını gösterecektir.


Yazar? Onu zar zor tanımıyorum!
-------------------------------

Silahımıza `Maybe` monad'ı, liste monad'ı ve `IO` monad'ı yükledik. Şimdi `Writer` monad'ını odaya koyalım ve onu ateşlediğimizde ne olacağını görelim!

`Maybe`, ek bir başarısızlık bağlamına sahip değerler içindir ve liste, kararsız değerler içindir,
`Writer` monad'ı, bir tür günlük(log) değeri olarak hareket eden başka bir değere sahip değerler içindir.
`Writer`, tüm günlük değerlerinin tek bir günlük değerinde birleştirilerek daha sonra sonuca eklenmesini sağlarken hesaplamalar yapmamızı sağlar.

Örneğin, değerlerimizi, muhtemelen hata ayıklama amacıyla neler olduğunu açıklayan string'lerle donatmak isteyebiliriz.
Bir çeteden birkaç haydut alan ve bize bunun büyük bir çete olup olmadığını söyleyen bir fonksiyonu düşünün. Bu çok basit bir fonksiyon:

~~~~ {.haskell: .ghci name="code"}
isBigGang :: Int -> Bool  
isBigGang x = x > 9  
~~~~

Şimdi, bize sadece `True` veya `False` bir değer vermek yerine, ne yaptığını söyleyen bir günlük string'i döndürmesini istiyorsak ne olur?
Pekala, biz sadece bu string'i oluşturup `Bool`'umuzun yanında döndürürüz:

~~~~ {.haskell: .ghci name="code"}
isBigGang :: Int -> (Bool, String)  
isBigGang x = (x > 9, "Compared gang size to 9.")  
~~~~

Şimdi sadece bir `Bool` döndürmek yerine, başlığın ilk bileşeninin gerçek değer ve ikinci bileşenin bu değere eşlik eden string olan bir demet döndürüyoruz.
Şimdi değerimize bazı ek bağlamlar var. Şuna bir bakalım:

~~~~ {.haskell: .ghci name="code"}
ghci> isBigGang 3  
(False,"Compared gang size to 9.")  
ghci> isBigGang 30  
(True,"Compared gang size to 9.")  
~~~~

![tuco](../img/tuco.png)
Çok uzak çok iyi. `isBigGang` normal bir değer alır ve bağlamlı bir değer döndürür. Az önce gördüğümüz gibi, onu normal bir değerle beslemek bir sorun değil.
Şimdi ya `(3, "Smallish gang.")` Gibi bir günlük string'i eklenmiş bir değerimiz varsa ve onu `isBigGang`'a beslemek istiyorsak?
Görünüşe göre bir kez daha şu soruyla karşı karşıyayız: Normal bir değer alan ve bağlamlı bir değer döndüren bir fonksiyonumuz varsa,
nasıl bağlamla bir değer alıp fonksiyonu besleriz?

`Maybe` monad'ını keşfederken, `Maybe a` değeri ve `a -> Maybe b` türü bir fonksiyonu alan ve fonksiyona `Maybe a` değerini besleyen bir `applyMaybe` fonksiyonu yaptık
fonksiyon `Maybe a` yerine normal bir `a` alsa bile. Bunu, `Maybe a` değerleriyle gelen bağlamı dikkate alarak yaptık, yani bunlar olası başarısızlıklara sahip değerlerdi.
Ancak `a -> Maybe b` fonksiyonunun içinde, bu değeri normal bir değer olarak ele alabildik, çünkü `applyMaybe` (daha sonra `>>=` oldu) bir `Nothing` mi yoksa
`Just` bir değer mi olduğunu kontrol etti.

Aynı şekilde, ekli bir günlükle bir değer alan, yani bir `(a, String)` değeri ve `a -> (b, String)` türünde bir fonksiyon alan ve
bu değerle fonksiyonu besleyen bir fonksiyon yapalım. Biz buna applyLog adını vereceğiz. Ancak bir `(a, String)` değeri beraberinde olası bir hata bağlamı taşımadığından,
daha ziyade ek bir günlük değeri bağlamı taşıdığından, `applyLog` orijinal değerin günlüğünün kaybolmadığından emin olacaktır,
ancak fonksiyondan kaynaklanan değerin günlüğü ile birleştirilir. `applyLog` uygulamasının uygulanması:

~~~~ {.haskell: .ghci name="code"}
applyLog :: (a,String) -> (a -> (b,String)) -> (b,String)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log ++ newLog)  
~~~~

Bağlamlı bir değere sahip olduğumuzda ve onu bir fonksiyona beslemek istediğimizde, genellikle gerçek değeri bağlamdan ayırmaya çalışırız ve
ardından fonksiyonu değere uygulamaya çalışırız ve ardından bağlamın halledildiğini görürüz. `Maybe` monadında, değerin `Just x` olup olmadığını kontrol ettik ve
eğer öyleyse, o `x`'i alıp fonksiyonu ona uyguladık. Bu durumda, gerçek değeri bulmak çok kolaydır, çünkü bir bileşenin değer, diğerinin günlük olduğu bir çiftle uğraşıyoruz.
İlk önce `x` olan değeri alıyoruz ve ona `f` fonksiyonunu uyguluyoruz. Bir `(y, newLog)` çifti elde ederiz, burada `y` yeni sonuçtur ve `newLog` yeni günlüktür.
Ancak bunu sonuç olarak döndürürsek, eski günlük değeri sonuca dahil edilmeyecektir, bu nedenle bir `(y, log ++ newLog)` çifti döndürürüz.
Yeni günlüğü eskisine eklemek için `++` kullanıyoruz.

İşte `applyLog` uygulamasında:

~~~~ {.haskell: .ghci name="code"}
ghci> (3, "Smallish gang.") `applyLog` isBigGang  
(False,"Smallish gang.Compared gang size to 9")  
ghci> (30, "A freaking platoon.") `applyLog` isBigGang  
(True,"A freaking platoon.Compared gang size to 9")  
~~~~

Sonuçlar öncekine benzer, ancak şimdi çetedeki kişi sayısına beraberinde günlüğü vardı ve sonuç günlüğüne dahil edildi.
Aşağıda, `applyLog`'u kullanmanın birkaç örneği daha verilmiştir:

~~~~ {.haskell: .ghci name="code"}
ghci> ("Tobin","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length."))  
(5,"Got outlaw name.Applied length.")  
ghci> ("Bathcat","Got outlaw name.") `applyLog` (\x -> (length x, "Applied length"))  
(7,"Got outlaw name.Applied length")  
~~~~

Lambda içinde, `x`'in bir demet değil, sadece normal bir string olduğunu ve `applyLog`'un günlükleri eklemeyle nasıl ilgilendiğini görün.


Kurtarıcı Monoid'ler
--------------------

Bu noktada [monoid'lerin](../tr/11-functors-applicative-functors-and-monoids.md#monoidler) ne olduğunu bildiğinizden emin olun! Şerefe.

Şu anda, `applyLog` `(a, String)` türündeki değerleri alıyor, ancak günlüğün `String` olması için bir neden var mı? Günlükleri eklemek için `++` kullanır,
bu nedenle bu yalnızca bir karakter listesi değil, herhangi bir listede çalışmaz mı? Elbette olur. Devam edip türünü şu şekilde değiştirebiliriz:

~~~~ {.haskell: .ghci name="code"}
applyLog :: (a,[c]) -> (a -> (b,[c])) -> (b,[c])  
~~~~

Şimdi, günlük bir listedir. Listede bulunan değerlerin türü, orijinal liste için olduğu kadar fonksiyonun döndürdüğü liste için de aynı olmalıdır,
aksi takdirde bunları birbirine yapıştırmak için `++` kullanamayız.

Bu, bytestrings için işe yarar mı? Olmaması için hiçbir sebep yok. Ancak, sahip olduğumuz tür artık yalnızca listeler için çalışıyor.
Görünüşe göre bytestrings için ayrı bir `applyLog` oluşturmalıyız. Fakat bekle! Hem listeler hem de yan testler monoid'lerdir.
Bu nedenle, her ikisi de `Monoid` tür sınıfının instance'larıdır, yani `mappend` fonksiyonunu uygularlar.
Hem listeler hem de bytestrings için `mappend` eklemek içindir. İzleyin:

~~~~ {.haskell: .ghci name="code"}
ghci> [1,2,3] `mappend` [4,5,6]  
[1,2,3,4,5,6]  
ghci> B.pack [99,104,105] `mappend` B.pack [104,117,97,104,117,97]  
Chunk "chi" (Chunk "huahua" Empty)  
~~~~

Güzel! Artık `applyLog`'umuz herhangi bir monoid için çalışabilir. Uygulamanın yanı sıra bunu yansıtacak şekilde türü de değiştirmeliyiz,
çünkü `++`'yı `mappend` olarak değiştirmeliyiz:

~~~~ {.haskell: .ghci name="code"}
applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)  
~~~~

Eşlik eden değer artık herhangi bir monoid değer olabileceğinden, artık demeti bir değer ve bir günlük olarak düşünmek zorunda değiliz,
ancak şimdi onu eşlik eden bir monoid değeri olan bir değer olarak düşünebiliriz. Örneğin, monoid değer olarak bir öğe adı ve bir öğe fiyatı olan bir demetimiz olabilir.
Öğelerle çalışırken fiyatların eklendiğinden emin olmak için `Sum` newtype'ını kullanırız. İşte bazı kovboy yiyeceklerine içecek ekleyen bir fonksiyon:

~~~~ {.haskell: .ghci name="code"}
import Data.Monoid  
  
type Food = String  
type Price = Sum Int  
  
addDrink :: Food -> (Food,Price)  
addDrink "beans" = ("milk", Sum 25)  
addDrink "jerky" = ("whiskey", Sum 99)  
addDrink _ = ("beer", Sum 30)  
~~~~

Yiyecekleri temsil etmek için string'leri ve bir şeyin kaç sente mal olduğunu takip etmek için bir `Sum` `newtype` sarmalayıcısında bir `Int` kullanırız.
`Sum` ile `mappend` yapmak, sarmalanmış değerlerin toplanmasına neden olur:

~~~~ {.haskell: .ghci name="code"}
ghci> Sum 3 `mappend` Sum 9  
Sum {getSum = 12}  
~~~~

`addDrink` fonksiyonu oldukça basittir. Fasulye yiyorsak, `Sum 25` ile birlikte `"milk"` verir, yani `Sum`'a sarılmış 25 sent.
Kurutulmuş et yiyorsak viski içeriz ve başka bir şey yiyorsak bira içeriz. Normalde bu fonksiyonu bir yiyeceğe uygulamak şu anda çok da ilginç olmayacaktır,
ancak bu fonksiyonda bir fiyatla gelen bir yiyeceği beslemek için `applyLog`'u kullanmak ilginç:

~~~~ {.haskell: .ghci name="code"}
ghci> ("beans", Sum 10) `applyLog` addDrink  
("milk",Sum {getSum = 35})  
ghci> ("jerky", Sum 25) `applyLog` addDrink  
("whiskey",Sum {getSum = 124})  
ghci> ("dogmeat", Sum 5) `applyLog` addDrink  
("beer",Sum {getSum = 35})  
~~~~

Süt `25` sente, ama eğer onu `10` sent olan fasulyeyle yersek, sonunda `35` sent ödüyoruz. Şimdi, eklenen değerin nasıl her zaman bir günlük olması gerekmediği,
herhangi bir monoid değer olabileceği ve bu tür iki değerin nasıl tek bir değerde birleştirileceğinin monoid'e bağlı olduğu açıktır.
Günlükleri oluştururken, bunlar eklendi, ama şimdi sayılar toplanıyor.

`addDrink` değerinin döndürdüğü değer bir tür demeti `(Food, Price)` olduğundan, bu sonucu tekrar `addDrink` ile besleyebiliriz,
böylece bize içeceğimizle birlikte ne içmemiz gerektiğini ve bunun bize ne kadara mal olacağını söyler. Bir deneyelim:

~~~~ {.haskell: .ghci name="code"}
ghci> ("dogmeat", Sum 5) `applyLog` addDrink `applyLog` addDrink  
("beer",Sum {getSum = 65})  
~~~~

Köpek etine bir içecek eklemek, bir bira ve ek `30` sent ile sonuçlanır, yani `("beer", Sum 35)`. Ve bunu `addDrink` olarak beslemek için `applyLog`'u kullanırsak,
başka bir bira alırız ve sonuç `("beer", Sum 65)` olur.


Yazar türü
----------

Artık monoid ekli bir değerin monadik bir değer gibi davrandığını gördüğümüze göre, bu tür değer türleri için `Monad` instance'ını inceleyelim.
`Control.Monad.Writer` modülü, `Writer w a` türünü `Monad` instance'yla birlikte ve bu türdeki değerlerle uğraşmak için bazı yararlı fonksiyonlarla birlikte dışa aktarır(export).

İlk önce türün kendisini inceleyelim. Bir değere bir monoid eklemek için, onları bir demet halinde bir araya getirmemiz yeterlidir.
`Writer w a` türü bunun için yalnızca `newtype` sarmalayıcıdır. Tanımı çok basit:

~~~~ {.haskell: .ghci name="code"}
newtype Writer w a = Writer { runWriter :: (a, w) }  
~~~~

Bir `Monad` instance'ı haline getirilebilmesi ve türünün normal bir demetten ayrı olması için bir `newtype` ile sarılmıştır.
`a` tür parametresi, değerin türünü ve `w` türü parametresi, ekli monoid değerin türünü temsil eder.

`Monad` instance'ı şu şekilde tanımlanır:

~~~~ {.haskell: .ghci name="code"}
instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')  
~~~~

![angeleyes](../img/angeleyes.png)
Öncelikle `>>=`'ı inceleyelim. Uygulanması esasen `applyLog` ile aynıdır, ancak artık başlığımız `Writer` `newtype` ile sarıldığı için,
desen eşleştirme sırasında onu açmamız gerekiyor. `x` değerini alıyoruz ve ona `f` fonksiyonunu uyguluyoruz.
Bu bize bir `Writer w a` değeri verir ve bunun üzerinde desen eşleştirme için bir `let` ifadesi kullanırız.
Yeni sonuç olarak `y`'yi sunuyoruz ve eski monoid değeri yenisiyle birleştirmek için `mappend` kullanıyoruz.
Bunu bir demet içinde sonuç değeri ile paketliyoruz ve sonra bunu `Writer` constructor'yla sarmalıyoruz, böylece sonucumuz sadece sarılmamış bir demet yerine `Writer` değeridir.

Peki ya `return`? Bir değer almalı ve onu sonuç olarak o değeri sunan varsayılan bir minimum bağlama koymalıdır. Öyleyse, `Writer` değerleri için böyle bir bağlam ne olabilir?
Eşlik eden monoid değerin diğer monoid değerleri olabildiğince az etkilemesini istiyorsak, `mempty` kullanmak mantıklıdır.
`mempty`, `""` ve `Sum 0` ve boş bytestrings gibi özdeş monoid değerlerini sunmak için kullanılır. Ne zaman `mempty` ile başka bir monoid değer arasında `mappend` kullansak,
sonuç o diğer monoid değerdir. Dolayısıyla, bir `Writer` değeri oluşturmak için `return` kullanırsak ve sonra bu değeri bir fonksiyonu beslemek için `>>=` kullanırsak,
elde edilen monoid değer yalnızca fonksiyonun döndürdüğü şey olacaktır. `3` numarada `return` kelimesini birkaç kez kullanalım,
sadece onu her seferinde farklı bir monoidle eşleştireceğiz:

~~~~ {.haskell: .ghci name="code"}
ghci> runWriter (return 3 :: Writer String Int)  
(3,"")  
ghci> runWriter (return 3 :: Writer (Sum Int) Int)  
(3,Sum {getSum = 0})  
ghci> runWriter (return 3 :: Writer (Product Int) Int)  
(3,Product {getProduct = 1})  
~~~~

`Writer`'ın bir `show` instance'ı olmadığı için, `Writer` değerlerimizi gösterilebilecek normal string'lere dönüştürmek için `runWriter` kullanmak zorunda kaldık.
`String` için, monoid değer boş string'dir. `Sum` ile `0`'dır, çünkü bir şeye 0 eklersek, bu bir şey aynı kalır. `Product`'ın, özdeş'i `1`'dir.

`Writer` instance'ı `fail` için bir uygulama içermez, bu nedenle bir desen eşleştirme `do` notasyonunda başarısız olursa `error` çağrılır.


Writer ile do gösterimini kullanma
----------------------------------

Artık bir `Monad` instance'ımız olduğuna göre, `Writer` değerleri için `do` notasyonu kullanmakta özgürüz. Birkaç `Writer` değerimiz olduğunda ve
onlarla bir şeyler yapmak istediğimizde kullanışlıdır. Diğer monad'larda olduğu gibi, onları normal değerler olarak ele alabiliriz ve bağlam bizim için alınır.
Bu durumda, eklenen tüm monoid değerler `mappend` olur ve böylece nihai sonuca yansıtılır. İki sayıyı çarpmak için `Writer` ile `do` notasyonu kullanmanın basit bir
örneğini burada bulabilirsiniz:

~~~~ {.haskell: .ghci name="code"}
import Control.Monad.Writer  
  
logNumber :: Int -> Writer [String] Int  
logNumber x = Writer (x, ["Got number: " ++ show x])  
  
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    return (a*b)  
~~~~

`logNumber` bir sayıyı alır ve ondan bir `Writer` değeri oluşturur. Monoid için, string'lerin bir listesini kullanırız ve sayıyı,
sadece bu sayıya sahip olduğumuzu söyleyen tekli bir liste ile donatırız. `multWithLog`, `3` ve `5`'i çarpan ve
ekli günlüklerinin son günlüğe dahil edilmesini sağlayan bir `Writer` değeridir. Sonuç olarak `a*b`'yi sunmak için `return` kullanırız. 
return sadece bir şeyi alıp minimal bir bağlama yerleştirdiğinden, günlüğe hiçbir şey eklemeyeceğinden emin olabiliriz.
Bunu çalıştırırsak gördüklerimiz:

~~~~ {.haskell: .ghci name="code"}
ghci> runWriter multWithLog  
(15,["Got number: 3","Got number: 5"])  
~~~~

Bazen belirli bir noktaya bazı monoid değerlerin dahil edilmesini isteriz. Bunun için `tell` fonksiyonu kullanışlıdır.
Bu, `MonadWriter` tür sınıfının bir parçasıdır ve Writer durumunda, `["This is going on"]` gibi monoid bir değer alır ve
sonucu olarak kukla değeri `()` sunan ancak bizim istenen monoid değer eklendi. Sonuç olarak `()` olan bir monadik değere sahip olduğumuzda, onu bir değişkene bağlamayız.
İşte `multWithLog`, ancak bazı ekstra raporlama dahil:

~~~~ {.haskell: .ghci name="code"}
multWithLog :: Writer [String] Int  
multWithLog = do  
    a <- logNumber 3  
    b <- logNumber 5  
    tell ["Gonna multiply these two"]  
    return (a*b)  
~~~~

`return (a*b)`'nin son satır olması önemlidir, çünkü bir `do` ifadesindeki son satırın sonucu, tüm `do` ifadesinin sonucudur.
Son satır olarak `tell`'i koysaydık, `()` bu `do` ifadesinin sonucu olurdu. Çarpmanın sonucunu kaybedeceğiz. Ancak günlük aynı olacaktır. İşte bu eylemde:

~~~~ {.haskell: .ghci name="code"}
ghci> runWriter multWithLog  
(15,["Got number: 3","Got number: 5","Gonna multiply these two"])  
~~~~


Programlara günlük kaydı ekleme
-------------------------------

Öklid'in algoritması, iki sayıyı alan ve en büyük ortak bölenini hesaplayan bir algoritmadır. Yani ikisini de ayıran en büyük sayı.
Haskell zaten tam olarak bunu yapan `gcd` fonksiyonunu içeriyor, ama hadi kendimizinkini uygulayalım ve ardından onu günlükleme yetenekleriyle donatalım. 
İşte normal algoritma:

~~~~ {.haskell: .ghci name="code"}
gcd' :: Int -> Int -> Int  
gcd' a b   
    | b == 0    = a  
    | otherwise = gcd' b (a `mod` b)  
~~~~

İşte normal algoritma: İlk olarak, ikinci sayının 0 olup olmadığını kontrol eder. Öyleyse, sonuç ilk sayıdır. Değilse, sonuç, ikinci sayının en büyük ortak bölenidir ve
ilk sayıyı ikinci sayıyla bölmenin geri kalanıdır. Örneğin, 8 ve 3'ün en büyük ortak böleninin ne olduğunu bilmek istiyorsak, sadece özetlenen algoritmayı takip ederiz.
3, 0 olmadığı için, 3 ve 2'nin en büyük ortak bölenini bulmalıyız (8'i 3'e bölersek, kalan 2'dir). Sonra, 3 ve 2'nin en büyük ortak bölenini buluyoruz.
2 hala 0 değil, bu yüzden şimdi 2 ve 1 var. İkinci sayı 0 değildir, bu yüzden algoritmayı 1 ve 0 için tekrar çalıştırırız, çünkü 2'yi 1'e bölmek bize 0'ın kalanını verir.
Ve son olarak, ikinci sayı artık 0 olduğu için, nihai sonuç 1'dir. Bakalım bizim kodumuz uyuyor mu:

~~~~ {.haskell: .ghci name="code"}
ghci> gcd' 8 3  
1  
~~~~

Öyle. Çok iyi! Şimdi, sonucumuzu bir bağlamla donatmak istiyoruz ve bağlam, bir günlük görevi gören monoid bir değer olacaktır.
Daha önce olduğu gibi, monoid'imiz olarak bir string listesi kullanacağız. Dolayısıyla, yeni `gcd'` fonksiyonumuzun türü şöyle olmalıdır:

~~~~ {.haskell: .ghci name="code"}
gcd' :: Int -> Int -> Writer [String] Int  
~~~~

Şimdi geriye kalan tek şey, fonksiyonumuzu log değerleriyle donatmak. İşte kod:

~~~~ {.haskell: .ghci name="code"}
import Control.Monad.Writer  
  
gcd' :: Int -> Int -> Writer [String] Int  
gcd' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd' b (a `mod` b)  
~~~~

Bu fonksiyon iki normal `Int` değeri alır ve bir `Writer [String] Int`, yani bir günlük bağlamı olan bir `Int` döndürür.
`b`'nin `0` olduğu durumda, sonuç olarak sadece `a` vermek yerine, sonuç olarak bir `Writer` değerini bir araya getirmek için bir `do` ifadesi kullanırız.
Önce bitirdiğimizi bildirmek için `tell`'i kullanırız ve sonra `do` ifadesinin sonucu olarak `a`'yı sunmak için `return` kullanırız.
Bunun `do` ifadesi yerine şunu da yazabilirdik:

~~~~ {.haskell: .ghci name="code"}
Writer (a, ["Finished with " ++ show a])  
~~~~

Ancak, bence `do` ifadesinin okunması daha kolay. Sonra, `b`'nin `0` olmadığı durum var. Bu durumda, `a` ve `b`'yi bölmenin geri kalanını bulmak için mod kullandığımızı
günlüğe kaydederiz. Daha sonra, `do` ifadesinin ikinci satırı özyinelemeli olarak `gcd'` çağırır. Unutmayın, `gcd'` artık nihayetinde bir `Writer` değeri döndürür,
bu nedenle `gcd' b (a 'mod' b)`'nin `do` ifadesindeki bir satır olması tamamen geçerlidir.

Günlüklerin nasıl eklendiğini görmek için bu yeni `gcd'`'nin yürütülmesini elle izlemek faydalı olsa da, bence sadece büyük resme bakmak ve
bunları bir bağlamla birlikte değerler olarak görmek ve bundan nihai sonucun ne olacağına dair içgörü kazandırır.

Yeni `gcd'`'yi deneyelim. Sonucu bir `Writer [String] Int` değeridir ve bunu `newtype`'dan ayırırsak, bir demet elde ederiz.
Demetin ilk kısmı sonuçtur. Bakalım sorun olacak mı:

~~~~ {.haskell: .ghci name="code"}
ghci> fst $ runWriter (gcd' 8 3)  
1  
~~~~

İyi! Şimdi günlük ne olacak? Günlük, string'lerin bir listesi olduğundan, bu string'lerin ekrana yazdırmak için `mapM_ putStrLn` kullanalım:

~~~~ {.haskell: .ghci name="code"}
ghci> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)  
8 mod 3 = 2  
3 mod 2 = 1  
2 mod 1 = 0  
Finished with 1 
~~~~

Sıradan algoritmamızı, sadece normal değerleri monadik değerlere çevirerek ve günlükleri bizim için `Writer` için `>>=` uygulamasına bırakarak,
ne yaptığını raporlayan bir algoritmaya değiştirebilmemiz harika. Hemen hemen her fonksiyona bir kayıt mekanizması ekleyebiliriz.
Normal değerleri, istediğimiz yerde `Writer` değerleriyle değiştiririz ve normal fonksiyon uygulamasını `>>=` (veya okunabilirliği artırıyorsa `do` ifadeleri) olarak değiştiririz.


Verimsiz liste yapımı
---------------------

`Writer` monad'ını kullanırken, hangi monoid'in kullanılacağına dikkat etmelisiniz, çünkü listeleri kullanmak bazen çok yavaş olabilir.
Bunun nedeni, listelerin mappend için `++` kullanması ve listenin sonuna bir şey eklemek için `++` kullanılması, bu liste gerçekten uzunsa yavaş olmasıdır.

`gcd'` fonksiyonumuzda, günlüğe kaydetme hızlıdır çünkü eklenen liste şu şekilde görünür:

~~~~ {.haskell: .ghci name="code"}
a ++ (b ++ (c ++ (d ++ (e ++ f))))  
~~~~

Listeler soldan sağa inşa edilen bir veri yapısıdır ve bu etkilidir çünkü önce bir listenin sol kısmını tam olarak oluştururuz ve
ancak daha sonra sağ tarafa daha uzun bir liste ekleriz. Ancak dikkatli olmazsak, `Writer` monad'ını kullanmak aşağıdaki gibi görünen liste ekleri oluşturabilir:

~~~~ {.haskell: .ghci name="code"}
((((a ++ b) ++ c) ++ d) ++ e) ++ f  
~~~~

Bu, sağ yerine solla ilişkilendirilir. Bu verimsizdir, çünkü sağ parçayı sol kısma eklemek istediği her seferinde, sol kısmı baştan sonuna kadar inşa etmek zorundadır!

Aşağıdaki fonksiyon `gcd'` gibi çalışır, yalnızca işleri tersine kaydeder. Önce prosedürün geri kalanı için günlüğü oluşturur ve ardından geçerli adımı günlüğün sonuna ekler.

~~~~ {.haskell: .ghci name="code"}
import Control.Monad.Writer  
  
gcdReverse :: Int -> Int -> Writer [String] Int  
gcdReverse a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        result <- gcdReverse b (a `mod` b)  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        return result 
~~~~

Önce özyinelemeyi yapar ve sonuç değerini `result` bağlar. Daha sonra, mevcut adımı günlüğe ekler, ancak mevcut adım, özyineleme tarafından üretilen günlüğün sonuna gider.
Son olarak, nihai sonuç olarak özyinelemenin sonucunu sunar. İşte eylemde:

~~~~ {.haskell: .ghci name="code"}
ghci> mapM_ putStrLn $ snd $ runWriter (gcdReverse 8 3)  
Finished with 1  
2 mod 1 = 0  
3 mod 2 = 1  
8 mod 3 = 2  
~~~~

Verimsizdir çünkü `++` kullanımını sağ yerine solla ilişkilendirir.


Fark listeleri
--------------

![cactus](../img/cactus.png)
Listeler bazen bu şekilde tekrar tekrar eklendiğinde verimsiz olabileceğinden, her zaman verimli eklemeyi destekleyen bir veri yapısı kullanmak en iyisidir.
Böyle bir veri yapısı, fark listesidir. Bir fark listesi, bir listeye benzer, yalnızca normal bir liste olmak yerine,
bir listeyi alıp ona başka bir liste ekleyen bir fonksiyondur. `[1,2,3]` gibi bir listenin fark listesi eşdeğeri `\xs -> [1,2,3] ++ xs` fonksiyonu olacaktır.
Normal bir boş liste `[]` iken boş bir fark listesi `\xs -> [] ++ xs` fonksiyonudur.

Fark listeleri ile ilgili harika olan şey, verimli eklemeyi desteklemeleri. `++` ile iki normal liste eklediğimizde, `++`'nın solundaki listenin sonuna kadar yürümek ve
diğerini oraya yapıştırmak zorundadır. Peki ya fark listesi yaklaşımını kullanırsak ve listelerimizi fonksiyonlar olarak temsil edersek?
Öyleyse, iki fark listesi eklemek şu şekilde yapılabilir:

~~~~ {.haskell: .ghci name="code"}
f `append` g = \xs -> f (g xs)  
~~~~

Unutmayın, `f` ve `g` listeleri alıp başına bir şeyler ekleyen fonksiyonlardır. Örneğin, `f` fonksiyonu `("dog"++)` (`\xs -> "dog" ++ xs` yazmanın başka bir yolu) ve
`g` fonksiyonu `("meat"++)`, ardından ``f `append` g`` aşağıdakine eşdeğer yeni bir fonksiyon yapar:

~~~~ {.haskell: .ghci name="code"}
\xs -> "dog" ++ ("meat" ++ xs)  
~~~~

İlk olarak bir fark listesini bir listeye ve sonra diğerine uygulayan yeni bir fonksiyon oluşturarak iki fark listesi ekledik.

Fark listeleri için bir `newtype` sarmalayıcı yapalım, böylece onlara kolayca monoid instance'ları verebiliriz:

~~~~ {.haskell: .ghci name="code"}
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }  
~~~~

Sarmaladığımız tür `[a] -> [a]`'dır, çünkü bir fark listesi sadece bir listeyi alıp başka bir tane döndüren bir fonksiyondur.
Normal listeleri fark listelerine dönüştürmek ve tersini yapmak kolaydır:

~~~~ {.haskell: .ghci name="code"}
toDiffList :: [a] -> DiffList a  
toDiffList xs = DiffList (xs++)  
  
fromDiffList :: DiffList a -> [a]  
fromDiffList (DiffList f) = f []  
~~~~

Normal bir listeyi bir fark listesine dönüştürmek için daha önce yaptığımız şeyi yaparız ve onu başka bir listenin başına ekleyen bir fonksiyon yaparız.
Bir fark listesi, bir şeyi başka bir listenin başına ekleyen bir fonksiyon olduğundan, sadece bunu istiyorsak, fonksiyonu boş bir listeye uygularız!

İşte `Monoid` instance'ı:

~~~~ {.haskell: .ghci name="code"}
instance Monoid (DiffList a) where  
    mempty = DiffList (\xs -> [] ++ xs)  
    (DiffList f) `mappend` (DiffList g) = DiffList (\xs -> f (g xs))  
~~~~

Listeler için `mempty`'nin yalnızca `id` fonksiyonu ve `mappend`'in aslında yalnızca fonksiyon bileşimi olduğuna dikkat edin. Bunun işe yarayıp yaramadığını görelim:

~~~~ {.haskell: .ghci name="code"}
ghci> fromDiffList (toDiffList [1,2,3,4] `mappend` toDiffList [1,2,3])  
[1,2,3,4,1,2,3]  
~~~~

En iyi ipucu! Artık `gcdReverse` fonksiyonumuzun verimliliğini, normal listeler yerine fark listeleri kullanmasını sağlayarak artırabiliriz:

~~~~ {.haskell: .ghci name="code"}
import Control.Monad.Writer  
  
gcd' :: Int -> Int -> Writer (DiffList String) Int  
gcd' a b  
    | b == 0 = do  
        tell (toDiffList ["Finished with " ++ show a])  
        return a  
    | otherwise = do  
        result <- gcd' b (a `mod` b)  
        tell (toDiffList [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)])  
        return result  
~~~~

Sadece monoid'in türünü `[String]`'den `DiffList String`'e değiştirmemiz ve daha sonra `tell` kullanırken normal listelerimizi `toDiffList` ile
fark listelerine dönüştürmemiz gerekiyordu. Bakalım günlüğün düzgün bir şekilde birleştirilip birleştirilmediğini görelim:

~~~~ {.haskell: .ghci name="code"}
ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ gcdReverse 110 34  
Finished with 2  
8 mod 2 = 0  
34 mod 8 = 2  
110 mod 34 = 8  
~~~~

`gcdReverse 110 34` yapıyoruz, ardından onu `newtype`'dan ayırmak için `runWriter` kullanıyoruz, sonra sadece günlüğü almak için buna `snd` uyguluyoruz,
sonra onu normal bir listeye dönüştürmek için `fromDiffList` uyguluyoruz ve son olarak ekrana basıyoruz.


Karşılaştırma Performansı
-------------------------

Fark listelerinin performansınızı ne kadar iyileştirebileceğine dair bir fikir edinmek için, sadece bir sayıdan sıfıra doğru sayan,
ancak günlüğünü `gcdReverse` gibi tersine üreten bu fonksiyonu göz önünde bulundurun, böylece günlükteki sayılar gerçekte sayılır:

~~~~ {.haskell: .ghci name="code"}
finalCountDown :: Int -> Writer (DiffList String) ()  
finalCountDown 0 = do  
    tell (toDiffList ["0"])  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell (toDiffList [show x])  
~~~~

`0` verirsek, sadece günlüğe kaydeder. Diğer herhangi bir sayı için, önce öncülü `0`'a doğru sayar ve ardından bu sayıyı günlüğe ekler.
Bu yüzden, `finalCountDown`'u `100`'e uygularsak, `"100"` string'i günlüğün en sonunda gelir.

Her neyse, bu fonksiyonu GHCi'a yüklerseniz ve `500000` gibi büyük bir sayıya uygularsanız, `0`'dan itibaren hızla saymaya başladığını göreceksiniz:

~~~~ {.haskell: .ghci name="code"}
ghci> mapM_ putStrLn . fromDiffList . snd . runWriter $ finalCountDown 500000  
0  
1  
2  
...  
~~~~

Ancak, bunu fark listeleri yerine normal listeleri kullanacak şekilde değiştirirsek, örneğin:

~~~~ {.haskell: .ghci name="code"}
finalCountDown :: Int -> Writer [String] ()  
finalCountDown 0 = do  
    tell ["0"]  
finalCountDown x = do  
    finalCountDown (x-1)  
    tell [show x]  
~~~~

Ve sonra GHCi'a saymaya başlamasını söyleyin:

~~~~ {.haskell: .ghci name="code"}
ghci> mapM_ putStrLn . snd . runWriter $ finalCountDown 500000  
~~~~

Saymanın gerçekten yavaş olduğunu göreceğiz.

Elbette, programlarımızın ne kadar hızlı olduğunu test etmenin doğru ve bilimsel yolu bu değil, ancak bu durumda,
fark listeleri kullanmanın hemen sonuç üretmeye başladığını, normal listelerin ise sonsuza kadar sürdüğünü görebildik.

Oh, bu arada, Final Countdown by Europe şarkısı şimdi kafanda sıkışmış durumda. Zevk alın!

Okuyucu? Ugh, bu şaka yine olmaz.


Okuyucu? Ugh, bu şaka yine olmaz.
---------------------------------

[Applicative'lerle ilgili bölümde](../tr/11-functors-applicative-functors-and-monoids.md), `(->) r` fonksiyon türünün bir `Functor` instance'ı olduğunu gördük. Bir `f` fonksiyonunu bir `g` fonksiyonu üzerinden eşlemek,
`g` ile aynı şeyi alan, ona `g` uygulayan ve sonra `f`'yi o sonuca uygulayan bir fonksiyon oluşturacaktır. Yani temelde, `g` gibi yeni bir fonksiyon yapıyoruz,
ancak sonucunu döndürmeden önce, `f` de bu sonuca uygulanır. Örneğin:

~~~~ {.haskell: .ghci name="code"}
ghci> let f = (*5)  
ghci> let g = (+3)  
ghci> (fmap f g) 8  
55  
~~~~

Ayrıca, fonksiyonların applicative functor'lar olduğunu da gördük. Sanki sonuçlarını zaten almışız gibi fonksiyonların nihai sonuçları üzerinde
işlem yapmamıza izin veriyorlar. İşte bir örnek:

~~~~ {.haskell: .ghci name="code"}
ghci> let f = (+) <$> (*2) <*> (+10)  
ghci> f 3  
19  
~~~~

`(+) <$> (*2) <*> (+10)` ifadesi, bir sayı alan, bu sayıyı `(*2)` ve `(+10)`'a veren ve ardından sonuçları toplayan bir fonksiyon yapar.
Örneğin, bu fonksiyonu `3`'e uygularsak, hem `(*2)` hem de `(+10)` `3`'e uygulayarak `6` ve `13` verir. Daha sonra `6` ve `13` ile `(+)` çağırır ve sonuç `19`'dur.

`(->) r` fonksiyon türü yalnızca bir functor ve applicative functor değil, aynı zamanda bir monad'dır. Şimdiye kadar karşılaştığımız diğer monadik değerler gibi,
bir fonksiyon da bağlamı olan bir değer olarak düşünülebilir. Fonksiyonların bağlamı, bu değerin henüz mevcut olmadığı ve
sonuç değerini elde etmek için bu fonksiyonu bir şeye uygulamamız gerektiğidir.

Fonksiyonların functor'lar ve applicative functor'ları olarak nasıl çalıştığını zaten bildiğimiz için, hemen konuya girelim ve `Monad` instance'ının neye benzediğini görelim.
`Control.Monad.Instances` içinde bulunur ve küçük bir şeye benzer:

~~~~ {.haskell: .ghci name="code"}
instance Monad ((->) r) where  
    return x = \_ -> x  
    h >>= f = \w -> f (h w) w  
~~~~

Fonksiyonlar için `pure`'un nasıl uygulandığını ve `return`'ün `pure` ile hemen hemen aynı şey olduğunu görmüştük. 
Bir değeri alır ve onu sonucu olarak her zaman bu değere sahip olan minimal bir bağlama yerleştirir. 
Ve sonucu olarak her zaman belirli bir değere sahip olan bir fonksiyonu yapmanın tek yolu, parametresini tamamen görmezden gelmesini sağlamaktır.

`>>=` için uygulama biraz şifreli görünüyor, ama aslında hepsi bu kadar değil. Bir fonksiyona bir monadik değeri beslemek için `>>=` kullandığımızda,
sonuç her zaman monadik bir değerdir. Yani bu durumda, bir fonksiyonu başka bir fonksiyona beslediğimizde, sonuç da bir fonksiyondur.
Bu nedenle sonuç lambda olarak başlar. Şimdiye kadarki tüm `>>=` uygulamaları, sonucu her zaman bir şekilde monadik değerden ayırdı ve sonra `f` fonksiyonunu bu sonuca uyguladı. 
Aynı şey burada da oluyor. Bir fonksiyondan sonucu elde etmek için, onu bir şeye uygulamalıyız, bu yüzden fonksiyondan sonucu elde etmek için burada `(h w)` yaparız ve
sonra `f`'yi buna uygularız. `f`, bizim durumumuzda bir fonksiyon olan monadik bir değer döndürür, bu yüzden onu `w`'ya de uygularız.

Bu noktada `>>=` nasıl çalıştığını anlamazsanız endişelenmeyin, çünkü örneklerle bunun nasıl gerçekten basit bir monad olduğunu göreceğiz.
İşte bu monad'ı kullanan bir `do` ifadesi:

~~~~ {.haskell: .ghci name="code"}
import Control.Monad.Instances  
  
addStuff :: Int -> Int  
addStuff = do  
    a <- (*2)  
    b <- (+10)  
    return (a+b)  
~~~~

Bu, daha önce yazdığımız applicative ifadeyle aynı şeydir, ancak şimdi fonksiyonların monad olmasına dayanıyor.
Bir `do` ifadesi her zaman tek bir değerle sonuçlanır ve bu da farklı değildir. Bu monadik değerin sonucu bir fonksiyondur.
Burada olan şey, bir sayı alması ve ardından `(*2)` bu sayıya uygulanması ve sonucun `a` olması. `(+10)` uygulandığı sayıya `(*2)` uygulanır ve sonuç `b` olur.
`return`'ün, diğer monad'larda olduğu gibi, başka bir etkisi yoktur, ancak bir sonuç sunan monadik bir değer yaratmaktır. Bu, bu fonksiyonun sonucu olarak `a+b`'yi sunar.
Test edersek, öncekiyle aynı sonucu elde ederiz:

~~~~ {.haskell: .ghci name="code"}
ghci> addStuff 3  
19  
~~~~

Bu durumda hem `(*2)` hem de `(+10)` `3` sayısına uygulanır. `return (a+b)`'de yapar, ancak onu yok sayar ve sonuç olarak her zaman `a+b`'yi sunar.
Bu nedenle, monad fonksiyonu aynı zamanda okuyucu monad olarak da adlandırılır. Tüm fonksiyonlar ortak bir kaynaktan okunur.
Bunu daha iyi göstermek için `addStuff`'ı şu şekilde yeniden yazabiliriz:

~~~~ {.haskell: .ghci name="code"}
addStuff :: Int -> Int  
addStuff x = let  
    a = (*2) x  
    b = (+10) x  
    in a+b  
~~~~

Okuyucu monad'ının, fonksiyonları bir bağlamla birlikte değerler olarak ele almamıza izin verdiğini görüyoruz.
Sanki fonksiyonların ne döneceğini biliyormuşuz gibi hareket edebiliriz. Bunu, fonksiyonları tek bir fonksiyonda birbirine yapıştırarak ve
ardından bu fonksiyonun parametresini yapıştırıldığı tüm fonksiyonlara vererek yapar. Dolayısıyla, bir parametresi eksik olan çok sayıda fonksiyonumuz varsa ve
sonunda aynı şeye uygulanacaklarsa, gelecekteki sonuçlarını çıkarmak için okuyucu monad'ını kullanabiliriz ve `>>=` uygulaması her şeyin yolunda gittiğinden emin olacaktır.


Zevkli stateful hesaplamalar
----------------------------

![texas](../img/texas.png)
Haskell saf(pure) bir dildir ve bu nedenle programlarımız herhangi bir küresel durumu veya değişkeni değiştiremeyen fonksiyonlardan yapılmıştır,
yalnızca bazı hesaplamalar yapıp sonuçlara geri dönebilirler. Bu kısıtlama aslında programlarımız hakkında düşünmeyi kolaylaştırıyor 
çünkü bizi her değişkenin değerinin bir noktada ne olduğu konusunda endişelenmekten kurtarıyor. Bununla birlikte, bazı sorunlar zamanla değişen bazı durumlara
dayandıkları için doğaları gereği durumsaldır. Bu tür sorunlar Haskell için bir sorun olmasa da, bazen modellemek biraz sıkıcı olabilir.
Bu nedenle Haskell, her şeyi güzel ve saf tutarken, durumsal problemlerle uğraşmayı bir esinti haline getiren durum monad'ı denen bir şeye sahiptir.

[Rastgele sayılarla uğraşırken](../tr/09-input-and-output.md#rastgelelik), rastgele bir üreteci parametre olarak alan ve rastgele bir sayı ve yeni bir rastgele oluşturucu döndüren fonksiyonlarla uğraştık.
Birkaç rastgele sayı üretmek isteseydik, her zaman önceki bir fonksiyonun sonucuyla birlikte döndürdüğü rastgele oluşturucuyu kullanmak zorunda kaldık.
Bir `StdGen` alan ve bu üreticiye bağlı olarak üç kez jeton atan bir fonksiyon yaparken, bunu yapmak zorundaydık:

~~~~ {.haskell: .ghci name="code"}
threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)  
~~~~

Bir üretici `gen` aldı ve ardından `random gen`, yeni bir üreticiyle birlikte bir `Bool` değeri döndürdü. İkinci jetonu atmak için yeni üretici kullandık, vb.
Diğer dillerin çoğunda, rastgele bir sayıyla birlikte yeni bir üretici döndürmemiz gerekmez. Sadece mevcut olanı değiştirebiliriz!
Ancak Haskell saf olduğu için bunu yapamayız, bu yüzden biraz durum almalı, ondan ve yeni bir durumdan bir sonuç çıkarmalı ve
sonra yeni sonuçlar üretmek için bu yeni durumu kullanmalıydık.

stateful  hesaplamalarla bu şekilde manuel olarak uğraşmaktan kaçınmak için Haskell'in saflığından vazgeçmemiz gerektiğini düşünürsünüz.
Buna gerek yok, çünkü bizim için tüm bu durum işlerini yürüten ve Haskell programlamasını bu kadar havalı kılan saflıktan vazgeçmeyen özel bir küçük monad var.

Öyleyse, bu stateful hesaplama kavramını daha iyi anlamamıza yardımcı olmak için hadi devam edip onlara bir tür verelim.
Stateful hesaplamanın, bazı durumları alan ve bazı yeni durumlarla birlikte bir değer döndüren bir fonksiyon olduğunu söyleyeceğiz.
Bu fonksiyon aşağıdaki türe sahip olacaktır:

~~~~ {.haskell: .ghci name="code"}
s -> (a,s)  
~~~~

`s` durum türüdür ve `a` stateful hesaplamaların sonucudur.

Diğer birçok dilde atama, stateful bir hesaplama olarak düşünülebilir. Örneğin, zorunlu bir dilde `x = 5` yaptığımızda,
genellikle `x` değişkenine `5` değerini atar ve ayrıca ifade olarak `5` değerini alır. Buna fonksiyonel olarak bakarsanız,
ona bir durumu alan bir fonksiyon olarak bakabilirsiniz (yani, daha önce atanmış olan tüm değişkenler) ve
bir sonuç (bu durumda `5`) ve önceki tüm değişken eşlemeleri artı yeni atanan değişken olacak yeni bir durum döndürür.

Bir durumu alan ve bir sonuç ve yeni bir durum döndüren bir fonksiyon olan bu durumsal hesaplama, bağlamı olan bir değer olarak da düşünülebilir.
Gerçek değer sonuçtur, oysa bağlam, bu sonucu gerçekten elde etmek için bir başlangıç durumu sağlamamız gerektiğidir ve
bir sonuç elde etmenin yanı sıra yeni bir durum da elde ederiz.


Yığınlar ve taşlar
------------------

Bir yığın(stack) çalıştırmayı modellemek istediğimizi varsayalım. Üst üste bir yığınlarınız var ve bu yığının üzerine bir şeyler itebilir veya
yığının üstünden bir şeyler çıkarabilirsiniz. Yığının üstüne bir şeyler koyduğunuzda, onu yığına ittiğinizi söyleriz ve
üst kısımdan bir şeyler çıkarırken onu patlattığınızı(popping) söyleriz. Yığının en altında olan bir şey istiyorsanız, üstündeki her şeyi patlatmanız gerekir.

Yığınımızı temsil etmek için bir liste kullanacağız ve listenin başı yığının en üstünde olacak. İşimizde bize yardımcı olmak için iki fonksiyon yapacağız:
`pop` ve `push`. `pop` bir yığın alır, bir öğeyi açar ve sonuç olarak o öğeyi döndürür ve ayrıca o öğe olmadan yeni bir yığın döndürür.
`push`, bir öğeyi ve bir yığını alır ve ardından bu öğeyi yığının üzerine iter. Sonuç olarak, yeni bir yığınla birlikte `()` döndürür. İşte:

~~~~ {.haskell: .ghci name="code"}
type Stack = [Int]  
  
pop :: Stack -> (Int,Stack)  
pop (x:xs) = (x,xs)  
  
push :: Int -> Stack -> ((),Stack)  
push a xs = ((),a:xs)  
~~~~

Yığına iterken sonuç olarak `()` kullandık çünkü bir öğeyi yığına itmenin önemli bir sonuç değeri yoktur, asıl işi yığını değiştirmektir.
`push`'un ilk parametresini nasıl uyguladığımıza dikkat edin, stateful bir hesaplama elde ederiz. `pop`, türü nedeniyle zaten stateful bir hesaplamadır.

Bu fonksiyonları kullanarak bir yığını simüle etmek için küçük bir kod parçası yazalım. Sırf tekme için bir deste alacağız, ona `3` itip iki öğeyi patlatacağız. İşte burada:

~~~~ {.haskell: .ghci name="code"}
stackManip :: Stack -> (Int, Stack)  
stackManip stack = let  
    ((),newStack1) = push 3 stack  
    (a ,newStack2) = pop newStack1  
    in pop newStack2  
~~~~

Bir `stack` alırız ve sonra `push 3 stack` yaparız, bu da bir demet ile sonuçlanır. Demetin ilk bölümü bir `()` ve ikincisi yeni bir yığın ve biz buna `newStack1` diyoruz.
Ardından, `newStack1`'den bir sayı çıkarırız, bu da ittiğimiz bir `a` sayısı (`3`'tür) ve `newStack2` adını verdiğimiz yeni bir yığınla sonuçlanır.
Ardından, `newStack2`'den bir sayı çıkarırız ve `b` olan bir sayı ve bir `newStack3` elde ederiz. Bu sayı ve bu yığın ile bir demet döndürürüz. Hadi deneyelim:

~~~~ {.haskell: .ghci name="code"}
ghci> stackManip [5,8,2,1]  
(5,[8,2,1])  
~~~~

Harika, sonuç `5` ve yeni yığın `[8,2,1]`. `stackManip`'in kendi başına stateful bir hesaplama olduğuna dikkat edin.
Bir dizi stateful içeren hesaplama yaptık ve bunları bir şekilde birbirine yapıştırdık. Hmm, tanıdık geliyor.

Yukarıdaki `stackManip` kodu, durumu her stateful'a sahip hesaplamaya manuel olarak verdiğimiz ve
depoladığımız ve ardından bir sonrakine verdiğimiz için biraz sıkıcıdır. Yığını her bir fonksiyona manuel olarak vermek yerine, şöyle bir şey yazsak daha iyi olmaz mıydı:

~~~~ {.haskell: .ghci name="code"}
stackManip = do  
    push 3  
    a <- pop  
    pop  
~~~~

Durum monad'ını kullanmak tam olarak bunu yapmamızı sağlayacaktır. Bununla birlikte, bunun gibi stateful hesaplamaları alabileceğiz ve
durumu manuel olarak yönetmek zorunda kalmadan bunları kullanabileceğiz.


State monad'ı
-------------

`Control.Monad.State` modülü, stateful hesaplamaları saran bir `newtype` sağlar. İşte tanımı:

~~~~ {.haskell: .ghci name="code"}
newtype State s a = State { runState :: s -> (a,s) }  
~~~~

Bir `State s a`, `s` türünün bir durumunu işleyen ve `a` türünden bir sonuca sahip olan stateful hesaplamadır.

Stateful hesaplamaların ne hakkında olduğunu ve bağlamlarla değerler olarak nasıl düşünülebileceklerini artık gördüğümüze göre, `Monad` instance'larına bir göz atalım:

~~~~ {.haskell: .ghci name="code"}
instance Monad (State s) where  
    return x = State $ \s -> (x,s)  
    (State h) >>= f = State $ \s -> let (a, newState) = h s  
                                        (State g) = f a  
                                    in  g newState  
~~~~

![badge](../img/badge.png)
Önce `return`'e bir göz atalım. `return` ile amacımız, bir değer almak ve sonucu olarak her zaman bu değere sahip olan durumsal bir hesaplama yapmaktır.
Bu yüzden sadece bir lambda `\s -> (x,s)` yapıyoruz. `x`'i her zaman stateful hesaplamanın sonucu olarak sunarız ve durum değişmeden tutulur,
çünkü `return` minimum bağlamda bir değer koymalıdır. Dolayısıyla `return`, sonuç olarak belirli bir değer sunan ve durumu değiştirmeden tutan durumsal bir hesaplama yapacaktır.

Peki ya `>>=`? stateful içeren bir hesaplamayı bir fonksiyona `>>=` ile beslemenin sonucu stateful içeren bir hesaplama olmalıdır, değil mi?
Yani `State` `newtype` sarmalayıcısı ile başlıyoruz ve sonra bir lambda yazıyoruz. Bu lambda bizim yeni stateful hesaplamamız olacak. Ama içinde neler oluyor?
Bir şekilde sonuç değerini ilk stateful hesaplamadan çıkarmamız gerekiyor. Şu anda stateful bir hesaplamada olduğumuz için,
mevcut durumumuza durumsal hesaplama verebiliriz, bu da bir çift sonuç ve yeni bir durumla sonuçlanır: `(a, newState)`.
Şimdiye kadar her `>>=`'ı uygularken, sonucu monadik değerden çıkardıktan sonra, yeni monadik değeri elde etmek için `f` fonksiyonunu ona uyguladık.
`Writer` da, bunu yaptıktan ve yeni monadik değeri elde ettikten sonra, eski monoid değeri yenisiyle `mappend` ederek bağlamın halledildiğinden emin olmamız gerekiyordu.
Burada `f a` yapıyoruz ve yeni bir durumsal hesaplama `g` elde ediyoruz. Artık yeni bir durumsal hesaplamaya ve yeni bir duruma (`newState` adıyla gider) sahip olduğumuza göre,
bu durumlu hesaplama `g`'yi `newState`'e uygularız. Sonuç, nihai sonucun ve nihai durumun bir demetidir!

Yani `>>=` ile, iki stateful hesaplamayı birbirine yapıştırıyoruz, sadece ikincisi bir öncekinin sonucunu alan bir fonksiyonun içinde gizli.
`pop` ve `push` halihazırda stateful hesaplamalar olduğundan, bunları bir `State` sarmalayıcıya sarmak kolaydır. İzleyin:

~~~~ {.haskell: .ghci name="code"}
import Control.Monad.State  
  
pop :: State Stack Int  
pop = State $ \(x:xs) -> (x,xs)  
  
push :: Int -> State Stack ()  
push a = State $ \xs -> ((),a:xs)  
~~~~

`pop` zaten stateful hesaplamadır ve `push` bir `Int` alır ve stateful hesaplama döndürür.
Şimdi, önceki örneğimiz olan `3`'ü yığına itip ardından iki sayıyı şu şekilde çıkararak yeniden yazabiliriz:

~~~~ {.haskell: .ghci name="code"}
import Control.Monad.State  
  
stackManip :: State Stack Int  
stackManip = do  
    push 3  
    a <- pop  
    pop  
~~~~

Bir push ve iki pop'u tek bir stateful olan bir hesaplamaya nasıl yapıştırdığımızı gördünüz mü? Onu `newtype` sarmalayıcısından açtığımızda,
bazı başlangıç durumlarını sağlayabileceğimiz bir fonksiyon elde ederiz: 

~~~~ {.haskell: .ghci name="code"}
ghci> runState stackManip [5,8,2,1]  
(5,[8,2,1])  
~~~~

İkinci `pop`'u `a`'ya bağlamak zorunda değildik çünkü o `a`'yı hiç kullanmadık. Yani bunu şöyle yazabilirdik:

~~~~ {.haskell: .ghci name="code"}
stackManip :: State Stack Int  
stackManip = do  
    push 3  
    pop  
    pop  
~~~~

Oldukça havalı. Ama ya bunu yapmak istersek: Bir sayıyı yığından çıkar ve sonra bu sayı `5` ise onu yığına geri koyar ve dururuz ama eğer `5` değilse,
`3` ve `8`'i tekrar basar mıyız? İşte kod:

~~~~ {.haskell: .ghci name="code"}
stackStuff :: State Stack ()  
stackStuff = do  
    a <- pop  
    if a == 5  
        then push 5  
        else do  
            push 3  
            push 8  
~~~~

Bu oldukça basittir. İlk yığınla çalıştıralım.

~~~~ {.haskell: .ghci name="code"}
ghci> runState stackStuff [9,0,2,1,0]  
((),[8,3,0,2,1,0]) 
~~~~

Unutmayın, `do` ifadeleri monadik değerlerle sonuçlanır ve `State` monad'ında tek bir `do` ifadesi de stateful bir fonksiyondur.
`stackManip` ve `stackStuff` sıradan stateful hesaplamalar olduğundan, daha fazla stateful hesaplamalar üretmek için bunları birbirine yapıştırabiliriz.

~~~~ {.haskell: .ghci name="code"}
moreStack :: State Stack ()  
moreStack = do  
    a <- stackManip  
    if a == 100  
        then stackStuff  
        else return ()  
~~~~

Mevcut yığındaki `stackManip`'in sonucu `100` ise, `stackStuff`'ı çalıştırırız, aksi takdirde hiçbir şey yapmayız. `return ()`, durumu olduğu gibi korur ve hiçbir şey yapmaz.

`Control.Monad.State` modülü, `MonadState` adlı bir tür sınıfı sağlar ve oldukça kullanışlı iki fonksiyon içerir, yani `get` ve `put`.
`State` için `get` fonksiyonu şu şekilde uygulanır:

~~~~ {.haskell: .ghci name="code"}
get = State $ \s -> (s,s)  
~~~~

Yani sadece mevcut durumu alır ve sonuç olarak sunar. `put` fonksiyonu bazı durumları alır ve mevcut durumu onunla değiştiren stateful bir fonksiyon yapar:

~~~~ {.haskell: .ghci name="code"}
put newState = State $ \s -> ((),newState)  
~~~~

Yani bunlarla, mevcut yığının ne olduğunu görebiliriz veya onu tamamen başka bir yığınla değiştirebiliriz. Şöyle:

~~~~ {.haskell: .ghci name="code"}
stackyStack :: State Stack ()  
stackyStack = do  
    stackNow <- get  
    if stackNow == [1,2,3]  
        then put [8,3,1]  
        else put [9,2,1]  
~~~~

Yalnızca `State` değerleri için çalışsaydı `>>=` türünün ne olacağını incelemeye değer:

~~~~ {.haskell: .ghci name="code"}
(>>=) :: State s a -> (a -> State s b) -> State s b  
~~~~

Durum yani `s` türünün nasıl aynı kaldığını, ancak sonucun türünün `a`'dan `b`'ye değişebileceğini görün? Bu, sonuçları farklı türlerde olan ancak
durum türünün aynı kalması gereken birkaç stateful hesaplamayı birbirine yapıştırabileceğimiz anlamına gelir. Şimdi neden bu?
Örneğin, `Maybe` için `>>=` şu türe sahiptir:

~~~~ {.haskell: .ghci name="code"}
(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b  
~~~~

`Maybe` monad'ın kendisinin değişmemesi mantıklı geliyor. İki farklı monad arasında `>>=` kullanmak mantıklı olmaz.
Durum monad'ı için, monad aslında `State s`'dir, yani eğer s farklı olsaydı, iki farklı monad arasında `>>=` kullanırdık.


Rastgelelik ve state monad'ı
----------------------------

Bu bölümün başında, sayı üretmenin bazen ne kadar garip olabileceğini gördük çünkü her rastgele fonksiyon bir üretici alır ve
yeni bir üretici ile birlikte rastgele bir sayı döndürür, eğer başka bir rastgele sayı üretmek istiyorsak eskisinin yerine kullanılmalıdır.
Durum monad'ı bununla başa çıkmayı çok daha kolay hale getiriyor.

`System.Random`'daki `random` fonksiyonu aşağıdaki türe sahiptir:

~~~~ {.haskell: .ghci name="code"}
random :: (RandomGen g, Random a) => g -> (a, g)  
~~~~

Yani rastgele bir üreteci alır ve yeni bir oluşturucu ile birlikte rastgele bir sayı üretir. Bunun stateful hesaplama olduğunu görebiliriz,
bu yüzden onu `State` `newtype` constructor'una sarabiliriz ve sonra onu monadik bir değer olarak kullanabiliriz, böylece durum geçişi bizim için ele alınır:

~~~~ {.haskell: .ghci name="code"}
import System.Random  
import Control.Monad.State  
  
randomSt :: (RandomGen g, Random a) => State g a  
randomSt = State random  
~~~~

Öyleyse şimdi üç jeton atmak istiyorsak (`True` yazıdır, `False` turadır) sadece aşağıdakileri yaparız:

~~~~ {.haskell: .ghci name="code"}
import System.Random  
import Control.Monad.State  
  
threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
    a <- randomSt  
    b <- randomSt  
    c <- randomSt  
    return (a,b,c)  
~~~~

`threeCoins` artık stateful bir hesaplamadır ve ilk rastgele üreticiyi aldıktan sonra, onu ilk `randomSt`'ye geçirir,
bu da bir sayı ve yeni bir üretici oluşturur, bu da bir sonrakine geçer ve bu böyle devam eder. En son üreticiyi değiştirmeden sonuç olarak `(a, b, c)` sunmak için
`return (a, b, c)`'yi kullanırız. Şuna bir bakalım:

~~~~ {.haskell: .ghci name="code"}
ghci> runState threeCoins (mkStdGen 33)  
((True,False,True),680029187 2103410263)  
~~~~

Güzel. Adımlar arasında bir miktar durumun korunmasını gerektiren bu tür şeyleri yapmak, çok daha az güçlük haline geldi!


Error error on the wall
-----------------------

Şimdiye kadar `Maybe`'nin değerlere olası başarısızlık bağlamını eklemek için kullanıldığını biliyoruz. Değer, `Just` bir şey veya `Nothing` olabilir.
Ne kadar yararlı olursa olsun, bir `Nothing`'e sahip olduğumuzda, tek bildiğimiz bir tür başarısızlık olduğudur,
ancak orada bize bunun ne tür bir başarısızlık olduğunu veya neden başarısız olduğunu söylemenin bir yolu yoktur.

Öte yandan, `Either e a` türü, değerlerimize olası bir başarısızlık bağlamını dahil etmemize ve aynı zamanda başarısızlığa değerler ekleyebilmemize olanak tanır,
böylece neyin yanlış gittiğini açıklayabilir veya başarısızlıkla ilgili başka yararlı bilgiler sağlayabilir. Bir `Either e a` değeri,
doğru yanıtı ve bir başarıyı belirten bir `Right` değer olabilir veya başarısızlığı belirten bir `Left` değer olabilir. Örneğin:

~~~~ {.haskell: .ghci name="code"}
ghci> :t Right 4  
Right 4 :: (Num t) => Either a t  
ghci> :t Left "out of cheese error"  
Left "out of cheese error" :: Either [Char] b  
~~~~

Bu hemen hemen sadece geliştirilmiş bir `Maybe`'dir, bu yüzden bir monad olması mantıklıdır, çünkü olası bir başarısızlık bağlamı eklenmiş bir değer olarak da görülebilir,
ancak artık bir hata olduğunda da eklenen bir değer vardır.

`Monad` instance'ı `Maybe` ile benzerdir ve `Control.Monad.Error`'da bulunabilir:

~~~~ {.haskell: .ghci name="code"}
instance (Error e) => Monad (Either e) where  
    return x = Right x   
    Right x >>= f = f x  
    Left err >>= f = Left err  
    fail msg = Left (strMsg msg)  
~~~~

`return`, her zaman olduğu gibi, bir değer alır ve bunu varsayılan bir minimum bağlama koyar. Değerimizi `Right` constructor'yla sarmalıyor çünkü
bir sonucun mevcut olduğu başarılı bir hesaplamayı temsil etmek için `Right`'ı kullanıyoruz. Bu, `Maybe` için `return`'e çok benziyor.

`>>=` iki olası durumu inceler: `Left` ve `Right`. `Right` durumunda, `f` fonksiyonu, `Just` durumunda olduğu gibi, fonksiyonun içeriğine uygulandığına benzer şekilde,
içindeki değere uygulanır. Hata durumunda, başarısızlığı tanımlayan içeriği ile birlikte `Left` değer tutulur.

`Either e` için `Monad` instance'ı ek bir gereksinim oluşturur ve bu, `e` türü parametresi tarafından indekslenen `Left` de bulunan değerin türünün,
`Error` tür sınıfının bir instance'ı olması gerektiğidir. `Error` tür sınıfı, değerleri hata mesajları gibi davranabilen türler içindir.
`Error` instance'ına iyi bir örnek, `String` türüdür! `String` durumunda, `strMsg` fonksiyonu yalnızca sahip olduğu string'i döndürür:

~~~~ {.haskell: .ghci name="code"}
ghci> :t strMsg  
strMsg :: (Error a) => String -> a  
ghci> strMsg "boom!" :: String  
"boom!"  
~~~~

Ancak `Either` kullanırken hatayı tanımlamak için genellikle `String` kullandığımız için, bu konuda çok fazla endişelenmemize gerek yok.
Bir desen eşleştirme `do` notasyonunda başarısız olduğunda, bu başarısızlığı belirtmek için bir `Left` değeri kullanılır.

Her neyse, işte birkaç kullanım örneği:

~~~~ {.haskell: .ghci name="code"}
ghci> Left "boom" >>= \x -> return (x+1)  
Left "boom"  
ghci> Right 100 >>= \x -> Left "no way!"  
Left "no way!"  
~~~~

Bir fonksiyona `Left` değerini beslemek için `>>=` kullandığımızda, fonksiyona göz ardı edilir ve aynı `Left` değeri döndürülür. Bir fonksiyona `Right` değeri verdiğimizde,
fonksiyon içerdekine uygulanır, ancak bu durumda bu fonksiyon yine de bir `Left` değer üretir!

Yine başarılı olan bir fonksiyona `Right` bir değer beslemeye çalıştığımızda, tuhaf bir tür hatasıyla tetikleniriz! Hmmm.

~~~~ {.haskell: .ghci name="code"}
ghci> Right 3 >>= \x -> return (x + 100)  
  
<interactive>:1:0:  
    Ambiguous type variable `a' in the constraints:  
      `Error a' arising from a use of `it' at <interactive>:1:0-33  
      `Show a' arising from a use of `print' at <interactive>:1:0-33  
    Probable fix: add a type signature that fixes these type variable(s)  
~~~~

Haskell, sadece `Right` kısmını yazdırıyor olsak da, `Either e a` yazılan değerimizin `e` bölümü için hangi türü seçeceğini bilmediğini söylüyor.
Bunun nedeni, `Monad` instance'ının `Error e` kısıtlamasıdır. Dolayısıyla, monad olarak `Either` kullanırken bunun gibi tür hataları alırsanız,
açık bir tür imzası eklemeniz yeterlidir:

~~~~ {.haskell: .ghci name="code"}
ghci> Right 3 >>= \x -> return (x + 100) :: Either String Int  
Right 103  
~~~~

Pekala, şimdi çalışıyor!

Bu küçük takılma dışında, bu monad'ı kullanmak `Maybe`'yi monad olarak kullanmaya çok benzer. Önceki bölümde, bir ip cambazının denge direğine inen kuşları simüle etmek için
`Maybe`'nin monadik yönlerini kullandık. Bir egzersiz olarak, bunu hata monad'ı ile yeniden yazabilirsiniz, böylece ip cambazı kayıp düştüğünde,
düştüğünde direğin her iki yanında kaç tane kuş olduğunu hatırlarız.


Bazı kullanışlı monadik fonksiyonlar
------------------------------------

Bu bölümde, monadik değerler üzerinde çalışan veya sonuçları olarak monadik değerleri (veya her ikisini de!) döndüren birkaç fonksiyonu inceleyeceğiz.
Bu tür fonksiyonlar genellikle monadik fonksiyonlar olarak adlandırılır. Bazıları yepyeni olurken, diğerleri `filter` ve `foldl` gibi zaten bildiğimiz fonksiyonların
monadik karşılıkları olacak. Bakalım ne olduklarını görelim!

liftM ve arkadaşlar
-------------------

![wolf](../img/wolf.png)
Monad Dağı'nın zirvesine yolculuğumuza başladığımızda, ilk önce haritası çıkarılabilecek şeyler için functor'lara baktık.
Daha sonra, applicative functor'lar adı verilen gelişmiş functor'ları öğrendik, bu da bizim birçok applicative değer arasında normal fonksiyonları uygulamamıza ve
normal bir değer alıp bunu bazı varsayılan bağlamlara koymamıza izin verdi. Son olarak, bu değerlere bağlamla birlikte bir şekilde normal fonksiyonlara beslenebilme
yeteneği ekleyen gelişmiş applicative functor'ları olarak monad'ları tanıttık.

Dolayısıyla, her monad bir applicative functor'dur ve her applicative functor bir functor'dur.
`Applicative` tür sınıfı, bir Applicative instance'ı yapabilmemiz için türümüzün bir `Functor` instance'ı olması gerektiği gibi bir sınıf kısıtlamasına sahiptir.
Ancak `Monad`, `Applicative` için aynı kısıtlamaya sahip olması gerekse de, her monad bir `Applicative` functor'u olduğundan, öyle değildir,
çünkü `Monad` tür sınıfı, `Applicative`'den önce Haskell'e tanıtılmıştır.

Ancak her monad bir functor olsa da, `liftM` fonksiyonu nedeniyle bir `Functor` instance'ına sahip olduğuna güvenmemize gerek yok.
`liftM`, bir fonksiyonu ve monadik bir değeri alır ve bunu monadik değerin üzerine eşler. Yani `fmap` ile hemen hemen aynı şey! Bu `liftM`'nin türü:

~~~~ {.haskell: .ghci name="code"}
liftM :: (Monad m) => (a -> b) -> m a -> m b  
~~~~

Ve bu `fmap` türüdür:

~~~~ {.haskell: .ghci name="code"}
fmap :: (Functor f) => (a -> b) -> f a -> f b  
~~~~

Bir tür için `Functor` ve `Monad` instance'ları, functor ve monad yasalarına uyuyorsa, bu ikisi aynı anlama gelir (ve şimdiye kadar tanıştığımız tüm monad'lar ikisine de uyar).
Bu, `pure` ve `return`'ün aynı şeyi yapması gibi bir şeydir, sadece birinin `Applicative` sınıf kısıtlaması varken diğerinin bir `Monad` kısıtlaması vardır.
`liftM`'i deneyelim:

~~~~ {.haskell: .ghci name="code"}
ghci> liftM (*3) (Just 8)  
Just 24  
ghci> fmap (*3) (Just 8)  
Just 24  
ghci> runWriter $ liftM not $ Writer (True, "chickpeas")  
(False,"chickpeas")  
ghci> runWriter $ fmap not $ Writer (True, "chickpeas")  
(False,"chickpeas")  
ghci> runState (liftM (+100) pop) [1,2,3,4]  
(101,[2,3,4])  
ghci> runState (fmap (+100) pop) [1,2,3,4]  
(101,[2,3,4])  
~~~~

`fmap`'in `Maybe` değerleriyle nasıl çalıştığını zaten çok iyi biliyoruz. `liftM` de aynı şeyi yapıyor. `Writer` değerleri için, fonksiyon başlığın ilk bileşeni üzerine eşlenir, bu da sonuçtur. Stateful bir hesaplama üzerinden `fmap` veya `liftM` yapmak, başka bir stateful hesaplama ile sonuçlanır,
yalnızca nihai sonucu sağlanan fonksiyon tarafından değiştirilir. Çalıştırmadan önce bu durumda `pop` üzerine `(+100)` eşlememiş olsaydık, `(1, [2,3,4])` döndürürdü.

`liftM` şu şekilde uygulanır:

~~~~ {.haskell: .ghci name="code"}
liftM :: (Monad m) => (a -> b) -> m a -> m b  
liftM f m = m >>= (\x -> return (f x))  
~~~~

Veya `do` notasyonu ile:

~~~~ {.haskell: .ghci name="code"}
liftM :: (Monad m) => (a -> b) -> m a -> m b  
liftM f m = do  
    x <- m  
    return (f x)  
~~~~

Monadik değeri `m`'yi fonksiyona besleriz ve ardından `f` fonksiyonunu varsayılan bağlama geri koymadan önce sonucuna uygularız.
Monad yasaları nedeniyle, bunun bağlamı değiştirmemesi garanti edilir, yalnızca monadik değerin sunduğu sonucu değiştirir.
`liftM`'nin `Functor` tür sınıfına hiç referans gösterilmeden uygulandığını görüyoruz. Bu, sadece monad'ların bize sunduğu güzellikleri kullanarak
`fmap`'i (veya `liftM`'i, ne demek isterseniz) uygulayabileceğimiz anlamına gelir. Bu nedenle, monad'ların normal eski fonksiyonlardan daha güçlü olduğu sonucuna varabiliriz.

`Applicative` tür sınıfı, fonksiyonları bağlamları olan değerler arasında normal değerlermiş gibi uygulamamıza izin verir. Böyle:

~~~~ {.haskell: .ghci name="code"}
ghci> (+) <$> Just 3 <*> Just 5  
Just 8  
ghci> (+) <$> Just 3 <*> Nothing  
Nothing  
~~~~

Bu applicative stili kullanmak işleri oldukça kolaylaştırır. `<$>` yalnızca `fmap`'tir ve `<*>`, aşağıdaki türe sahip olan `Applicative` tür sınıfından bir fonksiyondur:

~~~~ {.haskell: .ghci name="code"}
(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b  
~~~~

Yani bu bir tür `fmap` gibi, yalnızca fonksiyonun kendisi bir bağlamda. Bir şekilde onu bağlamdan çıkarmalı ve `f a` değeri üzerinde eşleştirmeli ve
ardından bağlamı tekrar bir araya getirmeliyiz. Haskell'de tüm fonksiyonlar varsayılan olarak curried olduğundan,
applicative değerler arasında birkaç parametre alan fonksiyonlar uygulamak için `<$>` ve `<*>` kombinasyonunu kullanabiliriz.

Her neyse, `fmap` gibi `<*>` da sadece `Monad` tür sınıfının bize verdiğini kullanarak gerçekleştirilebilir. `ap` fonksiyonu temelde `<*>`'dır,
yalnızca `Applicative` yerine `Monad` kısıtlaması vardır. İşte tanımı:

~~~~ {.haskell: .ghci name="code"}
ap :: (Monad m) => m (a -> b) -> m a -> m b  
ap mf m = do  
    f <- mf  
    x <- m  
    return (f x)  
~~~~

`mf`, sonucu bir fonksiyon olan monadik bir değerdir. Fonksiyon değerin yanı sıra bağlamda olduğu için, fonksiyonu bağlamdan alıp `f` olarak adlandırırız,
sonra değeri alır ve bu `x`'i çağırırız ve sonunda fonksiyonu değere uygular ve sonuç olarak sunarız. İşte hızlı bir gösterimi:

~~~~ {.haskell: .ghci name="code"}
ghci> Just (+3) <*> Just 4  
Just 7  
ghci> Just (+3) `ap` Just 4  
Just 7  
ghci> [(+1),(+2),(+3)] <*> [10,11]  
[11,12,12,13,13,14]  
ghci> [(+1),(+2),(+3)] `ap` [10,11]  
[11,12,12,13,13,14]  
~~~~

Şimdi monad'ların da applicative'lerden daha güçlü olduğunu görüyoruz, çünkü `Applicative` için olanları uygulamak için `Monad`'ın fonksiyonlarını kullanabiliriz.
Aslında, bir türün monad olduğu çoğu kez, insanlar önce bir `Monad` instance'ı yazarlar ve ardından `pure`'un, `return` ve `<*>`'nin `ap` olduğunu söyleyerek
bir `Applicative` instance'ı oluştururlar. Benzer şekilde, bir şey için zaten bir `Monad` instance'ınız varsa, `fmap`'in `liftM` olduğunu söyleyerek ona bir
`Functor` instance'ı verebilirsiniz.

`liftA2` fonksiyonu, iki applicative değer arasında bir fonksiyon uygulamak için uygun bir fonksiyondur. Basitçe şöyle tanımlanır:

~~~~ {.haskell: .ghci name="code"}
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f x y = f <$> x <*> y  
~~~~

`liftM2` fonksiyonu aynı şeyi yapar, yalnızca bir `Monad` kısıtlaması vardır. Ayrıca `liftM3` ve `liftM4` ve `liftM5` de mevcuttur.

Monad'ların applicative'lerden ve functor'lardan daha güçlü olduğunu ve tüm monad'ların nasıl birer functor'lar ve applicative functor olmasına rağmen,
mutlaka `Functor` ve `Applicative` instance'larına sahip olmadıklarını gördük, bu nedenle functor'ların ve applicative functor'ların kullandığı
fonksiyonların monadik eşdeğerlerini inceledik.


join fonksiyonu
---------------

İşte biraz düşünmek için yiyecek: eğer bir monadik değerin sonucu başka bir monadik değerse, yani bir monadik değer diğerinin içine yerleştirilmişse,
bunları tek bir normal monadik değere düzleştirebilir misiniz? Mesela, `Just (Just 9)` varsa, bunu `Just 9` yapabilir miyiz?
İç içe geçmiş herhangi bir monadik değerin düzleştirilebileceği ve bunun aslında monadlara özgü bir özellik olduğu ortaya çıktı. 
Bunun için `join` fonksiyonu mevcuttur. Türü şudur:

~~~~ {.haskell: .ghci name="code"}
join :: (Monad m) => m (m a) -> m a  
~~~~

Dolayısıyla, monadik bir değer içindeki monadik bir değeri alır ve bize sadece monadik bir değer verir, bu yüzden onu bir nevi düzleştirir.
İşte bazı `Maybe` değerleriyle:

~~~~ {.haskell: .ghci name="code"}
ghci> join (Just (Just 9))  
Just 9  
ghci> join (Just Nothing)  
Nothing  
ghci> join Nothing  
Nothing  
~~~~

İlk satır, başarılı bir hesaplamanın sonucu olarak başarılı bir hesaplamaya sahiptir, bu nedenle ikisi de tek bir büyük ve başarılı hesaplamaya dahil edilmiştir.
İkinci satırda, `Just` değerinin bir sonucu olarak `Nothing` özelliği bulunur. Daha önce `Maybe` değerleriyle uğraştığımızda ve
bunların birkaçını `<*>` veya `>>=` ile birleştirmek istediğimizde, sonucun `Just` değer olması için hepsinin `Just` değerleri olması gerekiyordu.
Yol boyunca herhangi bir başarısızlık olursa, sonuç bir başarısızlıktır ve aynı şey burada da olur.
Üçüncü satırda, başlangıçtan itibaren bir başarısız olan bir değeri düzleştirmeye çalışırız, böylece sonuç da bir başarısızlık olur.

Listeleri düzleştirmek oldukça sezgiseldir:

~~~~ {.haskell: .ghci name="code"}
ghci> join [[1,2,3],[4,5,6]]  
[1,2,3,4,5,6]  
~~~~

Gördüğünüz gibi, listeler için `join` sadece `concat`'dir. Sonucu bir `Writer` değeri olan bir `Writer` değerini düzleştirmek için, monoid değeri `mappend` etmeliyiz.

~~~~ {.haskell: .ghci name="code"}
ghci> runWriter $ join (Writer (Writer (1,"aaa"),"bbb"))  
(1,"bbbaaa")  
~~~~

Önce dış monoid değeri olan `"bbb"` gelir ve sonra ona `"aaa"` eklenir. Sezgisel olarak konuşursak, bir `Writer` değerinin sonucunun ne olduğunu incelemek istediğinizde,
önce onun monoid değerini günlüğe yazmanız gerekir ve ancak o zaman içinde ne olduğunu inceleyebilirsiniz.

`Either` değerlerinin düzleştirilmesi `Maybe` değerlerinin düzleştirilmesine çok benzer:

~~~~ {.haskell: .ghci name="code"}
ghci> join (Right (Right 9)) :: Either String Int  
Right 9  
ghci> join (Right (Left "error")) :: Either String Int  
Left "error"  
ghci> join (Left "error") :: Either String Int  
Left "error"  
~~~~

Eğer sonucu stateful bir hesaplama olan stateful hesaplamaya `join` uygularsak, sonuç, önce dış stateful hesaplamayı çalıştıran ve
sonra ortaya çıkan stateful hesaplamadır. İzleyin:

~~~~ {.haskell: .ghci name="code"}
ghci> runState (join (State $ \s -> (push 10,1:2:s))) [0,0,0]  
((),[10,1,2,0,0,0])  
~~~~

Burada lambda bir durumu alır ve yığına `2` ve `1`'i koyar ve sonuç olarak `push 10` gösterir. Yani tüm bu şey `join` ile düzleştirilip sonra çalıştırıldığında,
önce `2` ve `1`'i yığına koyar ve sonra `push 10`, üste `10`'u iter.

`join` uygulaması aşağıdaki gibidir:

~~~~ {.haskell: .ghci name="code"}
join :: (Monad m) => m (m a) -> m a  
join mm = do  
    m <- mm  
    m  
~~~~

`mm`'nin sonucu monadik bir değer olduğu için, o sonucu alırız ve sonra onu kendi satırına koyarız çünkü bu monadik bir değerdir.
Buradaki hile, `m <- mm` yaptığımızda, içinde bulunduğumuz monad bağlamının halledilmesidir. Bu nedenle, örneğin, `Maybe` değerleri,
yalnızca dış ve iç değerlerin her ikisi de `Just` değerlerse `Just` değerlerle sonuçlanır. `mm` değeri önceden `Just (Just 8)` olarak ayarlanmış olsaydı, şu şekilde görünürdü:

~~~~ {.haskell: .ghci name="code"}
joinedMaybes :: Maybe Int  
joinedMaybes = do  
    m <- Just (Just 8)  
    m  
~~~~

![tipi](../img/tipi.png)
`join` ile ilgili belki de en ilginç şey, her monad için, monadik bir değeri bir fonksiyona `>>=` ile beslemek, sadece bu fonksiyonu değerin üzerine eşlemek ve
sonra ortaya çıkan iç içe geçmiş monadik değeri düzleştirmek için join kullanmakla aynı şeydir! Diğer bir deyişle, `m >>= f` her zaman `join (fmap f m)` ile aynı şeydir!
Düşündüğünüz zaman mantıklı. `>>=` ile, her zaman monadik bir değeri normal bir değer alan ancak monadik bir değer döndüren bir fonksiyonu nasıl besleyeceğimizi düşünüyoruz.
Bu fonksiyonu sadece monadik değer üzerinden eşlersek, monadik bir değer içinde monadik bir değerimiz olur.
Örneğin, `Just 9 ve \x -> Just (x+1)` fonksiyonumuz olduğunu varsayalım. Bu fonksiyonu `Just 9` üzerinden eşlersek, `Just (Just 10)` ile kalırız.

`m >> = f`'in her zaman `join (fmap f m)`'e eşit olması gerçeği, eğer bir tür için kendi `Monad` instance'ımızı oluşturuyorsak çok yararlıdır
çünkü iç içe geçmiş bir monadik değeri nasıl düzleştireceğimizi anlamak, `>>=` nasıl uygulanacağını bulmaktan genellikle daha kolaydır.


filterM
-------

`filter` fonksiyonu, Haskell programlamasının ekmeğidir (`map` tereyağıdır). Filtrelemek için bir predicate ve bir liste alır ve
ardından yalnızca predicate'i karşılayan öğelerin tutulduğu yeni bir liste döndürür. Türü şudur:

~~~~ {.haskell: .ghci name="code"}
filter :: (a -> Bool) -> [a] -> [a]  
~~~~

Predicate, listenin bir öğesini alır ve bir `Bool` değeri döndürür. Şimdi, ya döndürdüğü `Bool` değeri aslında monadik bir değerse? Whoa! Yani, ya bir bağlamla gelirse?
Bu işe yarayabilir mi? Örneğin, predicate'in ürettiği her `True` veya `False` değerine eşlik eden bir monoid değer varsa,
örneğin `["Accepted the number 5"]` veya `["3 is too small"]` gibi? İşe yarayacak gibi görünüyor. 
Durum böyle olsaydı, ortaya çıkan listenin yol boyunca üretilen tüm günlük değerlerinin bir günlüğü ile gelmesini beklerdik.
Dolayısıyla, predicate'in döndürdüğü `Bool` bir bağlamla gelirse, nihai sonuç listesinin de bazı bağlamlara eklenmesini bekleriz,
aksi takdirde her `Bool`'un birlikte geldiği bağlam kaybolur.

`Control.Monad`'ın `filterM` fonksiyonu tam istediğimizi yapıyor! Türü şudur:

~~~~ {.haskell: .ghci name="code"}
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]  
~~~~

Predicate, sonucu `Bool` olan monadik bir değer döndürür, ancak monadik bir değer olduğu için bağlamı, olası bir başarısızlıktan belirsizlik ve daha fazlasına kadar
her şey olabilir! Bağlamın nihai sonuca yansıtılmasını sağlamak için, sonuç aynı zamanda monadik bir değerdir.

Bir liste alalım ve sadece 4'ten küçük olan değerleri tutalım. Başlamak için normal `filter` fonksiyonunu kullanacağız:

~~~~ {.haskell: .ghci name="code"}
ghci> filter (\x -> x < 4) [9,1,5,2,10,3]  
[1,2,3]  
~~~~

Bu oldukça kolay. Şimdi, `True` veya `False` sonucu göstermenin yanı sıra, ne yaptığına dair bir günlük de sağlayan bir predicate yapalım.
Elbette bunun için `Writer` monadını kullanacağız:

~~~~ {.haskell: .ghci name="code"}
keepSmall :: Int -> Writer [String] Bool  
keepSmall x  
    | x < 4 = do  
        tell ["Keeping " ++ show x]  
        return True  
    | otherwise = do  
        tell [show x ++ " is too large, throwing it away"]  
        return False  
~~~~

Sadece `Bool` döndürmek yerine, bu fonksiyon bir `Writer [String] Bool` döndürür. Bu monadik bir predicate'tir. Kulağa hoş geliyor, değil mi?
Sayı `4`'ten küçükse, onu koruduğumuzu ve ardından `return True` dediğimizi bildiririz.

Şimdi onu bir liste ile birlikte `filterM`'ye verelim. Koşul bir `Writer` değeri döndürdüğünden, ortaya çıkan liste aynı zamanda bir `Writer` değeri olacaktır.

~~~~ {.haskell: .ghci name="code"}
ghci> fst $ runWriter $ filterM keepSmall [9,1,5,2,10,3]  
[1,2,3]  
~~~~

Ortaya çıkan `Writer` değerinin sonucunu incelediğimizde her şeyin yolunda olduğunu görüyoruz. Şimdi, günlüğü yazdıralım ve neye sahip olduğumuzu görelim:

~~~~ {.haskell: .ghci name="code"}
ghci> mapM_ putStrLn $ snd $ runWriter $ filterM keepSmall [9,1,5,2,10,3]  
9 is too large, throwing it away  
Keeping 1  
5 is too large, throwing it away  
Keeping 2  
10 is too large, throwing it away  
Keeping 3  
~~~~

Harika. Bu yüzden, `filterM`'ye sadece monadik bir predicate sağlayarak, kullandığımız monadik bağlamdan yararlanırken bir listeyi filtreleyebildik.

Çok güzel bir Haskell numarası, bir listenin kuvvet kümesini elde etmek için `filterM`'yi kullanmaktır (bunları şimdilik kümeler olarak düşünürsek).
Bazı kümelerin kuvvet kümesi, o kümenin tüm alt kümelerinin bir kümesidir. Yani `[1,2,3]` gibi bir kümemiz varsa, onun kuvvet kümesi aşağıdaki kümeleri içerecektir:

~~~~ {.haskell: .ghci name="code"}
[1,2,3]  
[1,2]  
[1,3]  
[1]  
[2,3]  
[2]  
[3]  
[]  
~~~~

Başka bir deyişle, bir kuvvet kümesi elde etmek, bir kümedeki öğeleri saklama ve atmanın tüm kombinasyonlarını elde etmeye benzer. 
`[2,3]` orijinal küme gibidir, sadece `1` sayısını hariç tuttuk. 

Bir listenin kuvvet kümesini döndüren bir fonksiyon yapmak için belirsizliğe(non-determinism) güveneceğiz.
Listeyi `[1,2,3]` alıyoruz ve sonra `1` olan ilk öğeye bakıyoruz ve kendimize soruyoruz: saklamalı mıyız yoksa bırakmalı mıyız? Aslında ikisini de yapmak istiyoruz.
Bu yüzden bir listeyi filtreleyeceğiz ve belirsiz bir şekilde listedeki her öğeyi hem tutan hem de düşüren bir predicate kullanacağız. İşte `powerset` fonksiyonumuz:

~~~~ {.haskell: .ghci name="code"}
powerset :: [a] -> [[a]]  
powerset xs = filterM (\x -> [True, False]) xs  
~~~~

Bekle, bu mu? Evet. Öğenin ne olduğuna bakılmaksızın her öğeyi bırakıp saklamayı seçiyoruz. Belirsiz bir predicate'imiz var,
bu nedenle ortaya çıkan liste aynı zamanda belirsiz bir değer olacak ve bu nedenle bir liste listesi olacaktır. Şuna bir bakalım:

~~~~ {.haskell: .ghci name="code"}
ghci> powerset [1,2,3]  
[[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]  
~~~~

Bu biraz kafa yormak için biraz düşünmeyi gerektirir, ancak listeleri ne olacağını bilmeyen belirsiz değerler olarak düşünürseniz, 
bu yüzden her şeyi aynı anda yapmaya karar verirlerse, bu biraz daha kolaydır.


foldM
-----

`foldl`'nin monadik karşılığı `foldM`'dir. fold'larımızı [fold'lar bölümünden](../tr/06-higher-order-functions.md#sadece-foldlar-ve-atlar) hatırlarsanız, `foldl`'nin bir binary fonksiyonu, bir başlangıç toplayıcısını ve fold'lanacak bir listeyi aldığını ve ardından binary fonksiyonu kullanarak soldan tek bir değere fold'ladığını bilirsiniz.
`foldM`, monadik bir değer üreten ve listeyi bununla fold'layan bir binary fonksiyon alması dışında aynı şeyi yapar. 
Şaşırtıcı olmayan bir şekilde, ortaya çıkan değer de monadiktir. `foldl` türü şudur:

~~~~ {.haskell: .ghci name="code"}
foldl :: (a -> b -> a) -> a -> [b] -> a  
~~~~

`foldM` ise aşağıdaki türe sahiptir:

~~~~ {.haskell: .ghci name="code"}
foldM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a  
~~~~

İkili fonksiyonun döndürdüğü değer monadiktir ve bu nedenle tüm fold'un sonucu da monadiktir. fold bir sayı listesini özetleyelim:

~~~~ {.haskell: .ghci name="code"}
ghci> foldl (\acc x -> acc + x) 0 [2,8,3,1]  
14  
~~~~

Başlangıç toplayıcısı `0`'dır ve ardından toplayıcıya `2` eklenir, bu da `2` değerine sahip yeni bir toplayıcı ile sonuçlanır.
Bu toplayıcıya `8` eklenir ve bu da `10`'luk bir toplayıcıyla sonuçlanır ve bu şekilde devam eder ve sonuna ulaştığımızda, sonuç son toplayıcıdır.

Şimdi, bir sayılar listesini toplamak isteseydik, ancak listedeki herhangi bir sayı `9`'dan büyükse, her şeyin başarısız olması koşuluyla ne olur?
Mevcut sayının `9`'dan büyük olup olmadığını ve eğer öyleyse başarısız olur ve değilse, neşeli şekilde devam eden bir binary fonksiyon kullanmak mantıklı olacaktır.
Bu ek başarısızlık olasılığı nedeniyle, binary fonksiyonumuzun normal yerine `Maybe` toplayıcısı döndürmesini sağlayalım. İşte binary fonksiyon:

~~~~ {.haskell: .ghci name="code"}
binSmalls :: Int -> Int -> Maybe Int  
binSmalls acc x  
    | x > 9     = Nothing  
    | otherwise = Just (acc + x)  
~~~~

Binary fonksiyonumuz artık monadik bir fonksiyon olduğundan, onu normal `foldl` ile kullanamayız, ancak `foldM`'yi kullanmalıyız. İşte:

~~~~ {.haskell: .ghci name="code"}
ghci> foldM binSmalls 0 [2,8,3,1]  
Just 14  
ghci> foldM binSmalls 0 [2,11,3,1]  
Nothing  
~~~~

Mükemmel! Listedeki bir sayı `9`'dan büyük olduğu için, her şey bir `Nothing` ile sonuçlandı. Bir `Writer` değerini döndüren bir binary fonksiyonla fold da harikadır,
çünkü o zaman fold yolunda ilerlerken istediğinizi kaydedersiniz.


Güvenli bir RPN hesap makinesi yapmak
-------------------------------------

![miner](../img/miner.png)
Bir [RPN hesap makinesi](../tr/10-functionally-solving-problems.md#ters-lehçe-notasyon-hesaplayıcı) uygulama sorununu çözerken, girdi mantıklı olduğu sürece iyi çalıştığını fark ettik. Ancak bir şeyler ters gidince,
tüm programımızın çökmesine neden oldu. Artık sahip olduğumuz bazı kodları nasıl alacağımızı ve onu monadik hale getireceğimizi bildiğimize göre,
RPN hesap makinemizi alıp `Maybe` monad'ından yararlanarak ona error handling ekleyelim.

RPN hesaplayıcımızı `"1 3 + 2 *"` gibi bir string alarak, onu kelimelere bölerek `["1", "3", "+", "2", "*"]` gibi bir şey elde ederek uyguladık ve
sonra boş bir yığınla başlayarak ve ardından yığına sayılar ekleyen veya yığının üstündeki sayıları bir araya getirip bölmek için işleyen bir
binary bölme fonksiyonu kullanarak bu listeyi fold'luyoruz.

Bu, fonksiyonumuzun ana gövdesiydi:

~~~~ {.haskell: .ghci name="code"}
import Data.List  
  
solveRPN :: String -> Double  
solveRPN = head . foldl foldingFunction [] . words  
~~~~

İfadeyi string'ler listesi haline getirdik, fold fonksiyonumuzla üzerine fold uyguladık ve sonra yığında sadece bir öğe kaldığında, o öğeyi yanıt olarak döndürdük.
Bu, fold fonksiyonuydu:

~~~~ {.haskell: .ghci name="code"}
foldingFunction :: [Double] -> String -> [Double]  
foldingFunction (x:y:ys) "*" = (x * y):ys  
foldingFunction (x:y:ys) "+" = (x + y):ys  
foldingFunction (x:y:ys) "-" = (y - x):ys  
foldingFunction xs numberString = read numberString:xs  
~~~~

fold'un toplayıcısı, `Double` değerlerinin bir listesiyle temsil ettiğimiz bir yığındı. fold fonksiyonu RPN ifadesinin üzerinden geçtiğinde,
mevcut öğe bir operatörse, yığının tepesinden iki öğe çıkardı, operatörü aralarına uyguladı ve ardından sonucu yığına geri koydu.
Mevcut öğe bir sayıyı temsil eden bir string'se, bu string'i gerçek bir sayıya dönüştürdü ve en üste itilen numara dışında eskisine benzeyen yeni bir yığın döndürdü.

Öncelikle fold fonksiyonumuzu mükemmel bir başarısızlık yapabilecek hale getirelim. Türü şimdi olduğundan şu şekle değişecek:

~~~~ {.haskell: .ghci name="code"}
foldingFunction :: [Double] -> String -> Maybe [Double]  
~~~~

Yani ya `Just` yeni bir yığın döndürecektir veya `Nothing` ile başarısız olacaktır.

`reads` fonksiyonu `read` gibidir, yalnızca başarılı bir okuma durumunda tek öğeli bir liste döndürür. Bir şeyi okuyamazsa, boş bir liste döndürür.
Okuduğu değeri döndürmenin yanı sıra, string'in tüketmediği kısmını da döndürür. Çalışmak için her zaman tüm girdiyi tüketmesi ve
kolaylık sağlamak için onu bir `readMaybe` fonksiyonu haline getirmesi gerektiğini söyleyeceğiz. İşte burada:

~~~~ {.haskell: .ghci name="code"}
readMaybe :: (Read a) => String -> Maybe a  
readMaybe st = case reads st of [(x,"")] -> Just x  
                                _ -> Nothing  
~~~~

Test ediliyor:

~~~~ {.haskell: .ghci name="code"}
ghci> readMaybe "1" :: Maybe Int  
Just 1  
ghci> readMaybe "GO TO HELL" :: Maybe Int  
Nothing  
~~~~

Tamam, işe yarıyor gibi görünüyor. Öyleyse, fold fonksiyonumuzu başarısız olabilecek monadik bir fonksiyona dönüştürelim:

~~~~ {.haskell: .ghci name="code"}
foldingFunction :: [Double] -> String -> Maybe [Double]  
foldingFunction (x:y:ys) "*" = return ((x * y):ys)  
foldingFunction (x:y:ys) "+" = return ((x + y):ys)  
foldingFunction (x:y:ys) "-" = return ((y - x):ys)  
foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)  
~~~~

İlk üç durum eskileri gibidir, ancak yeni yığın bir `Just`'a sarılır (bunu yapmak için burada `return` kullandık, ancak `Just` da yazabilirdik).
Son durumda, `readMaybe` `numberString` yaparız ve sonra bunun üzerine `(:xs)` eşleriz. Dolayısıyla, `xs` yığını `[1.0,2.0]` ve `readMaybe` `numberString` ise
`Just 3.0` ile sonuçlanırsa, sonuç `Just [3.0,1.0,2.0]` olur. `readMaybe numberString` bir `Nothing` ile sonuçlanırsa, sonuç `Nothing` olur.
fold fonksiyonunu tek başına deneyelim:

~~~~ {.haskell: .ghci name="code"}
ghci> foldingFunction [3,2] "*"  
Just [6.0]  
ghci> foldingFunction [3,2] "-"  
Just [-1.0]  
ghci> foldingFunction [] "*"  
Nothing  
ghci> foldingFunction [] "1"  
Just [1.0]  
ghci> foldingFunction [] "1 wawawawa"  
Nothing  
~~~~

Çalışıyor gibi görünüyor! Ve şimdi yeni ve geliştirilmiş `solveRPN` zamanı. İşte bayanlar ve baylar!

~~~~ {.haskell: .ghci name="code"}
import Data.List  
  
solveRPN :: String -> Maybe Double  
solveRPN st = do  
    [result] <- foldM foldingFunction [] (words st)  
    return result  
~~~~

Tıpkı daha önce olduğu gibi, string'i alıp bir kelime listesine dönüştürüyoruz. Sonra, boş yığınla başlayarak bir fold yaparız,
sadece normal bir `foldl` yapmak yerine, bir `folldM` yaparız. Bu `foldM`'in sonucu, bir liste içeren bir Maybe değeri olmalıdır (bu bizim son yığınımızdır) ve
bu liste yalnızca bir değere sahip olmalıdır. Bu değeri elde etmek için bir `do` ifadesi kullanıyoruz ve buna `result` diyoruz.
`foldM`'nin `Nothing` döndürmesi durumunda, her şey bir `Nothing` olacaktır, çünkü `Maybe` böyle çalışır. Ayrıca, `do` ifadesinde desen eşleştirme yaptığımıza dikkat edin,
bu nedenle liste birden fazla değere sahipse veya hiç yoksa, desen eşleştirme başarısız olur ve bir `Nothing` üretilir.
Son satırda, RPN hesaplamasının sonucunu nihai `Maybe` değerinin sonucu olarak sunmak için sadece `return result` yaparız.

Bir deneyelim:

~~~~ {.haskell: .ghci name="code"}
ghci> solveRPN "1 2 * 4 +"  
Just 6.0  
ghci> solveRPN "1 2 * 4 + 5 *"  
Just 30.0  
ghci> solveRPN "1 2 * 4"  
Nothing  
ghci> solveRPN "1 8 wharglbllargh"  
Nothing  
~~~~

İlk başarısızlık, son yığının içinde bir öğe bulunan bir liste olmadığı ve bu nedenle `do` ifadesindeki desen eşleştirmenin başarısız olmasından kaynaklanır.
İkinci başarısızlık, `readMaybe` bir `Nothing` döndürdüğü için gerçekleşir.


Monadik fonksiyonları oluşturma
-------------------------------

Monad kanunlarını öğrenirken, `<=<` fonksiyonunun tıpkı kompozisyon gibi olduğunu, sadece `a -> b` gibi normal fonksiyonlar için çalışmak yerine,
`a -> m b` gibi monadik fonksiyonlar için çalıştığını söylemiştik. Örneğin:

~~~~ {.haskell: .ghci name="code"}
ghci> let f = (+1) . (*100)  
ghci> f 4  
401  
ghci> let g = (\x -> return (x+1)) <=< (\x -> return (x*100))  
ghci> Just 4 >>= g  
Just 401  
~~~~

Bu örnekte, önce iki normal fonksiyon oluşturduk, sonuçta elde edilen fonksiyonu `4`'e uyguladık ve sonra iki monadik fonksiyon oluşturduk ve
elde edilen fonksiyona `>>=` ile `Just 4`'ü verdik. 

Bir listede bir sürü fonksiyonumuz varsa, başlangıç toplayıcısı olarak `id`'yi ve
binary fonksiyon olarak `.` fonksiyonunu kullanarak hepsini tek bir büyük fonksiyonda oluşturabiliriz. İşte bir örnek:

~~~~ {.haskell: .ghci name="code"}
ghci> let f = foldr (.) id [(+1),(*100),(+1)]  
ghci> f 1  
201  
~~~~

`f` fonksiyonu bir sayıyı alır ve sonra ona `1` ekler, sonucu `100` ile çarpar ve sonra buna `1` ekler. Her neyse, monadik fonksiyonları aynı şekilde oluşturabiliriz,
sadece normal kompozisyon yerine `<=<` kullanırız ve `id` yerine return kullanırız. Bir `foldr` veya başka bir şey üzerinde `foldM` kullanmak zorunda değiliz
çünkü `<=<` fonksiyonu kompozisyonun monadik bir şekilde olmasını sağlar.

[Önceki bölümdeki](../tr/12-a-fistful-of-monads.md#liste-monad) liste monad'ını öğrenirken, bir atın satranç tahtasındaki bir konumdan diğerine tam olarak üç hamlede geçip geçemeyeceğini anlamak için kullandık.
`moveKnight` adında, atın tahtadaki konumunu alan ve daha sonra yapabileceği tüm olası hamleleri döndüren bir fonksiyonumuz vardı.
Ardından, üç hamle yaptıktan sonra sahip olabileceği tüm olası pozisyonları oluşturmak için aşağıdaki fonksiyonu yaptık:

~~~~ {.haskell: .ghci name="code"}
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight   
~~~~

Ve üç hamlede `start`'dan `end`'e geçip geçemeyeceğini kontrol etmek için şunları yaptık:

~~~~ {.haskell: .ghci name="code"}
canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start  
~~~~

Monadik fonksiyon kompozisyonunu kullanarak, `in3` gibi bir fonksiyon yapabiliriz, ancak atın üç hamle yaptıktan sonra sahip olabileceği tüm pozisyonları oluşturmak yerine,
bunu keyfi sayıda hareket için yapabiliriz. `in3`'e bakarsanız, üç kez `moveKnight` kullandığımızı ve her seferinde `>>=` kullanarak tüm olası önceki pozisyonları
beslediğimizi görürüz. Öyleyse şimdi daha genel hale getirelim. İşte bunu nasıl yapacağınız:

~~~~ {.haskell: .ghci name="code"}
import Data.List  
  
inMany :: Int -> KnightPos -> [KnightPos]  
inMany x start = return start >>= foldr (<=<) return (replicate x moveKnight)  
~~~~

Önce `replicate` fonksiyonunu, `moveKnight` fonksiyonunun `x` kopyasını içeren bir liste yapmak için kullanırız. 
Daha sonra, tüm bu fonksiyonları tek parça halinde tek bir yapıya dönüştürüyoruz, bu da bize bir başlangıç pozisyonu alan ve
atı `x` kez belirsiz bir şekilde hareket ettiren bir fonksiyon veriyor. Ardından, başlangıç pozisyonunu `return` ile tekli liste haline getirip fonksiyonu besliyoruz.

Artık `canReachIn3` fonksiyonumuzu daha genel olacak şekilde değiştirebiliriz:

~~~~ {.haskell: .ghci name="code"}
canReachIn :: Int -> KnightPos -> KnightPos -> Bool  
canReachIn x start end = end `elem` inMany x start  
~~~~


Monad'lar yapmak
----------------

![spearhead](../img/spearhead.png)

Bu bölümde, bir türün nasıl yapıldığına, monad olarak tanımlandığına ve ardından uygun `Monad` instance'ının verildiğine dair bir örneğe bakacağız.
Genelte tek amacı monad yapmak olan bir düşünceyle yola çıkamayız. Bunun yerine, genellikle amacı bir problemin bir yönünü modellemek olan bir tür yaparız ve
daha sonra türün bir bağlamla bir değeri temsil ettiğini ve bir monad gibi davranabileceğini görürsek, ona bir `Monad` instance'ı veririz.

Gördüğümüz gibi, listeler kararsız(non-deterministic) değerleri temsil etmek için kullanılır. `[3,5,9]` gibi bir liste,
ne olacağına karar veremeyen tek bir kararsız değer olarak görülebilir. Bir listeyi `>>=` ile bir fonksiyona beslediğimizde,
listeden bir öğe alıp ona fonksiyonu uygulamak için tüm olası seçimleri yapar ve ardından bu sonuçları bir listede de sunar.

Listeye `[3,5,9]`, `3`, `5` ve `9` sayılarının aynı anda oluştuğuna bakarsak, bu sayıların her birinin oluşma olasılığı hakkında hiçbir bilgi olmadığını görebiliriz.
`[3,5,9]` gibi kararsız bir değeri modellemek istiyorsak, ancak `3`'ün %50 olma şansına ve `5` ve `9`'un her ikisinin de %25 olma şansına sahip olduğunu ifade etmek istesek?
Deneyelim ve bunu gerçekleştirelim!

Listedeki her öğenin başka bir değerle, gerçekleşme olasılığıyla geldiğini varsayalım. O zaman bunu şu şekilde sunmak mantıklı olabilir:

~~~~ {.haskell: .ghci name="code"}
[(3,0.5),(5,0.25),(9,0.25)]  
~~~~

Matematikte olasılıklar genellikle yüzdelerle değil, 0 ile 1 arasındaki gerçek sayılarla ifade edilir.
0, cehennemde bir şeyin olma şansı olmadığı anlamına gelir ve 1, kesinlikle olduğu anlamına gelir.
Kayan nokta(Floating point) sayıları, hassasiyeti kaybetme eğiliminde oldukları için çok hızlı bir şekilde dağınık hale gelebilir,
bu nedenle Haskell bize hassasiyeti kaybetmeyen rasyonel sayılar için bir veri türü sunar. Bu tür `Rational` olarak adlandırılır ve `Data.Ratio`'da yaşar.
`Rational` yapmak için onu kesirmiş gibi yazıyoruz. Pay ve payda `%` ile ayrılır. İşte birkaç örnek:

~~~~ {.haskell: .ghci name="code"}
ghci> 1%4  
1 % 4  
ghci> 1%2 + 1%2  
1 % 1  
ghci> 1%3 + 5%4  
19 % 12  
~~~~

İlk satır sadece dörtte biridir. İkinci satırda bir bütün elde etmek için iki yarım ekliyoruz ve üçüncü satırda beş çeyrek ile üçte birini ekliyoruz ve
on dokuz on ikiyi elde ediyoruz. Öyleyse kayan noktalarımızı atalım ve olasılıklarımız için `Rational` kullanalım:

~~~~ {.haskell: .ghci name="code"}
ghci> [(3,1%2),(5,1%4),(9,1%4)]  
[(3,1 % 2),(5,1 % 4),(9,1 % 4)]  
~~~~

Tamam, yani `3`'ün ikide biri olma şansı varken, `5` ve `9` dörtte bir olacak. Oldukça temiz.

Listeleri aldık ve onlara fazladan bağlam ekledik, bu nedenle bu, bağlamları olan değerleri de temsil ediyor. Daha ileri gitmeden önce, bunu bir `newtype`'a saralım çünkü
bir şey bana bazı instance'lar oluşturacağımızı söylüyor.

~~~~ {.haskell: .ghci name="code"}
import Data.Ratio  
  
newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show  
~~~~

Peki. Bu bir functor mu? Liste bir functor, yani bu da bir functor olmalı, çünkü listeye bazı şeyler ekledik. Bir fonksiyonu bir liste üzerinde eşlediğimizde,
onu her elemana uygularız. Burada bunu her elemente de uygulayacağız, sadece olasılıkları olduğu gibi bırakacağız. Bir instance oluşturalım:

~~~~ {.haskell: .ghci name="code"}
instance Functor Prob where  
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x,p)) xs  
~~~~

Onu `newtype` desen eşleştirmeyle açıp, olasılıkları olduğu gibi korurken değerlere `f` fonksiyonunu uygularız ve sonra geri sararız. İşe yarayıp yaramadığını görelim:

~~~~ {.haskell: .ghci name="code"}
ghci> fmap negate (Prob [(3,1%2),(5,1%4),(9,1%4)])  
Prob {getProb = [(-3,1 % 2),(-5,1 % 4),(-9,1 % 4)]}  
~~~~

Unutulmaması gereken bir diğer nokta da olasılıkların toplamının her zaman `1` olması gerektiğidir. Bunların hepsi olabilecek şeylerse,
olasılıklarının toplamının `1`'den başka bir şey olması bir anlam ifade etmiyor. Zamanın %75'inde yazı getiren ve %50 tura getiren bir madeni para,
yalnızca başka bir garip evrende işe yarayabilirmiş gibi görünüyor.

Şimdi büyük soru, bu bir monad mı? Listenin nasıl bir monad olduğu göz önüne alındığında, bu da bir monad olması gerektiği gibi görünüyor. Önce `return` hakkında düşünelim.
Listeler için nasıl çalışır? Bir değer alır ve onu tekil(singleton) listeye koyar. Ya burası? Varsayılan bir minimum bağlam olması gerektiğinden,
aynı zamanda tekil bir liste de yapmalıdır. Ya olasılık? Pekala, `return x`, `x`'i her zaman sonucu olarak sunan monadik bir değer oluşturması gerekiyor,
bu nedenle `0` olma olasılığının bir anlamı yok. Her zaman sonucu olarak sunması gerekiyorsa, olasılık `1` olmalıdır!

Peki ya >>=? Biraz aldatıcı görünüyor, öyleyse, monadlar için `m >> = f`'in her zaman `join (fmap f m)`'e eşit olduğu gerçeğini kullanalım ve
olasılık listelerinin bir olasılık listesini nasıl düzleştireceğimizi düşünelim. Örnek olarak, %25 olasılıkla `'a'` veya `'b'`'den birinin gerçekleşmesi olasılığının olduğu
bu listeyi ele alalım. Hem `'a'` hem de `'b'` eşit derecede olasıdır. Ayrıca, `'c'` veya `'d'` değerlerinden tam olarak birinin gerçekleşmesi için %75 şans vardır.
`'c'` ve `'d'` de eşit derecede olasıdır. İşte bu senaryoyu modelleyen bir olasılık listesi resmi:

![prob](../img/prob.png)
Bu harflerin her birinin oluşma şansı nedir? Bunu her biri bir olasılığa sahip dört kutu olarak çizseydik, bu olasılıklar ne olurdu?
Bulmak için tek yapmamız gereken, her olasılığı içerdiği tüm olasılıklarla çarpmaktır. `'a'`, `'b'` gibi sekizde bir ortaya çıkar,
çünkü yarıyı çeyrek ile çarparsak sekizde bir elde ederiz. `'c'` sekizde üç olur çünkü dörtte üçü yarı ile çarpıldığında sekizde üçtür. `'d'` ayrıca sekizde üç kez olur.
Tüm olasılıkları toplarsak, toplamları yine de bir olur. 

İşte olasılık listesi olarak ifade edilen bu durum:

~~~~ {.haskell: .ghci name="code"}
thisSituation :: Prob (Prob Char)  
thisSituation = Prob  
    [( Prob [('a',1%2),('b',1%2)] , 1%4 )  
    ,( Prob [('c',1%2),('d',1%2)] , 3%4)  
    ]  
~~~~

Türünün `Prob (Prob Char)` olduğuna dikkat edin. Artık iç içe geçmiş bir olasılık listesini nasıl düzleştireceğimizi bulduğumuza göre,
tek yapmamız gereken bunun kodunu yazmak ve sonra basitçe `join (fmap f m)` olarak `>>=` yazabiliriz ve kendimiz bir monad'a sahibiz!
İşte `flatten`, bunu kullanacağız çünkü `join` adı zaten alınmış:

~~~~ {.haskell: .ghci name="code"}
flatten :: Prob (Prob a) -> Prob a  
flatten (Prob xs) = Prob $ concat $ map multAll xs  
    where multAll (Prob innerxs,p) = map (\(x,r) -> (x,p*r)) innerxs  
~~~~

`multAll` fonksiyonu, bir olasılık listesi demeti ve onunla birlikte gelen bir olasılık `p` alır ve ardından her iç olasılığı `p` ile çarparak öğe ve
olasılık çiftlerinin bir listesini döndürür. İç içe geçmiş olasılık listemizdeki her bir çiftin üzerinde `multAll` eşleriz ve
sonra ortaya çıkan iç içe geçmiş listeyi düzleştiririz.

Artık ihtiyacımız olan her şeye sahibiz, bir `Monad` instance'ı yazabiliriz!

~~~~ {.haskell: .ghci name="code"}
instance Monad Prob where  
    return x = Prob [(x,1%1)]  
    m >>= f = flatten (fmap f m)  
    fail _ = Prob [] 
~~~~

![ride](../img/ride.png)
Tüm zor işi zaten yaptığımız için, instance çok basit. Listeler için olanla aynı olan `fail` fonksiyonunu de tanımladık,
bu nedenle bir `do` ifadesinde desen eşleştirme hatası varsa, olasılık listesi bağlamında bir hata oluşur.

Az önce yaptığımız monad için monad yasalarının geçerli olup olmadığını kontrol etmek de önemlidir. İlki `return x >>= f`'nin `f x`'e eşit olması gerektiğini söylüyor.
Sıkı bir ispat oldukça sıkıcı olurdu, ancak şunu görebiliriz: varsayılan bir bağlama bir değer koyarsak `return` ve
sonra bunun üzerinde bir fonksiyonu `fmap`'leyin ve sonuçta ortaya çıkan olasılık listesini düzleştirirsek,
fonksiyondan kaynaklanan her olasılık return ile yaptığımız `1%1` olasılıkla çarpılır, böylece bağlamı etkilemez.
`m >>= return`'ün sadece `m`'ye eşit olmasının gerekçesi de benzerdir. Üçüncü yasa, `f <=< (g <=< h)`'nin `(f <=< g) <=< h` ile aynı olması gerektiğini belirtir.
Bu da geçerlidir, çünkü olasılık monad'ının temelini oluşturan liste monad'ı için ve çarpma ilişkisel olduğu için geçerlidir.
`1%2 * (1%3 * 1%5)` eşittir `(1%2 * 1%3) * 1%5`.

Artık bir monad'ımız olduğuna göre, onunla ne yapabiliriz? Olasılıklarla hesaplamalar yapmamıza yardımcı olabilir. 
Olasılıksal olayları bağlamları olan değerler olarak ele alabiliriz ve olasılık monad'ı, bu olasılıkların nihai sonucun olasılıklarına yansıtılmasını sağlayacaktır.

Diyelim ki, iki normal bozuk paramız ve onda dokuzda şaşırtıcı bir şekilde yazı atan ve onda yalnızca bir tura atan bir yüklü bozuk para var.
Tüm paraları bir defada atarsak, hepsinin yazı gelme ihtimali nedir? İlk olarak, normal bir yazı tura atma ve yüklü bir yazı için olasılık değerleri yapalım:
İlk olarak, normal bir yazı tura atma ve yüklü bir yazı tura için olasılık değerleri yapalım:

~~~~ {.haskell: .ghci name="code"}
data Coin = Heads | Tails deriving (Show, Eq)  
  
coin :: Prob Coin  
coin = Prob [(Heads,1%2),(Tails,1%2)]  
  
loadedCoin :: Prob Coin  
loadedCoin = Prob [(Heads,1%10),(Tails,9%10)]  
~~~~

Ve son olarak, bozuk para atma eylemi:

~~~~ {.haskell: .ghci name="code"}
import Data.List (all)  
  
flipThree :: Prob Bool  
flipThree = do  
    a <- coin  
    b <- coin  
    c <- loadedCoin  
    return (all (==Tails) [a,b,c])  
~~~~

Bir deneme yaptığımızda, yüklü bozuk paramızla hile yapmamıza rağmen, üç düşen yazının hepsinin şansının o kadar iyi olmadığını görüyoruz:

~~~~ {.haskell: .ghci name="code"}
ghci> getProb flipThree  
[(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),  
 (False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]  
~~~~

Üçü de kırktan dokuzunda yazı atacak, bu da %25'ten az. Monad'ımızın, tüm bozuk paraların tek bir sonuca dönüşmediği tüm `False` sonuçlarına nasıl katılacağını
bilmediğini görüyoruz. Bu büyük bir problem değil, çünkü aynı sonuçları tek bir sonuca yerleştirmek için bir fonksiyon yazmak oldukça kolaydır ve
okuyucuya bir egzersiz olarak bırakılmıştır (siz!)

Bu bölümde, bir soru sormaktan (ya listeler aynı zamanda olasılık hakkında bilgi taşıyorsa?) bir tür oluşturmaya, bir monad'ı tanımaya ve
sonunda bir instance oluşturmaya ve onunla bir şeyler yapmaya geçtik. Bence bu oldukça çekici! Şimdiye kadar, monad'ları ve
ne hakkında olduklarını oldukça iyi kavramış olmalıyız.


