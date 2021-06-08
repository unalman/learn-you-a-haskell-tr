Birkaç Monad İçin Daha Fazla
============================

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

~~~~

~~~~ {.haskell: .ghci name="code"}

~~~~

~~~~ {.haskell: .ghci name="code"}

~~~~

~~~~ {.haskell: .ghci name="code"}

~~~~

~~~~ {.haskell: .ghci name="code"}

~~~~





































































































































































































































































































































































































































































































































