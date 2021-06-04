Functor'lar, Applicative Functor'lar ve Monoidler
=================================================

Haskell'in saflık, daha yüksek dereceli fonksiyonlar, parametreli cebirsel veri türleri ve tür sınıfları kombinasyonu,
polimorfizmi diğer dillerde mümkün olandan çok daha yüksek bir seviyede uygulamamıza izin verir. Büyük bir türler hiyerarşisine ait türleri düşünmemize gerek yok.
Bunun yerine, türlerin nasıl davranabileceğini düşünür ve ardından bunları uygun tür sınıflarıyla birleştiririz. `Int` pek çok şey gibi davranabilir.
Eşitlenebilir bir şey gibi hareket edebilir, sıralı bir şey gibi, sayılabilir bir şey gibi vb.

Tür sınıfları açıktır, bu da kendi veri türümüzü tanımlayabileceğimiz, nasıl davranabileceğini düşünebileceğimiz ve
davranışlarını tanımlayan tür sınıflarına bağlayabileceğimiz anlamına gelir. Bu nedenle ve Haskell'in sadece tür bildirimini bilerek bir fonksiyon hakkında çok şey
bilmemizi sağlayan harika tür sistemi sayesinde, çok genel ve soyut olan davranışı tanımlayan tür sınıfları tanımlayabiliriz. 
İki şeyin eşit olup olmadığını görmek veya iki şeyi bir sırayla karşılaştırmak için işlemleri tanımlayan tür sınıflarıyla tanıştık.
Bunlar çok soyut ve zarif davranışlardır, ancak onları çok özel bir şey olarak düşünmüyoruz çünkü hayatımızın çoğunda onlarla uğraşıyoruz.
Kısa süre önce, temelde eşlenebilir şeyler olan functor'larla tanıştık. Bu, tür sınıflarının tanımlayabileceği kullanışlı ve
yine de oldukça soyut bir özelliğe bir örnektir. Bu bölümde, aplikatif functor denen functor'ların biraz daha güçlü ve
daha kullanışlı versiyonlarının yanı sıra functor'lara daha yakından bakacağız. Ayrıca çoraplara benzeyen monoid'lere de bir göz atacağız.


Functors redux
--------------

[Kendi küçük bölümlerinde](../tr/08-making-our-own-types-and-typeclasses.md#functor-tür-sınıfı) functorlardan bahsetmiştik. Henüz okumadıysanız, muhtemelen hemen şimdi veya daha sonra daha fazla zamanınız olduğunda bir göz atmalısınız. Ya da okuyormuş gibi yapabilirsiniz.

Yine de, işte hızlı bir tazeleme: Functors, listeler, `Maybe`ler, ağaçlar ve benzeri gibi üzerinde eşlenebilir şeylerdir.
Haskell'de, tek bir tür sınıfı yöntemi olan tür sınıfı `Functor` ile tanımlanırlar, yani `fmap :: (a -> b) -> f a -> f b` türüne sahip olan fmap.
Diyor ki: bana `a` alan ve `b` döndüren bir fonksiyon ve içinde `a` (veya birkaç tanesi) olan bir kutu verin, ben de size içinde `b` (veya birkaç tanesi) olan bir kutu vereceğim. Fonksiyonu kutunun içindeki elemana bir nevi uygular.

**Küçük bir tavsiye.** Çoğu zaman kutu benzetmesi, functor'ların nasıl çalıştığına dair bir sezgiye sahip olmanıza yardımcı olmak için kullanılır ve daha sonra,
muhtemelen aynı benzetmeyi applicative functorlar ve monadlar için kullanacağız. Bu, insanların ilk başta functor'ları anlamalarına yardımcı olan iyi bir benzetme,
sadece bunu tam anlamıyla almayın, çünkü bazı functor'lar için kutu benzetmesinin hala bir gerçeği tutması için gerçekten çok ince olması gerekir.
Bir functor'un ne olduğu için daha doğru bir terim, hesaplama bağlamı olacaktır. Bağlam, hesaplamanın bir değeri olabilir veya başarısız olabilir (`Maybe` ve `Either a`)
veya daha fazla değer (listeler) olabilir, bunun gibi şeyler olabilir.

Bir type constructor'ını `Functor` instance'ı yapmak istiyorsak, bir tür `* -> *` olması gerekir,
bu da tür parametresi olarak tam olarak bir somut türü alması gerektiği anlamına gelir.
Örneğin, `Maybe` bir instance haline getirilebilir, çünkü `Maybe Int` veya `Maybe String` gibi somut bir tür üretmek için tek tür parametresi gerekir.
Bir type constructor'ı iki parametre alırsa, `Either` gibi, type constructor'ını yalnızca bir tür parametresi alana kadar kısmen uygulamamız gerekir.
Yani `instance Functor Either where` yazamayız, ancak `instance Functor (Either a) where` yazabiliriz ve sonra `fmap`'in yalnızca `Either a` için olduğunu düşünürsek,
`fmap :: (b -> c) -> Either a b -> Either a c`. Gördüğünüz gibi, `Either a` kısmı sabittir, çünkü `Either a` yalnızca bir tür parametresi alırken,
yalnızca `Either` iki tür parametresini alır `fmap :: (b -> c) -> Either b -> Either c` gerçekten mantıklı değil.

`[]`, `Maybe`, `Either a` ve kendi başımıza yaptığımız bir `Tree` türü gibi birçok türün (aslında type constructor'larının) Functor instance'ları olduğunu şimdiye kadar öğrendik.
Büyük bir iyilik için fonksiyonları üzerlerine nasıl eşleyebileceğimizi(map) gördük. Bu bölümde, `IO` ve `(->) r` olmak üzere iki functor instance'ına daha göz atacağız.

Bir değerin, örneğin `IO String` türünde bir türü varsa, bu, gerçekleştirildiğinde, gerçek dünyaya çıkıp bizim için sonuç olarak ortaya çıkacak bir dizi string alacağı
bir I/O eylemi olduğu anlamına gelir. Bu sonucu bir isme bağlamak için do sözdiziminde <- kullanabiliriz. 
I/O eylemlerinin dışarı çıkan ve bizim için dış dünyadan bir değer getiren küçük ayaklı kutular gibi olduğundan bahsetmiştik.
Ne getirdiklerini inceleyebiliriz, ancak inceledikten sonra, değeri `IO`'ya geri sarmamız gerekir. Bu kutuyu küçük ayak benzetmesiyle düşünürsek,
`IO`'nun nasıl bir functor gibi davrandığını görebiliriz.

`IO`'nun nasıl bir `Functor` instance'ı olduğunu görelim. Bir I/O eylemi üzerinden bir fonksiyonu `fmap` ettiğimizde,
aynı şeyi yapan, ancak fonksiyonumuzun sonuç değerine uygulanmasını sağlayan bir I/O eylemini geri almak isteriz.

~~~~ {.haskell: .ghci name="code"}
instance Functor IO where  
    fmap f action = do  
        result <- action  
        return (f result)  
~~~~

Bir I/O eylemi üzerinde bir şeyi eşlemenin sonucu bir I/O eylemi olacaktır, bu nedenle hemen iki eylemi yapıştırmak ve yeni bir eylem yapmak için *do* sözdizimi kullanırız.
`fmap` uygulamasında, ilk olarak orijinal I/O eylemini gerçekleştiren ve sonucunu `result` olarak adlandıran yeni bir I/O eylemi yaparız. Sonra `return (f result)` yaparız.
`return`, bildiğiniz gibi, hiçbir şey yapmayan ancak yalnızca sonucu olarak bir şeyler sunan bir I/O işlemi yapan bir fonksiyondur. 
Bir *do* bloğunun ürettiği eylem her zaman son eyleminin result değerine sahip olacaktır. Bu nedenle, gerçekten hiçbir şey yapmayan bir I/O eylemi yapmak için
return kullanıyoruz, yalnızca yeni I/O eyleminin sonucu olarak `f result` sunuyor.

Biraz sezgi kazanmak için onunla oynayabiliriz. Gerçekten oldukça basit. Şu kod parçasına bir göz atın:

~~~~ {.haskell: .ghci name="code"}
main = do line <- getLine   
          let line' = reverse line  
          putStrLn $ "You said " ++ line' ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"  
~~~~

Kullanıcıdan bir satır istenir ve biz onu kullanıcıya geri veririz, sadece tersine çevrilir. Bunu `fmap` kullanarak nasıl yeniden yazacağınız aşağıda açıklanmıştır:

~~~~ {.haskell: .ghci name="code"}
main = do line <- fmap reverse getLine  
          putStrLn $ "You said " ++ line ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ line ++ " backwards!"  
~~~~

![alien](../img/alien.png)
Tıpkı `Just "blah"` üzerinde `fmap` `reverse` yaptığımızda `Just "halb"` alırız. `fmap` `reverse`'i `getLine` üzerinde de yapabiliriz.
`getLine`, bir tür IO String türüne sahip bir I/O eylemidir ve bunun üzerinde `reverse` eşleme yapmak bize gerçek dünyaya çıkıp bir satır alacak ve
ardından sonucuna `reverse` uygulayacak bir I/O eylemi verir. `Maybe` kutusunun içindeki bir şeye bir fonksiyon uygulayabileceğimiz gibi, bir `IO` kutusunun içindekilere bir fonksiyon uygulayabiliriz, yalnızca bir şeyi elde etmek için gerçek dünyaya gitmesi gerekir. Sonra onu bir isme `<-` kullanarak bağladığımızda, isim zaten `reverse` uygulanmış sonucu yansıtacaktır.

I/O eylemi `fmap (++"!") getLine` tıpkı `getLine` gibi davranır, yalnızca sonucu her zaman `"!"` sembolü eklenir!

IO ile sınırlı olsaydı `fmap` türünün ne olacağına bakarsak, `fmap :: (a -> b) -> IO a -> IO b` olurdu. `fmap` bir fonksiyonu ve bir I/O eylemini alır ve
fonksiyonun içerdiği sonuca uygulanması dışında eskisine benzer yeni bir I/O eylemi döndürür.

Kendinizi bir I/O eyleminin sonucunu bir isme bağlarken bulursanız, sadece buna bir fonksiyon uygulamak ve buna başka bir şey vermek için, `fmap` kullanmayı düşünün,
çünkü daha güzel görünüyor. Bir functor içindeki bazı verilere birden fazla dönüşüm uygulamak istiyorsanız, kendi fonksiyonunuzu en üst seviyede ilan edebilir,
bir lambda fonksiyonu oluşturabilir veya ideal olarak fonksiyon kompozisyonunu kullanabilirsiniz:

~~~~ {.haskell: .ghci name="code"}
import Data.Char  
import Data.List  
  
main = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine  
          putStrLn line  
~~~~

~~~~ {.haskell: .ghci name="code"}
$ runhaskell fmapping_io.hs  
hello there  
E-R-E-H-T- -O-L-L-E-H  
~~~~

Muhtemelen bildiğiniz gibi, `intersperse '-' . reverse . map toUpper` bir string'i alan, onun üzerine eşleyen,
`reverse`'i bu sonuca uygulayan ve ardından bu sonuca `intersperse '-'` uygulayan bir fonksiyondur.
`(\xs -> intersperse '-' (reverse (map toUpper xs)))` yazmak gibi , sadece daha güzel.

En başından beri uğraştığımız ancak `Functor` olduğunu bilmediğimiz bir başka `Functor` instance'ı `(->) r`. `(->) r` ne anlama geliyor? Fonksiyon türü `r -> a`
`r -> a` fonksiyon türü `(->) r a` olarak yeniden yazılabilir, tıpkı `2 + 3`'ü `(+) 2 3` olarak yazabildiğimiz gibi.
Buna `(->) r a` olarak baktığımızda, `(->)` biraz farklı bir ışıkta görebiliriz,
çünkü bunun sadece iki tür parametresi alan bir type constructor olduğunu görüyoruz, tıpkı `Either` gibi.
Ancak unutmayın, bir type constructor'ının bir `Functor` instance'ı yapılabilmesi için tam olarak bir tür parametresi alması gerektiğini söylemiştik.
Bu yüzden `(->)` `Functor` instance'ını yapamayız, ancak onu `(->) r`'ye kısmen uygularsak herhangi bir sorun oluşturmaz.
Type constructor'larının bölümlere kısmen uygulanmasına izin verdiyse (`(+) 2` ile aynı olan `(2+)` yaparak `+` uygulayabiliriz), `(->) r`'yi `(r ->)`.
Fonksiyonların functor'ları nasıldır? Peki, `Control.Monad.Instances` altında yatan uygulamaya bir göz atalım.

Genellikle herhangi bir şeyi alan ve herhangi bir şeyi `a -> b` olarak döndüren fonksiyonları işaretleriz. 
`r -> a` aynı şeydir, biz sadece tür değişkenleri için farklı harfler kullandık.

~~~~ {.haskell: .ghci name="code"}
instance Functor ((->) r) where  
    fmap f g = (\x -> f (g x))  
~~~~

Sözdizimi buna izin verdiyse, şu şekilde yazılabilirdi.

~~~~ {.haskell: .ghci name="code"}
instance Functor (r ->) where  
    fmap f g = (\x -> f (g x))  
~~~~

Ama öyle değil, bu yüzden onu eski tarzda yazmalıyız.

Öncelikle `fmap` türünü düşünelim. `fmap :: (a -> b) -> f a -> f b`'dir. Şimdi yapacağımız şey, functor instance'ımızın oynadığı rol olan tüm `f`'leri
zihinsel olarak `(->) r`'ler ile değiştirmektir. Bunu `fmap`'in bu belirli instance için nasıl davranması gerektiğini görmek için yapacağız.
`fmap :: (a -> b) -> ((->) r a) -> ((->) r b)` elde ederiz. Şimdi yapabileceğimiz şey, `(->) r a ve (-> r b)` türlerini, normalde fonksiyonlarda yaptığımız gibi,
infix `r -> a` ve `r -> b` olarak yazmaktır. Şimdi elde ettiğimiz şey `fmap :: (a -> b) -> (r -> a) -> (r -> b)`.

Hmm tamam. Bir fonksiyonu bir fonksiyonun üzerine eşlemek bir fonksiyon üretmeli, tıpkı bir fonksiyonu `Maybe` üzerine eşlemenin bir `Maybe` üretmesi ve
bir fonksiyonu bir liste üzerinde eşlemenin bir liste üretmesi gibi. Bu instance için `fmap :: (a -> b) -> (r -> a) -> (r -> b)` türü bize ne söylüyor?
Görüyoruz ki `a`'dan `b`'ye bir fonksiyon ve `r`'den `a`'ya bir fonksiyon alıp `r`'den `b`'ye bir fonksiyon döndürüyor. Bu sana bir şey hatırlatıyor mu? Evet!
Fonksiyon bileşimi! Bir `r -> b` fonksiyonu elde etmek için, `r -> a`'nın çıktısını `a -> b`'nin girişine aktarıyoruz, bu tam olarak fonksiyon bileşimi ile ilgilidir.
Instance'ın yukarıda nasıl tanımlandığına bakarsanız, bunun sadece fonksiyon kompozisyonu olduğunu göreceksiniz. Bu instance'ı yazmanın başka bir yolu şudur:

~~~~ {.haskell: .ghci name="code"}
instance Functor ((->) r) where  
    fmap = (.)  
~~~~

Bu, fonksiyonlar üzerinde `fmap` kullanmanın sadece bir tür kompozisyon olduğunun açığa çıkmasını sağlar. 
Instance burada tanımlandığı için `:m + Control.Monad.Instances` işlemini yapın ve ardından fonksiyonlar üzerinde eşleme yaparak oynamayı deneyin.

~~~~ {.haskell: .ghci name="code"}
ghci> :t fmap (*3) (+100)  
fmap (*3) (+100) :: (Num a) => a -> a  
ghci> fmap (*3) (+100) 1  
303  
ghci> (*3) `fmap` (+100) $ 1  
303  
ghci> (*3) . (+100) $ 1  
303  
ghci> fmap (show . (*3)) (*100) 1  
"300"  
~~~~

`fmap`'i bir infix fonksiyonu olarak adlandırabiliriz, böylece `.`'ya benzerlik netleşir. İkinci giriş satırında, `(*3)` ile `(+100)` eşlemesini yapıyoruz,
bu da bir girdi alacak bir fonksiyonla sonuçlanır, buna `(+100)` çağırır ve sonra bu sonuçta `(*3)` çağırır. Bu fonksiyonu `1` ile adlandırıyoruz.

Kutu benzetmesi burada nasıl duruyor? Eğer uzatırsan, tutar. `Just 3` yerine `fmap (+3)` kullandığımızda, `Maybe`'yi `(+3)` fonksiyonunu uyguladığımız bazı içeriklere
sahip bir kutu olarak hayal etmek kolaydır. Peki ya `fmap (*3) (+100)` yaptığımızda? Pekala, `(+100)` fonksiyonunu nihai sonucunu içeren bir kutu olarak düşünebilirsiniz.
Bir I/O eyleminin, gerçek dünyaya çıkıp bazı sonuçlar getirecek bir kutu olarak nasıl düşünülebileceği gibi. `(+100)` üzerinde `fmap (*3)` kullanılması,
`(+100)` gibi davranan başka bir fonksiyon oluşturacaktır, yalnızca bir sonuç üretmeden önce, `(*3)` bu sonuca uygulanacaktır.
Şimdi `fmap`'in fonksiyonlar için `.` gibi nasıl davrandığını görebiliriz.

`fmap`'in fonksiyonlar üzerinde kullanıldığında fonksiyon bileşimi olduğu gerçeği şu anda çok kullanışlı değil, ama en azından çok ilginç.
Aynı zamanda zihnimizi biraz esnetiyor ve kutulardan (`IO` ve `(->) r`) daha çok hesaplama gibi
davranan şeylerin nasıl functors olabileceğini görmemize izin veriyor. Bir hesaplama üzerinden eşlenen fonksiyon, aynı hesaplama ile sonuçlanır,
ancak bu hesaplamanın sonucu, fonksiyonla değiştirilir.

![lifter](../img/lifter.png)
`fmap`'in uyması gereken kurallara geçmeden önce, `fmap`'in türünü bir kez daha düşünelim. Türü `fmap :: (a -> b) -> f a -> f b`'dir.
`(Functor f) =>` sınıf kısıtlamasını kaçırıyoruz, ancak kısalık için burada bıraktık, çünkü yine de functor'lardan bahsediyoruz,
böylece `f`'nin ne anlama geldiğini biliyoruz. [Curried fonksiyonları](../tr/06-higher-order-functions.md#curried-fonksiyonlar) ilk öğrendiğimizde, tüm Haskell fonksiyonlarının aslında bir parametre aldığını söylemiştik. Bir `a -> b -> c` fonksiyonu aslında `a` türünde yalnızca bir parametre alır ve sonra bir parametre alan ve bir `c` döndüren bir `b -> c` fonksiyonu döndürür. Bu şekilde, çok az parametresi olan bir fonksiyonu çağırırsak (yani onu kısmen uygularsak), dışarıda bıraktığımız parametre sayısını alan bir fonksiyonu geri alırız (fonksiyonlar hakkında tekrar birkaç parametre alarak düşünüyorsak). Yani `a -> b -> c`, curried'i daha belirgin hale getirmek için `a -> (b -> c)` olarak yazılabilir.

Aynı şekilde, `fmap :: (a -> b) -> (f a -> f b)` yazarsak, `fmap`'i bir fonksiyon ve bir functor alan ve bir functor döndüren bir fonksiyon olarak düşünmeyebiliriz,
ancak bir fonksiyonu alan ve tıpkı eskisi gibi yeni bir fonksiyon döndüren bir fonksiyon olarak,
yalnızca parametre olarak bir functor alır ve sonuç olarak bir functor döndürür. Bir `a -> b` fonksiyonunu alır ve `f a -> f b` fonksiyonunu döndürür.
Buna bir fonksiyonu kaldırma denir. GHCI'nin `:t` komutunu kullanarak bu fikirle oynayalım:

~~~~ {.haskell: .ghci name="code"}
ghci> :t fmap (*2)  
fmap (*2) :: (Num a, Functor f) => f a -> f a  
ghci> :t fmap (replicate 3)  
fmap (replicate 3) :: (Functor f) => f a -> f [a]  
~~~~

`fmap (*2)` ifadesi, sayılar yerine `f` functor'unu alan ve sayılar yerine bir functor döndüren bir fonksiyondur. Bu functor bir liste olabilir,
`Maybe`, `Either String`, ne olursa olsun. `fmap (replicate 3)` ifadesi, herhangi bir türden bir functor alacak ve bu türden bir öğe listesi üzerinde bir functor döndürecektir.

**Rakamların üzerinde bir functor** dediğimizde, bunu içinde sayılar olan bir functor olarak düşünebilirsiniz.
İlki biraz daha meraklı ve teknik olarak daha doğrudur, ancak ikincisini elde etmek genellikle daha kolaydır.

Kısmen `fmap (++"!")` uygularsak ve sonra onu GHCI'deki bir isme bağlarsak, bu daha da belirgindir.

`fmap`'i ya bir fonksiyon ve bir functor alan ve ardından functor üzerinde çalışan bir fonksiyon olarak düşünebilirsiniz ya da functor üzerinde çalışması için
bir fonksiyonu alan ve bu fonksiyonu kaldıran bir fonksiyon olarak düşünebilirsiniz. Her iki görüş de doğrudur ve Haskell'de eşdeğerdir.

`fmap (replicate 3) :: (Functor f) => f a -> f [a]` türü, işlevin herhangi bir fonksiyon üzerinde çalışacağı anlamına gelir.
Tam olarak ne yapacağı, onu hangi fonksiyon üzerinde kullandığımıza bağlıdır. 
Bir listede `fmap (replicate 3)` kullanırsak, listenin `fmap` uygulaması seçilecektir, bu sadece map'dir.
Onu `Maybe a` üzerinde kullanırsak, `Just` içindeki değere `replicate 3` uygular veya `Nothing` ise `Nothing` olarak kalır.

~~~~ {.haskell: .ghci name="code"}
ghci> fmap (replicate 3) [1,2,3,4]  
[[1,1,1],[2,2,2],[3,3,3],[4,4,4]]  
ghci> fmap (replicate 3) (Just 4)  
Just [4,4,4]  
ghci> fmap (replicate 3) (Right "blah")  
Right ["blah","blah","blah"]  
ghci> fmap (replicate 3) Nothing  
Nothing  
ghci> fmap (replicate 3) (Left "foo")  
Left "foo" 
~~~~

Sırada, **functor yasalarına** bakacağız. Bir şeyin functor olabilmesi için bazı yasaları karşılaması gerekir. 
Tüm functor'ların belirli tür functor'lara benzer özellikler ve davranışlar sergilemeleri beklenir.
Üzerinde eşleşebilecek(mapped) şeyler gibi güvenilir bir şekilde davranmalıdırlar. Bir functor'da `fmap`'i çağırmak sadece functor üzerinde bir fonksiyonu eşlemelidir,
daha fazlası değil. Bu davranış, functor yasalarında açıklanmıştır. Tüm `Functor` instance'larının uyması gereken iki tane var.
Haskell tarafından otomatik olarak uygulanmazlar, bu yüzden bunları kendiniz test etmeniz gerekir.

**İlk functor yasası, id fonksiyonunu bir functor üzerinden eşleştirirsek, geri aldığımız functor'un orijinal functor ile aynı olması gerektiğini belirtir.**
Bunu biraz daha biçimsel olarak yazarsak, bu `fmap id = id` anlamına gelir. Yani esasen bu, bir functor üzerinden `fmap id` yaparsak,
bunun sadece functor'da `id`'yi çağırmakla aynı olması gerektiğini söylüyor. Unutmayın, `id`, değiştirilmemiş parametresini döndüren özdeş fonksiyondur. 
Ayrıca `\x -> x` olarak da yazılabilir. functor'u üzerinde eşlenebilecek bir şey olarak görürsek, `fmap id = id` yasası biraz önemsiz veya açık görünür.

Bakalım bu yasanın birkaç functor değeri için geçerli olup olmadığını görelim.

~~~~ {.haskell: .ghci name="code"}
ghci> fmap id (Just 3)  
Just 3  
ghci> id (Just 3)  
Just 3  
ghci> fmap id [1..5]  
[1,2,3,4,5]  
ghci> id [1..5]  
[1,2,3,4,5]  
ghci> fmap id []  
[]  
ghci> fmap id Nothing  
Nothing  
~~~~

`Maybe` için `fmap` uygulamasına bakarsak, ilk functor yasasının neden geçerli olduğunu anlayabiliriz.

~~~~ {.haskell: .ghci name="code"}
instance Functor Maybe where  
    fmap f (Just x) = Just (f x)  
    fmap f Nothing = Nothing  
~~~~

`id`'nin uygulamada `f` parametresinin rolünü oynadığını düşünüyoruz. Görüyoruz ki, eğer `fmap id`, `Just x` üzerinde ise, sonuç `Just (id x)` olacaktır ve
`id` parametresini döndürdüğü için `Just (id x)`'in `Just x`'e eşit olduğu sonucunu çıkarabiliriz. Şimdi biliyoruz ki,
`id`'yi `Maybe` değerinin üzerine bir `Just` value constructor'yla eşlersek, aynı değeri geri alırız.

Bir `Nothing` değeri üzerinden `"id"` eşlemesinin aynı değeri döndürdüğünü görmek önemsizdir. Dolayısıyla, `fmap` uygulamasındaki bu iki denklemden,
`fmap id = id` yasasının geçerli olduğunu görüyoruz.

![justice](../img/justice.png)
**İkinci yasa, iki fonksiyonu oluşturmanın ve ardından ortaya çıkan fonksiyonu bir functor üzerinde eşlemenin,
önce bir fonksiyonu functor üzerine eşleyip ardından diğerini eşlemeyle aynı olması gerektiğini söyler.**
Biçimsel olarak yazılmış, bu `fmap (f . g) = fmap f . fmap g` anlamına gelir. Veya başka bir şekilde yazmak için, herhangi bir functor *F* için aşağıdakiler geçerli olmalıdır:
`fmap (f . g) F = fmap f (fmap g F)`.

Bazı türlerin her iki functor yasasına da uyduğunu gösterebilirsek, haritalama söz konusu olduğunda diğer functor'larla aynı temel davranışlara sahip olduğuna güvenebiliriz.
Üzerinde `fmap` kullandığımızda, perde arkasında eşleştirmeden başka bir şey olmayacağını ve eşleştirebilecek bir şey, yani bir functor gibi davranacağını bilebiliriz.
O tür için `fmap` uygulamasına bakarak ve sonra `Maybe`'nin birinci yasaya uyup uymadığını kontrol etmek için kullandığımız yöntemi kullanarak,
ikinci yasanın bir tür için nasıl geçerli olduğunu anlarsınız.

İsterseniz, ikinci functor yasasının `Maybe` için nasıl geçerli olduğuna bakabiliriz. `Nothing` yerine `fmap (f . g)` yaparsak,
`Nothing` elde ederiz, çünkü Nothing üzerinde herhangi bir fonksiyonla bir `fmap` yapmak Nothing döndürür. `fmap f (fmap g Nothing)` yaparsak, aynı nedenle `Nothing` elde ederiz. Tamam, ikinci yasanın `Maybe` için nasıl geçerli olduğunu görmek, eğer bir `Nothing` değeriyse oldukça kolay, neredeyse önemsiz.

`Just something` değerine ne dersiniz? Peki, `fmap (f . g) (Just x)` yaparsak, uygulamadan `Just ((f . g) x)` olarak uygulandığını görürüz, bu da tabii ki `Just (f (g x))`.
`fmap f (fmap g (Just x))` yaparsak, uygulamadan `fmap g (Just x)`'in `Just (g x)` olduğunu görürüz. 
Dolayısıyla, `fmap f (fmap g (Just x))` eşittir `fmap f (Just (g x))` ve uygulamadan bunun `Just (f (g x))` olduğunu görüyoruz.

Bu kanıtla kafanız biraz karıştıysa endişelenmeyin. [Fonksiyon bileşimi](../tr/06-higher-order-functions.md#fonksiyon-bileşimi) nasıl çalıştığını anladığınızdan emin olun.
Çoğu zaman, bu yasaların nasıl geçerli olduğunu sezgisel olarak görebilirsiniz çünkü türler kaplar veya fonksiyonlar gibi hareket eder.
Ayrıca, onları bir türden bir dizi farklı değer üzerinde deneyebilir ve bir türün gerçekten de yasalara uyduğunu kesin olarak söyleyebilirsiniz.

`Functor` tür sınıfının bir instance'ı olan ancak gerçekte bir functor olmayan bir type constructor'ının patolojik bir örneğine bakalım, 
çünkü yasaları karşılamıyor. Diyelim ki bir türümüz var:

~~~~ {.haskell: .ghci name="code"}
data CMaybe a = CNothing | CJust Int a deriving (Show)  
~~~~

Buradaki C, sayaç(count) anlamına gelir. Bu, `Maybe a`'ya çok benzeyen bir veri türüdür, yalnızca `Just` bölümünde bir yerine iki alan bulunur.
`CJust` value constructor'daki ilk alan her zaman bir `Int` türüne sahip olacak ve bir tür sayaç olacak ve ikinci alan tür parametresinden gelen `a` türüdür ve 
türü elbette `CMaybe a` için seçtiğimiz somut türü bağlı olacaktır.

~~~~ {.haskell: .ghci name="code"}
ghci> CNothing  
CNothing  
ghci> CJust 0 "haha"  
CJust 0 "haha"  
ghci> :t CNothing  
CNothing :: CMaybe a  
ghci> :t CJust 0 "haha"  
CJust 0 "haha" :: CMaybe [Char]  
ghci> CJust 100 [1,2,3]  
CJust 100 [1,2,3]  
~~~~

`CNothing` contructor'ını kullanırsak, alan yoktur ve `CJust` contructor'ını kullanırsak, ilk alan bir tamsayıdır ve ikinci alan herhangi bir tür olabilir.
Bunu bir `Functor` instance'ı yapalım, böylece `fmap`'i her kullandığımızda, fonksiyon ikinci alana uygulanır, oysa ilk alan 1 artar.

~~~~ {.haskell: .ghci name="code"}
instance Functor CMaybe where  
    fmap f CNothing = CNothing  
    fmap f (CJust counter x) = CJust (counter+1) (f x)  
~~~~

Bu, `Maybe` için instance uygulamasına benzer, ancak boş bir kutuyu (`CJust` değeri) temsil etmeyen bir değer üzerinden `fmap` yaptığımızda,
fonksiyonu içeriğe yalnızca uygulamıyoruz, sayacı da 1 artırıyoruz. Şimdiye kadar her şey yolunda görünüyor, bununla biraz oynayabiliriz:

~~~~ {.haskell: .ghci name="code"}
ghci> fmap (++"ha") (CJust 0 "ho")  
CJust 1 "hoha"  
ghci> fmap (++"he") (fmap (++"ha") (CJust 0 "ho"))  
CJust 2 "hohahe"  
ghci> fmap (++"blah") CNothing  
CNothing  
~~~~

Bu, functor yasalarına uyuyor mu? Bir şeyin bir yasaya uymadığını görmek için tek bir karşı örnek bulmak yeterlidir.

~~~~ {.haskell: .ghci name="code"}
ghci> fmap id (CJust 0 "haha")  
CJust 1 "haha"  
ghci> id (CJust 0 "haha")  
CJust 0 "haha"  
~~~~

Ah! İlk functor yasasının, `id`'yi bir functor üzerinden eşlersek, aynı functor ile `id`'yi çağırmakla aynı olması gerektiğini, ancak bu örnekte gördüğümüz gibi,
`CMaybe` functor'umuz için doğru olmadığını belirttiğini biliyoruz. `Functor` tür sınıfının bir parçası olmasına rağmen,
functor yasalarına uymaz ve bu nedenle bir functor değildir. Eğer birisi bizim `CMaybe` türümüzü bir functor olarak kullanırsa,
onun iyi bir functor gibi functor kanunlarına uymasını beklerlerdi. Bir functor kullandığımızda, önce birkaç fonksiyon oluşturup sonra onları functor üzerinde
eşleştirmemizin veya her bir fonksiyonu arka arkaya bir functor üzerinde eşleştirmemizin bir önemi olmamalıdır.
Ancak `CMaybe` ile bu önemlidir, çünkü kaç kez eşleştirildiğini takip eder. Hiç hoş değil! `CMaybe`'nin functor yasalarına uymasını istiyorsak,
`fmap` kullandığımızda `Int` alanının aynı kalması için bunu yapmalıyız.

İlk başta, functor yasaları biraz kafa karıştırıcı ve gereksiz görünebilir, ancak o zaman bir türün her iki yasaya da uyduğunu bilirsek,
nasıl davranacağına dair belirli varsayımlar yapabileceğimizi görürüz. Bir tür, functor yasalarına uyuyorsa,
o türden bir değerde `fmap` çağırmanın yalnızca fonksiyonu onun üzerinde eşleyeceğini biliyoruz, başka bir şey değil.
Bu, daha soyut ve genişletilebilir bir koda götürür, çünkü herhangi bir functor'un sahip olması gereken davranışlar hakkında mantık yürütmek için yasaları kullanabilir ve
herhangi bir functor üzerinde güvenilir şekilde çalışan fonksiyonlar yapabiliriz.

Standart kütüphanedeki tüm `Functor` instance'ları bu yasalara uyar, ancak bana inanmıyorsanız kendiniz kontrol edebilirsiniz.
Ve bir dahaki sefere bir yazıyı `Functor` instance'ı haline getirdiğinizde, bir dakikanızı ayırıp functor yasalarına uyduğundan emin olun.
Yeterli sayıda functors ile uğraştıktan sonra, ortak özellikleri ve davranışları sezgisel olarak görürsünüz ve
bir türün functor yasalarına uyup uymadığını sezgisel olarak görmek zor değildir. Ancak sezginiz olmasa bile, her zaman uygulama satırının üzerinden satır satır geçebilir ve
yasaların geçerli olup olmadığına bakabilir veya bir karşı instance bulmaya çalışabilirsiniz.

Functor'lara, bir bağlamda değerler üreten şeyler olarak da bakabiliriz. Örneğin, `Just 3` herhangi bir değer çıktılayabileceği veya vermeyeceği bağlamında 3 değerini verir.
`[1,2,3]` üç değer verir - `1`, `2` ve `3`; bağlam, birden çok değer olabileceği veya hiç olmaması olabilir. 
`(+3)` fonksiyonu, hangi parametreye verildiğine bağlı olarak bir değer çıkaracaktır.

Functor'ları değer üreten şeyler olarak düşünürseniz, functor'un çıktısına değeri değiştiren bir dönüşüm eklemek olarak functor'ları eşlemeyi düşünebilirsiniz.
`fmap (+3) [1,2,3]` yaptığımızda, dönüşümü `(+3) [1,2,3]` çıkışına ekleriz, böylece listenin çıkardığı bir sayıya baktığımızda, `(+3)` ona uygulanacaktır.
Diğer bir örnek, fonksiyonlar üzerinden eşleştirme. `fmap (+3) (*3)` yaptığımızda, dönüşümü `(+3)` nihai `(*3)` çıktısına ekleriz.
Buna bu şekilde bakmak bize fmap fonksiyonlarında neden sadece kompozisyon (`fmap (+3) (*3)` eşittir `(+3) . (*3)`, `\x -> ((x*3)+3`),
çünkü `(*3)` gibi bir fonksiyon alırız ve sonra dönüşümü (+3) çıkışına ekleriz. Sonuç hala bir fonksiyondur, ancak ona bir sayı verdiğimizde, 
üçe çarpılacak ve sonra üçe ekleneceği ekli dönüşümden geçecektir. Kompozisyonla olan budur.


Applicative Functor'lar
-----------------------

![present](../img/present.png)
Bu bölümde, Haskell'de `Control.Applicative` modülünde bulunan `Applicative` tür sınıfıyla temsil edilen functors olarak adlandırılan applicative functor'larına bir göz atacağız.

Bildiğiniz gibi, Haskell'deki fonksiyonlar varsayılan olarak curried, yani birkaç parametre alıyor gibi görünen bir fonksiyon aslında yalnızca bir parametre alır ve
sonraki parametreyi alan bir fonksiyon döndürür ve bu böyle devam eder. Bir fonksiyon `a -> b -> c` türündeyse, genellikle iki parametre aldığını ve bir `c` döndürdüğünü söyleriz, ancak aslında bir `a` alır ve bir `b -> c` fonksiyonunu döndürür. Bu yüzden bir fonksiyonu `f x y` veya `(f x) y` olarak adlandırabiliriz.
Bu mekanizma, fonksiyonları yalnızca çok az parametre ile çağırarak kısmen uygulamamıza olanak sağlayan şeydir,
bu da daha sonra diğer fonksiyonlara aktarabileceğimiz fonksiyonlarla sonuçlanır.

Şimdiye kadar, fonksiyonları functor'lar üzerinden eşlerken, genellikle yalnızca bir parametre alan fonksiyonları eşledik.
Peki, iki parametre alan `*` gibi bir fonksiyonu bir functor yerine eşlediğimizde ne olur? Bunun birkaç somut örneğine bir göz atalım.
`Just 3`'e sahipsek ve `fmap (*) (Just 3)` yaparsak, ne elde ederiz? `Functor` için `Maybe`'nin instance uygulamasından, bir `Just something` değeriyse,
fonksiyonu `Just` içindeki `something`'e uygulayacağını biliyoruz. Bu nedenle, `fmap (*) (Just 3)` yapmak `Just ((*) 3)` ile sonuçlanır,
bu da eğer section'ları kullanırsak `Just (*3)` olarak da yazılabilir. İlginç! `Just` içine sarılmış bir fonksiyon elde ederiz!

~~~~ {.haskell: .ghci name="code"}
ghci> :t fmap (++) (Just "hey")  
fmap (++) (Just "hey") :: Maybe ([Char] -> [Char])  
ghci> :t fmap compare (Just 'a')  
fmap compare (Just 'a') :: Maybe (Char -> Ordering)  
ghci> :t fmap compare "A LIST OF CHARS"  
fmap compare "A LIST OF CHARS" :: [Char -> Ordering]  
ghci> :t fmap (\x y z -> x + y / z) [3,4,5,6]  
fmap (\x y z -> x + y / z) [3,4,5,6] :: (Fractional a) => [a -> a -> a]  
~~~~

Bir karakter listesi üzerinde `(Ord a) => a -> a -> Ordering` türüne sahip olan `compare`'ı eşlersek,
`Char -> Ordering` türünde bir fonksiyon listesi elde ederiz çünkü fonksiyon listedeki karakterlerle kısmen uygulanır.
Bu bir `(Ord a) => a -> Ordering` fonksiyonu listesi değildir, çünkü uygulanan ilk `a` bir `Char`'dı ve bu nedenle ikinci `a` `Char` türünde olmaya karar vermelidir.

"Çok parametreli" fonksiyonları functors üzerinden eşleştirerek, içlerinde fonksiyonlar içeren functor'lar elde ettiğimizi görüyoruz.
Birincisi, bu fonksiyonları onların üzerine parametre olarak alan fonksiyonları eşleyebiliriz,
çünkü bir functor içinde ne varsa onu parametre olarak eşlediğimiz fonksiyona verilecektir.

~~~~ {.haskell: .ghci name="code"}
ghci> let a = fmap (*) [1,2,3,4]  
ghci> :t a  
a :: [Integer -> Integer]  
ghci> fmap (\f -> f 9) a  
[9,18,27,36]  
~~~~

Peki ya `Just (3 *)` olan bir functor değerimiz ve `Just 5` olan bir functor değerimiz varsa ve `Just (3 *)`'dan fonksiyonu çıkarıp `Just 5` ile eşlemek istiyorsak ne olur?
Normal functor'larla şansımız kalmadı, çünkü tek destekledikleri normal fonksiyonları mevcut functor'ların üzerine eşlemek.
`\f -> f 9`'u içinde fonksiyonlar bulunan bir functor üzerine eşlediğimizde bile, onun üzerine normal bir fonksiyonu eşleştiriyorduk.
Ancak, `fmap`'in bize sunduğu şeyle, bir functor içindeki bir fonksiyonu başka bir functor üzerine eşleyemeyiz.
`Just` constructor'yla pattern-match uygulayarak ondan fonksiyonu çıkarabilir ve ardından `Just 5` üzerinden eşleyebiliriz, 
ancak bunu yapmanın daha genel ve soyut bir yolunu arıyoruz, bu da functor'lar arasında çalışır.

`Applicative` tür sınıfıyla tanışın. `Control.Applicative` modülünde bulunur ve iki yöntem tanımlar: `pure` ve `<*>`. 
Bunların hiçbiri için varsayılan bir uygulama sağlamaz, bu nedenle bir şeyin applicative bir functor olmasını istiyorsak ikisini de tanımlamamız gerekir.
Sınıf şu şekilde tanımlanır:

~~~~ {.haskell: .ghci name="code"}
class (Functor f) => Applicative f where  
    pure :: a -> f a  
    (<*>) :: f (a -> b) -> f a -> f b  
~~~~

Bu basit üç satırlık class tanımı bize çok şey anlatıyor! İlk satırdan başlayalım. `Applicative` sınıfının tanımını başlatır ve ayrıca bir sınıf kısıtlaması getirir.
`Applicative` tür sınıfının bir type constructor'ını parçası yapmak istiyorsak, önce `Functor`'da olması gerektiğini söylüyor.
Bu nedenle, bir type constructor'ın `Applicative` tür sınıfının bir parçası olduğunu bildiğimizde, `Functor`'da da olduğunu bilirsek, onun üzerinde `fmap` kullanabiliriz.

Tanımladığı ilk yönteme pure denir. Tür bildirimi `pure :: a -> f a`'dır. `f` burada bizim uygulamalı functor instance'ımızın rolünü oynar.
Haskell çok iyi bir tür sistemine sahip olduğundan ve bir fonksiyonun yapabileceği her şey bazı parametreler alıp bir değer döndürdüğü için,
tür bildiriminden çok şey söyleyebiliriz ve bu bir istisna değildir. `pure`, herhangi bir türden bir değer almalı ve
içinde bu değere sahip bir uygulama fonksiyonunu döndürmelidir. İçinde dediğimizde, her zaman incelemeye dayanmadığını görmemize rağmen, 
kutu benzetmesini tekrar kullanıyoruz. Ancak `a -> f a` tür bildirimi hala oldukça açıklayıcıdır.
Bir değer alırız ve onu, içinde sonuç olarak bu değere sahip olan bir uygulama functor'una sararız.

`pure` hakkında daha iyi bir düşünme şekli, onun bir değer aldığını ve onu bir tür
varsayılan (veya saf) bağlama koyduğunu söylemek olabilir - bu değeri hala veren minimal bir bağlam.

`<*>` fonksiyonu gerçekten ilginç. `f (a -> b) -> f a -> f b` tür bildirimine sahiptir. Bu sana bir şey hatırlatıyor mu? Tabii ki, `fmap :: (a -> b) -> f a -> f b`.
Bu bir çeşit güçlendirilmiş `fmap`. `fmap` bir fonksiyonu ve bir functor alıp functor içindeki fonksiyonu uygularken,
`<*>` içinde bir fonksiyonu olan bir functor ve başka bir functor ve ilk fonksiyondan functor gören türden çıkarımlar ve sonra onu ikincisinin üzerine eşler.
Ayıkla dediğimde, aslında ortalama koşarım ve sonra çıkarırım, hatta belki sekans. Neden yakında göreceğiz.
extract dediğimde, aslında run'ı kastettim ve sonra extract ediyorum, hatta belki sequence. Neden yakında göreceğiz.

`Maybe` için `Applicative` instance uygulamasına bir göz atalım.

~~~~ {.haskell: .ghci name="code"}
instance Applicative Maybe where  
    pure = Just  
    Nothing <*> _ = Nothing  
    (Just f) <*> something = fmap f something  
~~~~

Yine, sınıf tanımından, applicative functor rolünü oynayan `f`'nin parametre olarak tek bir somut türü alması gerektiğini görüyoruz,
bu nedenle `instance Applicative Maybe where` yazmak yerine `instance Applicative (Maybe a) where` yazıyoruz.

Öncelikle, `pure`. Daha önce bir şeyi alması ve onu bir applicative functor'una sarması gerektiğini söylemiştik. `pure = Just` yazdık,
çünkü `Just` gibi value constructor'lar normal fonksiyonlardır. Ayrıca `pure x = Just x`'te yazabilirdik.

Sırada `<*>` için tanımımız var. `Nothing`'den bir fonksiyon çıkaramayız çünkü içinde hiçbir fonksiyonu yoktur.
Bu nedenle, `Nothing`'den bir fonksiyonu çıkarmaya çalışırsak, sonucun `Nothing` olduğunu söyleriz. İlk parametre bir Nothing değil, 
içinde bazı fonksiyonlar olan bir `Just` ise, o zaman bu fonksiyonu ikinci parametrenin üzerine eşlemek istediğimizi söyleriz.
Bu aynı zamanda ikinci parametrenin `Nothing` olduğu durumu da dikkate alır, çünkü bir `Nothing` üzerinden herhangi bir fonksiyonla `fmap` yapmak Nothing döndürür.

Dolayısıyla, `Maybe` için `<*>`, fonksiyonu `Just` ise soldaki değerden çıkarır ve doğru değerin üzerine eşler. Parametrelerden herhangi biri `Nothing` ise,
sonuç `Nothing` dir.

Tamam harika harika. Bunu bir koşuşturma verelim.

~~~~ {.haskell: .ghci name="code"}
ghci> Just (+3) <*> Just 9  
Just 12  
ghci> pure (+3) <*> Just 10  
Just 13  
ghci> pure (+3) <*> Just 9  
Just 12  
ghci> Just (++"hahah") <*> Nothing  
Nothing  
ghci> Nothing <*> Just "woot"  
Nothing  
~~~~

Biz yapıyoruz `pure (+3)` ve `Just (+3)` Bu durumda aynı olduğunu görmek. applicative bir bağlamda `Maybe` değerleriyle
uğraşıyorsanız (yani bunları `<*>` ile kullanıyorsanız) `pure` kullanın, aksi takdirde `Just` seçeneğine bağlı kalın.
İlk dört girdi satırı, fonksiyonun nasıl çıkarıldığını ve sonra eşlendiğini gösterir, ancak bu durumda, 
bunlar yalnızca sarmalanmamış fonksiyonları functor'lar üzerinden eşleyerek elde edilebilirdi. Son satır ilginç, çünkü `Nothing`'den bir fonksiyonu çıkarmaya ve
sonra onu bir şeyin üzerine eşleştirmeye çalışıyoruz, bu da elbette `Nothing` ile sonuçlanıyor.

Normal functor'larla, bir fonksiyonu bir functor üzerinden eşleyebilirsiniz ve sonra sonuç kısmen uygulanmış bir fonksiyon olsa bile,
sonucu herhangi bir genel yolla çıkaramazsınız. Öte yandan, geçerli functor'lar, tek bir functor'la birkaç fonksiyon üzerinde çalışmanıza izin verir.
Şu kod parçasına bir göz atın:

~~~~ {.haskell: .ghci name="code"}
ghci> pure (+) <*> Just 3 <*> Just 5  
Just 8  
ghci> pure (+) <*> Just 3 <*> Nothing  
Nothing  
ghci> pure (+) <*> Nothing <*> Just 5  
Nothing  
~~~~

Burada neler oluyor? Adım adım bir göz atalım. `<*>` sol-ilişkilidir, yani `pure (+) <*> Just 3 <*> Just 5`, `(pure (+) <*> Just 3) <*> Just 5` ile aynıdır.
İlk olarak, `+` fonksiyonu bir functor'a yerleştirilir, bu durumda bu fonksiyonu içeren bir `Maybe` değeridir. Yani ilk başta, `Just (+)` olan `pure (+)` var.
Ardından, `Just (+) <*> Just 3` olur. Bunun sonucu `Just (3+)`'dır. Bunun nedeni kısmi uygulama. `+` fonksiyonuna yalnızca `3` uygulamak,
bir parametre alan ve ona `3` ekleyen bir fonksiyonla sonuçlanır. Son olarak, `Just (3+) <*> Just 5` gerçekleştirilir, bu da `Just 8` ile sonuçlanır.

Bu harika değil mi ?! applicative functor'lar ve `pure f <*> x <*> y <*> ...` yapmanın applicative stili,
zorunlu olarak fonksiyonlarla sarmalanmış olmayan parametreleri bekleyen bir fonksiyonu almamıza ve
bu fonksiyon üzerinde functor bağlamlarında bulunan birkaç değer çalışmak için kullanmamıza izin verir. Fonksiyon, istediğimiz kadar parametre alabilir,
çünkü `<*>` oluşumları arasında her zaman kısmen adım adım uygulanır.

Bu, `pure f <*> x`'in `fmap f x`'e eşit olduğu gerçeğini düşünürsek daha da kullanışlı ve belirgin hale gelir. Bu, geçerli kanunlardan biridir.
Onlara daha sonra daha yakından bakacağız, ancak şimdilik bunun böyle olduğunu sezgisel olarak görebiliriz. Bir düşünün, mantıklı.
Daha önce söylediğimiz gibi, pure varsayılan bağlama bir değer koyar. 
Bir fonksiyonu varsayılan bir bağlama koyup sonra onu başka bir applicative fonksiyon içindeki bir değere uygulayıp çıkarırsak,
aynı fonksiyonu o fonksiyon functor'unun üzerine eşlemekle aynı şeyi yaptık. `pure f <*> x <*> y <*> ...` yazmak yerine `fmap f x <*> y <*> ...` yazabiliriz.
Bu nedenle `Control.Applicative`, bir infix operatörü olarak sadece `fmap` olan `<$>` adlı bir fonksiyonu export eder. Nasıl tanımlandığı aşağıda açıklanmıştır:

~~~~ {.haskell: .ghci name="code"}
(<$>) :: (Functor f) => (a -> b) -> f a -> f b  
f <$> x = fmap f x  
~~~~

**Yo!** Hızlı hatırlatma: tür değişkenleri(type variables), parametre adlarından veya diğer değer adlarından bağımsızdır.
Buradaki fonksiyon bildirimindeki `f`, `f`'nin yerini alan herhangi bir type constructor'ın `Functor` tür sınıfında olması gerektiğini söyleyen bir sınıf kısıtlamasına
sahip bir tür değişkenidir. Fonksiyon gövdesindeki `f`, `x` üzerinde eşlediğimiz bir fonksiyonu belirtir.
Her ikisini de temsil etmek için `f`'yi kullanmamız, bir şekilde aynı şeyi temsil ettikleri anlamına gelmez.

`<$>` Kullanarak, applicative stil gerçekten parlar, çünkü şimdi üç applicative functor arasına bir `f` fonksiyonunu uygulamak istersek, `f <$> x <*> y <*> z` yazabiliriz.
Parametreler applicative functor'ler değil de normal değerler olsaydı, `f x y z` yazardık.

Bunun nasıl çalıştığına daha yakından bakalım. Bir `Just "johntra"` değerine ve `Just "volta"` değerine sahibiz ve
onları bir `Maybe` functor'nda tek bir `String` olarak birleştirmek istiyoruz. Bunu yaparız:

~~~~ {.haskell: .ghci name="code"}
ghci> (++) <$> Just "johntra" <*> Just "volta"  
Just "johntravolta"  
~~~~

Bunun nasıl olduğunu görmeden önce, yukarıdaki satırı bununla karşılaştırın:

~~~~ {.haskell: .ghci name="code"}
ghci> (++) "johntra" "volta"  
"johntravolta"  
~~~~

Harika! applicative functor'lar üzerinde normal bir fonksiyon kullanmak için, sadece biraz `<$>` ve `<*>` serpiştirin ve fonksiyon applicative çalışacak ve
bir applicative'i iade edecektir. Ne kadar havalı?

Her neyse, `(++) <$> Just "johntra" <*> Just "volta"`, ilk `(++)`, `(++) :: [a] -> [a] -> [a]` türünde olan, `Just "johntra"` ile eşlenir ve
`Just ("johntra"++)` ile aynı olan ve `Maybe ([Char] -> [Char])` türüne sahip bir değerle sonuçlanır.
`(++)`'nın ilk parametresinin nasıl yenildiğine ve a'nın nasıl `Char`'a dönüştüğüne dikkat edin.
Ve şimdi `Just ("johntra"++) <*> Just "volta"` gerçekleşir, bu da fonksiyonu `Just`'dan çıkarır ve onu `Just "volta"` ile eşler ve `Just "johntravolta"` ile sonuçlanır.
İki değerden herhangi biri `Nothing` olsaydı, sonuçta `Nothing` olurdu.

Şimdiye kadar, instance'larımızda yalnızca `Maybe`'yi kullandık ve applicative functor'unun tamamının `Maybe` ile ilgili olduğunu düşünüyor olabilirsiniz.
`Applicative`'in başka pek çok instance'ı var, hadi gidip onlarla tanışalım!

Listeler (aslında liste type contructor `[]`) applicative functors'larıdır. Ne sürpriz! `Applicative` instance'ının `[]` nasıl olduğu aşağıda açıklanmıştır:

~~~~ {.haskell: .ghci name="code"}
instance Applicative [] where  
    pure x = [x]  
    fs <*> xs = [f x | f <- fs, x <- xs]  
~~~~

Daha önce, `pure`'ın bir değer aldığını ve onu varsayılan bir bağlama koyduğunu söylemiştik. Ya da başka bir deyişle, yine de bu değeri veren minimal bir bağlam.
Listeler için minimum bağlam, boş liste olabilir `[]`, ancak boş liste bir değerin eksikliğini temsil eder, bu nedenle `pure` olarak kullandığımız değeri kendi içinde tutamaz.
Bu yüzden `pure` bir değer alır ve onu tekli listeye koyar. Benzer şekilde, `Maybe` applicative functor'u için minimum bağlam Nothing olacaktır,
ancak bir değer yerine bir değerin eksikliğini temsil eder, bu nedenle `pure`, `Maybe` instance'ının uygulamasında `Just` olarak uygulanır.

~~~~ {.haskell: .ghci name="code"}
ghci> pure "Hey" :: [String]  
["Hey"]  
ghci> pure "Hey" :: Maybe String  
Just "Hey"  
~~~~

Peki ya `<*>`? `<*>` Türünün sadece listelerle sınırlı olsaydı ne olacağına bakarsak, `(<*>) :: [a -> b] -> [a] -> [b]` elde ederiz. Bir [liste anlayışı](../tr/02-starting-out.md#ben-bir-liste-anlayışıyımlist-comprehension) ile uygulanmaktadır.
`<*>`, bir şekilde fonksiyonu sol parametresinden çıkarmalı ve ardından onu sağ parametre üzerinde eşlemelidir.
Ancak buradaki şey, soldaki listenin sıfır fonksiyonu, bir fonksiyonu veya içinde birkaç fonksiyonu olabileceğidir. Sağdaki liste birkaç değeri de tutabilir.
Bu nedenle, her iki listeden de bir list comprehension'ını kullanıyoruz. Soldaki listedeki olası her fonksiyonu, sağdaki listedeki olası her değere uygularız
Ortaya çıkan liste, soldaki listeden bir fonksiyonu sağdaki bir değere uygulamanın olası her kombinasyonuna sahiptir.

~~~~ {.haskell: .ghci name="code"}
ghci> [(*0),(+100),(^2)] <*> [1,2,3]  
[0,0,0,101,102,103,1,4,9]  
~~~~

Soldaki listenin üç fonksiyonu vardır ve sağdaki listenin üç değeri vardır, bu nedenle ortaya çıkan liste dokuz öğeye sahip olacaktır.
Soldaki listedeki her fonksiyon, sağdaki her fonksiyona uygulanır. İki parametre alan fonksiyonlar listemiz varsa, bu fonksiyonları iki liste arasında uygulayabiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> [(+),(*)] <*> [1,2] <*> [3,4]  
[4,5,5,6,3,4,6,8]  
~~~~

`<*>` Sol-ilişkisel olduğundan, `[(+),(*)] <*> [1,2]` önce gelir ve `[(1+),(2+),(1*),(2*)]` aynı listeyle sonuçlanır, çünkü soldaki her fonksiyon sağdaki her değere uygulanır.
Ardından, final sonucu üreten `[(1+),(2+),(1*),(2*)] <*> [3,4]` olur.

Applicative stili listelerle kullanmak eğlencelidir! İzleyin:

~~~~ {.haskell: .ghci name="code"}
ghci> (++) <$> ["ha","heh","hmm"] <*> ["?","!","."]  
["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]  
~~~~

Yine, sadece uygun applicative operatörlerini ekleyerek string'lerin iki applicative functor'ı arasında iki string'i alan normal bir fonksiyonu nasıl kullandığımıza bakın.

Listeleri deterministik olmayan hesaplamalar olarak görüntüleyebilirsiniz. `100` veya `"what"` gibi bir değer, yalnızca bir sonucu olan deterministik bir hesaplama olarak
görülebilirken, `[1,2,3]` gibi bir liste, hangi sonuca sahip olmak istediğine karar veremeyen bir hesaplama olarak görülebilir, bu nedenle bize olası tüm sonuçları sunar.
Dolayısıyla, `(+) <$> [1,2,3] <*> [4,5,6]` gibi bir şey yaptığınızda, bunu iki deterministik olmayan hesaplamayı `+` ile,
yalnızca sonucu hakkında daha da az emin olan başka bir deterministik olmayan hesaplama üretir.

Applicative stili listelerde kullanmak, genellikle list comprehension'lar için iyi bir alternatiftir.
İkinci bölümde, `[2,5,10]` ve `[8,10,11]`'in tüm olası ürünlerini görmek istedik, bu yüzden şunu yaptık:

~~~~ {.haskell: .ghci name="code"}
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]     
[16,20,22,40,50,55,80,100,110]     
~~~~

Sadece iki listeden çizim yapıyoruz ve her eleman kombinasyonu arasına bir fonksiyon uyguluyoruz. Bu, applicative tarzında da yapılabilir:

~~~~ {.haskell: .ghci name="code"}
ghci> (*) <$> [2,5,10] <*> [8,10,11]  
[16,20,22,40,50,55,80,100,110]  
~~~~

Bu bana daha net görünüyor, çünkü belirleyici olmayan iki hesaplama arasında sadece `*` çağırdığımızı görmek daha kolay.
Bu iki listenin 50'den fazla olası tüm ürünlerini isteseydik, şunu yapardık:

~~~~ {.haskell: .ghci name="code"}
ghci> filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]  
[55,80,100,110]  
~~~~

Listelerle `pure f <*> xs`'nin `fmap f xs`'e eşit olduğunu görmek kolaydır. `pure f` yalnızca `[f]`'dir ve `[f] <*> xs` soldaki listedeki her fonksiyonu sağdaki her değere
uygulayacaktır, ancak soldaki listede yalnızca bir fonksiyon vardır, bu nedenle eşleme gibidir.

Daha önce karşılaştığımız bir başka `Applicative` instance'ı da `IO`. Instance şu şekilde uygulanır:

~~~~ {.haskell: .ghci name="code"}
instance Applicative IO where  
    pure = return  
    a <*> b = do  
        f <- a  
        x <- b  
        return (f x)  
~~~~

![knight](../img/knight.png)
`pure`, bir değeri yine de sonucu olarak tutan minimal bir bağlama yerleştirmekle ilgili olduğundan, `pure`'un sadece `return` olduğu mantıklıdır,
çünkü `return` tam olarak bunu yapar; hiçbir şey yapmayan bir I/O eylemi yapar, sadece sonuç olarak bir değer verir, ancak terminale yazdırma veya
bir dosyadan okuma gibi herhangi bir I/O işlemini gerçekten yapmaz.

`<*>` `IO` için özelleştirilmiş olsaydı, bir `(<*>) :: IO (a -> b) -> IO a -> IO b` türüne sahip olurdu. 
Sonuç olarak bir fonksiyon ve başka bir I/O eylemi veren bir I/O eylemi gerçekleştirir ve bu ikisinden yeni bir I/O eylemi oluşturur;
ikincisi değeri almak için ve sonra sonuç olarak değere uygulanan fonksiyonu verir. Burada uygulamak için do sözdizimi kullandık.
Unutmayın, do sözdizimi birkaç I/O eylemi almak ve bunları tek bir işlemde yapıştırmakla ilgilidir, biz de burada tam olarak bunu yapıyoruz.

`Maybe`, `[]` ile `<*>` basitçe sol parametresinden bir fonksiyonu çıkarmak ve sonra onu sağdaki parametreye uygulamak olarak düşünebiliriz.
`IO` ile, ayıklama hala oyunda, ancak şimdi bir sequencing(sıralama) kavramına da sahibiz, çünkü iki I/O eylemi yapıyoruz ve bunları bire sıralıyoruz veya yapıştırıyoruz.
Fonksiyon ilk I/O eyleminden çıkarmalıyız, ancak bir I/O eyleminden bir sonuç çıkarmak için gerçekleştirilmesi gerekir.

Bunu düşün:

~~~~ {.haskell: .ghci name="code"}
myAction :: IO String  
myAction = do  
    a <- getLine  
    b <- getLine  
    return $ a ++ b 
~~~~

Bu, kullanıcıyı iki satır için uyaracak ve sonuç olarak bu iki satırı birleştiren bir I/O eylemidir. Bunu, iki `getLine` I/O eylemini ve
bir `return`'ü birbirine yapıştırarak başardık, çünkü yeni yapıştırılmış I/O eylemimizin `a ++ b` sonucunu tutmasını istedik.
Bunu yazmanın başka bir yolu da applicative stili kullanmak olacaktır. 

~~~~ {.haskell: .ghci name="code"}
myAction :: IO String  
myAction = (++) <$> getLine <*> getLine  
~~~~

Daha önce yaptığımız şey, diğer iki I/O eyleminin sonuçları arasında bir
fonksiyon uygulayan bir I/O eylemi yapmaktı ve bu aynı şey. Unutmayın, `getLine` `getLine :: IO String` türünde bir I/O eylemidir.
İki applicative functor arasında `<*>` kullandığımızda, sonuç applicative bir functor'dur,bu nedenle bunların hepsi mantıklıdır.

Kutu benzetmesine geri dönersek, `getLine`'ı gerçek dünyaya çıkıp bize bir string getirecek bir kutu olarak hayal edebiliriz.
`(++) <$> getLine <*> getLine` yapmak, bu iki kutuyu terminalden satırları getirmeye gönderen ve ardından bu iki satırın birleştirilmesini sonuç olarak sunan yeni,
daha büyük bir kutu oluşturur.

`(++) <$> getLine <*> getLine` ifadesinin türü `IO String`'dir; bu, bu ifadenin diğerleri gibi tamamen normal bir I/O eylemi olduğu anlamına gelir,
bu da içinde bir sonuç değeri tutar, sadece diğer I/O eylemleri gibi. Bu yüzden aşağıdaki gibi şeyler yapabiliriz:

~~~~ {.haskell: .ghci name="code"}
main = do  
    a <- (++) <$> getLine <*> getLine  
    putStrLn $ "The two lines concatenated turn out to be: " ++ a  
~~~~

Kendinizi bazı I/O eylemlerini adlara bağlarken ve sonra bunlarda bazı fonksiyonları çağırıp bunu sonuç olarak `return` kullanarak sunarken bulursanız,
applicative stili kullanmayı düşünün çünkü muhtemelen biraz daha kısa ve özdür.

`Applicative`'in başka bir instance'ı `(->) r`'dir, yani fonksiyonlar. Kod golfü dışında applicative stili ile nadiren kullanılırlar, ancak yine de applicative olarak
ilgi çekicidirler, bu yüzden fonksiyon instance'ının nasıl uygulandığına bir göz atalım.

`(->) r`'nin ne anlama geldiği konusunda kafanız karıştıysa, `(->) r`'nin nasıl bir functor olduğunu açıkladığımız önceki bölüme bakın.

~~~~ {.haskell: .ghci name="code"}
instance Applicative ((->) r) where  
    pure x = (\_ -> x)  
    f <*> g = \x -> f x (g x)  
~~~~

Bir değeri bir applicative functor'una `pure` ile sardığımızda, verdiği sonuç her zaman bu değer olmalıdır. Sonuç olarak hala bu değeri veren minimum varsayılan bağlam.
Bu nedenle, fonksiyon instance'ı uygulamasında, `pure` bir değer alır ve parametresini yok sayan ve her zaman bu değeri döndüren bir fonksiyon oluşturur.
`pure` türüne bakarsak, ancak `(->) r` instance'ı için özelleşirsek, bu `pure :: a -> (r -> a)` olur.

~~~~ {.haskell: .ghci name="code"}
ghci> (pure 3) "blah"  
3  
~~~~

Currying nedeniyle, fonksiyon uygulaması sola ilişkilidir, bu nedenle parantezleri atlayabiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> pure 3 "blah"  
3  
~~~~

`<*>` için instance uygulaması biraz şifreli, bu nedenle en iyisi, applicative stilinde applicative functor'ları olarak fonksiyonların nasıl kullanılacağına bir göz atmaktır.

~~~~ {.haskell: .ghci name="code"}
ghci> :t (+) <$> (+3) <*> (*100)  
(+) <$> (+3) <*> (*100) :: (Num a) => a -> a  
ghci> (+) <$> (+3) <*> (*100) $ 5  
508  
~~~~

`<*>`'yi iki applivative functor ile çağırmak applivative bir functor ile sonuçlanır, bu yüzden eğer onu iki fonksiyonda kullanırsak, bir fonksiyonu geri alırız.
Peki burada neler oluyor? `(+) <$> (+3) <*> (*100)` yaptığımızda, `(+3)` ve `(*100)` sonuçlarında `+` kullanıp bunu döndürecek bir fonksiyon yapıyoruz.
Gerçek bir örnek üzerinde göstermek için, `(+) <$> (+3) <*> (* 100) $ 5` yaptığımızda, `5`, ilk olarak `(+3)` ve `(* 100)`'e uygulandı ve sonuçta `8` ve `500`.
Ardından `8` ve `500` ile `+` çağrılır ve `508` ile sonuçlanır.

~~~~ {.haskell: .ghci name="code"}
ghci> (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5  
[8.0,10.0,2.5]  
~~~~

![jazzb](../img/jazzb.png)
Burada aynı. `(+3)`, `(*2)` ve `(/2)`'den sonuçlarla `\x y z -> [x, y, z]` fonksiyonunu çağıracak bir fonksiyon yaratıyoruz.
`5`, üç fonksiyonun her biriyle beslenir ve sonra bu sonuçlarla `\x y z -> [x, y, z]` çağrılır.

Fonksiyonları nihai sonuçlarını içeren kutular olarak düşünebilirsiniz, bu nedenle `k <$> f <*> g` yapmak,
`f` ve `g`'nin nihai sonuçlarıyla `k`'yi çağıracak bir fonksiyon oluşturur. `(+) <$> Just 3 <*> Just 5` gibi bir şey yaptığımızda,
orada olabilecek veya olmayabilecek değerlerde `+` kullanıyoruz, bu da orada olabilecek veya olmayabilecek bir değerle sonuçlanıyor.
`(+) <$> (+10) <*> (+5)` yaptığımızda, `(+10)` ve `(+5)`'in gelecekteki dönüş değerleri için `+` kullanıyoruz ve 
sonuç aynı zamanda yalnızca bir parametre ile çağrıldığında bir değer üretecektir.

Fonksiyonları applicative olarak sıklıkla kullanmıyoruz, ancak bu yine de gerçekten ilginç. Applicative için `(->) r` instance'ının nasıl çalıştığını anlamanız çok önemli değil,
bu yüzden şu anda bunu anlamıyorsanız umutsuzluğa kapılmayın. `Applicative` olarak fonksiyonlar için bir sezgi oluşturmak için
applicative stil ve fonksiyonlarla oynamayı deneyin.

Henüz karşılaşmadığımız bir `Applicative` instance'ı `ZipList`'tir ve `Control.Applicative`'de yaşamaktadır. 

Listelerin applicative functor'lar olması için aslında daha fazla yol olduğu ortaya çıktı. Daha önce ele aldığımız yollardan biri,
bir fonksiyon listesi ve bir değer listesi ile `<*>` çağrısının, soldaki listeden sağ listedeki değerlere fonksiyon uygulama olası tüm kombinasyonlarını içeren bir listeyle
sonuçlandığını söyleyen yol. Eğer biz `[(+3), (* 2)] <*> [1,2], (+3)` yaparsak hem `1` hem de `2`'ye uygulanacak ve `(*2)` de aynı şekilde hem `1` hem de `2`'ye uygulanacak,
son durumda `[4,5,2,4]` olmak üzere dört öğeye sahip bir listeyle sonuçlanır.

Bununla birlikte, `[(+3), (* 2)] <*> [1,2]`, soldaki listedeki ilk fonksiyonun sağdaki ilk değere, ikinci fonksiyonun uygulandığı şekilde de çalışabilir ve bu böyle devam eder.
Bu, `[4,4]` olmak üzere iki değer içeren bir listeyle sonuçlanır. Buna `[1 + 3, 2 * 2]` olarak bakabilirsiniz.

Bir tür aynı tür sınıfı için iki instance'a sahip olamayacağından, yalnızca bir alanı ve `ZipList` constructor'u olan `ZipList a` türü tanıtıldı ve bu alan bir listedir.
Instance burada:

~~~~ {.haskell: .ghci name="code"}
instance Applicative ZipList where  
        pure x = ZipList (repeat x)  
        ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)  
~~~~

`<*>` dediğimiz şeyi yapar. İlk fonksiyonu ilk değere, ikinci fonksiyonu ikinci değere vb. uygular. Bu, `zipWith (\f x-> f x) fs xs` ile yapılır.
`zipWith`'in nasıl çalıştığından dolayı, ortaya çıkan liste iki listeden daha kısa olanı kadar uzun olacaktır.

`pure` burada da ilginçtir. Bir değeri alır ve onu, belirsiz olarak tekrar eden bu değere sahip bir listeye koyar.
`pure "haha"`, `ZipList (["haha","haha","haha"...` ile sonuçlanır. Bu biraz kafa karıştırıcı olabilir, çünkü `pure`'un minimum bağlamda yine bu değeri veren bir
değer koyması gerektiğini söylediğimizden. Ve bir şeylerin sonsuz bir listesinin pek de asgari olmadığını düşünüyor olabilirsiniz.
Ancak zip listelerinde mantıklıdır, çünkü değeri her pozisyonda üretmesi gerekir. Bu aynı zamanda `pure f <*> xs`'nin `fmap f xs`'e eşit olması gerektiği yasasını da karşılar.
`pure 3` az önce `ZipList [3]` döndürdüyse, `pure (*2) <*> ZipList [1,5,10]`, `ZipList [2]` ile sonuçlanır, 
çünkü sonuçta iki sıkıştırılmış liste ikisinden kısa olanın uzunluğuna sahiptir. Sonlu bir listeyi sonsuz bir listeyle sıkıştırırsak,
ortaya çıkan listenin uzunluğu her zaman sonlu listenin uzunluğuna eşit olacaktır.

Peki zip listeleri nasıl uygulanabilir bir tarzda çalışır? Bakalım. Oh, `ZipList a` türünün `Show` instance'ı yoktur, bu nedenle zip listesinden ham bir liste çıkarmak için
`getZipList` fonksiyonunu kullanmalıyız.

~~~~ {.haskell: .ghci name="code"}
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100]  
[101,102,103]  
ghci> getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100..]  
[101,102,103]  
ghci> getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2]  
[5,3,3,4]  
ghci> getZipList $ (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "rat"  
[('d','c','r'),('o','a','a'),('g','t','t')]  
~~~~

`(,,)` fonksiyonu `\x y z -> (x,y,z)` ile aynıdır. Ayrıca, `(,)` fonksiyonu `\x y -> (x,y)` ile aynıdır. 

`zipWith` dışında, standart kütüphanede `zipWith3`, `zipWith4` gibi `7`'ye kadar fonksiyonlara sahiptir. 
`zipWith`, iki parametre alan ve onunla iki listeyi sıkıştıran bir fonksiyonu alır.
`zipWith3`, üç parametre alan ve onunla üç listeyi sıkıştıran bir fonksiyonu alır ve bu böyle devam eder.
Applicative bir stile sahip zip listeleri kullanarak, birlikte sıkıştırmak istediğimiz her liste sayısı için ayrı bir zip fonksiyonuna sahip olmamıza gerek kalmaz.
Bir fonksiyonu olan rastgele sayıda listeyi bir araya getirmek için sadece applicative stili kullanıyoruz ve bu oldukça havalı.

`Control.Applicative`, `liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c` türüne sahip `liftA2` adlı bir fonksiyonu tanımlar. Şöyle tanımlanır:

~~~~ {.haskell: .ghci name="code"}
liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f a b = f <$> a <*> b  
~~~~

Özel bir şey değil, sadece aşina olduğumuz applicative stili gizleyerek iki başvuru arasında bir fonksiyon uygular.
Buna bakmamızın nedeni, applicative functor'larının neden sıradan functor'lardan daha güçlü olduğunu açıkça göstermesidir.
Sıradan functor'larla, fonksiyonları tek bir functor üzerinden eşleyebilirsiniz. Ancak applicative functor'larla, birkaç functor arasında bir fonksiyon uygulayabiliriz.
Bu fonksiyon türüne `(a -> b -> c) -> (f a -> f b -> f c)` olarak bakmak da ilginçtir. Bu şekilde baktığımızda, `liftA2`'nin normal bir ikili fonksiyonu aldığını ve
onu iki functor üzerinde çalışan bir fonksiyona yükselttiğini söyleyebiliriz.

İşte ilginç bir kavram: iki applicative functor'ını alıp bunları içinde bir listede bu iki applicative functor'ın sonuçlarını içeren
tek bir applicative functor'nda birleştirebiliriz. Örneğin, `Just 3` ve `Just 4`'ümüz var. İkincinin içinde tekli bir liste olduğunu varsayalım,
çünkü bunu başarmak gerçekten çok kolay:

~~~~ {.haskell: .ghci name="code"}
ghci> fmap (\x -> [x]) (Just 4)  
Just [4]  
~~~~

Tamam, diyelim ki bizde `Just 3` ve `Just [4]` var. Nasıl `Just [3,4]` elde ederiz? Kolay.

~~~~ {.haskell: .ghci name="code"}
ghci> liftA2 (:) (Just 3) (Just [4])  
Just [3,4]  
ghci> (:) <$> Just 3 <*> Just [4]  
Just [3,4]  
~~~~

Unutmayın, `:` bir öğeyi ve bir listeyi alan ve başında bu öğeyle yeni bir liste döndüren bir fonksiyondur.
Şimdi `Just [3,4]` olduğuna göre, `Just [2,3,4]` üretmek için bunu `Just 2` ile birleştirebilir miyiz? Elbette yapabiliriz.
Görünüşe göre, herhangi bir sayıda applicative'i, içinde bu applicative'lerin sonuçlarının bir listesini içeren tek bir applicative'de birleştirebiliriz.
Bir applicative listesi alan ve sonuç değeri olarak bir listeye sahip bir applicative döndüren bir fonksiyonu uygulamayı deneyelim.
Buna `sequenceA` adını vereceğiz.

~~~~ {.haskell: .ghci name="code"}
sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA [] = pure []  
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs  
~~~~

Ah, özyineleme! İlk önce türe bakıyoruz. Bir applicatives listesini, bir liste ile applicative haline dönüştürecektir.
Bundan, bir uç durumu için biraz zemin hazırlayabiliriz. Boş bir listeyi, sonuç listesiyle birlikte applicative bir listeye dönüştürmek istiyorsak,
sadece varsayılan bağlamda boş bir liste koyarız. Şimdi özyineleme geliyor. Başı ve kuyruğu olan bir listemiz varsa
(unutmayın, `x` bir applicative'dir ve `xs` bunların bir listesidir), kuyrukta `sequenceA`'yı çağırırız, bu da listeli bir applicative ile sonuçlanır.
Ardından, applicative `x`'in içindeki değeri bir liste ile o uygulamaya ekleriz ve işte bu kadar!

Öyleyse, `sequenceA [Just 1, Just 2]` yaparsak, bu `(:) <$> Just 1 <*> sequenceA [Just 2]` olur.
Bu, `(:) <$> Just 1 <*> ((:) <$> Just 2 <*> sequenceA [])` eşittir. Ah! `sequenceA []`'nın `Just []` olarak bittiğini biliyoruz,
bu yüzden bu ifade artık `(:) <$> Just 1 <*> ((:) <$> Just 2 <*> Just [])`, yani `(:) <$> Just 1 <*> Just [2]`, yani `Just[1,2]`!

`sequenceA`'yı uygulamanın başka bir yolu fold'dur. Unutmayın, bir liste öğesinin üzerinden öğeye göre gittiğimiz ve 
yol boyunca bir sonuç topladığımız hemen hemen her fonksiyon bir fold ile uygulanabilir.

~~~~ {.haskell: .ghci name="code"}
sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA = foldr (liftA2 (:)) (pure [])  
~~~~

Listeye sağdan yaklaşıyoruz ve `pure []` toplam değeriyle başlıyoruz. toplayıcı ile listenin son elemanı arasında `liftA2 (:)` yapıyoruz,
bu da içinde bir singleton olan bir applicative ile sonuçlanıyor. Sonra, son eleman ve mevcut toplayıcıyla `liftA2 (:)` yaparız ve böyle devam eder,
ta ki tüm applicative'lerin sonuçlarının bir listesini tutan akümülatörle kalana kadar.

Fonksiyonumuza bazı uygulamalarla koşturalım.

~~~~ {.haskell: .ghci name="code"}
ghci> sequenceA [Just 3, Just 2, Just 1]  
Just [3,2,1]  
ghci> sequenceA [Just 3, Nothing, Just 1]  
Nothing  
ghci> sequenceA [(+3),(+2),(+1)] 3  
[6,5,4]  
ghci> sequenceA [[1,2,3],[4,5,6]]  
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  
ghci> sequenceA [[1,2,3],[4,5,6],[3,4,4],[]]  
[]  
~~~~

Ah! Oldukça havalı. `Maybe` değerlerinde kullanıldığında, `sequenceA`, içindeki tüm sonuçları liste halinde içeren bir Maybe değeri oluşturur.
Değerlerden biri `Nothing` ise, sonuç da `Nothing` olur. `Maybe` değerlerinin bir listesine sahip olduğunuzda ve yalnızca hiçbiri `Nothing` değilse 
değerlerle ilgilendiğinizde bu harikadır.

`sequenceA`, fonksiyonlarla birlikte kullanıldığında bir fonksiyonlar listesi alır ve bir liste döndüren bir fonksiyon döndürür.
Örneğimizde, parametre olarak bir sayı alan ve listedeki her bir fonksiyona uygulayan ve ardından bir sonuç listesi veren bir fonksiyon yaptık.
`sequenceA [(+3),(+2),(+1)] 3`'ü, `(+2)` `3`'ü ve `(+1)` `3`'ü arayacak ve tüm bu sonuçları liste halinde sunacaktır.

`(+) <$> (+3) <*> (*2)` yapmak, bir parametre alan, onu hem `(+3)` hem de `(*2)`'ye besleyen ve ardından bu iki sonuçla `+` çağıran bir fonksiyon oluşturacaktır. 
Aynı şekilde, `sequenceA [(+3),(*2)]`'nin bir parametreyi alan ve listedeki tüm fonksiyonları besleyen bir fonksiyon yaptığı anlamlıdır.
Fonksiyonların sonuçlarıyla `+` çağırmak yerine, bu sonuçları o fonksiyonun sonucu olan bir listede toplamak için `:` ve `pure []` kombinasyonu kullanılır.

`sequenceA`'yı kullanmak, bir fonksiyonlar listemiz olduğunda harikadır ve hepsine aynı girdiyi beslemek ve ardından sonuçların listesini görüntülemek istiyoruz.
Örneğin, bir numaramız var ve bir listedeki tüm predicate'leri karşılayıp karşılamadığını merak ediyoruz. Bunu yapmanın bir yolu şuna benzer:

~~~~ {.haskell: .ghci name="code"}
ghci> map (\f -> f 7) [(>4),(<10),odd]  
[True,True,True]  
ghci> and $ map (\f -> f 7) [(>4),(<10),odd]  
True  
~~~~

Unutmayın, `and` mantıksal değerlerin bir listesini alır ve hepsi `True` ise `True` döndürür. Aynı şeyi başarmanın başka bir yolu da `sequenceA` ile olabilir:

~~~~ {.haskell: .ghci name="code"}
ghci> sequenceA [(>4),(<10),odd] 7  
[True,True,True]  
ghci> and $ sequenceA [(>4),(<10),odd] 7  
True  
~~~~

`sequenceA [(>4),(<10),odd]`, bir sayıyı alıp `[(>4), (<10), odd]`'daki tüm predicate'leri besleyecek ve dönüş yapacak bir fonksiyon oluşturur boolean'ların listesi.
`(Num a) => [a -> Bool]` türündeki bir listeyi `(Num a) => a -> [Bool]` türünde bir fonksiyona dönüştürür. Oldukça temiz, ha?

Listeler homojen olduğundan, listedeki tüm fonksiyonlar elbette aynı türde fonksiyonlar olmalıdır. `[ord, (+3)]` gibi bir listeniz olamaz çünkü `ord` bir karakter alır ve 
bir sayı döndürür, oysa `(+3)` bir sayı alır ve bir sayı döndürür.

`[]`, `sequenceA` ile birlikte kullanıldığında, `sequenceA` bir liste listesi alır ve bir liste listesi döndürür. Hmm, ilginç.
Aslında, öğelerinin tüm olası kombinasyonlarını içeren listeler oluşturur. Örnek olarak, işte önce sequencerA ile yapılır ve
daha sonra bir liste anlayışıyla ile yapılır:

~~~~ {.haskell: .ghci name="code"}
ghci> sequenceA [[1,2,3],[4,5,6]]  
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  
ghci> [[x,y] | x <- [1,2,3], y <- [4,5,6]]  
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]  
ghci> sequenceA [[1,2],[3,4]]  
[[1,3],[1,4],[2,3],[2,4]]  
ghci> [[x,y] | x <- [1,2], y <- [3,4]]  
[[1,3],[1,4],[2,3],[2,4]]  
ghci> sequenceA [[1,2],[3,4],[5,6]]  
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]  
ghci> [[x,y,z] | x <- [1,2], y <- [3,4], z <- [5,6]]  
[[1,3,5],[1,3,6],[1,4,5],[1,4,6],[2,3,5],[2,3,6],[2,4,5],[2,4,6]]  
~~~~

Bunu kavramak biraz zor olabilir, ancak onunla bir süre oynarsanız, nasıl çalıştığını göreceksiniz. Diyelim ki `sequenceA [[1,2],[3,4]]` yapıyoruz.
Bunun nasıl olduğunu görmek için, `sequenceA`'nın `sequenceA (x: xs) = (:) <$> x <*> sequenceA xs` tanımını ve `sequenceA [] = pure []` kenar koşulunu kullanalım.
Bu değerlendirmeyi takip etmek zorunda değilsiniz, ancak `sequenceA`'nın liste listeleri üzerinde nasıl çalıştığını hayal etmekte güçlük çekiyorsanız size yardımcı olabilir,
çünkü biraz kafa karıştırıcı olabilir.

- `sequenceA [[1,2], [3,4]]` ile başlıyoruz
- Bu, `(:) <$> [1,2] <*> sequenceA [[3,4]]` olarak değerlendirilir.
- İçteki `sequenceA`'yı daha da değerlendirdiğimizde, `(:) <$> [1,2] <*> ((:) <$> [3,4] <*> sequenceA [])` elde ederiz.
- Uç koşuluna ulaştık, şimdi bu `(:) <$> [1,2] <*> ((:) <$> [3,4] <*> [[]])`
- Şimdi, sol listedeki olası her değerle (olası değerler `3` ve `4`) `:` kullanacak `(:) <$> [3,4] <*> [[]]` bölümünü değerlendiriyoruz sağdaki listedeki her olası değer (yalnızca olası değer `[]`), `[3: []", "4: []]`, yani `[[3], [4]]`. Şimdi elimizde `(:) <$> [1,2] <*> [[3], [4]]` var.
- Şimdi, `:` sol listedeki (`1` ve `2`) olası her değerle ve sağdaki listedeki (`[3]` ve `[4]`) olası her değerle birlikte kullanılır ve `[1: [3], 1 : [4], 2: [3], 2: [4]]`, yani `[[1,3], [1,4], [2,3], [2,4]` 

`(+) <$> [1,2] <*> [4,5,6]` yapmak deterministik olmayan bir hesaplama ile sonuçlanır `x + y` burada `x`, `[1,2]`'den her değeri alır ve `y` onu alır `[4,5,6]`'dan her bir değer. Bunu olası tüm sonuçları içeren bir liste olarak temsil ediyoruz. Benzer şekilde, `sequence [[1,2],[3,4],[5,6],[7,8]]` yaptığımızda,
sonuç deterministik olmayan bir hesaplamadır `[x, y, z, w ]`, burada `x` `[1,2]`'den her değeri alırken, `y` `[3,4]`'ten her değeri alır ve bu böyle devam eder.
Bu deterministik olmayan hesaplamanın sonucunu temsil etmek için, listedeki her bir öğenin olası bir liste olduğu bir liste kullanırız. Bu yüzden sonuç bir liste listesidir.

I/O eylemleriyle kullanıldığında, `sequenceA`, `sequence` ile aynı şeydir! I/O eylemlerinin bir listesini alır ve bu eylemlerin her birini gerçekleştirecek ve 
sonucunda bu I/O eylemlerinin sonuçlarının bir listesine sahip olacak bir I/O eylemi döndürür. Bunun nedeni, bir `[IO a]` değerini bir `IO [a]` değerine dönüştürmek,
gerçekleştirildiğinde sonuçların bir listesini veren bir I/o işlemi yapmak için, tüm bu I/O eylemlerinin sıralı olması gerekir,
böylece değerlendirme zorlandığında birbiri ardına gerçekleştirilir. Bir I/O eyleminin sonucunu gerçekleştirmeden alamazsınız.

~~~~ {.haskell: .ghci name="code"}
ghci> sequenceA [getLine, getLine, getLine]  
heyh  
ho  
woo  
["heyh","ho","woo"]  
~~~~

Normal functor'lar gibi, applicative functor'lar da birkaç yasayla birlikte gelir. En önemlisi, daha önce bahsettiğimiz, yani `pure f <*> x = fmap f x` geçerli olmasıdır.
Alıştırma olarak, bu bölümde karşılaştığımız bazı applicative functor'ları için bu yasayı kanıtlayabilirsiniz. Diğer functor yasaları şunlardır:

- `pure id <*> v = v`
- `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`
- `pure f <*> pure x = pure (f x)`
- `u <*> pure y = pure ($ y) <*> u`

Şu anda bunların üzerinden ayrıntılı bir şekilde geçmeyeceğiz çünkü bu çok fazla sayfa alacaktır ve muhtemelen biraz sıkıcı olacaktır,
ancak göreve hazırsanız, onlara daha yakından bakabilir ve bazı instance'lar için geçerli olup olmadıklarını görebilirsiniz.

Sonuç olarak, applicative functor'ları sadece ilginç değil, aynı zamanda kullanışlıdırlar, çünkü I/O hesaplamaları, deterministik olmayan hesaplamalar,
başarısız olabilecek hesaplamalar gibi farklı hesaplamaları applicative stili kullanarak birleştirmemize izin verirler. 
Sadece `<$>` ve `<*>` kullanarak, herhangi bir sayıdaki applicative functor'lar üzerinde düzgün bir şekilde çalışmak ve
her birinin anlambiliminden yararlanmak için normal fonksiyonları kullanabiliriz.


newtype keyword'ü
-------------------

![maoi](../img/maoi.png)
Şimdiye kadar, **data** keyword'ü kullanarak kendi cebirsel veri türlerinizi nasıl oluşturacağınızı öğrendik.
Ayrıca, mevcut türlere **type** keyword'ü ile nasıl synonyms vereceğimizi de öğrendik. Bu bölümde, **newtype** keyword'ünü kullanarak mevcut veri türlerinden
nasıl yeni türler oluşturabileceğimize ve ilk etapta bunu neden yapmak istediğimize bir göz atacağız.

Önceki bölümde, liste türünün applicative functor olmasının aslında daha fazla yolu olduğunu gördük.
Bunun bir yolu, `<*>` listedeki her fonksiyonu, yani sol parametresini çıkarmak ve bunu sağdaki listedeki her değere uygulamaktır;
bu, soldaki listenin tüm değerlerinin sağdaki listeyle olası her kombinasyonuyla sonuçlanır. 

~~~~ {.haskell: .ghci name="code"}
ghci> [(+1),(*100),(*5)] <*> [1,2,3]  
[2,3,4,100,200,300,5,10,15] 
~~~~

İkinci yol, `<*>`'ın sol tarafındaki ilk fonksiyonu alıp sağdaki ilk değere uygulamaktır,
ardından soldaki listeden ikinci fonksiyonu alıp sağdaki ikinci değere uygulayın ve bu şekilde devam edin. Sonuçta, iki listeyi bir araya getirmek gibi bir şey.
Ancak listeler zaten `Applicative`'in bir instance'ıdır, öyleyse listeleri nasıl bu ikinci yolla Applicative instance'ı yaptık?
Hatırlarsanız, `ZipList a`'nın bu nedenle öğretildiğini söylemiştik, tek bir value constructor'u olan `ZipList`, sadece bir alana sahip. Bu alana sardığımız listeyi koyarız.
Ardından `ZipList`, `Applicative`'in bir instance'ı haline getirildi, böylece listeleri zipleme tarzında applicative olarak kullanmak istediğimizde onları `ZipList` yapıcısı ile sarmalıyoruz ve işimiz bittiğinde onları getZipList ile açıyoruz:
Daha sonra, `ZipList` bir `Applicative` instance'ı haline getirildi, böylece listeleri sıkıştırmada applicative'ler olarak kullanmak istediğimizde,
onları `ZipList` constructor'u ile sarıyoruz ve işimiz bittiğinde onları `getZipList` ile açıyoruz:

~~~~ {.haskell: .ghci name="code"}
ghci> getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]  
[2,200,15]  
~~~~

Peki bunun bu newtype keyword'yle ne ilgisi var? `ZipList a` türü için data declaration'ı nasıl yazabileceğimizi düşünün. Bunun bir yolu, bunu böyle yapmaktır:

~~~~ {.haskell: .ghci name="code"}
data ZipList a = ZipList [a]  
~~~~

Yalnızca bir value contructor'una sahip olan ve bu value contructor'ın, bir şeylerin listesi olan tek bir alanı vardır.
`ZipList`'ten bir listeyi otomatik olarak çıkaran bir fonksiyonu otomatik olarak elde etmek için record sözdizimini kullanmak isteyebiliriz:

~~~~ {.haskell: .ghci name="code"}
data ZipList a = ZipList { getZipList :: [a] }  
~~~~

Bu iyi görünüyor ve aslında oldukça iyi çalışıyor. Mevcut bir türü bir tür sınıfının instance'ı yapmak için iki yolumuz vardı,
bu nedenle *data* keyword'ünü bu türü başka bir türe sarmak için kullandık ve diğer türü ikinci şekilde instance yaptık.

Haskell'deki *newtype* keyword'ü, sadece bir türü alıp başka bir tür olarak sunmak için bir şeye sarmak istediğimizde tam olarak bu durumlar için yapılır.
Güncel kütüphanelerde `ZipList a` şu şekilde tanımlanır:

~~~~ {.haskell: .ghci name="code"}
newtype ZipList a = ZipList { getZipList :: [a] }  
~~~~

*data* keyword'ü yerine *newtype* keyword'ü kullanılır. Şimdi neden bu? Birincisi, *newtype* daha hızlıdır. Bir türü sarmak için *data* keyword'ünü kullanırsanız,
programınız çalışırken tüm bu sarmalama ve açma işlemlerinin bazı ek yükleri vardır. Ancak *newtype*'ı kullanırsanız,
Haskell bunu sadece mevcut bir türü yeni bir türe (dolayısıyla adı) sarmak için kullandığınızı bilir, çünkü dahili olarak aynı olmasını ama
farklı bir türe sahip olmasını istersiniz. Bunu akılda tutarak Haskell, hangi değerin hangi türden olduğunu çözdükten sonra paketlemeden kurtulabilir.

Öyleyse neden her zaman *data* yerine *newtype kullanmıyoruz? *newtype* keyword'ünü kullanarak mevcut bir türden yeni bir tür oluşturduğunuzda,
yalnızca bir value constructor'a sahip olabilirsiniz ve bu value constructor yalnızca bir alana sahip olabilir.
Ancak *data*'yla, birkaç value constructor'a sahip veri türleri yapabilirsiniz ve her constructor sıfır veya daha fazla alana sahip olabilir:

~~~~ {.haskell: .ghci name="code"}
data Profession = Fighter | Archer | Accountant  
  
data Race = Human | Elf | Orc | Goblin  
  
data PlayerCharacter = PlayerCharacter Race Profession  
~~~~

*newtype* kullanırken, tek alanlı tek bir constructor ile sınırlandırılırsınız.

Ayrıca *data* ile yapacağımız gibi *newtype* ile *deriving* keyword'ünü de kullanabiliriz. `Eq`, `Ord`, `Enum`, `Bounded`, `Show` ve `Read` için instance'lar türetebiliriz.
Bir tür sınıfı için instance türetirsek, sarmaladığımız tür, başlamak için bu tür sınıfında olmalıdır. Bu mantıklı, çünkü *newtype* mevcut bir türü sarıyor.
Şimdi şunu yaparsak, yeni türümüzün değerlerini yazdırabilir ve eşitleyebiliriz:

~~~~ {.haskell: .ghci name="code"}
newtype CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)  
~~~~

Şuna bir bakalım:

~~~~ {.haskell: .ghci name="code"}
ghci> CharList "this will be shown!"  
CharList {getCharList = "this will be shown!"}  
ghci> CharList "benny" == CharList "benny"  
True  
ghci> CharList "benny" == CharList "oisters"  
False  
~~~~

Bu özellikle *newtype* da, value constructor'u aşağıdaki türe sahiptir:

~~~~ {.haskell: .ghci name="code"}
CharList :: [Char] -> CharList  
~~~~

`"my sharona"` gibi bir `[Char]` değeri alır ve bir `CharList` değeri döndürür. `CharList` value constructor'unu kullandığımız yukarıdaki örneklerden,
durumun gerçekten böyle olduğunu görüyoruz. Tersine, yeni türümüzde *record* sözdizimi kullandığımız için bizim için oluşturulan `getCharList` fonksiyonu şu türe sahiptir:

~~~~ {.haskell: .ghci name="code"}
getCharList :: CharList -> [Char]  
~~~~

Bir CharList değerini alır ve bunu [Char] değerine dönüştürür. Bunu sarma ve açma olarak düşünebilirsiniz,
ancak aynı zamanda değerleri bir türden diğerine dönüştürmek olarak da düşünebilirsiniz.


Tür sıfını instance'ları yapmak için newtype kullanma
------------------------------------------------------

Çoğu zaman, türlerimizi belirli type class'larının instance'ları yapmak isteriz, ancak tür parametreleri yapmak istediğimiz şeyle eşleşmez.
`Maybe`'yi bir `Functor` instance'ı yapmak kolaydır, çünkü `Functor` tür sınıfı şu şekilde tanımlanır:

~~~~ {.haskell: .ghci name="code"}
class Functor f where  
    fmap :: (a -> b) -> f a -> f b  
~~~~

Yani şununla başlıyoruz:

~~~~ {.haskell: .ghci name="code"}
instance Functor Maybe where 
~~~~

Ve sonra `fmap`'i uygulayın. Tüm tür parametreleri toplanır çünkü `Maybe`, `Functor` tür sınıfının tanımında `f`'nin yerini alır ve
bu nedenle `fmap`'e yalnızca `Maybe` üzerinde çalışmış gibi bakarsak, şu şekilde davranır:

~~~~ {.haskell: .ghci name="code"}
fmap :: (a -> b) -> Maybe a -> Maybe b  
~~~~

![krakatoa](../img/krakatoa.png)
Bu sadece şeftali gibi değil mi? Şimdi, demeti, bir fonksiyonu bir demet üzerinde `fmap` ettiğimizde, 
demetin ilk bileşenine uygulanacak şekilde bir `Functor` instance'ı yapmak istersek ne olur? Bu şekilde, `fmap (+3) (1,1)` yapmak `(4,1)` ile sonuçlanır.
Bunun instance'ını yazmanın biraz zor olduğu ortaya çıktı. `Maybe` ile, sadece `instance Functor Maybe` diyoruz,
çünkü yalnızca tam olarak bir parametre alan type constructor'ları `Functor`'un bir instance'ı yapılabilir.
Ancak `(a, b)` ile böyle bir şey yapmanın bir yolu yok gibi görünüyor, böylece `a` türü parametresi `fmap` kullandığımızda değişen bir parametre haline gelir.
Bunu aşmak için, demetimizi, ikinci tür parametresi, demetteki ilk bileşenin türünü temsil edecek şekilde *newtype* yapabiliriz:

~~~~ {.haskell: .ghci name="code"}
newtype Pair b a = Pair { getPair :: (a,b) }  
~~~~

Ve şimdi, onu bir `Functor` instance'ı yapabiliriz, böylece fonksiyon ilk bileşen üzerine eşlenir:

~~~~ {.haskell: .ghci name="code"}
instance Functor (Pair c) where  
    fmap f (Pair (x,y)) = Pair (f x, y)  
~~~~

Gördüğünüz gibi, *newtype* ile tanımlanan türler üzerinde desen eşleştirme yapabiliriz. Esas demeti elde etmek için desen eşleştirme yaparız, 
sonra `f` fonksiyonunu demetteki ilk bileşene uygularız ve ardından demeti `Pair b a`'ya dönüştürmek için `Pair` value constructor'unu kullanırız.
Yalnızca yeni çiftlerimizde çalışsaydı `fmap` türünün ne olacağını hayal edersek, şöyle olurdu:

~~~~ {.haskell: .ghci name="code"}
fmap :: (a -> b) -> Pair c a -> Pair c b  
~~~~

Yine, `instance Functor (Pair c) where` dedik ve bu nedenle `Pair c`, `Functor` için tür sınıfı tanımında `f`'nin yerini aldı:

~~~~ {.haskell: .ghci name="code"}
class Functor f where  
    fmap :: (a -> b) -> f a -> f b  
~~~~

Şimdi, bir demeti `Pair b a`'ya çevirirsek, onun üzerinde `fmap` kullanabiliriz ve fonksiyon ilk bileşenin üzerine eşlenir:

~~~~ {.haskell: .ghci name="code"}
ghci> getPair $ fmap (*100) (Pair (2,3))  
(200,3)  
ghci> getPair $ fmap reverse (Pair ("london calling", 3))  
("gnillac nodnol",3)  
~~~~

newtype laziness üzerine
------------------------

*newtype*'ın genellikle verilerden daha hızlı olduğundan bahsetmiştik. *newtype* ile yapılabilecek tek şey, mevcut bir türü yeni bir türe dönüştürmektir,
Haskell, tıpkı orijinal olanlar gibi *newtype* ile tanımlanan türlerin değerlerini temsil edebilir,
yalnızca, türlerinin artık farklı olduğunu akılda tutması gerekir. Bu olgu, sadece *newtype*'ın daha hızlı olmadığı, aynı zamanda daha tembel(lazier) olduğu anlamına gelir.
Bunun ne anlama geldiğine bir bakalım.

Daha önce de söylediğimiz gibi, Haskell varsayılan olarak tembeldir, bu da sadece fonksiyonlarımızın sonuçlarını gerçekten yazdırmaya çalıştığımızda
herhangi bir hesaplama yapılacağı anlamına gelir. Dahası, yalnızca fonksiyonumuzun sonucu bize bildirmesi için gerekli olan hesaplamalar gerçekleştirilecektir.
Haskell'deki `undefined` değer, hatalı bir hesaplamayı temsil eder. Bunu terminale yazdırarak değerlendirmeye çalışırsak (yani Haskell'i gerçekten hesaplamaya zorlarsak),
Haskell beğenmediğini belirten bir kriz atacak (teknik olarak bir istisna olarak adlandırılır):

~~~~ {.haskell: .ghci name="code"}
ghci> undefined  
*** Exception: Prelude.undefined  
~~~~

Ancak, içinde `undefined` değerler olan bir liste yaparsak, ancak `undefined` olmayan sadece listenin başını istersek, her şey yolunda gider çünkü
sadece ilk öğenin ne olduğunu görmek istiyorsak, Haskell'in listedeki diğer öğeleri gerçekten değerlendirmesine gerek yoktur:

~~~~ {.haskell: .ghci name="code"}
ghci> head [3,4,5,undefined,2,undefined]  
3  
~~~~

Şimdi şu türü düşünün:

~~~~ {.haskell: .ghci name="code"}
data CoolBool = CoolBool { getCoolBool :: Bool }  
~~~~

*data* keyword'ü ile tanımlanan, run-of-the-mill cebirsel veri türünüzdür. `Bool` türünde bir alana sahip tek bir value constructor'a sahiptir.
`CoolBool`'daki `Bool`'un `True` veya `False` olmasına bakılmaksızın, bir `CoolBool` ile eşleşen ve `"hello"` değerini döndüren bir fonksiyon yapalım:

~~~~ {.haskell: .ghci name="code"}
helloMe :: CoolBool -> String  
helloMe (CoolBool _) = "hello"  
~~~~

Bu fonksiyonu normal bir `CoolBool`'a uygulamak yerine, ona bir curveball atalım ve `undefined` uygulayalım!

~~~~ {.haskell: .ghci name="code"}
ghci> helloMe undefined  
"*** Exception: Prelude.undefined  
~~~~

Eyvah! Bir istisna! Şimdi bu istisna neden oldu? *data* keyword'ü ile tanımlanan türler birden çok value constructor'a sahip olabilir
(`CoolBool`'da yalnızca bir tane olmasına rağmen). Dolayısıyla, fonksiyonumuza verilen değerin `(CoolBool _)` modeline uyup uymadığını görmek için
Haskell, değeri ürettiğimizde hangi value constructor'un kullanıldığını görebilecek kadar değeri değerlendirmelidir.
Ve `undefined` bir değeri değerlendirmeye çalıştığımızda, biraz bile olsa, bir istisna(exception) atılır.

`CoolBool` için *data* keyword'ü kullanmak yerine, *newtype* kullanmayı deneyelim:

~~~~ {.haskell: .ghci name="code"}
newtype CoolBool = CoolBool { getCoolBool :: Bool }  
~~~~

`helloMe` fonksiyonumuzu değiştirmemize gerek yok, çünkü türünüzü tanımlamak için *newtype* veya *data* kullanırsanız desen eşleştirmenin sözdizimi aynıdır.
Burada da aynı şeyi yapalım ve `helloMe`'yi `undefined` bir değere uygulayalım:

~~~~ {.haskell: .ghci name="code"}
ghci> helloMe undefined  
"hello"  
~~~~

![shamrock](../img/shamrock.png)
İşe yaradı! Hmmm, neden böyle? Söylediğimiz gibi, newtype kullandığımızda Haskell, yeni türün değerlerini, orijinal değerlerle aynı şekilde dahili olarak temsil edebilir.
Çevresine başka bir kutu eklemek zorunda değil, sadece değerlerin farklı türlerde olduğunun farkında olması gerekiyor.
Haskell, newtype keyword'ü ile yapılan türlerin yalnızca bir constructor'a sahip olabileceğini bildiğinden, (`CoolBool _`) modeline uyduğundan emin olmak için
fonksiyona iletilen değeri değerlendirmek zorunda değildir, çünkü newtype türleri yalnızca bir olası value constructor'a ve bir alana sahip olabilir!

Davranıştaki bu farklılık önemsiz görünebilir, ancak aslında oldukça önemlidir çünkü *data* ve *newtype* ile tanımlanan türlerin programcının bakış açısından
benzer şekilde davrandıklarını düşünülebilir, çünkü her ikisinin de value constructor'ları ve alanları olduğu için, aslında iki farklı mekanizmadır.
*data*, kendi türlerinizi sıfırdan oluşturmak için kullanılabilirken, newtype, mevcut bir türden tamamen yeni bir tür oluşturmak içindir.
*newtype* değerlerde desen eşleştirme, bir kutudan bir şey çıkarmak gibi değildir (*data* ile olduğu gibi), daha çok bir türden diğerine doğrudan dönüşüm yapmakla ilgilidir.


type vs. newtype vs. data
--------------------------

Bu noktada, tür, *data* ve *newtype* arasındaki farkın tam olarak ne olduğu konusunda biraz kafanız karışabilir, bu yüzden hafızamızı biraz yenileyelim.

**type** keyword'ü, tür eşlanlamlıları yapmak içindir. Bunun anlamı, zaten var olan bir türe başka bir ad vermemizdir, böylece türe atıfta bulunmak daha kolay olur.
Aşağıdakileri yaptığımızı varsayalım:

~~~~ {.haskell: .ghci name="code"}
type IntList = [Int]  
~~~~

Bütün bunlar, `[Int]` türüne `IntList` olarak başvurmamıza izin vermektir. Birbirlerinin yerine kullanılabilirler.
Bir `IntList` value constructor veya buna benzer bir şey elde etmiyoruz. `[Int]` ve `IntList` aynı türe başvurmanın yalnızca iki yolu olduğundan,
tür ek açıklamalarında hangi adı kullandığımız önemli değildir:

~~~~ {.haskell: .ghci name="code"}
ghci> ([1,2,3] :: IntList) ++ ([1,2,3] :: [Int])  
[1,2,3,1,2,3]  
~~~~

Tür imzalarımızı daha açıklayıcı hale getirmek istediğimizde, türlere, kullanıldıkları fonksiyonlar bağlamında bize amaçları hakkında
bir şeyler söyleyen adlar vererek tür eşlanlamlılarını kullanırız. Örneğin, bir telefon rehberini temsil etmek için `[(String, String)]` türünde bir ilişkilendirme listesi
kullandığımızda, fonksiyonlarımızın tür imzalarının daha kolay okunması için ona `PhoneBook`'un tür eşlanlamlısını verdik.

**newtype** keyword'ü, mevcut türleri alıp yeni türlere sarmak içindir, böylece çoğunlukla onları belirli tür sınıflarının instance'ları haline getirmeyi kolaylaştırır.
Mevcut bir türü sarmak için *newtype* kullandığımızda, aldığımız tür orijinal türden ayrıdır. Aşağıdaki *newtype* değerini yaparsak:

~~~~ {.haskell: .ghci name="code"}
newtype CharList = CharList { getCharList :: [Char] }  
~~~~

Bir `CharList`'i ve `[Char]` türünün bir listesini bir araya getirmek için `++` kullanamayız. İki `CharList`'i bir araya getirmek için bile `++` kullanamayız,
çünkü `++` yalnızca listelerde çalışır ve `CharList` türü bir liste içerdiği söylense bile bir liste değildir.
Bununla birlikte, iki `CharList`'i `++` listelerine dönüştürebilir ve sonra bunu bir `CharList`'e dönüştürebiliriz.

Yeni tür bildirimlerimizde record sözdizimi kullandığımızda, yeni tür ile orijinal tür arasında dönüştürme yapmak için fonksiyonlar elde ederiz:
yani newtype value constructor'u ve alanındaki değeri çıkarma fonksiyonu. Yeni tür ayrıca, orijinal türün ait olduğu tür sınıflarının bir instance'ı otomatik olarak
oluşturmaz, bu nedenle bunları türetmemiz veya manuel olarak yazmamız gerekir.

Pratikte, yeni tür bildirimlerini yalnızca bir constructor ve bir alana sahip olabilen data declaration'ları olarak düşünebilirsiniz.
Kendinizi böyle bir data declaration'ı yazarken yakalarsanız, newtype kullanmayı düşünün.

**data** keyword'ü, kendi veri türlerinizi oluşturmak içindir ve onlarla birlikte, çılgınca davranabilirsiniz.
İstediğiniz kadar constructor ve alana sahip olabilirler ve herhangi bir cebirsel veri türünü kendiniz uygulamak için kullanılabilirler.
Listelerden Maybe benzeri türlerden ağaçlara kadar her şey.

Yalnızca tür imzalarınızın daha temiz görünmesini ve daha açıklayıcı olmasını istiyorsanız, muhtemelen tür eşanlamlıları istersiniz.
Mevcut bir türü alıp, onu bir tür sınıfının instance'ı haline getirmek için yeni bir türe sarmak istiyorsanız, muhtemelen bir newtype arıyorsunuzdur.
Ve tamamen yeni bir şey yapmak istiyorsanız, data keyword'ünü aramanızın olasılığı yüksektir.


Monoid'ler
---------

![pirateship](../img/pirateship.png)
Haskell'deki tür sınıfları, bazı ortak davranışlara sahip türler için bir interface sunmak için kullanılır.
Değerleri eşitlenebilen türler için olan `Eq` ve sıraya konulabilen `Ord` gibi basit tür sınıflarıyla başladık ve
daha sonra `Functor` ve `Applicative` gibi daha ilginç olanlara geçtik.

Bir tür oluşturduğumuzda, hangi davranışları desteklediğini, yani neye benzeyebileceğini düşünürüz ve 
sonra buna dayanarak hangi tür sınıflarının onu bir instance'ı yapacağına karar veririz.
Türümüzün değerlerinin eşitlenmesi mantıklıysa, onu `Eq` tür sınıfının bir instance'ını yaparız. 
Türünüzün bir çeşit functor olduğunu görürsek, onu bir `Functor` instance'ı yaparız ve bu böyle devam eder.

Şimdi şunu düşünün: `*` iki sayıyı alıp onları çarpan bir fonksiyondur. Bir sayıyı `1` ile çarparsak, sonuç her zaman bu sayıya eşittir.
`1 * x` veya `x * 1` yapmamızın bir önemi yok, sonuç her zaman `x`'tir. Benzer şekilde, `++` da iki şeyi alan ve üçüncüyü döndüren bir fonksiyondur.
Yalnızca sayıları çoğaltmak yerine iki liste alır ve bunları birleştirir. Ve `*` gibi, aynı zamanda, `++` ile kullanıldığında diğerini değiştirmeyen belirli bir değere sahiptir. Bu değer boş listedir: `[]`.

~~~~ {.haskell: .ghci name="code"}
ghci> 4 * 1  
4  
ghci> 1 * 9  
9  
ghci> [1,2,3] ++ []  
[1,2,3]  
ghci> [] ++ [0.5, 2.5]  
[0.5,2.5]  
~~~~

`1` ile birlikte `*` ve `[]` ile birlikte `++` bazı ortak özellikleri paylaşıyor gibi görünüyor:

- Fonksiyon iki parametre alır.
- Parametreler ve döndürülen değer aynı türe sahiptir.
- İkili fonksiyonla birlikte kullanıldığında diğer değerleri değiştirmeyen böyle bir değer vardır.

Bu iki operasyonun ortak bir yönü daha var ki bu önceki gözlemlerimiz kadar açık olmayabilir: üç veya daha fazla değere sahip olduğumuzda ve
bunları tek bir sonuca indirgemek için ikili fonksiyon kullanmak istediğimizde, ikili fonksiyonu değerlere uygulama sıramız önemli değildir.
`(3 * 4) * 5` veya `3 * (4 * 5)` yapmamız önemli değil. Her iki durumda da sonuç `60`'tır. Aynısı `++` için de geçerlidir:

~~~~ {.haskell: .ghci name="code"}
ghci> (3 * 2) * (8 * 5)  
240  
ghci> 3 * (2 * (8 * 5))  
240  
ghci> "la" ++ ("di" ++ "da")  
"ladida"  
ghci> ("la" ++ "di") ++ "da"  
"ladida"  
~~~~

Bu özelliğe birliktelik(associativity) diyoruz. `*` ilişkiseldir ve `++` da aynıdır, ancak örneğin `-` değildir. `(5 - 3) - 4 ve 5 - (3 - 4)` ifadeleri farklı sayılarla sonuçlanır.

Bu özellikleri fark edip yazarak, monoid'lere rastladık! Bir monoid, ilişkisel bir ikili fonksiyona ve
bu fonksiyonlara göre bir özdeşlik olarak hareket eden bir değere sahip olduğunuz zamandır. Bir şey, bir fonksiyona göre bir özdeşlik olarak hareket ettiğinde,
bu, o fonksiyonla ve başka bir değerle çağrıldığında, sonucun her zaman diğer değere eşit olduğu anlamına gelir. 
`1 *` ile ilgili özdeştir ve `[]` `++` ile ilgili özdeştir. Haskell dünyasında bulunabilecek birçok başka monoid var, bu yüzden Monoid tür sınıfı var.
`Monoid` gibi davranabilen türler içindir. Tür sınıfıının nasıl tanımlandığını görelim:

~~~~ {.haskell: .ghci name="code"}
class Monoid m where  
    mempty :: m  
    mappend :: m -> m -> m  
    mconcat :: [m] -> m  
    mconcat = foldr mappend mempty  
~~~~

`Monoid` tür sınıfı `import Data.Monoid` bölümünde tanımlanmıştır. Biraz zaman ayıralım ve onunla iyice tanışalım. 

Her şeyden önce, yalnızca somut türlerin `Monoid`'in instance'larını oluşturulabileceğini görüyoruz, çünkü tür sınıfı tanımındaki `m`, herhangi bir tür parametresi almıyor.
Bu, instance'larının bir parametre alan type constructor'ları olmasını gerektiren `Functor` ve `Applicative`'den farklıdır.

İlk fonksiyon `mempty`'dir. Parametre almadığı için gerçekte bir fonksiyon değildir, bu yüzden bu polimorfik bir sabittir, `Bounded`'dan `minBound`'a benzer.
`mempty`, belirli bir monoid için özdeşlik değerini temsil eder.

Sırada, muhtemelen tahmin ettiğiniz gibi ikili fonksiyon olan `mappend` var. Aynı türde iki değer alır ve bu türden bir değer de döndürür.
`mappend` olarak adlandırılma kararının bir tür talihsizlik olduğunu belirtmekte fayda var, çünkü bu, bir şekilde iki şeyi eklediğimizi ima ediyor.
`++` iki listeyi alıp birini diğerine eklerken, `*` gerçekten herhangi bir ekleme yapmaz, sadece iki sayıyı bir araya getirir.
`Monoid`'in diğer instance'larıyla karşılaştığımızda, bunların çoğunun da değer eklemediğini göreceğiz, bu nedenle ekleme açısından düşünmekten kaçının ve
`mappend`'in iki monoid değer alıp üçüncü bir değer döndüren ikili bir fonksiyon olduğunu düşünün.

Bu tür sınıfı tanımındaki son fonksiyon `mconcat`'tir. Monoid değerlerin bir listesini alır ve listenin öğeleri arasında mappend yaparak bunları tek bir değere düşürür.
`mempty`'yi başlangıç değeri olarak alan ve listeyi sağdan `mappend` ile fold'layan varsayılan bir uygulaması vardır.
Varsayılan uygulama çoğu durumda iyi olduğundan, bundan sonra `mconcat` ile çok fazla ilgilenmeyeceğiz.
Bir türü `Monoid`'in bir instance'ı haline getirirken, sadece `mempty` ve `mappend` uygulamak yeterlidir.
`mconcat`'in orada olmasının nedeni, bazı instance'lar için `mconcat`'i uygulamanın daha verimli bir yolu olabilir, ancak çoğu durumda varsayılan uygulama gayet iyi.

`Monoid`'in belirli instance'larına geçmeden önce, monoid yasalarına kısaca bir göz atalım. İkili fonksiyona göre özdeşlik görevi gören bir değerin olması ve 
ikili fonksiyonun ilişkisel olması gerektiğinden bahsetmiştik. Bu kurallara uymayan `Monoid` instance'ları oluşturmak mümkündür, 
ancak bu tür instance'lar kimsenin işine yaramaz çünkü `Monoid` tür sınıfını kullanırken, monoid gibi davranan instance'lara güveniriz. Aksi takdirde, ne anlamı var?
Bu nedenle, instance'lar oluştururken aşağıdaki yasalara uyduklarından emin olmalıyız:

- ``mempty `mappend` x = x``
- ``x `mappend` mempty = x``
- ``(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)``

İlk ikisi `mempty`'nin `mappend`'e göre özdeş olarak hareket etmesi gerektiğini belirtir ve üçüncüsü, `mappend`'in ilişkisel olması gerektiğini,
yani birkaç monoid değeri bire indirgemek için `mappend`'i kullandığımız sıranın önemli olmadığını söyler.
Haskell bu yasaları uygulamaz, bu nedenle programcı olarak biz instance'larımızın gerçekten onlara uyması konusunda dikkatli olmalıyız


Listeler monoid'lerdir
-----------------------

Evet, listeler monoiddir! Gördüğümüz gibi, `++` fonksiyonu ve boş `[]` listesi bir monoid oluşturur. instance çok basittir:

~~~~ {.haskell: .ghci name="code"}
instance Monoid [a] where  
    mempty = []  
    mappend = (++)  
~~~~

Listeler, tuttukları öğelerin türüne bakılmaksızın `Monoid` tür sınıfının bir instance'ıdır. `Monoid` bir instance için somut bir tür gerektirdiğinden,
`instance Monoid []` yazmadığımıza, `instance Monoid [a]` yazdığımıza dikkat edin.

Bunu bir test çalıştırması yaptığımızda hiçbir sürprizle karşılaşmayız:

~~~~ {.haskell: .ghci name="code"}
ghci> [1,2,3] `mappend` [4,5,6]  
[1,2,3,4,5,6]  
ghci> ("one" `mappend` "two") `mappend` "tree"  
"onetwotree"  
ghci> "one" `mappend` ("two" `mappend` "tree")  
"onetwotree"  
ghci> "one" `mappend` "two" `mappend` "tree"  
"onetwotree"  
ghci> "pang" `mappend` mempty  
"pang"  
ghci> mconcat [[1,2],[3,6],[9]]  
[1,2,3,6,9]  
ghci> mempty :: [a]  
[]  
~~~~

![smug](../img/smug.png)
Son satırda, açık bir tür ek açıklaması yazmamız gerektiğine dikkat edin, çünkü eğer `mempty` yaparsak, GHCi hangi instance'ı kullanacağını bilemezdi,
bu yüzden liste instance'ını istediğimizi söylemeliydik. `[a]` genel türünü (`[Int]` veya `[String]` belirtmenin aksine) kullanabildik çünkü
boş liste herhangi bir tür içeriyormuş gibi davranabilir.

`mconcat`'in varsayılan bir uygulaması olduğu için, bir şeyi `Monoid`'in bir instance'ını yaptığımızda ücretsiz olarak elde ederiz.
Listede `mconcat`'in sadece `concat` olduğu ortaya çıkıyor. Listelerin bir listesini alır ve düzleştirir, çünkü bu, 
bir listedeki tüm bitişik listeler arasında `++` yapmaya eşdeğerdir.

Tek biçimli yasalar gerçekten liste durumu için geçerlidir. Birkaç listeye sahip olduğumuzda ve onları birlikte `mappend` (veya `++`),
hangisini önce yaptığımız önemli değildir, çünkü bunlar zaten uçlarda birleştirilmiştir. Ayrıca, boş liste özdeş görevi görür, bu nedenle her şey yolunda.
Monoidlerin ``a `mappend` b``'nin ``b `mappend` a``'ya eşit olmasını gerektirmediğine dikkat edin. Liste durumunda, bunlar açıkça:

~~~~ {.haskell: .ghci name="code"}
ghci> "one" `mappend` "two"  
"onetwo"  
ghci> "two" `mappend` "one"  
"twoone"  
~~~~

Ve sorun değil. `3 * 5` ve `5 * 3` çarpımları için aynı olması sadece çarpma özelliğidir, ancak tüm (ve aslında çoğu) monoid için geçerli değildir.


Product ve Sum
--------------

Sayıların monoid olarak kabul edilmesi için bir yolu daha önce inceledik. Sadece ikili fonksiyon `*` ve özdeş değeri `1` olsun.
Sayıların monoid olmasının tek yolunun bu olmadığı ortaya çıktı. Diğer bir yol ise ikili fonksiyonun `+` ve özdeş değerinin `0` olmasıdır:

~~~~ {.haskell: .ghci name="code"}
ghci> 0 + 4  
4  
ghci> 5 + 0  
5  
ghci> (1 + 3) + 5  
9  
ghci> 1 + (3 + 5)  
9  
~~~~

monoid yasalar geçerlidir, çünkü herhangi bir sayıya 0 eklerseniz, sonuç o sayıdır. Ekleme de ilişkiseldir, bu yüzden orada sorun yaşamıyoruz.
Şimdi sayıların monoid olmasının eşit derecede geçerli iki yolu olduğuna göre, hangi yolu seçmelisiniz? Eh, mecbur değiliz.
Unutmayın, bir türün aynı tür sınıfının bir instance'ı olmasının birkaç yolu olduğunda, bu türü bir newtype olarak sarabiliriz ve
sonra yeni türü farklı bir şekilde tür sınıfının bir instance'ı yapabiliriz. Pastamızı da yiyebiliriz

`Data.Monoid` modülü bunun için `Product` ve `Sum` olmak üzere iki tür verir. `Product` şu şekilde tanımlanır:

~~~~ {.haskell: .ghci name="code"}
newtype Product a =  Product { getProduct :: a }  
    deriving (Eq, Ord, Read, Show, Bounded)
~~~~

Basit, bazı türetilmiş instance'larla birlikte bir tür parametresine sahip bir newtype sarmalayıcı.
`Monoid` instance'ı biraz şuna benzer:

~~~~ {.haskell: .ghci name="code"}
instance Num a => Monoid (Product a) where  
    mempty = Product 1  
    Product x `mappend` Product y = Product (x * y)  
~~~~

`mempty`, bir `Product` constructor'una yalnızca `1` sarılmıştır. `Product` constructor'daki `mappend` desen eşleşmeleri, iki sayıyı çarpar ve sonra elde edilen sayıyı geri sarar. Gördüğünüz gibi, bir `Num a` sınıf kısıtlaması var. Bu, `Product a`'nın, zaten `Num`'un bir instance'ı olan tüm `a`'lar için bir `Monoid `instance'ı olduğu anlamına gelir. `Product a`'yı monoid olarak kullanmak için, biraz newtype sarma ve sarılan şeyi açmalıyız:

~~~~ {.haskell: .ghci name="code"}
ghci> getProduct $ Product 3 `mappend` Product 9  
27  
ghci> getProduct $ Product 3 `mappend` mempty  
3  
ghci> getProduct $ Product 3 `mappend` Product 4 `mappend` Product 2  
24  
ghci> getProduct . mconcat . map Product $ [3,4,2]  
24  
~~~~

Bu, `Monoid` tür sınıfının bir gösterimi olarak güzel, ancak aklı başında hiç kimse, sadece `3 * 9` ve `3 * 1` yazmak yerine sayıları çarpmak için bu yöntemi kullanmaz.
Ancak biraz sonra, şu anda önemsiz görünebilecek bu `Monoid` instance'larının nasıl işe yarayabileceğini göreceğiz.

`Sum`, `Product` gibi tanımlanır ve instance da benzerdir. Aynı şekilde kullanıyoruz:

~~~~ {.haskell: .ghci name="code"}
ghci> getSum $ Sum 2 `mappend` Sum 9  
11  
ghci> getSum $ mempty `mappend` Sum 3  
3  
ghci> getSum . mconcat . map Sum $ [1,2,3]  
6  
~~~~



-- Any ve All
-------------

İki farklı ancak eşit derecede geçerli yolla bir monoid gibi hareket edebilen başka bir tür `Bool`'dur.
İlk yol, *or* fonksiyonuna sahip olmaktır `||` özdeş değeri olarak `False` ile birlikte binary fonksiyon olarak davranır.
Mantıkta *or*'un çalışma şekli, iki parametresinden herhangi biri `True` ise, `True` döndürür, aksi takdirde `False` döndürür.
Bu nedenle, özdeş değeri olarak `False` kullanırsak, `False` ile sorulduğunda `False` ve `True` ile belirtildiğinde `True` döndürür.
`Any` *newtype* constructor'u, bu şekilde bir `Monoid` instance'ıdır. Şöyle tanımlanır:

~~~~ {.haskell: .ghci name="code"}
newtype Any = Any { getAny :: Bool }  
    deriving (Eq, Ord, Read, Show, Bounded)  
~~~~

Instance'ı şöyle görünüyor:

~~~~ {.haskell: .ghci name="code"}
instance Monoid Any where  
        mempty = Any False  
        Any x `mappend` Any y = Any (x || y)  
~~~~

Bunun `Any` olarak adlandırılmasının nedeni, bu ikisinden herhangi biri `True` ise ``x `mappend` y``'nin `True` olmasıdır. 
Üç veya daha fazla `Any` sarmalanmış `Bool` birlikte mappend olsa bile, sonuçlardan herhangi biri `True` ise `True` olarak kalacaktır:

~~~~ {.haskell: .ghci name="code"}
ghci> getAny $ Any True `mappend` Any False  
True  
ghci> getAny $ mempty `mappend` Any True  
True  
ghci> getAny . mconcat . map Any $ [False, False, False, True]  
True  
ghci> getAny $ mempty `mappend` mempty  
False  
~~~~

`Bool`'un bir `Monoid` instance'ı olmasının diğer yolu, tam tersini yapmaktır: `&&`'nin binary fonksiyon olması ve ardından özdeş değerini `True` yapın.
Mantıksaldır ve yalnızca her iki parametresi de `True` ise `True` döndürür. Bu *newtype* bildirimi, hiç de zevkli değil:

~~~~ {.haskell: .ghci name="code"}
newtype All = All { getAll :: Bool }  
        deriving (Eq, Ord, Read, Show, Bounded)  
~~~~

Ve bu instance:

~~~~ {.haskell: .ghci name="code"}
instance Monoid All where  
        mempty = All True  
        All x `mappend` All y = All (x && y)  
~~~~

`All` türünün değerlerini `mappend` kullandığımızda, sonuç yalnızca `mappend` işlemlerinde kullanılan tüm değerler `True` ise `True` olacaktır:

~~~~ {.haskell: .ghci name="code"}
ghci> getAll $ mempty `mappend` All True  
True  
ghci> getAll $ mempty `mappend` All False  
False  
ghci> getAll . mconcat . map All $ [True, True, True]  
True  
ghci> getAll . mconcat . map All $ [True, True, False]  
False  
~~~~

Çarpma ve toplamada olduğu gibi, genellikle ikili fonksiyonları *newtype*'lara sarmak ve ardından `mappend` ve `mempty` kullanmak yerine açıkça belirtiriz.
`mconcat`, `Any` ve `All` için yararlı görünmektedir, ancak genellikle `Bool` listelerini alan ve bunlardan herhangi biri `True` ise veya
tümü `True` ise `True` döndüren `or` ve `and` fonksiyonlarını kullanmak daha kolaydır.



Ordering monoid
---------------


Hey, `Ordering` türünü hatırlıyor musun? Nesneleri karşılaştırırken sonuç olarak kullanılır ve üç değere sahip olabilir:
`LT`, `EQ` ve `GT`, sırasıyla *less than*, *equal* ve *greater than* anlamına gelir:

~~~~ {.haskell: .ghci name="code"}
ghci> 1 `compare` 2  
LT  
ghci> 2 `compare` 2  
EQ  
ghci> 3 `compare` 2  
GT  
~~~~

Listeler, sayılar ve boole değerleriyle, monoidleri bulmak yalnızca zaten var olan yaygın olarak kullanılan fonksiyonlara bakmak ve
bir tür monoid davranış sergileyip sergilemediklerini görmekten ibaretti. `Ordering` ile bir monoid'i tanımak için biraz daha dikkatli bakmalıyız, 
ancak `Monoid` instance'ının şimdiye kadar tanıştıklarımız kadar sezgisel ve oldukça kullanışlı olduğu ortaya çıktı:

~~~~ {.haskell: .ghci name="code"}
instance Monoid Ordering where  
    mempty = EQ  
    LT `mappend` _ = LT  
    EQ `mappend` y = y  
    GT `mappend` _ = GT  
~~~~

instance şu şekilde kurulur: İki `Ordering` değerini `mappend`'liyoruz, soldaki değer `EQ` olmadığı sürece soldaki korunur, bu durumda sağdaki değer sonuçtur.
Özdeşi `EQ`'dur. İlk başta, bu biraz keyfi görünebilir, ancak aslında kelimeleri alfabetik olarak karşılaştırma şeklimize benziyor.
İlk iki harfi karşılaştırıyoruz ve eğer farklılarsa, bir sözlükte hangi kelimenin önce geleceğini zaten belirleyebiliriz.
Bununla birlikte, ilk iki harf eşitse, sonraki harf çiftini karşılaştırmaya geçip işlemi tekrar ederiz.

Örneğin, `"ox"` ve `"on"` kelimelerini alfabetik olarak karşılaştıracak olsaydık, önce her kelimenin ilk iki harfini karşılaştırırdık,
eşit olduklarını görürdük ve sonra her kelimenin ikinci harfini karşılaştırmaya geçirdik. `"x"`'in alfabetik olarak `"n"`'den büyük olduğunu görüyoruz ve
bu nedenle kelimelerin nasıl karşılaştırıldığını biliyoruz. `EQ`'nun özdeş olduğuna dair bir sezgiye sahip olmak için,
aynı harfi her iki kelimede de aynı pozisyonda sıkıştırırsak, alfabetik sıralarını değiştirmeyeceğini fark edebiliriz.
`"oix"` hala alfabetik olarak `"oin"`'den büyüktür.

`Ordering` için `Monoid` instance'nda, ``x `mappend` y``'nin ``y `mappend` x``'e eşit olmadığına dikkat etmek önemlidir.
İlk parametre `EQ` olmadığı sürece tutulduğundan, ``LT `mappend` GT`` `LT` ile sonuçlanırken ``GT `mappend` LT``, `GT` ile sonuçlanır:

~~~~ {.haskell: .ghci name="code"}
ghci> LT `mappend` GT  
LT  
ghci> GT `mappend` LT  
GT  
ghci> mempty `mappend` LT  
LT  
ghci> mempty `mappend` GT  
GT  
~~~~

Tamam, peki bu monoid nasıl kullanışlı? Diyelim ki iki string'i alan, uzunluklarını karşılaştıran ve bir `Ordering` döndüren bir fonksiyon yazıyorsunuz.
Ancak string'ler aynı uzunluktaysa, `EQ`'yu hemen döndürmek yerine alfabetik olarak karşılaştırmak istiyoruz. Bunu yazmanın bir yolu şu şekildedir:

~~~~ {.haskell: .ghci name="code"}
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = let a = length x `compare` length y   
                        b = x `compare` y  
                    in  if a == EQ then b else a  
~~~~

`a`'nın uzunluklarını karşılaştırmanın sonucunu ve `b` alfabetik karşılaştırmasının sonucunu adlandırırız ve sonra uzunlukların eşit olduğu ortaya çıkarsa,
alfabetik sıralarını döndürürüz.

Ancak `Ordering`'in nasıl bir monoid olduğu konusundaki anlayışımızı kullanarak, bu fonksiyonu çok daha basit bir şekilde yeniden yazabiliriz:

~~~~ {.haskell: .ghci name="code"}
import Data.Monoid  
  
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = (length x `compare` length y) `mappend`  
                    (x `compare` y)  
~~~~

Bunu deneyebiliriz:

~~~~ {.haskell: .ghci name="code"}
ghci> lengthCompare "zen" "ants"  
LT  
ghci> lengthCompare "zen" "ant"  
GT  
~~~~

Unutmayın, `mappend`'i kullandığımızda, sol parametresi `EQ` olmadığı sürece her zaman korunur, bu durumda sağdaki tutulur.
Bu yüzden birinci olduğunu düşündüğümüz karşılaştırmayı daha önemli kriter olarak ilk parametre olarak koyuyoruz.
Bu fonksiyonu ünlülerin sayısını karşılaştırmak için genişletmek ve bunu karşılaştırma için en önemli ikinci kriter olarak ayarlamak isteseydik,
bunu şu şekilde değiştirirdik:

~~~~ {.haskell: .ghci name="code"}
import Data.Monoid  
  
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = (length x `compare` length y) `mappend`  
                    (vowels x `compare` vowels y) `mappend`  
                    (x `compare` y)  
    where vowels = length . filter (`elem` "aeiou")  
~~~~

Bir string alan ve bize ilk önce sadece `"aeiou"` string'ndeki harfler için filtreden geçirip sonra ona `length` uygulayarak kaç sesliye sahip olduğunu söyleyen
bir yardımcı fonksiyon yaptık.

~~~~ {.haskell: .ghci name="code"}
ghci> lengthCompare "zen" "anna"  
LT  
ghci> lengthCompare "zen" "ana"  
LT  
ghci> lengthCompare "zen" "ann"  
GT  
~~~~

Çok havalı. Burada, ilk örnekte uzunlukların nasıl farklı bulunduğunu ve böylece `LT`'nin nasıl döndürüldüğünü görüyoruz,
çünkü `"zen"`'in uzunluğu `"anna"`'nın uzunluğundan daha azdır. İkinci örnekte, uzunluklar aynıdır, ancak ikinci string'de daha fazla sesli harf vardır,
bu nedenle `LT` tekrar döndürülür. Üçüncü örnekte, ikisi de aynı uzunlukta ve aynı sayıda ünlüdür, bu nedenle alfabetik olarak karşılaştırılırlar ve `"zen"` kazanır.

`Ordering` monoid çok havalı, çünkü eşyaları birçok farklı kritere göre kolayca karşılaştırmamıza ve bu kriterleri en önemlisinden
en aza doğru bir sıraya koymamıza izin veriyor.


Maybe monoid
------------

`Maybe a`'nın bir `Monoid` instance'ı haline getirilebileceği çeşitli yollara ve bu instance'ların ne için yararlı olduğuna bir göz atalım.

Bunun bir yolu, `Maybe a`'yı yalnızca tür parametresi `a` da bir monoid ise bir monoid olarak ele almak ve sonra `mappend`'i `Just` ile sarılmış değerlerin
`mappend` işlemini kullanacak şekilde uygulamaktır. Özdeş olarak `Nothing` kullanıyoruz ve bu nedenle, `mappend` iki değerden biri `Nothing` ise, diğer değeri koruyoruz.

~~~~ {.haskell: .ghci name="code"}
instance Monoid a => Monoid (Maybe a) where  
    mempty = Nothing  
    Nothing `mappend` m = m  
    m `mappend` Nothing = m  
    Just m1 `mappend` Just m2 = Just (m1 `mappend` m2)  
~~~~

Sınıf kısıtlamasına dikkat edin. `Maybe a`'nın, yalnızca bir `Monoid` instance'ysa bir `Monoid` instance'ı olduğunu söyler.
Bir şeyi `Nothing` ile `mappend`'lersek, sonuç o bir şeydir. İki `Just` değerini `mappend`'lersek, `Just`'ın içeriği `mappend`'lenir ve ardından `Just` ile geri sarılır.
Bunu yapabiliriz çünkü sınıf kısıtlaması `Just`'ın içindeki şeyin türünün bir Monoid instance'ı olmasını sağlar.

~~~~ {.haskell: .ghci name="code"}
ghci> Nothing `mappend` Just "andy"  
Just "andy"  
ghci> Just LT `mappend` Nothing  
Just LT  
ghci> Just (Sum 3) `mappend` Just (Sum 4)  
Just (Sum {getSum = 7})  
~~~~

Bu, başarısız olmuş olabilecek hesaplamaların sonucu olarak monoidlerle uğraşırken kullanılır. Bu instance nedeniyle, hesaplamaların `Nothing` veya `Just` değer olup
olmadıklarını görerek başarısız olup olmadığını kontrol etmemize gerek yoktur; onlara normal monoidler gibi davranmaya devam edebiliriz.

Peki ya `Maybe` içeriğinin türü bir `Monoid` instance'î değilse? Önceki instance bildiriminde, içeriğin monoid olmasına güvenmemiz gereken tek durumun,
`mappend`'in her iki parametresinin de `Just` değerleri olduğu durum olduğuna dikkat edin. Ancak içeriklerin monoid olup olmadığını bilmiyorsak,
aralarında mappend kullanamayız, peki ne yapmalıyız? Yapabileceğimiz tek şey, ikinci değeri atmak ve ilkini korumaktır.
Bunun için, `First a` türü vardır ve bu onun tanımıdır:

~~~~ {.haskell: .ghci name="code"}
newtype First a = First { getFirst :: Maybe a }  
    deriving (Eq, Ord, Read, Show)  
~~~~

`Maybe a` alırız ve onu newtype ile sarmalarız. `Monoid` instance'ı aşağıdaki gibidir:

~~~~ {.haskell: .ghci name="code"}
instance Monoid (First a) where  
    mempty = First Nothing  
    First (Just x) `mappend` _ = First (Just x)  
    First Nothing `mappend` x = x  
~~~~

`Just` dediğimiz gibi. `mempty`, `First` *newtype* constructor'yla sarmalanmış bir `Nothing`'dir. `mappend`'in ilk parametresi bir `Just` değer ise, ikincisini yok sayarız.
İlki `Nothing` ise, ikinci parametreyi `Just` veya `Nothing` olmasına bakılmaksızın sonuç olarak sunarız:

~~~~ {.haskell: .ghci name="code"}
ghci> getFirst $ First (Just 'a') `mappend` First (Just 'b')  
Just 'a'  
ghci> getFirst $ First Nothing `mappend` First (Just 'b')  
Just 'b'  
ghci> getFirst $ First (Just 'a') `mappend` First Nothing  
Just 'a'  
~~~~

`First`, bir dizi `Maybe` değerimiz olduğunda ve bunlardan herhangi birinin `Just` olup olmadığını bilmek istediğimizde kullanışlıdır. `mconcat` fonksiyonu kullanışlıdır:

~~~~ {.haskell: .ghci name="code"}
ghci> getFirst . mconcat . map First $ [Nothing, Just 9, Just 10]  
Just 9  
~~~~

`Maybe a` üzerinde bir monoid istersek, eğer `mappend`'in her iki parametresi `Just` değer ise, ikinci parametrenin tutulması için `Data.Monoid`,
`First a` gibi çalışan, sadece son non-`Nothing` değer `mappend` ve `mconcat` kullanılırken tutulur:

`Maybe a` üzerinde bir monoid istersek, böylece ikinci parametrenin her iki `mappend` parametresi de `Just` değerler ise,
`Data.Monoid` bir `Last a` türü sağlar ve `First a`, yalnızca son `mappend` ve `mconcat` kullanıldığında non-`Nothing` değeri tutulur:

~~~~ {.haskell: .ghci name="code"}
ghci> getLast . mconcat . map Last $ [Nothing, Just 9, Just 10]  
Just 10  
ghci> getLast $ Last (Just "one") `mappend` Last (Just "two")  
Just "two"  
~~~~



Veri yapılarında fold için monoid'leri kullanma
-----------------------------------------------
Monoidleri çalıştırmanın en ilginç yollarından biri, çeşitli veri yapıları üzerindeki fold'ları tanımlamamıza yardımcı olmalarını sağlamaktır.
Şimdiye kadar, sadece listelerde fold'lama yaptık, ancak fold'lanabilen tek veri yapısı listeler değil. Hemen hemen her veri yapısı üzerinde fold'lar tanımlayabiliriz.
Ağaçlar özellikle fold'lamaya elverişlidir.

fold'larla iyi çalışan çok sayıda veri yapısı olduğundan, `Foldable` tür sınıfı tanıtıldı.
`Functor`'ın eşleştirilebilen şeyler için olduğu gibi, `Foldable` da fold'lanabilen şeyler içindir! `Data.Foldable` içinde bulunabilir ve
adları `Prelude`'da bulunanlarla çakışan export fonksiyonları olduğu için, en iyisi import qualified (ve fesleğenle servis edilir):

~~~~ {.haskell: .ghci name="code"}
import qualified Foldable as F  
~~~~

Kendimizi değerli tuş vuruşlarından kurtarmak için, `F` olarak qualified edilmiş onu import ediyoruz. Pekala, bu tür sınıfının tanımladığı bazı fonksiyonlar nelerdir?
Aralarında `foldr`, `foldl`, `foldr1` ve `foldl1` var. Huh? Ancak bu fonksiyonları zaten biliyoruz, bunda bu kadar yeni olan ne?
`Foldable` `foldr` ve `Prelude`'daki `foldr` türlerini karşılaştırarak nasıl farklı olduklarını görelim:

~~~~ {.haskell: .ghci name="code"}
ghci> :t foldr  
foldr :: (a -> b -> b) -> b -> [a] -> b  
ghci> :t F.foldr  
F.foldr :: (F.Foldable t) => (a -> b -> b) -> b -> t a -> b  
~~~~

Ah! Dolayısıyla, `foldr` bir listeyi alıp fold'larken, `Data.Foldable`'daki `foldr`, sadece listeleri değil, fold'lanabilen herhangi bir türü kabul eder!
Beklendiği gibi, her iki foldr fonksiyonu da listeler için aynı şeyi yapar:

~~~~ {.haskell: .ghci name="code"}
ghci> foldr (*) 1 [1,2,3]  
6  
ghci> F.foldr (*) 1 [1,2,3]  
6  
~~~~

Tamam o zaman, fold'ları destekleyen diğer bazı veri yapıları nelerdir? Eh, hepimizin bildiği ve sevdiğimiz `Maybe` var!

~~~~ {.haskell: .ghci name="code"}
ghci> F.foldl (+) 2 (Just 9)  
11  
ghci> F.foldr (||) False (Just True)  
True  
~~~~

Ancak bir `Maybe` değerinin üzerine fold'lamak çok ilginç değildir, çünkü fold'lama söz konusu olduğunda, `Just` değeri ise tek öğeli bir liste gibi,
`Nothing` ise boş bir liste gibi davranır. Öyleyse biraz daha karmaşık olan bir veri yapısını inceleyelim. 

[Özyinelemeli veri yapıları](../tr/08-making-our-own-types-and-typeclasses.md#özyinelemeli-veri-yapıları) bölümündeki tree veri yapısını hatırlıyor musunuz? Bunu şöyle tanımladık:

~~~~ {.haskell: .ghci name="code"}
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)  
~~~~

Bir ağacın ya hiçbir değeri olmayan boş bir ağacı olduğunu ya da bir değeri ve ayrıca iki ağacı daha tutan bir node olduğunu söyledik.
Tanımladıktan sonra, onu bir `Functor` instance'ı yaptık ve bununla birlikte `fmap` fonksiyonlarını onun üzerinden kullanma yeteneği kazandık.
Şimdi, fold yeteneğine sahip olabilmemiz için onu `Foldable` bir instance yapacağız. Bir type contructor'u bir `Foldable` instance'ı yapmanın bir yolu,
bunun için doğrudan foldr uygulamaktır. Ancak, çoğu zaman çok daha kolay olan başka bir yol, yine `Foldable` tür sınıfının bir parçası olan `foldMap` fonksiyonunu uygulamaktır.
`foldMap` fonksiyonu aşağıdaki türe sahiptir:

~~~~ {.haskell: .ghci name="code"}
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m  
~~~~

İlk parametresi, foldable yapımızın içerdiği (burada `a` ile belirtilmiştir) türden bir değer alan ve monoid bir değer döndüren bir fonksiyondur.
İkinci parametresi, `a` türü değerleri içeren foldable bir yapıdır. Foldable yapı üzerinde fonksiyonu eşler, böylece monoid değerler içeren foldable bir yapı oluşturur.
Daha sonra, bu monoid değerler arasında `mappend` yaparak, hepsini tek bir monoid değerde birleştirir.
Bu fonksiyon şu anda biraz tuhaf gelebilir, ancak uygulanmasının çok kolay olduğunu göreceğiz. Ayrıca harika olan şey, bu fonksiyonu uygulamak,
türümüzün `Foldable` bir instance haline getirilmesi için gereken tek şey olmasıdır. Öyleyse, eğer bir tür için sadece `foldMap`'i uygularsak,
bu türde ücretsiz olarak `foldr` ve `foldl` elde ederiz!

`Tree`'yi böyle bir `Foldable` instance'ı yapıyoruz:

~~~~ {.haskell: .ghci name="code"}
instance F.Foldable Tree where  
    foldMap f Empty = mempty  
    foldMap f (Node x l r) = F.foldMap f l `mappend`  
                             f x           `mappend`  
                             F.foldMap f r  
~~~~

![accordion](../img/accordion.png)
Şöyle düşünüyoruz: Ağacımızın bir unsurunu alan ve tek bir değer döndüren bir fonksiyon sağlanmışsa, tüm ağacımızı nasıl tek bir monoid değere indirebiliriz?
Ağacımızın üzerinde `fmap` yaparken, bir node'a eşleştirdiğimiz fonksiyonu uyguladık ve ardından fonksiyonu sağ alt ağacın yanı sıra sol alt ağacın üzerine de
yinelemeli(recursively) olarak eşledik. Burada, sadece bir fonksiyonu eşlemekle değil, aynı zamanda sonuçları `mappend` kullanarak tek bir monoid değerde birleştirmekle de
görevlendirildik. İlk önce boş ağacın durumunu ele alıyoruz - hiçbir değeri veya alt ağaçları olmayan, hüzünlü ve yalnız bir ağaç.
Monoid oluşturma fonksiyonumuza verebileceğimiz herhangi bir değeri taşımaz, bu yüzden sadece ağacımız boşsa, monoid değerinin `mempty` olacağını söyleriz.

Boş olmayan bir node durumu biraz daha ilginçtir. Bir değerin yanı sıra iki alt ağaç içerir. Bu durumda, sol ve sağ alt ağaçların üzerinde aynı `f` fonksiyonunu
yinelemeli olarak `foldMap` yaparız. `foldMap`'inizin tek bir monoid değerle sonuçlandığını unutmayın. Ayrıca `f` fonksiyonumuzu node'daki değere uygularız.
Şimdi üç monoid değerimiz var (alt ağaçlarımızdan ikisi ve node'daki değere `f` uygulamasından) ve onları tek bir değerde bir araya getirmemiz gerekiyor.
Bu amaçla `mappend` kullanıyoruz ve doğal olarak önce sol alt ağaç, sonra node değeri ve ardından sağ alt ağaç geliyor.

Bir değer alan ve monoid bir değer döndüren fonksiyonu sağlamamız gerekmediğine dikkat edin. Bu fonksiyonu `foldMap` için bir parametre olarak alıyoruz ve
karar vermemiz gereken tek şey, bu fonksiyonu nereye uygulayacağımız ve ondan elde edilen monoidleri nasıl birleştireceğimiz.

Artık ağaç türümüz için `Foldable` bir instance'ımız olduğuna göre, ücretsiz olarak `foldr` ve `foldl` alıyoruz! Bu ağacı düşünün:

~~~~ {.haskell: .ghci name="code"}
testTree = Node 5  
            (Node 3  
                (Node 1 Empty Empty)  
                (Node 6 Empty Empty)  
            )  
            (Node 9  
                (Node 8 Empty Empty)  
                (Node 10 Empty Empty)  
            )  
~~~~

Kökünde `5`, ardından sol node'nda `3`, solda `1` ve sağda `6` bulunur. Kökün sağ node'nda `9`, solunda `8` ve en sağda `10` bulunur.
`Foldable` bir instance'la, listeler üzerinde yapabildiğimiz tüm fold'ları yapabiliriz:

~~~~ {.haskell: .ghci name="code"}
ghci> F.foldl (+) 0 testTree  
42  
ghci> F.foldl (*) 1 testTree  
64800  
~~~~

Ayrıca, `foldMap` yalnızca yeni `Foldable` instance'larını oluşturmak için kullanışlı değildir; yapımızı tek bir monoid değere düşürmek için kullanışlıdır.
Örneğin, ağacımızdaki herhangi bir sayının `3`'e eşit olup olmadığını bilmek istiyorsak, bunu yapabiliriz:

~~~~ {.haskell: .ghci name="code"}
ghci> getAny $ F.foldMap (\x -> Any $ x == 3) testTree  
True  
~~~~

Burada, `\x -> Any bir $ x == 3`, bir sayıyı alan ve monoid bir değer, yani `Any` ile sarılmış bir `Bool` döndüren bir fonksiyondur.
`foldMap` bu fonksiyonu ağacımızdaki her elemana uygular ve sonra ortaya çıkan monoidleri `mappend` ile tek bir monoid haline getirir. Bunu yaparsak:

~~~~ {.haskell: .ghci name="code"}
ghci> getAny $ F.foldMap (\x -> Any $ x > 15) testTree  
False  
~~~~

Ağacımızdaki tüm node'lar, onlara lambda fonksiyonu uygulandıktan sonra `Any` `False` değerini tutacaktır.
Ancak `True` sonucunu vermek için, `Any` için `mappend` parametresinin en az bir `True` değerine sahip olması gerekir.
Bu yüzden nihai sonuç `False`'tır, bu mantıklıdır çünkü ağacımızdaki hiçbir değer `15`'ten büyük değildir.

Ayrıca `\x -> [x]` fonksiyonu ile `foldMap` yaparak ağacımızı kolaylıkla bir listeye dönüştürebiliriz. İlk önce bu fonksiyonu ağacımıza yansıttığımızda,
her eleman tekil bir liste haline gelir. Tüm bu tek liste arasında gerçekleşen `mappend` eylemi, ağacımızdaki tüm öğeleri içeren tek bir liste ile sonuçlanır:

~~~~ {.haskell: .ghci name="code"}
ghci> F.foldMap (\x -> [x]) testTree  
[1,3,6,5,8,9,10]  
~~~~

Harika olan, tüm bu numaraların ağaçlarla sınırlı olmaması, herhangi bir `Foldable` instance'nda işe yaramasıdır.

