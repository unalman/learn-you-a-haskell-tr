Yüksek dereceli fonksiyonlar
============================

Haskell fonksiyonları, fonksiyonları parametre olarak alabilir ve fonksiyonları dönüş değerleri olarak döndürebilir. 
Bunlardan herhangi birini yapan bir fonksiyona yüksek dereceli fonksiyon denir. Yüksek dereceli fonksiyonlar sadece Haskell deneyiminin bir parçası değil,
hemen hemen Haskell deneyimidir. Bir durumu değiştiren ve belki onları döngüye sokan adımları tanımlamak yerine,
hangi şeyin ne olduğunu tanımlayarak hesaplamaları tanımlamak istiyorsanız, yüksek dereceli fonksiyonlar vazgeçilmezdir.
Sorunları çözmenin ve programlar hakkında düşünmenin gerçekten güçlü bir yoludur.

Curried fonksiyonlar
--------------------

Haskell'deki her fonksioyn resmi olarak yalnızca bir parametre alır. Öyleyse, şu ana kadar birden fazla parametre alan birkaç fonksiyonu tanımlayıp
kullanmamız nasıl mümkün olabilir? Bu akıllıca bir numara! Şimdiye kadar *birçok parametreyi* kabul eden tüm fonksiyonlar **curried fonksiyonlardır**. O ne demek?
Bunu en iyi bir örnekte anlayacaksınız. İyi arkadaşımızı, `max` fonksiyonunu alalım. Görünüşe göre iki parametre alıyor ve daha büyük olanı döndürüyor.
`max 4 5` yapmak önce bir parametre alan ve hangisinin daha büyük olduğuna bağlı olarak `4` veya bu parametreyi döndüren bir fonksiyon oluşturur.
Daha sonra bu fonksiyona `5` uygulanır ve bu fonksiyon istediğimiz sonucu verir. Bu biraz ağız dolusu gibi geliyor ama aslında gerçekten harika bir konsept.
Aşağıdaki iki çağrı eşdeğerdir:

~~~~ {.haskell: .ghci name="code"}
ghci> max 4 5  
5  
ghci> (max 4) 5  
5  
~~~~

İki şey arasına boşluk koymak basitçe **fonksiyon uygulamasıdır**. Alan bir tür operatör gibidir ve en yüksek önceliğe sahiptir.
`max` fonksiyonunu kendimiz yapalım. `max :: (Ord a) => a -> a -> a` Bu aynı zamanda şu şekilde de yazılabilir:
`max :: (Ord a) => a -> (a -> a)`. Bu şöyle okunabilir: `max` bir `a` alır ve döndürür (bu `->`) bir `a` alan ve bir `a` döndüren bir fonksiyon.
Bu nedenle, dönüş türü ve fonksiyonların parametrelerinin tümü oklarla ayrılmıştır.

Peki bu bizim için ne kadar faydalı? Basitçe söylemek gerekirse, çok az parametresi olan bir fonksiyonu çağırırsak, **kısmen uygulanan** bir fonksiyonu geri alırız,
bu, bıraktığımız kadar çok parametre alan bir fonksiyon anlamına gelir. Kısmi uygulamayı kullanmak (isterseniz, çok az parametresi olan fonksiyonları çağırmak),
fonksiyonları anında oluşturmanın düzgün bir yoludur, böylece onları başka bir fonksiyona aktarabilir veya bazı verilerle tohumlayabiliriz.

Bu saldırgan basit fonksiyona bir göz atın:

~~~~ {.haskell: .ghci name="code"}
multThree :: (Num a) => a -> a -> a -> a  
multThree x y z = x * y * z  
~~~~

`multThree 3 5 9` veya `((multThree 3) 5) 9` yaptığımızda gerçekten ne olur? İlk olarak, `3`, bir boşlukla ayrıldıkları için `multThree`'ye uygulanır.
Bu, bir parametre alan ve bir fonksiyon döndüren bir fonksiyon oluşturur. Sonra buna `5` uygulanır, bu da bir parametreyi alıp 15 ile çarpan bir fonksiyon oluşturur.
Bu fonksiyona `9` uygulanır ve sonuç 135 veya başka bir şeydir. Bu fonksiyonun türünün `multThree :: (Num a) => a -> (a -> (a -> a))` olarak da yazılabileceğini unutmayın.
`->` işaretinden önceki şey, bir fonksiyonun aldığı parametredir ve ondan sonraki şey, döndüğü şeydir. Yani fonksiyonumuz bir `a` alır ve
`(Num a) => a -> (a -> a)` türünde bir fonksiyon döndürür. Benzer şekilde, bu fonksiyon bir `a` alır ve `(Num a) => a -> a` türünde bir fonksiyon döndürür.
Ve bu fonksiyon, nihayet, sadece bir `a` alır ve bir `a` döndürür. Şuna bir bak:

~~~~ {.haskell: .ghci name="code"}
ghci> let multTwoWithNine = multThree 9  
ghci> multTwoWithNine 2 3  
54  
ghci> let multWithEighteen = multTwoWithNine 2  
ghci> multWithEighteen 10  
180  
~~~~

Çok az parametresi olan fonksiyonları çağırarak, tabiri caizse, anında yeni fonksiyonlar yaratıyoruz. 
Ya bir sayıyı alan ve onu `100` ile karşılaştıran bir fonksiyon yaratmak istersek? Bunun gibi bir şey yapabiliriz:

~~~~ {.haskell: .ghci name="code"}
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred x = compare 100 x  
~~~~

`99` ile çağırırsak, bir `GT` döndürür. Basit şeyler. Denklemin her iki tarafında da `x`'in sağ tarafta olduğuna dikkat edin. Şimdi `compare 100`'ün ne döndürdüğünü düşünelim.
Bir sayıyı alan ve onu '100' ile karşılaştıran bir fonksiyon döndürür. Vaov! İstediğimiz fonksiyon bu değil mi? Bunu şu şekilde yeniden yazabiliriz:

~~~~ {.haskell: .ghci name="code"}
compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100  
~~~~

Tür bildirimi aynı kalır, çünkü `compare 100` bir fonksiyon döndürür. Karşılaştırma `(Ord a) => a -> (a -> Ordering)` türüne sahiptir ve bunu `100` ile
çağırmak bir `(Num a, Ord a) => a -> Ordering` döndürür. `100` aynı zamanda `Num` tür sınıfının bir parçası olduğu için ek sınıf kısıtlaması gizlice oraya gelir.

**Yo!** Curried fonksiyonların ve kısmi uygulamanın nasıl çalıştığını gerçekten anladığınızdan emin olun çünkü bunlar gerçekten önemlidir!

Infix fonksiyonları, bölümler kullanılarak kısmen de uygulanabilir. Bir infix fonksiyonunu bölümlemek için, onu parantez içine alın ve
yalnızca bir tarafta bir parametre girin. Bu, bir parametre alan ve ardından bunu bir işlenenin eksik olduğu tarafa uygulayan bir fonksiyon oluşturur.
Aşağılayıcı derecede önemsiz bir fonksiyon:

~~~~ {.haskell: .ghci name="code"}
divideByTen :: (Floating a) => a -> a  
divideByTen = (/10)  
~~~~

`divideByTen 200` çağrısı yapmak, `(/10) 200` yapmak gibi `200/10` yapmaya eşdeğerdir. Kendisine sağlanan bir karakterin büyük harf olup olmadığını kontrol eden bir fonksiyon:

~~~~ {.haskell: .ghci name="code"}
isUpperAlphanum :: Char -> Bool  
isUpperAlphanum = (`elem` ['A'..'Z'])  
~~~~

Bölümlerle ilgili tek özel şey `-` kullanmaktır. Bölümlerin tanımından `(-4)`, bir sayı alan ve ondan 4 çıkaran bir fonksiyonla sonuçlanır.
Bununla birlikte, kolaylık sağlamak için `(-4)` eksi dört anlamına gelir. Dolayısıyla, parametre olarak aldığı sayıdan 4 çıkaran bir fonksiyon yapmak istiyorsanız,
`subtract` fonksiyonunu kısmen uygulayın: `(subtract 4)`.

GHCI'da onu bir let ile bir isme bağlamak veya başka bir fonksiyona geçirmek yerine sadece `multThree 3 4` yapmaya çalışırsak ne olur?

~~~~ {.haskell: .ghci name="code"}
ghci> multThree 3 4  
<interactive>:1:0:  
    No instance for (Show (t -> t))  
      arising from a use of `print' at <interactive>:1:0-12  
    Possible fix: add an instance declaration for (Show (t -> t))  
    In the expression: print it  
    In a 'do' expression: print it  
~~~~

GHCI bize ifadenin `a -> a` türünde bir fonksiyon ürettiğini, ancak bunu ekrana nasıl yazdıracağını bilmediğini söylüyor.
Fonksiyonlar, `Show` tür sınıfının instance'ları değildir, bu yüzden bir fonksiyonun düzgün bir string temsilini alamayız. GHCI isteminde `1 + 1` yaptığımızda,
önce bunu `2` olarak hesaplar ve ardından bu sayının metinsel bir temsilini almak için `2`'de `show`'u çağırır. Ve `2`'nin metinsel temsili sadece `"2"` string'idir ve
daha sonra ekranımıza yazdırılır.


Bazı yüksek sıralılık sıralamalarda
-----------------------------------

Fonksiyonlar, fonksiyonları parametre olarak alabilir ve ayrıca Fonksiyonlar döndürebilir.
Bunu açıklamak için, bir Fonksiyon alan ve sonra onu bir şeye iki kez uygulayan bir Fonksiyon yapacağız!

~~~~ {.haskell: .ghci name="code"}
applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x)  
~~~~

Herşeyden önce tür bildirimine dikkat edin. Daha önce parantezlere ihtiyacımız yoktu çünkü `->` doğal olarak sağa ilişkilidir. Ancak burada zorunludurlar.
İlk parametrenin bir şeyi alan ve aynı şeyi döndüren bir fonksiyon olduğunu belirtirler.
İkinci parametre de bu türde bir şeydir ve dönüş değeri de aynı türdedir. İlk parametre bir fonksiyondur (tür `a -> a`) ve ikincisi de aynı `a`.
Fonksiyon ayrıca `Int -> Int` veya `String -> String` veya her neyse olabilir. Ancak ikinci parametrenin de bu türde olması gerekir.

**Not**: Şu andan itibaren, her fonksiyon aslında yalnızca bir parametre alıp, solid bir değer döndüren bir fonksiyona ulaşana kadar kısmen 
uygulanan fonksiyonlar döndürmesine rağmen, fonksiyonların birkaç parametre aldığını söyleyeceğiz.
Bu yüzden, basitlik uğruna, örtünün altında gerçekte neler olduğunu bilsek bile `a -> a -> a`'nın iki parametre aldığını söyleyeceğiz.

Sadece `f` parametresini bir fonksiyon olarak kullanıyoruz, ona `x`'i bir boşlukla ayırarak ve sonra sonucu tekrar `f`'ye uygulayarak.
Örnekle gösterelim:

~~~~ {.haskell: .ghci name="code"}
ghci> applyTwice (+3) 10  
16  
ghci> applyTwice (++ " HAHA") "HEY"  
"HEY HAHA HAHA"  
ghci> applyTwice ("HAHA " ++) "HEY"  
"HAHA HAHA HEY"  
ghci> applyTwice (multThree 2 2) 9  
144  
ghci> applyTwice (3:) [1]  
[3,3,1]  
~~~~

Kısmi uygulamanın mükemmelliği ve faydası ortadadır. Fonksiyonumuzun sadece bir parametre alan bir fonksiyonu iletmemizi gerektiriyorsa,
sadece bir parametreyi aldığı noktaya bir fonksiyonu kısmen uygulayabilir ve sonra onu iletebiliriz.

Şimdi, standart kitaplıkta bulunan gerçekten yararlı bir fonksiyonu uygulamak için yüksek dereceli programlama kullanacağız.
Bu da `zipWith`. Parametre olarak bir fonksiyon ve iki listeyi alır ve ardından fonksiyona karşılık gelen öğeler arasına uygulayarak iki listeyi birleştirir.
İşte bunu nasıl uygulayacağız:

~~~~ {.haskell: .ghci name="code"}
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys  
~~~~

Tür bildirimine bakın. İlk parametre, iki şeyi alan ve üçüncü bir şey üreten bir fonksiyondur. Aynı türden olmak zorunda değiller ama yapabilirler.
İkinci ve üçüncü parametre listelerdir. Sonuç aynı zamanda bir listedir. Birincisi `a`'ların listesi olmalıdır, çünkü birleştirme fonksiyonu `a`'yı ilk argüman olarak alır.
İkincisi, `b`'lerin bir listesi olmalıdır, çünkü birleştirme fonksiyonunun ikinci parametresi `b` türündedir. Sonuç bir `c` listesidir.
Bir fonksiyonun tür bildirimi parametre olarak bir `a -> b -> c` fonksiyonunu kabul ettiğini söylüyorsa, aynı zamanda bir `a -> a -> a` fonksiyonunu da kabul eder,
ancak tersi olmaz!! Unutmayın, özellikle yüksek dereceli fonksiyonlar oluştururken ve türünden emin değilseniz, tür bildirimini çıkarmayı deneyebilir ve 
ardından Haskell'in `:t` kullanarak ne olduğunu kontrol edebilirsiniz.

Fonksiyondaki eylem, normal zip'e oldukça benzer. Uç koşullar aynıdır, yalnızca fazladan bir argüman, birleştirme Fonksiyon vardır, 
ancak bu argüman uç koşullarda önemli değildir, bu yüzden onun için sadece bir `_` kullanıyoruz. Ve son kalıptaki fonksiyon gövdesi de `zip`'e benzer,
yalnızca `(x, y)` değil, `f x y` yapar. Yeterince genel ise, çok sayıda farklı görev için tek bir üst düzey fonksiyon kullanılabilir.
`zipWith'` fonksiyonumuzun yapabileceği tüm farklı şeylerin küçük bir gösterimi:

~~~~ {.haskell: .ghci name="code"}
ghci> zipWith' (+) [4,2,5,6] [2,6,2,3]  
[6,8,7,9]  
ghci> zipWith' max [6,3,2,1] [7,3,1,5]  
[7,3,2,5]  
ghci> zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]  
["foo fighters","bar hoppers","baz aldrin"]  
ghci> zipWith' (*) (replicate 5 2) [1..]  
[2,4,6,8,10]  
ghci> zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]  
[[3,4,6],[9,20,30],[10,12,12]]  
~~~~

Gördüğünüz gibi, tek bir yüksek dereceli foksiyon çok yönlü şekillerde kullanılabilir. Zorunlu programlama genellikle döngü, whele döngüsü,
bir değişkene bir şey ayarlama, durumunu kontrol etme vb. gibi şeyler kullanır. Bazı davranışları elde etmek ve 
sonra onu bir fonksiyon gibi bir arayüzün etrafına sarmak için. Fonksiyonel programlama, iki listeyi çiftler halinde incelemek ve
bu çiftlerle bir şeyler yapmak veya bir dizi çözüm elde etmek ve ihtiyacınız olmayanları ortadan kaldırmak gibi ortak kalıpları soyutlamak için
yüksek dereceli fonksiyonlar kullanır.

Halihazırda standart kitaplıkta bulunan, `flip` adı verilen başka bir fonksiyonu uygulayacağız. Flip basitçe bir fonksiyonu alır ve 
orijinal fonksiyonumuza benzer bir fonksiyonu döndürür, yalnızca ilk iki argüman ters çevrilir. Bunu şu şekilde uygulayabiliriz:

~~~~ {.haskell: .ghci name="code"}
flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f = g  
    where g x y = f y x    
~~~~

Tür bildirimini okurken, `a` ve `b` alan ve `b` ve `a` alan bir fonksiyonu döndüren bir fonksiyon aldığını söylüyoruz.
Ancak fonksiyonlar varsayılan olarak curried olduğundan, ikinci parantez çifti gerçekten gereksizdir, çünkü `->` varsayılan olarak doğru ilişkilidir.
`(a -> b -> c) -> (b -> a -> c)`, `(a -> b -> c) -> b -> a -> c` ile aynı olan `(a -> b -> c) -> (b -> (a -> c))` ile aynıdır.
`g x y = f y x` olduğunu yazdık. Bu doğruysa, o zaman `f y x = g x y` de geçerli olmalı, değil mi?
Bunu akılda tutarak, bu fonksiyonu daha da basit bir şekilde tanımlayabiliriz.

~~~~ {.haskell: .ghci name="code"}
flip' :: (a -> b -> c) -> b -> a -> c  
flip' f y x = f x y   
~~~~

Burada, fonksiyonların curried olması gerçeğinden yararlanıyoruz. `y` ve `x` parametreleri olmadan `flip' f` olarak adlandırdığımızda,
bu iki parametreyi alan ama onları ters yüz olarak çağıran bir `f` döndürür.
Tersine çevrilmiş fonksiyonlar genellikle diğer fonksiyonlara aktarılsa da, ileriyi düşünerek ve tam olarak çağrıldıklarında nihai sonuçlarının
ne olacağını yazarak yüksek dereceli fonksiyonlar yaparken currying'den faydalanabiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> flip' zip [1,2,3,4,5] "hello"  
[('h',1),('e',2),('l',3),('l',4),('o',5)]  
ghci> zipWith (flip' div) [2,2..] [10,8,6,4,2]  
[5,4,3,2,1]     
~~~~


Maps ve filters
---------------

`map` bir fonksiyonu ve bir listeyi alır ve bu fonksiyonu listedeki her öğeye uygulayarak yeni bir liste oluşturur. Tür imzasının ne olduğunu ve nasıl tanımlandığını görelim.

~~~~ {.haskell: .ghci name="code"}
map :: (a -> b) -> [a] -> [b]  
map _ [] = []  
map f (x:xs) = f x : map f xs    
~~~~












































































