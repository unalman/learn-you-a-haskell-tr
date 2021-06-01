Kendi Türlerimizi ve Tür Sınıflarımızı Yapmak
=============================================

Önceki bölümlerde, bazı mevcut Haskell tür ve tür sınıflarını ele aldık. Bu bölümde, kendimizinkini nasıl yapacağımızı ve onları nasıl çalıştıracağımızı öğreneceğiz!

Cebirsel veri türlerine giriş
-----------------------------

Şimdiye kadar birçok veri türüyle karşılaştık. `Bool`, `Int`, `Char`, `Maybe`, vs. ama nasıl kendimizinkini yapabiliriz? 
Bunun bir yolu, bir tür tanımlamak için **data** keyword'ü kullanmaktır. Standart kütüphanede `Bool` türünün nasıl tanımlandığını görelim.

~~~~ {.haskell: .ghci name="code"}
data bool True | False
~~~~

`data`, yeni bir veri türü tanımladığımız anlamına gelir. `=` Öncesindeki kısım `Bool` olan türü belirtir.
`=` İşaretinden sonraki kısımlar **value constructor'lardır**. Bu türün sahip olabileceği farklı değerleri belirtirler.
`|` veya olarak okunur. Yani bunu şu şekilde okuyabiliriz: `Bool` türü `True` veya `False` değerine sahip olabilir.
Hem tür adı hem de value constructor'lar büyük harfle yazılmalıdır.

Benzer bir şekilde, Int türürünün şu şekilde tanımlandığını düşünebiliriz:

~~~~ {.haskell: .ghci name="code"}
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647  
~~~~

![caveman](../img/caveman.png)
İlk ve son value constructor'lar, `Int`'in olası minimum ve maksimum değerleridir. Aslında bu şekilde tanımlanmadı, 
elipsler buradadır çünkü bir yığın sayıyı atladık, bu yüzden bu sadece açıklama amaçlıdır.

Şimdi Haskell'de bir şekil nasıl temsil edeceğimizi düşünelim. Bir yol, demet kullanmaktır.
Bir daire `(43.1, 55.0, 10.4)` olarak gösterilebilir, burada birinci ve ikinci alanlar dairenin merkezinin koordinatlarıdır ve üçüncü alan yarıçaptır.
Kulağa hoş geliyor, ancak bunlar bir 3D vektörü veya başka herhangi bir şeyi de temsil edebilir.
Bir şekili temsil etmek için kendi türünüzü yapmak daha iyi bir çözüm olacaktır.
Diyelim ki bir şekil daire veya dikdörtgen olabilir. İşte burada:

~~~~ {.haskell: .ghci name="code"}
data Shape = Circle Float Float Float | Rectangle Float Float Float Float   
~~~~

Şimdi bu nedir? Bunu şöyle düşün. `Circle` value constructor'dan 3 tane float değer alan bir alanı vardır.
Yani bir value contructor yazdığımızda, isteğe bağlı olarak ondan sonra bazı türler ekleyebiliriz ve bu türleri içereceği değerleri tanımlar.
Burada, ilk iki alan merkezinin koordinatları, üçüncüsü ise yarıçapıdır. `Rectangle` value contructor'ının float kabul eden dört alanı vardır.
İlk ikisi, sol üst köşesinin koordinatları ve ikinci ikisi, sağ alt köşesinin koordinatlarıdır.

Şimdi alanlar dediğimde, aslında parametreleri kastediyorum. Value consturctor'lar, aslında bir data türünün değerini nihayetinde döndüren fonksiyonlardır.
Bu iki value constructor için tür imzalarına bir göz atalım.

~~~~ {.haskell: .ghci name="code"}
ghci> :t Circle  
Circle :: Float -> Float -> Float -> Shape  
ghci> :t Rectangle  
Rectangle :: Float -> Float -> Float -> Float -> Shape  
~~~~

Bu nedenle value constructor, her şey gibi fonksiyonlardır. Kimin aklına gelirdi? Bir şekil alıp yüzeyini döndüren bir fonksiyon yapalım.

~~~~ {.haskell: .ghci name="code"}
surface :: Shape -> Float  
surface (Circle _ _ r) = pi * r ^ 2  
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)  
~~~~

Buradaki ilk kayda değer şey, tür bildirimidir. Fonksiyonun bir şekil aldığını ve bir float değer döndürdüğünü söylüyor.
`Circle -> Float` için bir tür bildirimi yazamadık çünkü `Circle` bir tür değil, `Shape`'tir.
Tıpkı `True -> Int` tür bildirimi ile bir fonksiyon yazamayacağımız gibi. Burada farkına vardığımız bir sonraki şey,
constructor'lara karşı desen eşleştirme yapabileceğimizdir. `[]` veya `False` veya `5` gibi değerlerle eşleştirdiğimizde daha önce (aslında her zaman)
constructor'larla desen eşleeştirdik, yalnızca bu değerlerde alan yoktu. Sadece bir constructor yazarız ve sonra alanlarını isimlere bağlarız.
Yarıçapla ilgilendiğimiz için, aslında bize dairenin nerede olduğunu söyleyen ilk iki alanı umursamıyoruz.

~~~~ {.haskell: .ghci name="code"}
ghci> surface $ Circle 10 20 10  
314.15927  
ghci> surface $ Rectangle 0 0 100 100  
10000.0  
~~~~

Yaşasın, işe yarıyor! Ancak komut isteminde yalnızca `Circle 10 20 5`'i yazdırmaya çalışırsak, bir hata alırız. 
Bunun nedeni Haskell'in veri türümüzü string(henüz) olarak nasıl görüntüleyeceğini bilmemesidir.
Unutmayın, istemde bir değer yazdırmaya çalıştığımızda, Haskell değerimizin string temsilini almak için önce `show` fonksiyonunu çalıştırır ve sonra bunu terminale yazdırır.
`Shape` türümüzü `Show` tür sınıfınınının bir parçası yapmak için, onu şu şekilde değiştiriyoruz:

~~~~ {.haskell: .ghci name="code"}
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)  
~~~~

Şimdilik çok fazla şey türetmekle(deriving) ilgilenmeyeceğiz. Diyelim ki bir veri bildirimi sonuna `deriving (Show)` eklersek,
Haskell bu türü otomatik olarak `Show` tür sınıfının bir parçası yapar. Şimdi bunu yapabiliriz:

~~~~ {.haskell: .ghci name="code"}
ghci> Circle 10 20 5  
Circle 10.0 20.0 5.0  
ghci> Rectangle 50 230 60 90  
Rectangle 50.0 230.0 60.0 90.0  
~~~~

Value constructor'lar fonksiyonlardır, bu yüzden onları eşleyebilir ve kısmen uygulayabiliriz. Farklı yarıçaplara sahip eşmerkezli dairelerin bir listesini istiyorsak,
bunu yapabiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> map (Circle 10 20) [4,5,6,6]  
[Circle 10.0 20.0 4.0,Circle 10.0 20.0 5.0,Circle 10.0 20.0 6.0,Circle 10.0 20.0 6.0]  
~~~~

Veri türümüz daha iyi olsa da iyidir. İki boyutlu uzayda bir noktayı tanımlayan bir ara veri türü yapalım.
O zaman şekillerimizi daha anlaşılır hale getirmek için bunu kullanabiliriz.

~~~~ {.haskell: .ghci name="code"}
data Point = Point Float Float deriving (Show)  
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)  
~~~~

Bir noktayı tanımlarken, veri türü ve value constructor için aynı adı kullandığımıza dikkat edin. Bunun özel bir anlamı yoktur,
ancak yalnızca bir value constructor varsa tür olarak aynı adı kullanmak yaygındır. Şimdi `Circle`'ın iki alanı var, biri `Point` türünde, diğeri `Float` türünde.
Bu, neyin ne olduğunu anlamayı kolaylaştırır. rectangle(dikdörtgen) için de aynı şey geçerli. 
`surface` fonksiyonumuzu bu değişiklikleri yansıtacak şekilde ayarlamamız gerekiyor.

~~~~ {.haskell: .ghci name="code"}
surface :: Shape -> Float  
surface (Circle _ r) = pi * r ^ 2  
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)  
~~~~

Değiştirmemiz gereken tek şey kalıplardı. Cirle kalıbındaki Point'i göz ardı ettik. 
Rectangle kalıbında, Point'lerin alanlarını elde etmek için iç içe geçmiş bir desen eşleştirme kullandık.
Point'lerin kendilerine herhangi bir nedenle referans vermek isteseydik, kalıp olarak kullanabilirdik.

~~~~ {.haskell: .ghci name="code"}
ghci> surface (Rectangle (Point 0 0) (Point 100 100))  
10000.0  
ghci> surface (Circle (Point 0 0) 24)  
1809.5574  
~~~~

Bir şekli dürtükleyen bir fonksiyona ne dersiniz? Bir şekli, onu x ekseninde hareket ettirme miktarını ve onu y ekseninde hareket ettirme miktarını alır ve
sonra aynı boyutlara sahip yeni bir şekil döndürür, sadece başka bir yerde bulunur.

~~~~ {.haskell: .ghci name="code"}
nudge :: Shape -> Float -> Float -> Shape  
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r  
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))  
~~~~

Oldukça basit. Dürtme miktarlarını şeklin konumunu gösteren noktalara ekleriz.

~~~~ {.haskell: .ghci name="code"}
ghci> nudge (Circle (Point 34 34) 10) 5 10  
Circle (Point 39.0 44.0) 10.0  
~~~~

Doğrudan noktalarla uğraşmak istemiyorsak, sıfır koordinatlarda belirli büyüklükte şekiller oluşturan ve sonra bunları dürten bazı yardımcı fonksiyonlar yapabiliriz.

~~~~ {.haskell: .ghci name="code"}
baseCircle :: Float -> Shape  
baseCircle r = Circle (Point 0 0) r  
  
baseRect :: Float -> Float -> Shape  
baseRect width height = Rectangle (Point 0 0) (Point width height)  
~~~~

~~~~ {.haskell: .ghci name="code"}
ghci> nudge (baseRect 40 100) 60 23  
Rectangle (Point 60.0 23.0) (Point 100.0 123.0)  
~~~~

Veri türünüzü elbette modüllerinize aktarabilirsiniz. Bunu yapmak için, türünüzü export ettiğiniz fonksiyonlarla birlikte yazın ve ardından bazı parantezler ekleyin ve
bunlarda export etmek istediğiniz value constructor'lara virgülle ayırarak belirtin. Belirli bir tür için tüm value constructor'ları export etmek istiyorsanız,
şunu yazın `".."`.

Burada tanımladığımız fonksiyonları ve türleri bir modülde dışa aktarmak istersek, şu şekilde başlayabiliriz:

~~~~ {.haskell: .ghci name="code"}
module Shapes   
( Point(..)  
, Shape(..)  
, surface  
, nudge  
, baseCircle  
, baseRect  
) where  
~~~~

`Shape (..)` yaparak, Shape için tüm value contructor'larını export ettik, yani modülümüzü import eden kişi 
`Rectangle` ve `Circle` value constructor'larını kullanarak şekiller yapabilir. `Shape (Rectangle, Circle)` yazmakla aynıdır.

Ayrıca, export bildirimine yalnızca `Shape` yazarak `Shape` için herhangi bir value constructor'u export etmeyi tercih edebilirdik.
Bu şekilde, modülümüzü import eden biri, yalnızca `baseCircle` ve `baseRect` yardımcı fonkssiyonlarını kullanarak şekiller oluşturabilir. `Data.Map` bu yaklaşımı kullanır.
`Map.Map [(1,2), (3,4)]` yaparak bir map oluşturamazsınız çünkü bu value constructor'u export etmez.
Ancak, `Map.fromList` gibi yardımcı fonksiyonlardan birini kullanarak bir map'leme yapabilirsiniz.
Unutmayın, value constructor'lar, alanları parametre olarak alan ve sonuç olarak bir türden (`Shape` gibi) bir değer döndüren fonksiyonlardır.
Dolayısıyla, bunları export etmeyi seçtiğimizde, modülümüzü import eden kişinin bu fonksiyonları kullanmasını engelleriz,
ancak export edilen diğer bazı fonksiyonlar bir tür döndürürse, bunları custom veri türlerimizden değerlerini oluşturmak için kullanabiliriz.

Bir veri türünün value constructor'larını export etmek, uygulamalarını gizleyeceğimiz şekilde onları daha soyut hale getirir.
Ayrıca, modülümüzü kullanan kişi, value constructor'lara karşı desen eşleştirme yapamaz.


Sözdizimi kaydı
---------------
![record](../img/record.png)

Tamam, bir kişiyi tanımlayan bir veri türü oluşturmakla görevlendirildik. Bu kişi hakkında saklamak istediğimiz bilgiler:
first name, last name, age, height, phone number ve favorite ice-cream flavor. Seni bilmiyorum ama bir insan hakkında bilmek istediğim tek şey bu. Hadi bir deneyelim!

~~~~ {.haskell: .ghci name="code"}
data Person = Person String String Int Float String String deriving (Show)   
~~~~

İlk alan first name, ikincisi surname, üçüncüsü age vb. Bir insan yapalım.

~~~~ {.haskell: .ghci name="code"}
ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"  
ghci> guy  
Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"    
~~~~

Biraz okuması zor olsa da bu harika. Bir kişiden ayrı bilgi almak için bir fonksiyon oluşturmak istersek ne olur?
Bir kişinin adını alan bir fonksiyon, bir kişinin soyadını alan bir fonksiyon vb. Onları böyle tanımlamamız gerekirdi.

~~~~ {.haskell: .ghci name="code"}
firstName :: Person -> String  
firstName (Person firstname _ _ _ _ _) = firstname  
  
lastName :: Person -> String  
lastName (Person _ lastname _ _ _ _) = lastname  
  
age :: Person -> Int  
age (Person _ _ age _ _ _) = age  
  
height :: Person -> Float  
height (Person _ _ _ height _ _) = height  
  
phoneNumber :: Person -> String  
phoneNumber (Person _ _ _ _ number _) = number  
  
flavor :: Person -> String  
flavor (Person _ _ _ _ _ flavor) = flavor  
~~~~

Vay canına! Bunu yazmaktan kesinlikle zevk almadım! Yazmak çok zahmetli ve sıkıcı olmasına rağmen, bu yöntem işe yarıyor.

~~~~ {.haskell: .ghci name="code"}
ghci> let guy = Person "Buddy" "Finklestein" 43 184.2 "526-2928" "Chocolate"  
ghci> firstName guy  
"Buddy"  
ghci> height guy  
184.2  
ghci> flavor guy  
"Chocolate"
~~~~

Daha iyi bir yolu olmalı, diyorsun! Hayır, yok, üzgünüm. :(

Şaka yapıyorum, var. Hahaha! Haskell'in yapımcıları çok akıllıydı ve bu senaryoyu öngörmüşlerdi. Veri türlerini yazmak için alternatif bir yol içeriyorlardı.
Kayıt sözdizimi ile yukarıdaki işlevselliği nasıl elde edebileceğimiz aşağıda açıklanmıştır.

~~~~ {.haskell: .ghci name="code"}
data Person = Person { firstName :: String  
                     , lastName :: String  
                     , age :: Int  
                     , height :: Float  
                     , phoneNumber :: String  
                     , flavor :: String  
                     } deriving (Show)   
~~~~

Bu nedenle, alan türlerini sadece birbiri ardına adlandırmak ve boşluklarla ayırmak yerine, küme parantezleri kullanıyoruz.
İlk önce alanın adını yazıyoruz, mesela, `firstName` ve sonra bir çift kolon `::` (Paamayim Nekudotayim, olarak da adlandırılıyor haha) yazıyoruz ve sonra türü belirliyoruz.
Ortaya çıkan veri türü tamamen aynıdır. Bunun ana yararı, veri türündeki alanları arayan fonksiyonlar oluşturmasıdır.
Bu veri türünü oluşturmak için kayıt sözdizimini kullanarak Haskell şu fonksiyonları otomatik olarak yaptı:
`firstName`, `lastName`, `age`, `height`, `phoneNumber` ve `flavor`.

~~~~ {.haskell: .ghci name="code"}
ghci> :t flavor  
flavor :: Person -> String  
ghci> :t firstName  
firstName :: Person -> String   
~~~~

Sözdizimi kaydını kullanmanın başka bir yararı daha var. Tür için `Show` türetdiğimizde, türü tanımlamak ve başlatmak için sözdizimi kaydı kullanırsak,
onu farklı bir şekilde görüntüler. Bir arabayı temsil eden bir türümüz olduğunu varsayalım. Onu yapan firmayı, model adını ve üretim yılını takip etmek istiyoruz. 

~~~~ {.haskell: .ghci name="code"}
data Car = Car String String Int deriving (Show)  
~~~~

~~~~ {.haskell: .ghci name="code"}
ghci> Car "Ford" "Mustang" 1967  
Car "Ford" "Mustang" 1967  
~~~~

Sözdizimi kaydı kullanarak tanımlarsak, bunun gibi yeni bir araba yapabiliriz.

~~~~ {.haskell: .ghci name="code"}
data Car = Car {company :: String, model :: String, year :: Int} deriving (Show)  
~~~~

~~~~ {.haskell: .ghci name="code"}
ghci> Car {company="Ford", model="Mustang", year=1967}  
Car {company = "Ford", model = "Mustang", year = 1967}  
~~~~

Yeni bir araba yaparken, hepsini listelediğimiz sürece alanları doğru sıraya koymamız gerekmez. Ancak sözdizimi kaydını kullanmazsak, bunları sırayla belirtmemiz gerekir.

Bir constructor birkaç alana sahipse ve hangi alanın hangisi olduğu belli olmadığında sözdizimi kaydı kullanın.
data `Vector = Vector Int Int Int` yaparak bir 3D vektör veri türü yaparsak, alanların bir vektörün bileşenleri olduğu oldukça açıktır.
Ancak, `Person` ve `Car` türlerimizde bu çok açık değildi ve sözdizimi kaydını kullanmaktan büyük ölçüde yararlandık.


Tür parametreleri
----------------

Bir value constructor, bazı değer parametrelerini alabilir ve ardından yeni bir değer üretebilir. Örneğin, `Car` constructor'ı üç değer alır ve bir car değeri üretir.
Benzer bir şekilde,** type constructor'lar** yeni türler üretmek için türlerı parametre olarak alabilir.
Bu ilk başta biraz fazla meta gibi gelebilir, ancak o kadar da karmaşık değil. C++'daki şablonlara aşina iseniz, bazı paralellikler göreceksiniz.
Hangi tür parametrelerin eylemde nasıl çalıştığına dair net bir resim elde etmek için, daha önce karşılaştığımız bir türün nasıl uygulandığına bir göz atalım.

~~~~ {.haskell: .ghci name="code"}
data Maybe a = Nothing | Just a  
~~~~

![yeti](../img/yeti.png)
Buradaki `a`, tür parametresidir. Ve işin içinde bir tür parametresi olduğu için, `Maybe` bir type constructor'dır diyoruz.
Bu veri türünün `Nothing` olmadığında neyi tutmasını istediğimize bağlı olarak, bu type constructor bir tür `Maybe Int`, `Maybe Car`, `Maybe String` vb. üretebilir.
Hiçbir değer sadece `Maybe` türünde olamaz, çünkü bu kendi başına bir tür değildir, bir type constructor'dur.
Bunun bir değerin parçası olabileceği gerçek bir tür olması için, tüm tür parametrelerinin doldurulmuş olması gerekir.

Yani eğer `Char`'ı tür parametresi olarak `Maybe`'ye geçirirsek, bir tür `Maybe Char` elde ederiz.
Örneğin, `Just 'a'` değerinin bir türü Maybe Char vardır.

Bilmiyor olabilirsiniz, ancak `Maybe`'yi kullanmadan önce bir tür parametresi olan bir tür kullandık.
Bu tür, list türüdür. Oyunda bir miktar sözdizimsel şeker olsa da, list türü somut bir tür üretmek için bir parametre alır.
Değerler bir `[Int]` türüne, `[Char]` türüne, `[[String]]` türüne sahip olabilir, ancak sadece `[]` türü olan bir değere sahip olamazsınız.

`Maybe` türü ile oynayalım.

~~~~ {.haskell: .ghci name="code"}
ghci> Just "Haha"  
Just "Haha"  
ghci> Just 84  
Just 84  
ghci> :t Just "Haha"  
Just "Haha" :: Maybe [Char]  
ghci> :t Just 84  
Just 84 :: (Num t) => Maybe t  
ghci> :t Nothing  
Nothing :: Maybe a  
ghci> Just 10 :: Maybe Double  
Just 10.0  
~~~~

Tür parametreleri kullanışlıdır çünkü veri türümüzde ne tür türlerin bulunmasını istediğimize bağlı olarak onlarla farklı türler oluşturabiliriz.
`:t Just "Haha"` yaptığımızda, tür çıkarım motoru(type inference engine) bunun `Maybe [Char]` türünde olduğunu anlar, 
çünkü `Just a`'daki a bir string'se, o zaman `Maybe a`'daki `a`'da bir string olmalıdır.

`Nothing` türünün `Maybe a` olduğuna dikkat edin. Türü polimorfiktir. Eğer bir fonksiyon parametre olarak `Maybe Int` gerektiriyorsa,
ona `Nothing` verebiliriz, çünkü `Nothing` zaten bir değer içermez ve bu yüzden önemi yoktur. `Maybe a` türü, mecbur kalırsa `Maybe Int` gibi davranabilir,
tıpkı `5`'in bir `Int` veya `Double` gibi davranabileceği gibi. Benzer şekilde, boş listenin türü `[a]`'dır.
Boş bir liste, herhangi bir şeyin listesi gibi davranabilir. Bu yüzden `[1,2,3] ++ []` ve `["ha", "ha", "ha"] ++ []` yapabiliriz.

Tür parametrelerini kullanmak çok faydalıdır, ancak yalnızca bunları kullanırken mantıklıdır.
Genellikle bunları, `Maybe a` türünde olduğu gibi, veri türünün daha sonra içinde tuttuğu değerin türüne bakılmaksızın çalışacağı zaman kullanırız. 
Türümüzün bir tür kutu gibi davranıyorsa, onları kullanmak iyidir. `Car` veri türümüzü buradan değiştirebiliriz:

~~~~ {.haskell: .ghci name="code"}
data Car = Car { company :: String  
               , model :: String  
               , year :: Int  
               } deriving (Show)  
~~~~

Buna:

~~~~ {.haskell: .ghci name="code"}
data Car a b c = Car { company :: a  
                     , model :: b  
                     , year :: c   
                     } deriving (Show)  
~~~~

Ama gerçekten faydalanır mıyız? Cevap şudur: Muhtemelen hayır, çünkü sonunda sadece `Car String String Int` türünde çalışan fonksiyonları tanımlayacağız.
Örneğin, ilk `Car` tanımımız göz önüne alındığında, arabanın özelliklerini güzel küçük bir metinle gösteren bir fonksiyon yapabiliriz.

~~~~ {.haskell: .ghci name="code"}
tellCar :: Car -> String  
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y  
~~~~

~~~~ {.haskell: .ghci name="code"}
ghci> let stang = Car {company="Ford", model="Mustang", year=1967}  
ghci> tellCar stang  
"This Ford Mustang was made in 1967" 
~~~~

Sevimli küçük bir fonksiyon! tür bildirimi sevimli ve güzel çalışıyor. Peki ya `Car` `Car a b c` olsaydı?

~~~~ {.haskell: .ghci name="code"}
tellCar :: (Show a) => Car String String a -> String  
tellCar (Car {company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ " was made in " ++ show y  
~~~~

Bu fonksiyonu, `Car` türü `(Show a) => Car String String a` almaya zorlamamız gerekir. Tür imzasının daha karmaşık olduğunu ve gerçekten elde edeceğimiz tek yararın,
`c` türü olarak `Show` tür sınıfının bir instance'ı olan herhangi bir tür kullanabilmemiz olduğunu görebilirsiniz.

~~~~ {.haskell: .ghci name="code"}
ghci> tellCar (Car "Ford" "Mustang" 1967)  
"This Ford Mustang was made in 1967"  
ghci> tellCar (Car "Ford" "Mustang" "nineteen sixty seven")  
"This Ford Mustang was made in \"nineteen sixty seven\""  
ghci> :t Car "Ford" "Mustang" 1967  
Car "Ford" "Mustang" 1967 :: (Num t) => Car [Char] [Char] t  
ghci> :t Car "Ford" "Mustang" "nineteen sixty seven"  
Car "Ford" "Mustang" "nineteen sixty seven" :: Car [Char] [Char] [Char]
~~~~

![cowboy](../img/cowboy.png)
Yine de gerçek hayatta, çoğu zaman `Car String Int`'i kullanırdık ve bu yüzden `Car` türünün parametreleştirmenin(parameterized) gerçekten buna değmeyeceği anlaşılıyor.
Veri türünün çeşitli value constructor'lar içinde bulunan tür, türün çalışması için gerçekten önemli olmadığında genellikle tür parametrelerini kullanırız.
Bir şeyler listesi, bir şeylerin listesidir ve ne tür bir şey olduğu önemli değildir, yine de işe yarayabilir. Bir sayı listesi toplamak istiyorsak,
daha sonra toplama fonksiyonunda özellikle bir sayı listesi istediğimizi belirtebiliriz. 
Aynısı `Maybe` için de geçerli. `Maybe` hiçbir şeye sahip olmama veya bir şeye sahip olma seçeneğini temsil eder. Bunun ne türü'ı olduğu önemli değil.

Daha önce tanıştığımız parametreleştirilmiş bir türe başka bir örnek, `Data.Map`'ten `Map k v`'dir.
`k`, bir map'deki key'lerin türüdür ve `v`, value'ların türüdür. Bu, tür parametrelerinin çok kullanışlı olduğu iyi bir örnektir. 
map'lerin parametreleştirilmiş olması, key'lerin türü `Ord` tür sınıfının bir parçası olduğu sürece, herhangi bir türden diğer türlere eşlemeler yapmamızı sağlar.
Bir eşleme türü tanımlıyor olsaydık, veri bildirimine bir tür sınıfı kısıtlaması ekleyebilirdik:

~~~~ {.haskell: .ghci name="code"}
data (Ord k) => Map k v = ...  
~~~~

Bununla birlikte, Haskell'de **veri bildirimlerine asla tür sınıfı kısıtlamalarının eklenmemesi** çok güçlü bir kuraldır.
Neden? Pekala, çünkü pek fayda sağlamıyoruz, ancak ihtiyacımız olmasa bile daha fazla sınıf kısıtlaması yazıyoruz.
`Ord k` kısıtlamasını `Map k v` için veri bildirimi koyarsak veya koymazsak, kısıtlamayı bir map'teki key'lerin sıralanabileceğini varsayan fonksiyonlara koymamız gerekir.
Ancak veri bildirimine kısıtlama koymazsak, key'lerin sıralanıp sıralanmayacağına aldırış etmeyen fonksiyonların tür bildirimlerine `(Ord k) =>` koymamız gerekmez.
Böyle bir fonksiyonun bir instance'ı, sadece bir map alan ve onu bir ilişkilendirmeli listeye dönüştüren `toList`'tir.
Tür imzası `toList :: Map k a -> [(k, a)]` şeklindedir. `Map k v` veri bildiriminde bir tür kısıtlamasına sahipse,
fonksiyon anahtarları sırayla karşılaştırmasa bile toList türünün `toList :: (Ord k) => Map k a -> [(k, a)]` olması gerekir.

Bu nedenle, mantıklı görünse bile veri bildirimine tür kısıtlamaları koymayın, çünkü bunları her iki şekilde de fonksiyon tür bildiriminelerine eklemeniz gerekir.

Bir 3D vektör türü uygulayalım ve bunun için bazı işlemler ekleyelim.
Parametreli bir tür kullanacağız çünkü genellikle sayısal türler içerse de, yine de birkaçını destekleyecektir.

~~~~ {.haskell: .ghci name="code"}
data Vector a = Vector a a a deriving (Show)  
  
vplus :: (Num t) => Vector t -> Vector t -> Vector t  
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)  
  
vectMult :: (Num t) => Vector t -> t -> Vector t  
(Vector i j k) `vectMult` m = Vector (i*m) (j*m) (k*m)  
  
scalarMult :: (Num t) => Vector t -> Vector t -> t  
(Vector i j k) `scalarMult` (Vector l m n) = i*l + j*m + k*n   
~~~~

`vplus`, iki vektörü birbirine eklemek içindir. Yalnızca karşılık gelen bileşenleri eklenerek iki vektör eklenir. `scalarMult`, iki vektörün skaler çarpımı içindir ve
`vectMult`, bir vektörü skaler ile çarpmak içindir. Bu fonksiyonlar `Vector Int, Vector Integer, Vector Float` türlerinde, ne olursa olsun, `Vector a`'dan gelen `a`, 
`Num` tür sınıfından olduğu sürece çalışabilir. Ayrıca, bu fonksiyonlar için tür bildirimini incelerseniz,
yalnızca aynı türdeki vektörler üzerinde çalışabileceklerini ve ilgili sayıların da vektörlerde bulunan türden olması gerektiğini göreceksiniz.
veri bildirimine `Num` class kısıtlaması koymadığımıza dikkat edin, çünkü bunu yine de fonksiyonlarda tekrar etmemiz gerekir.

Bir kez daha, type constructor ile value constructor birbirinden ayırmak çok önemlidir. Bir veri türü bildirirken,
`=`'den önceki kısım type constructor'dur ve ondan sonraki constructor'lar (muhtemelen |'ler ile ayrılırlar) value constructor'lardır.
Bir fonksiyona bir tür `Vektor t t t -> Vektor t t t -> t` vermek yanlış olur, çünkü tür bildirimine türleri koymamız gerekir ve 
vektör **type** constructor'u yalnızca bir parametre alır, oysa value constructor'u üç alır. Vektörlerimizle oynayalım.

~~~~ {.haskell: .ghci name="code"}
ghci> Vector 3 5 8 `vplus` Vector 9 2 8  
Vector 12 7 16  
ghci> Vector 3 5 8 `vplus` Vector 9 2 8 `vplus` Vector 0 2 3  
Vector 12 9 19  
ghci> Vector 3 9 7 `vectMult` 10  
Vector 30 90 70  
ghci> Vector 4 9 5 `scalarMult` Vector 9.0 2.0 4.0  
74.0  
ghci> Vector 2 9 3 `vectMult` (Vector 4 9 5 `scalarMult` Vector 9 2 4)  
Vector 148 666 222   
~~~~

Türetilmiş instance'lar
-----------------------

![gob](../img/gob.png)
[Tür sınıfları 101 bölümünde](../tr/03-types-and-typeclasses.md#tür-sınıfları-101) tür sınıfların temellerini açıkladık. Bir tür sınıfının bazı davranışları tanımlayan bir tür arayüz(type interface) olduğunu açıkladık.
Bir tür, bu davranışı destekliyorsa, bir tür sınıfının bir instance'ı yapılabilir. Örnek: Int türü, Eq tür sınıfının bir instance'ıdır, çünkü Eq tür sınıfı, 
eşitlenebilen şeyler için davranışı tanımlar. Ve tamsayılar eşitlenebildiği için, Int, Eq tür sınıfının bir parçasıdır.
Gerçek kullanışlılık, Denklem için arayüz görevi gören == ve /= fonksiyonlarıyla birlikte gelir. Bir tür, Eq tür sınıfıının bir parçasıysa,
o türdeki değerlerle == fonksiyonlarını kullanabiliriz. Bu nedenle 4 == 4 ve "foo" /= "bar" tür kontrolü gibi ifadeler.













































































































