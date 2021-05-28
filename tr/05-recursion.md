Özyineleme
==========

Merhaba özyineleme!
-------------------

![recursion](../img/recursion.png)
Bir önceki bölümde kısaca özyinelemeden bahsediyoruz. Bu bölümde özyinelemeye, Haskell için neden önemli olduğuna ve özyinelemeli düşünerek sorunlara nasıl çok özlü ve
zarif çözümler bulabileceğimize daha yakından bakacağız.

Yinelemenin ne olduğunu hala bilmiyorsanız, bu cümleyi okuyun. Haha! Şaka yapıyorum! Özyineleme, aslında fonksiyonun kendi tanımı içinde uygulandığı
fonksiyonları tanımlamanın bir yoludur. Matematikteki tanımlar genellikle yinelemeli olarak verilir. Örneğin, fibonacci dizisi özyinelemeli olarak tanımlanır.
İlk olarak, ilk iki fibonacci sayısını yinelemesiz olarak tanımlıyoruz. F(0) = 0 ve F(1) = 1 olduğunu söylüyoruz, yani 0. ve 1. fibonacci sayıları sırasıyla 0 ve 1'dir.
Daha sonra, başka herhangi bir doğal sayı için, bu fibonacci sayısının önceki iki fibonacci sayısının toplamı olduğunu söylüyoruz.
Yani F(n) = F(n-1) + F(n-2). Bu şekilde, F(3), F(2) + F(1), yani (F(1) + F(0)) + F(1) olur. Şimdi sadece yinelemeli olmayan şekilde tanımlanmış
fibonacci sayılarına geldiğimiz için, F(3)'ün 2 olduğunu rahatlıkla söyleyebiliriz. Özyinelemesiz olarak tanımlanan bir özyineleme tanımında
bir veya iki öğeye sahip olmak (burada F(0) ve F(1) gibi) aynı zamanda **uç koşulu** olarak adlandırılır ve özyinelemeli fonksiyonunuzun sona ermesini istiyorsanız önemlidir.
F(0) ve F(1)'i özyinelemesiz olarak tanımlamasaydık, hiçbir zaman bir çözüm elde edemezdiniz çünkü 0'a ulaşırdınız ve sonra negatif sayılara girersiniz.
Birdenbire, F(-2000)'nin F(-2001) + F (-2002) olduğunu ve görünürde bir son olmadığını söylersiniz!

Özyineleme Haskell için önemlidir, çünkü zorunlu dillerin aksine, Haskell'de bir şeyi nasıl elde ettiğinizi bildirmek yerine bir şeyin ne olduğunu bildirerek
hesaplar yaparsınız. Bu nedenle Haskell'de while döngüleri veya döngüleri yoktur ve bunun yerine bir şeyin ne olduğunu bildirmek için
birçok kez özyineleme kullanmak zorunda kalırız.


Maksimum harika
---------------

`maximum` fonksiyonu, sıralanabilecek şeylerin bir listesini alır (örneğin, `Ord` tür sınıfının instance'ları) ve bunların en büyüğünü döndürür.
Bunu zorunlu bir şekilde nasıl uygulayacağınızı düşünün. Muhtemelen şimdiye kadarki maksimum değeri tutacak bir değişken oluşturursunuz ve
sonra bir listenin öğeleri arasında döngü yaparsınız ve eğer bir öğe mevcut maksimum değerden daha büyükse, onu o öğeyle değiştirirsiniz.
Sonunda kalan maksimum değer sonuçtur. Vay canına! Bu kadar basit bir algoritmayı tanımlamak için oldukça fazla kelime var!

Şimdi, onu yinelemeli olarak nasıl tanımlayacağımızı görelim. İlk önce bir uç koşulu oluşturabilir ve bir tekli listenin maksimumunun içindeki
tek öğeye eşit olduğunu söyleyebiliriz. O zaman daha uzun bir listenin maksimumunun kafa, kuyruk maksimumundan büyükse kafa olduğunu söyleyebiliriz.
Kuyruğun maksimumu daha büyükse, o zaman kuyruğun maksimumudur. Bu kadar! Şimdi bunu Haskell'de uygulayalım.

~~~~ {.haskell: .ghci name="code"}
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  
~~~~

Gördüğünüz gibi, desen eşleştirme özyineleme ile harika gidiyor! Zorunlu dillerin çoğunda desen eşleme yoktur,
bu nedenle uç koşulları test etmek için çok sayıda if else ifadesi yapmanız gerekir. Burada onları basitçe kalıplar olarak ortaya koyuyoruz.
Yani ilk uç koşulu, liste boşsa çökme demektir! Mantıklı çünkü boş bir listenin maksimum değeri nedir? Bilmiyorum.
İkinci desen ayrıca bir uç koşulunu da ortaya koyar. Tekil liste ise, sadece tek öğeyi geri verin diyor.

Şimdi üçüncü model, *where* eyleminin gerçekleştiği yerdir. Bir listeyi başlığa ve kuyruğa ayırmak için desen eşleştirmeyi kullanırız.
Bu, listelerle özyineleme yaparken çok yaygın bir deyimdir, bu yüzden buna alışın. Listenin geri kalanının maksimumu olarak `maxTail`'i tanımlamak için bir
*where* bağlaması kullanıyoruz. Ardından, kafanın listenin geri kalanının maksimumundan daha büyük olup olmadığını kontrol ederiz. Eğer öyleyse, başı döndürürüz.
Aksi takdirde, listenin geri kalanının maksimumunu döndürürüz.

Örnek bir sayı listesi alalım ve bunların üzerinde nasıl çalışacağına bakalım: `[2,5,1]`. Bunun üzerinde `maximum'` çağırırsak, ilk iki kalıp eşleşmeyecektir.
Üçüncüsü olacaktır ve liste `2` ve `[5,1]`'e bölünmüştür. *where* cümlesi maksimum `[5,1]` değerini bilmek istiyor, bu yüzden biz bu rotayı izliyoruz.
Yine üçüncü kalıpla eşleşir ve `[5,1]`, `5` ve `[1]`'e bölünür. Yine, `where` cümlesi maksimum `[1]`'i bilmek istiyor. Çünkü bu uç koşulu `1` döndürür.
En sonunda! Yani bir adım yukarı çıkarsak, `5`'i maksimum `[1]` ile karşılaştırırsak (ki bu `1`'dir), açıkça `5`'i geri alırız. Artık maksimum `[5,1]`'in `5` olduğunu biliyoruz.
`2` ve `[5,1]` olduğumuz yerde tekrar bir adım yukarı çıkıyoruz. `2`'yi maksimum `[5,1]` olan `5` ile karşılaştırarak `5`'i seçeriz.

Bu fonksiyonu yazmanın daha net bir yolu, `max`. Hatırlarsanız, `max` iki sayıyı alıp büyük olanı döndüren bir fonksiyondur.
`max` kullanarak `maximum'` değerini şöyle yeniden yazabiliriz:

~~~~ {.haskell: .ghci name="code"}
maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs) = max x (maximum' xs)  
~~~~

Zariflik için bu nasıl! Temelde, bir listenin maksimum değeri, ilk elemanın maksimum ve kuyruk kısmının maksimumudur.

![maxs](../img/maxs.png)


Birkaç tane daha yinelemeli fonksiyon
-------------------------------------

Artık genel olarak özyinelemeli düşünmeyi bildiğimize göre, özyinelemeyi kullanarak birkaç fonksiyona uygulayalım. İlk olarak, `replicate`'i uygulayacağız.
`replicate`, bir `Int` ve bir eleman alır ve aynı elemanın birkaç tekrarına sahip bir liste döndürür. Örneğin, `replicate 3 5` `[5,5,5]` değerini döndürür.
En uç durumu düşünelim. Tahminimce uç koşulu 0 veya daha az. Bir şeyi sıfır kez kopyalamaya çalışırsak, boş bir liste döndürmelidir.
Ayrıca negatif sayılar için, çünkü gerçekten mantıklı değil.

~~~~ {.haskell: .ghci name="code"}
replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  
~~~~

Burada kalıplar yerine korumalar kullandık çünkü boolean koşulunu test ediyoruz. `n` 0'dan küçükse veya 0'a eşitse, boş bir liste döndür.
Aksi takdirde, ilk öğe olarak `x` ve ardından `x`'in kuyruk olarak n-1 kez çoğaltıldığı bir liste döndürür. 
Sonunda, `(n-1)` bölümü, fonksiyonumuzun uç durumuna ulaşmasına neden olacaktır.

**Not**: `Num`, `Ord`'un bir alt sınıfı değildir. Bu, bir sayıyı oluşturan şeyin gerçekten bir sıralamaya uymak zorunda olmadığı anlamına gelir.
Bu nedenle, toplama veya çıkarma ve ayrıca karşılaştırma yaparken hem `Num` hem de `Ord` sınıf kısıtlamalarını belirtmemiz gerekiyor.

Sırada, `take`'i uygulayacağız. Bir listeden belirli sayıda öğe alır. Örneğin, `take 3 [5,4,3,2,1]` `[5,4,3]` sonucunu döndürür.
Bir listeden 0 veya daha az eleman almaya çalışırsak, boş bir liste alırız. Ayrıca boş bir listeden bir şey almaya çalışırsak, boş bir liste alırız.
Bunların iki uç koşul olduğuna dikkat edin. Öyleyse şunu yazalım:

~~~~ {.haskell: .ghci name="code"}
take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  
    | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  
~~~~

İlk örüntü, 0 veya negatif sayıda eleman almaya çalışırsak boş bir liste aldığımızı belirtir. Listeyle eşleştirmek için `_` kullandığımıza dikkat edin 
çünkü bu durumda ne olduğu gerçekten umurumuzda değil. Ayrıca bir koruma kullandığımıza dikkat edin, ancak `otherwise` bölümü olmadan.
Bu, `n`'nin 0'dan büyük olduğu ortaya çıkarsa, eşleşmenin bir sonraki modele geçeceği anlamına gelir. İkinci kalıp, boş bir listeden bir şey almaya çalışırsak
boş bir liste aldığımızı gösterir. Üçüncü kalıp, listeyi bir baş ve bir kuyruğa olarak böler. Ve sonra, bir listeden `n` eleman almanın,
başında `x` olan bir listeye ve ardından kuyruktan `n-1` elemanlarını kuyruk olarak alan bir listeye eşit olduğunu belirtiyoruz.
Örneğin `[4,3,2,1]`'den 3 tanesini almaya çalışırsak değerlendirmenin nasıl görüneceğini yazmak için bir kağıt parçası kullanmayı deneyin.

`reverse` bir listeyi ters çevirir. Uç durumu düşünün. Bu ne? Hadi ... bu boş liste! Tersine çevrilmiş boş bir liste, boş listenin kendisine eşittir.
Tamam. Geri kalanı ne olacak? Bir listeyi bir başa ve bir kuyruğa ayırırsak, tersine çevrilmiş listenin ters kuyruk ve ardından en sondaki başa eşit olduğunu söyleyebilirsin.

~~~~ {.haskell: .ghci name="code"}
reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]  
~~~~

İşte başlıyoruz!

Haskell sonsuz listeleri desteklediğinden, özyinelememizin gerçekten bir uç koşula sahip olması gerekmez. Ama eğer ona sahip değilse,
ya sonsuza dek çalkalamaya devam edecek ya da sonsuz bir liste gibi sonsuz bir veri yapısı üretecektir. Sonsuz listelerin iyi yanı, onları istediğimiz yerde kesebilmemizdir.
`repeat` bir elemanı alır ve sadece o elemana sahip sonsuz bir liste döndürür. Bunun yinelemeli bir uygulaması gerçekten kolaydır, izleyin.

~~~~ {.haskell: .ghci name="code"}
repeat' :: a -> [a]  
repeat' x = x:repeat' x  
~~~~

`repeat 3`'ü çağırmak bize 3 ile başlayan ve sonra kuyruk olarak sonsuz sayıda 3 olan bir liste verecektir.  Dolayısıyla `repeat 3` demek `3:repeat 3` olarak değerlendirilir,
`3:(3:repeat 3)`, yani `3:(3:(3:repeat 3))`, vb. `repeat 3` değerlendirmeyi asla bitirmez, oysa `take 5 (repeat 3)` bize beş 3'lü bir liste verir. 
Yani aslında `replicate 5 3` yapmak gibi.

`zip` iki listeyi alır ve bunları birlikte sıkıştırır. `zip [1,2,3] [2,3]`, `[(1,2), (2,3)]` sonucunu döndürür, çünkü uzun listeyi, daha kısa olanın uzunluğuyla
eşleşecek şekilde kısaltır. Boş bir liste olan bir şeyi sıkıştırsak nasıl olur? O zamanlar boş bir listemiz var. Yani bizim uç durumumuz var. 
Ancak `zip`, parametre olarak iki listeyi alır, bu nedenle aslında iki uç koşulu vardır.

~~~~ {.haskell: .ghci name="code"}
zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys  
~~~~

İlk iki kalıp, birinci liste veya ikinci liste boşsa boş bir liste aldığımızı söylüyor. Üçüncüsü, sıkıştırılmış iki listenin kafalarını eşleştirmeye ve
ardından sıkıştırılmış kuyruklara dokunmaya eşit olduğunu söylüyor. Sıkıştırma `[1,2,3]` ve `['a', 'b']` sonunda `[3]`'ü `[]` ile sıkıştırmayı deneyecektir.
Uç koşulu kalıpları devreye girer ve sonuç `(1,'a'):(2,'b'):[]` olur, bu da `[(1,'a'),(2,'b')]`.

Bir tane daha standart kütüphane fonksiyonu uygulayalım - `elem`. Bir öğeyi ve bir listeyi alır ve bu öğenin listede olup olmadığını görür.
Listelerde çoğu zaman olduğu gibi uç koşulu boş listedir. Boş bir listenin hiçbir öğe içermediğini biliyoruz, bu nedenle aradığımız droidlere kesinlikle sahip değil.

~~~~ {.haskell: .ghci name="code"}
elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs   
~~~~

Oldukça basit ve beklenen. Baş öğe değilse, kuyruğu kontrol ederiz. Boş bir listeye ulaşırsak, sonuç `False` olur.

Quick, sort!
------------

Sıralanabilecek bir öğe listemiz var. Türleri, `Ord` tür sınıfının bir instance'ıdır. Ve şimdi, onları sıralamak istiyoruz!
Quick sort(Hızlı sıralama) adı verilen çok güzel bir sıralama algoritması var. Öğeleri sınıflandırmanın çok akıllıca bir yolu.
Zorunlu dillerde hızlı sıralamayı uygulamak 10 satırdan fazla sürerken, uygulama Haskell'de çok daha kısa ve zariftir. 
Quicksort, Haskell için bir tür poster çocuğu haline geldi. Bu nedenle, hızlı sıralamayı Haskell'de uygulamak gerçekten sevimsiz olarak görülse de,
herkes bunu Haskell'in ne kadar zarif olduğunu göstermek için yapıyor.

![quickman](../img/quickman.png) Yani, tür imzası `quicksort :: (Ord a) => [a] -> [a]` olacaktır. Orada sürpriz yok. Uç koşulu?
Beklendiği gibi boş liste. Sıralanmış bir boş liste, boş bir listedir.
Şimdi ana algoritma geliyor: **Sıralı bir liste, öndeki listenin başlığından küçük (veya ona eşit) tüm değerleri içeren (ve bu değerler sıralanan),
ardından listenin başına gelen ve ardından listenin başına gelen bir listedir ve sonra baştan daha büyük olan tüm değerler gelir (ayrıca sıralanırlar).**
Bu tanımda iki kez sıralandığımızı söylediğimize dikkat edin, bu yüzden muhtemelen özyinelemeli çağrıyı iki kez yapmak zorunda kalacağız!
Ayrıca bunu fiil kullanarak tanımladığımıza dikkat edin, bunu yap, şunu yap, sonra şunu yap ... demek yerine algoritmayı tanımlamaktır.
Fonksiyonel programlamanın güzelliği budur! Listeyi, yalnızca listemizin başından daha küçük öğeleri ve yalnızca daha büyük öğeleri elde etmek için nasıl filtreleyeceğiz?
Anlamaları listeleyin. Öyleyse, dalalım ve bu fonksiyonu tanımlayalım.

~~~~ {.haskell: .ghci name="code"}
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  
~~~~

Doğru davranıp davranmadığını görmek için küçük bir test çalıştıralım.

~~~~ {.haskell: .ghci name="code"}
ghci> quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]  
[1,2,2,3,3,4,4,5,6,7,8,9,10]  
ghci> quicksort "the quick brown fox jumps over the lazy dog"  
"        abcdeeefghhijklmnoooopqrrsttuuvwxyz"  
~~~~

Booyah! Ben bundan bahsediyorum! Öyleyse, `[5,1,9,4,6,7,3]` diyelim ve onu sıralamak istiyorsak, bu algoritma önce 5 olan kafayı alacak ve sonra onu kendisinden daha küçük ve
büyük olan iki listenin ortasına koyacak. Yani bir noktada `[1,4,3] ++ [5] ++ [9,6,7]` olacak. Liste tamamen sıralandıktan sonra `5` sayısının dördüncü sırada
kalacağını biliyoruz, çünkü ondan küçük 3 ve ondan büyük 3 sayı var. Şimdi, `[1,4,3]` ve `[9,6,7]`'yi sıralarsak, sıralı bir listemiz var!
İki listeyi aynı fonksiyonu kullanarak sıralıyoruz. Sonunda, o kadar çok parçalayacağız ki boş listelere ulaşıyoruz ve boş bir liste zaten
boş olduğu için bir şekilde sıralı. İşte bir örnek:

![quicksort](../img/quicksort.png)

Yerinde olan ve artık hareket etmeyecek bir öğe turuncu renkte gösterilir. Bunları soldan sağa okursanız, sıralı listeyi görürsünüz.
Tüm unsurları kafalarla karşılaştırmayı seçmemize rağmen, karşılaştırmak için herhangi bir unsuru kullanabilirdik. Hızlı sıralamada, karşılaştırdığınız bir öğeye pivot denir.
Burada yeşil renkteler. Kafayı seçtik çünkü desen eşleştirmeyle elde etmek kolay. Pivottan daha küçük olan öğeler açık yeşil ve pivottan daha büyük olan öğeler koyu yeşildir.
Sarımsı gradyan şey, bir hızlı sıralama uygulamasını temsil eder.


Özyinelemeli Düşünme
--------------------

Şimdiye kadar epey özyineleme yaptık ve muhtemelen fark ettiğiniz gibi, burada bir model var. Genellikle bir uç durumu tanımlarsınız ve
sonra bir öğe ile geri kalanına uygulanan fonksiyon arasında bir şeyler yapan bir fonksiyon tanımlarsınız. Liste, ağaç veya başka bir veri yapısı olması fark etmez.
Toplam, bir listenin ilk öğesi artı listenin geri kalanının toplamıdır. Bir listenin ürünü, listenin ilk öğesi ile listenin geri kalanının çarpımıdır.
Bir listenin uzunluğu, bir artı listenin kuyruğunun uzunluğudur. Ekcetera, ekcetera ...

![brain](../img/brain.png)
Tabii ki, bunların da uç durumları var. Genellikle uç durum, özyinelemeli bir uygulamanın mantıklı olmadığı bir senaryodur. Listelerle uğraşırken,
uç durum genellikle boş listedir. Ağaçlarla uğraşıyorsanız, uç durum genellikle çocuğu olmayan bir node'dur.

Yinelemeli olarak sayılarla uğraşırken benzer. Genellikle bir sayı ve bu sayıya uygulanan fonksiyonun değiştirilmesiyle ilgilidir.
Faktöriyel fonksiyonu daha önce yaptık ve bu bir sayının çarpımı ve bu sayının eksi bir faktöriyeli. Böyle bir yinelemeli uygulama sıfır ile bir anlam ifade etmez,
çünkü faktöriyeller yalnızca pozitif tamsayılar için tanımlanır. Çoğunlukla uç durum değerinin bir kimlik olduğu ortaya çıkar.
Çarpmanın özdeşliği 1'dir, çünkü bir şeyi 1 ile çarparsanız, o şeyi geri alırsınız. Ayrıca listelerin toplamını yaparken, boş bir listenin toplamını 0 olarak tanımlarız ve
0, toplanacak kimliktir. Hızlı sıralamada, uç durum boş listedir ve özdeş de boş listedir, çünkü bir listeye boş bir liste eklerseniz,
yalnızca orijinal listeyi geri alırsınız.

Bu yüzden, bir sorunu çözmek için özyinelemeli bir yol düşünmeye çalışırken, özyinelemeli bir çözümün ne zaman geçerli olmadığını düşünmeye çalışın ve
bunu bir uç durum olarak kullanıp kullanamayacağınızı görün, özdeşleri düşünün ve parçalanıp ayrılmayacağınızı düşünün fonksiyonun parametreleri
(örneğin, listeler genellikle desen eşleştirme yoluyla bir başlığa ve bir kuyruğa bölünür) ve özyinelemeli çağrıyı hangi bölümde kullanacağınız.

