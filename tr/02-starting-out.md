İşe Koyulalım
============

Hazır, başla!
-------------

 ![egg](../img/startingout.png) Pekala, başlayalım! Bir şeylerin tanıtımlarını okumayan korkunç bir insansanız ve atladıysanız, yine de girişteki son bölümü okumak isteyebilirsiniz
 çünkü bu eğitimi takip etmeniz gerekenleri ve fonksiyonları yüklemek için nasıl ilerlediğimizi açıklıyor. Yapacağımız ilk şey,
 ghc'nin etkileşimli modunu çalıştırmak ve haskell için çok temel bir his elde etmek için bazı fonksiyonlar çağırmak.
 Terminalinizi açın ve `ghci` yazın. Böyle bir şeyle karşılanacaksınız.
 
 ~~~~ {.haskell: .ghci name="code"}
 GHCi, version 6.8.2: http://www.haskell.org/ghc/  :? for help  
Loading package base ... linking ... done.  
Prelude>  
~~~~

Tebrikler, GHCI'dasınız! Buradaki komut `Prelude>`'dür, ancak oturuma bir şeyler yüklediğinizde daha uzun sürebileceğinden, `ghci>` kullanacağız.
Aynı istemi görmek istiyorsanız, `:set prompt "ghci> "` yazmanız yeterlidir.
 
Burada bazı basit aritmetik işlemleri.

~~~~ {.haskell: .ghci name="code"} 
ghci> 2 + 15  
17  
ghci> 49 * 100  
4900  
ghci> 1892 - 1472  
420  
ghci> 5 / 2  
2.5  
ghci>  
~~~~

Bu oldukça kendini açıklıyor. Ayrıca bir satırda birkaç operatör kullanabiliriz ve tüm olağan öncelik kurallarına uyulur.
Önceliği açık hale getirmek veya değiştirmek için parantez kullanabiliriz.

~~~~ {.haskell: .ghci name="code"} 
ghci> (50 * 100) - 4999  
1  
ghci> 50 * 100 - 4999  
1  
ghci> 50 * (100 - 4999)  
-244950  
~~~~

Oldukça havalı, ha? Evet, biliyorum ama bana katlan. Burada dikkat edilmesi gereken küçük bir tuzak, negatif sayılardır.
Negatif bir sayı istiyorsak, onu parantez içine almak her zaman en iyisidir. `5 * -3` yapmak GHCI'nin size bağırmasına neden olur,
ancak `5 * (-3)` yapmak gayet iyi çalışacaktır.

Boolean cebri de oldukça basittir. Muhtemelen bildiğiniz gibi, `&&` bir mantıksal ve anlamına gelir, `||` boole veya anlamına gelir. not, `True` veya `False`'ı tersini yapar.

~~~~ {.haskell: .ghci name="code"} 
ghci> True && False  
False  
ghci> True && True  
True  
ghci> False || True  
True   
ghci> not False  
True  
ghci> not (True && True)  
False  
~~~~

Eşitlik testi böyle yapılır.

~~~~ {.haskell: .ghci name="code"} 
ghci> 5 == 5  
True  
ghci> 1 == 0  
False  
ghci> 5 /= 5  
False  
ghci> 5 /= 4  
True  
ghci> "hello" == "hello"  
True   
~~~~

`5 + "llama"` veya `5 == True` yapmaya ne dersiniz? İlk parçayı denersek, büyük bir korkunç hata mesajı alırız!

~~~~ {.haskell: .ghci name="code"}
No instance for (Num [Char])  
arising from a use of `+' at <interactive>:1:0-9  
Possible fix: add an instance declaration for (Num [Char])  
In the expression: 5 + "llama"  
In the definition of `it': it = 5 + "llama"   
~~~~

Eyvah! GHCI'ın burada bize söylediği şey, `"llama"`'nın bir sayı olmadığı ve bu yüzden onu 5'e nasıl ekleyeceğini bilmediğidir.
`"llama"` değil, `"four"` veya `"4"` olsa bile, Haskell bunu bir sayı olarak görmezdi. `+` sol ve sağ tarafının sayı olmasını bekler.
`True == 5` yapmaya çalışırsak, GHCI bize türlerin eşleşmediğini söylerdi. `+` Yalnızca sayı olarak kabul edilen şeyler üzerinde çalışırken,
`==` karşılaştırılabilecek herhangi iki şey üzerinde çalışır. Ancak işin püf noktası, ikisinin de aynı türde olması gerektiğidir. Elmaları ve portakalları karşılaştıramazsınız.
Türlere biraz sonra daha yakından bakacağız. Not: `5 + 4.0` yapabilirsiniz çünkü 5 sinsidir ve bir tamsayı veya kayan noktalı(floating-point) sayı gibi davranabilir.
`4.0` bir tamsayı gibi davranamaz, bu nedenle `5` uyum sağlaması gereken değerdir.

Bunu bilmiyor olabilirsiniz, ancak şu anda fonksiyonlar kullanıyoruz. Örneğin, `*` iki sayıyı alıp onları çarpan bir fonksiyondur. Gördüğünüz gibi,
aralarına sıkıştırarak diyoruz. Bu, infix fonksiyonu dediğimiz şeydir. Sayılarla kullanılmayan çoğu fonksiyon önek fonksiyonlarıdır. Onlara bir göz atalım.

Fonksiyonlar genellikle öneklerdir, bu nedenle bundan sonra bir fonksiyonun önek biçiminde olduğunu açıkça belirtmeyeceğiz, sadece varsayacağız.
Zorunlu(imperative) dillerin çoğunda fonksiyonlar, fonksiyon adının yazılması ve ardından parametrelerinin genellikle virgülle ayrılan parantez içinde yazılmasıyla çağrılır.
Haskell'de fonksiyonlar, fonksiyon adı, bir boşluk ve ardından boşluklarla ayrılmış parametreler yazılarak çağrılır.
Başlangıç olarak Haskell'deki en sıkıcı fonksiyonlardan birini çağırmayı deneyeceğiz.

![phone](../img/ringring.png)
~~~~ {.haskell: .ghci name="code"}
ghci> succ 8  
9   
~~~~

`succ` fonksiyonu, tanımlanmış bir ardılı olan her şeyi alır ve o ardılı döndürür. Gördüğünüz gibi, fonksiyon adını parametreden bir boşlukla ayırıyoruz.
Birkaç parametreye sahip bir fonksiyonu çağırmak da basittir. `min` ve `max` fonksiyonları sıraya konulabilen iki şeyi alır (sayılar gibi!).
`min`, daha küçük olanı döndürür ve `max`, daha büyük olanı döndürür. Kendin için gör:

~~~~ {.haskell: .ghci name="code"}
ghci> min 9 10  
9  
ghci> min 3.4 3.2  
3.2  
ghci> max 100 101  
101   
~~~~

Fonksiyon uygulaması (arkasına bir boşluk koyarak ve ardından parametreleri yazarak bir fonksiyon çağırmak) hepsinden en yüksek önceliğe sahiptir.
Bunun bizim için anlamı, bu iki ifadenin eşdeğer olmasıdır.

~~~~ {.haskell: .ghci name="code"}
ghci> succ 9 + max 5 4 + 1  
16  
ghci> (succ 9) + (max 5 4) + 1  
16  
~~~~

Ancak, 9 ve 10 sayılarının çarpımının ardılını elde etmek isteseydik, `succ 9 * 10` yazamazdık çünkü bu 9'un ardılını elde eder ve bu da 10 ile çarpılır. Yani 100.
91'i elde etmek için `succ (9 * 10)` yazmamız gerekir.

Bir fonksiyon iki parametre alırsa, onu ters işaretlerle çevreleyerek bir infix fonksiyonu olarak da adlandırabiliriz. Örneğin, `div` fonksiyonu iki tamsayı alır ve
bunlar arasında integral bölme yapar. `div 92 10` yapmak 9 ile sonuçlanır. Ama böyle adlandırdığımızda, hangi sayının bölme yaptığına ve
hangisinin bölündüğüne dair bazı karışıklıklar olabilir. Yani ``92 `div` 10`` yaparak buna infix fonksiyon diyebiliriz ve birdenbire çok daha net hale gelir.

Zorunlu dillerden gelen birçok insan, parantezlerin fonksiyon uygulamasını göstermesi gerektiği fikrine bağlı kalma eğilimindedir.
Örneğin, C'de `foo()`, `bar(1)` veya `baz(3, "haha")` gibi fonksiyonları çağırmak için parantez kullanırsınız. Söylediğimiz gibi,
Haskell'de fonksiyon uygulaması için boşluklar kullanılır. Yani Haskell'deki bu fonksiyonlar `foo`, `bar 1` ve `baz 3 "haha"` olacaktır.
Dolayısıyla, `bar (bar 3)` gibi bir şey görürseniz, bu `bar`'ın `bar` ve parametre olarak `3` ile çağrıldığı anlamına gelmez.
Bu, bir sayı elde etmek için önce `bar` fonksiyonunu parametre olarak `3`'le çağır, ardından bu numara ile tekrar `bar`'ı çağır anlamına gelir.
C'de, bu `bar (bar (3))` gibi bir şey olacaktır.

Bebeğin ilk fonksiyonları
-------------------------

Önceki bölümde fonksiyonları çağırmak için temel bir fikir edindik. Şimdi kendi başımıza yapmayı deneyelim! En sevdiğiniz metin düzenleyiciyi açın ve
bir sayıyı alıp ikiyle çarpan bu fonksiyonu delin.

~~~~ {.haskell: .ghci name="code"}
doubleMe x = x + x  
~~~~

Fonksiyonlar, çağrıldıkları gibi tanımlanır. Fonksiyon adının ardından boşluklarla ayrılmış parametreler gelir. Ancak fonksiyonları tanımlarken bir `=` vardır ve 
bundan sonra fonksiyonun ne yapacağını tanımlarız. Bunu `baby.hs` veya başka bir şey olarak kaydedin. Şimdi kaydedildiği yere gidin ve buradan `ghci`'ı çalıştırın.
GHCI'a girdikten sonra şunu yapın `:l baby`. Artık betiğimiz yüklendiğine göre, tanımladığımız fonksiyon ile oynayabiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> :l baby  
[1 of 1] Compiling Main             ( baby.hs, interpreted )  
Ok, modules loaded: Main.  
ghci> doubleMe 9  
18  
ghci> doubleMe 8.3  
16.6 
~~~~

`+` Tamsayılar ve kayan nokta sayıları üzerinde çalıştığı için (gerçekten sayı olarak kabul edilebilecek herhangi bir şey),
fonksiyonumuz herhangi bir sayı üzerinde de çalışır. İki sayıyı alıp ikiyle çarpan ve sonra bunları toplayan bir fonksiyon yapalım.

~~~~ {.haskell: .ghci name="code"}
doubleUs x y = x*2 + y*2   
~~~~

Basit. Bunu `doubleUs x y = x + x + y + y` olarak da tanımlayabilirdik. Test etmek oldukça tahmin edilebilir sonuçlar üretir 
(bu fonksiyonu `baby.hs` dosyasına eklemeyi, kaydetmeyi ve sonra GHCI içinde `:l baby` yapmayı unutmayın).

~~~~ {.haskell: .ghci name="code"}
ghci> doubleUs 4 9  
26  
ghci> doubleUs 2.3 34.2  
73.0  
ghci> doubleUs 28 88 + doubleMe 123  
478  
~~~~

Beklendiği gibi, yaptığınız diğer fonksiyonlardan kendi fonksiyonlarınız çağırabilirsiniz. Bunu akılda tutarak, `doubleUs`'ı şu şekilde yeniden tanımlayabiliriz:

doubleUs x y = doubleMe x + doubleMe y   

Bu, Haskell'de göreceğiniz yaygın bir modelin çok basit bir örneğidir. Açıkça doğru olan temel fonksiyonları yapmak ve
sonra bunları daha karmaşık fonksiyonlarda birleştirmek. Bu şekilde tekrardan da kaçınırsınız. Ya bazı matematikçiler 2'nin aslında 3 olduğunu anlarsa ve
programınızı değiştirmek zorunda kalırsanız? `doubleMe`'yi `x + x + x` olarak yeniden tanımlayabilirsiniz ve `doubleUs` `doubleMe`'yi çağırdığından,
2'nin 3 olduğu bu garip yeni dünyada otomatik olarak çalışacaktır.

Haskell'deki fonksiyonların belirli bir sırada olması gerekmez, bu nedenle önce `doubleMe`'yi sonra `doubleUs`'ı tanımlamanız veya
bunu tam tersi şekilde yapmanız önemli değildir.

Şimdi bir sayıyı 2 ile çarpan bir fonksiyon yapacağız, ancak bu sayı 100'den küçük veya 100'e eşitse çünkü 100'den büyük sayılar yeterince büyüktür!

~~~~ {.haskell: .ghci name="code"}
doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2   
~~~~

![baby](../img/baby.png)Tam burada Haskell'in if ifadesini tanıttık. Muhtemelen diğer dillerdeki if ifadelerine aşinasınızdır. Haskell'in if ifadesi ile
zorunlu dillerdeki if ifadeleri arasındaki fark, else bölümünün Haskell'de zorunlu olmasıdır. Zorunlu dillerde, koşul yerine getirilmezse birkaç adımı atlayabilirsiniz,
ancak Haskell'de her ifade ve fonksiyon bir şey döndürmelidir. Bunu bir satırda da yazabilirdik ama ben bu yolu daha okunaklı buluyorum.
Haskell'deki if ifadesiyle ilgili bir diğer husus da bir ifade olmasıdır. İfade, temelde bir değer döndüren bir kod parçasıdır.
`5`, 5 döndürdüğü için bir ifadedir, `4 + 8` bir ifadedir, `x + y` bir ifadedir çünkü `x` ve `y`'nin toplamını verir. Diğer zorunlu olduğu için,
bir if ifadesi her zaman bir şey döndürür ve bu yüzden bir ifadedir. Önceki fonksiyonumuzda üretilen her sayıya bir tane eklemek isteseydik, gövdesini şöyle yazabilirdik.

~~~~ {.haskell: .ghci name="code"}
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1  
~~~~

Parantezleri atlasaydık, yalnızca `x` 100'den büyük olmasaydı bir tane eklerdi. Fonksiyon adının sonundaki `'` işaretine dikkat edin.
Bu kesme işaretinin Haskell'in sözdiziminde özel bir anlamı yok. Bir fonksiyon adında kullanmak için geçerli bir karakterdir.
Genellikle `'` bir fonksiyonun katı bir versiyonunu (tembel olmayan) veya bir fonksiyonun veya bir değişkenin biraz değiştirilmiş versiyonunu belirtmek için kullanırız.
Fonksiyonlarda `'` geçerli bir karakter olduğu için, böyle bir fonksiyon yapabiliriz.

~~~~ {.haskell: .ghci name="code"}
conanO'Brien = "It's a-me, Conan O'Brien!"   
~~~~

Burada dikkate değer iki şey var. Birincisi, fonksiyon adında Conan'ın adını büyük harfle yazmadık. Bunun nedeni, fonksiyonların büyük harflerle başlayamamasıdır.
Nedenini biraz sonra göreceğiz. İkincisi, bu fonksiyonun herhangi bir parametre almamasıdır. Bir fonksiyon herhangi bir parametre almadığında,
genellikle bunun bir tanım (veya isim) olduğunu söyleriz. Çünkü isimleri (ve fonksiyonları) tanımladıktan sonra ne anlama geldiğini değiştiremeyiz,
`conanO'Brien` ve `"It's a-me, Conan O'Brien!"` birbirinin yerine kullanılabilir.

Listelere giriş
---------------
![list](../img/list.png)

Gerçek dünyadaki alışveriş listeleri gibi, Haskell'deki listeler de çok kullanışlıdır. En çok kullanılan veri yapısıdır ve bir sürü problemi modellemek ve
çözmek için çok sayıda farklı şekilde kullanılabilir. Listeler ÇOK harika. Bu bölümde listelerin, string'lerin (listeler olan) ve
liste anlamalarının (list comprehension) temellerine bakacağız.

Haskell'de listeler **homojen** bir veri yapısıdır. Aynı türden birkaç öğeyi depolar. Bu, bir tamsayı listesine veya bir karakter listesine sahip olabileceğimiz anlamına gelir,
ancak birkaç tam sayı ve ardından birkaç karakter içeren bir listeye sahip olamayız. Ve şimdi, bir liste!

**Not**: GHCI'da bir ad tanımlamak için `let` keyword'ünü kullanabiliriz. GHCI içinde `let a = 1` yapmak, bir betiğe `a = 1` yazıp onu yüklemeye eşdeğerdir.

~~~~ {.haskell: .ghci name="code"}
ghci> let lostNumbers = [4,8,15,16,23,42]  
ghci> lostNumbers  
[4,8,15,16,23,42] 
~~~~

Gördüğünüz gibi listeler köşeli parantez ile belirtilmiş ve listelerdeki değerler virgülle ayrılmıştır. `[1,2,'a',3,'b','c',4]` gibi bir listeyi deneseydik,
Haskell karakterlerin (bu arada tek tırnak arasında bir karakter olarak belirtilir) sayılar değil. Karakterlerden bahsetmişken, stringler sadece karakter listeleridir.
`"hello"` sadece `['h','e','l','l','o']` için sözdizimsel şekerdir. String'ler liste olduğu için, bunlarda gerçekten kullanışlı olan liste fonksiyonlarını kullanabiliriz.

Ortak bir görev, iki listeyi bir araya getirmektir. Bu, `++` operatörü kullanılarak yapılır.

~~~~ {.haskell: .ghci name="code"}
ghci> [1,2,3,4] ++ [9,10,11,12]  
[1,2,3,4,9,10,11,12]  
ghci> "hello" ++ " " ++ "world"  
"hello world"  
ghci> ['w','o'] ++ ['o','t']  
"woot"  
~~~~

Uzun string'lerde `++` operatörünü tekrar tekrar kullanırken dikkatli olun. İki listeyi bir araya getirdiğinizde (örneğin bir listeye tek liste ekleseniz bile: `[1,2,3] ++ [4]`)
Haskell dahili olarak `++`'nın sol tarafındaki tüm listeyi gözden geçirmek zorundadır. Çok büyük olmayan listelerle uğraşırken bu bir sorun değil.
Ancak elli milyon giriş uzunluğundaki bir listenin sonuna bir şey koymak biraz zaman alacak. Ancak, `:` operatörünü (aynı zamanda cons operatörü olarak da adlandırılır)
kullanarak bir listenin başına bir şey koymak anlıktır.

~~~~ {.haskell: .ghci name="code"}
ghci> 'A':" SMALL CAT"  
"A SMALL CAT"  
ghci> 5:[1,2,3,4,5]  
[5,1,2,3,4,5] 
~~~~

 `:` Öğesinin bir sayı ve bir sayı listesi veya bir karakter ve bir karakter listesi alırken, `++`'nın iki liste aldığına dikkat edin.
 Bir listenin sonuna `++` ile bir öğe ekleseniz bile, onu köşeli parantez içine almanız gerekir, böylece bir liste haline gelir.
 
 `[1,2,3]` aslında `1: 2: 3: []` için sadece sözdizimsel şekerdir. `[]` boş bir listedir. Başına `3` eklersek, `[3]` olur. Bunun başına `2` eklersek, `[2,3]` olur ve böyle devam eder.

`Not`: `[]`, `[[]]` ve `[[], [], []]` farklı şeylerdir. İlki boş bir liste, ikincisi bir boş liste içeren bir liste, üçüncüsü ise üç boş liste içeren bir listedir.

Index'e göre listeden bir eleman almak istiyorsanız, `!!` kullanın. Index'ler 0'dan başlar.

~~~~ {.haskell: .ghci name="code"}
ghci> "Steve Buscemi" !! 6  
'B'  
ghci> [9.4,33.2,96.2,11.2,23.25] !! 1  
33.2  
~~~~

Ancak altıncı öğeyi yalnızca dört öğe içeren bir listeden almaya çalışırsanız, bir hata alırsınız, bu yüzden dikkatli olun!

Listeler ayrıca listeler içerebilir. Listeler içeren listeleri içeren listeleri de içerebilirler…

~~~~ {.haskell: .ghci name="code"}
ghci> let b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]  
ghci> b  
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]  
ghci> b ++ [[1,1,1,1]]  
[[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3],[1,1,1,1]]  
ghci> [6,6,6]:b  
[[6,6,6],[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]  
ghci> b !! 2  
[1,2,2,3,4]  
~~~~

Bir listedeki listeler farklı uzunluklarda olabilir ancak farklı türde olamazlar. Tıpkı bazı karakterler ve bazı rakamlardan oluşan bir listeye sahip olamayacağınız gibi,
bazı karakter listeleri ve bazı sayı listeleri olan bir listeniz olamaz. 

Listeler, içerdikleri şeyler karşılaştırılabilirse karşılaştırılabilir. Listeleri karşılaştırmak için `<`, `<=`,`>` ve `>=` kullanıldığında, sözlükbilimsel sırayla karşılaştırılırlar. Önce baştan karşılaştırılır. Eşitse, ikinci unsurlar karşılaştırılır, vb.

~~~~ {.haskell: .ghci name="code"}
ghci> [3,2,1] > [2,1,0]  
True  
ghci> [3,2,1] > [2,10,100]  
True  
ghci> [3,4,2] > [3,4]  
True  
ghci> [3,4,2] > [2,4]  
True  
ghci> [3,4,2] == [3,4,2]  
True  
~~~~

Listelerle başka ne yapabilirsiniz? Listelerde çalışan bazı temel fonksiyonlar şunlardır.

`head` bir liste alır ve başını döndürür. Bir listenin başı temelde onun ilk öğesidir.

~~~~ {.haskell: .ghci name="code"}
ghci> head [5,4,3,2,1]  
5   
~~~~

`tail` bir liste alır ve kuyruğunu döndürür. Başka bir deyişle, bir listenin başını keser.

~~~~ {.haskell: .ghci name="code"}
ghci> tail [5,4,3,2,1]  
[4,3,2,1]   
~~~~

`last` bir liste alır ve son elemanını döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> last [5,4,3,2,1]  
1   
~~~~

`init` bir liste alır ve son elemanı dışındaki her şeyi döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> init [5,4,3,2,1]  
[5,4,3,2]   
~~~~

Bir listeyi canavar olarak düşünürsek, suna benzer.

![listmonster](../img/listmonster.png)

Ama boş bir listenin başına geçmeye çalışırsak ne olur?

~~~~ {.haskell: .ghci name="code"}
ghci> head []  
*** Exception: Prelude.head: empty list  
~~~~

Aman! Her şey yüzümüzde patlıyor! Canavar yoksa kafası da yoktur. `head`, `tail`, `last` ve `init`'i kullanırken bunları boş listelerde kullanmamaya dikkat edin.
Bu hata derleme sırasında yakalanamaz, bu nedenle Haskell'e yanlışlıkla boş bir listeden bazı öğeler vermesini istemeye karşı önlem almak her zaman iyi bir uygulamadır.

`length` bir listeyi alır ve belli ki uzunluğunu döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> length [5,4,3,2,1]  
5  
~~~~

`null`, bir listenin boş olup olmadığını kontrol eder. Eğer öyleyse `True` döndürür, aksi takdirde `False` döndürür. `xs == []` yerine bu fonksiyonu kullanın
(`xs` adında bir listeniz varsa)

~~~~ {.haskell: .ghci name="code"}
ghci> null [1,2,3]  
False  
ghci> null []  
True  
~~~~

`reverse` bir listeyi tersine çevirir.

~~~~ {.haskell: .ghci name="code"}
ghci> reverse [5,4,3,2,1]  
[1,2,3,4,5]  
~~~~

`take` sayı ve bir liste alır. Listenin başındaki birçok öğeyi çıkarır. İzleyin.

~~~~ {.haskell: .ghci name="code"}
ghci> take 3 [5,4,3,2,1]  
[5,4,3]  
ghci> take 1 [3,9,3]  
[3]  
ghci> take 5 [1,2]  
[1,2]  
ghci> take 0 [6,6,6]  
[]  
~~~~

Listede bulunandan daha fazla öğe almaya çalışırsak, listeyi nasıl döndürdüğüne bakın. 0 eleman almaya çalışırsak, boş bir liste alıyoruz.

`drop` benzer şekilde çalışır, yalnızca bir listenin başındaki öğelerin sayısını düşürür.

~~~~ {.haskell: .ghci name="code"}
ghci> drop 3 [8,4,2,1,5,6]  
[1,5,6]  
ghci> drop 0 [1,2,3,4]  
[1,2,3,4]  
ghci> drop 100 [1,2,3,4]  
[]   
~~~~

`maximum`, bir tür sıraya konulabilen şeylerin bir listesini alır ve en büyük öğeyi döndürür.

`minimum`, en küçük olanı döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> minimum [8,4,2,1,5,6]  
1  
ghci> maximum [1,9,2,3,4]  
9 
~~~~

`sum`, sayıların bir listesini alır ve toplamlarını döndürür.

`product`, sayıların bir listesini alır ve çarpımlarını döndürür.

~~~~ {.haskell: .ghci name="code"}
ghci> sum [5,2,1,6,3,2,5,7]  
31  
ghci> product [6,2,1,2]  
24  
ghci> product [1,2,5,6,7,9,2,0]  
0  
~~~~

`elem` bir şey ve bir şeyler listesi alır ve bize o şeyin listenin bir öğesi olup olmadığını söyler. Genellikle bir infix işlevi olarak adlandırılır 
çünkü bu şekilde okumak daha kolaydır.

~~~~ {.haskell: .ghci name="code"}
ghci> 4 `elem` [3,4,5,6]  
True  
ghci> 10 `elem` [3,4,5,6]  
False  
~~~~

Bunlar, listeler üzerinde çalışan birkaç temel fonksiyondu. Daha sonra daha fazla [liste fonksiyonuna](../tr/07-modules.md) göz atacağız.

Teksas aralıkları
-----------------

![cowboy](../img/cowboy.png)
Ya 1 ile 20 arasındaki tüm sayıların bir listesini istersek? Elbette, hepsini yazabilirdik ama açıkçası bu, programlama dillerinden mükemmellik isteyen beyler için
bir çözüm değil. Bunun yerine aralıkları kullanacağız. Aralıklar, numaralandırılabilen elemanların aritmetik dizileri olan listeler oluşturmanın bir yoludur.
Numaralar numaralandırılabilir. Bir, iki, üç, dört vb. Karakterler de numaralandırılabilir. Alfabe, A'dan Z'ye karakterlerin bir numaralandırmasıdır.
İsimler numaralandırılamaz. "John" dan sonra ne geliyor? Bilmiyorum.

1'den 20'ye kadar tüm doğal sayıları içeren bir liste yapmak için `[1..20]` yazmanız yeterlidir. Bu `[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]` yazmaya eşdeğerdir ve
uzun numaralandırma dizilerini manuel olarak yazmanın aptalca olması dışında birini veya diğerini yazmak arasındaki fark.

~~~~ {.haskell: .ghci name="code"}
ghci> [1..20]  
[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]  
ghci> ['a'..'z']  
"abcdefghijklmnopqrstuvwxyz"  
ghci> ['K'..'Z']  
"KLMNOPQRSTUVWXYZ"   
~~~~

Aralıklar harikadır çünkü bir adım da belirtebilirsiniz. Ya 1 ile 20 arasındaki tüm çift sayıları istiyorsak? Veya 1 ile 20 arasındaki her üçüncü sayı?

~~~~ {.haskell: .ghci name="code"}
ghci> [2,4..20]  
[2,4,6,8,10,12,14,16,18,20]  
ghci> [3,6..20]  
[3,6,9,12,15,18]   
~~~~

Bu basitçe, ilk iki öğeyi virgülle ayırmak ve ardından üst sınırın ne olduğunu belirlemek meselesidir. Oldukça akıllı olsa da, basamaklı aralıklar,
bazı insanların beklediği kadar akıllı değildir. `[1,2,4,8,16..100]` yapıp 2'nin tüm güçlerini elde etmeyi bekleyemezsiniz. Birincisi,
yalnızca bir adım belirleyebildiğiniz için. İkinci olarak, aritmetik olmayan bazı diziler, ilk terimlerinin yalnızca birkaçıyla verilirse belirsizdir.

20'den 1'e kadar tüm sayıları içeren bir liste yapmak için, sadece `[20..1]` yapamazsınız, `[20,19..1]` yapmanız gerekir.

Aralıklarda kayan nokta sayılarını kullanırken dikkat edin! Tamamen kesin olmadıklarından (tanım gereği), aralıklardaki kullanımları oldukça ilginç sonuçlar verebilir.

~~~~ {.haskell: .ghci name="code"}
ghci> [0.1, 0.3 .. 1]  
[0.1,0.3,0.5,0.7,0.8999999999999999,1.0999999999999999]  
~~~~

Benim tavsiyem onları liste aralıklarında kullanmamaktır.

Bir üst sınır belirtmeyerek sonsuz listeler oluşturmak için aralıkları da kullanabilirsiniz. Daha sonra sonsuz listelerde daha fazla ayrıntıya gireceğiz.
Şimdilik, 13'ün ilk 24 katını nasıl elde edeceğinizi inceleyelim. Elbette, `[13,26..24 * 13]` yapabilirsiniz. Ama daha iyi bir yol var: `take 24 [13,26..]`.
Haskell tembel olduğu için sonsuz listeyi hemen değerlendirmeye çalışmayacaktır çünkü asla bitmeyecektir.
O sonsuz listelerden ne elde etmek istediğinizi görmek için bekleyecek. Ve burada sadece ilk 24 elementi istediğinizi görüyor ve memnuniyetle bunu zorunlu kılıyor.

Sonsuz listeler oluşturan bir avuç fonksiyon:

`cycle` bir listeyi alır ve onu sonsuz bir listeye dönüştürür. Sonucu görüntülemeye çalışırsanız, sonsuza kadar devam eder, bu yüzden onu bir yerden dilimlemeniz gerekir.

~~~~ {.haskell: .ghci name="code"}
ghci> take 10 (cycle [1,2,3])  
[1,2,3,1,2,3,1,2,3,1]  
ghci> take 12 (cycle "LOL ")  
"LOL LOL LOL " 
~~~~

`repeat` bir elemanı alır ve sadece o elemanın sonsuz bir listesini oluşturur. Bu, tek bir öğe içeren bir listeyi döndürmek gibidir.

~~~~ {.haskell: .ghci name="code"}
ghci> take 10 (repeat 5)  
[5,5,5,5,5,5,5,5,5,5]  
~~~~

Bir listede aynı öğeden birkaç tane istiyorsanız, `replicate` fonksiyonunu kullanmak daha basit olsa da. `replicate 3 10`, `[10,10,10]` değerini döndürür.


Ben bir liste anlayışıyım(list comprehension)
---------------------------------------------

Eğer daha önce matematik dersi aldıysanız, muhtemelen belirli anlayışlarla karşılaşırsınız. Normalde genel kümelerden daha spesifik kümeler oluşturmak için kullanılırlar.
İlk on çift doğal sayıyı içeren bir küme için temel bir anlayış ![setnotation](../img/setnotation.png)'dır. Pipe'dan önceki kısım çıktı fonksiyonu olarak adlandırılır,
`x` değişkendir, `N` giriş kümesidir ve `x <= 10` predicate'dir. Bu, kümenin predicate'ini karşılayan tüm doğal sayıların iki katını içerdiği anlamına gelir.

Bunu Haskell'de yazmak isteseydik, `take 10 [2,4..]` gibi bir şey yapabilirdik. Peki ya ilk 10 doğal sayının iki katını istemiyorsak,
ancak bunlara uygulanmış bir tür daha karmaşık fonksiyon varsa? Bunun için bir liste anlayışı kullanabiliriz.
Liste anlayışları, küme anlayışlarına çok benzer. Şimdilik ilk 10 çift sayıyı almaya devam edeceğiz. Kullanabileceğimiz liste anlayışı `[x * 2 | x <- [1..10]]`.
`x` `[1..10]`'dan çizilir ve `[1..10]`'daki (`x`'e bağladığımız) her eleman için, bu elemanı sadece iki katına çıkarırız. İşte bu eylem halindeki anlayış.

~~~~ {.haskell: .ghci name="code"}
ghci> [x*2 | x <- [1..10]]  
[2,4,6,8,10,12,14,16,18,20]  
~~~~

Gördüğünüz gibi istenen sonuçları alıyoruz. Şimdi bu anlayışa bir koşul (veya bir predicate) ekleyelim.
Predicate'ler, bağlayıcı parçaların peşinden gider ve onlardan virgülle ayrılır. Diyelim ki, sadece ikiye katlanan 12'ye eşit veya 12'ye eşit olan öğeleri istiyoruz.

~~~~ {.haskell: .ghci name="code"}
ghci> [x*2 | x <- [1..10], x*2 >= 12]  
[12,14,16,18,20]  
~~~~

 Harika, işe yarıyor. 50'den 100'e kadar olan ve 7 ile bölündüğünde kalanı 3 olan tüm sayıları istesek nasıl olur? Kolay.

~~~~ {.haskell: .ghci name="code"}
[ x | x <- [50..100], x `mod` 7 == 3]  
[52,59,66,73,80,87,94]   
~~~~

Başarılı! Listeleri predicate'lere göre ayıklamanın **filtering** olarak da adlandırıldığını unutmayın. Numaraların bir listesini aldık ve onları predicate'e göre filtreledik.
Şimdi başka bir örnek verelim. Diyelim ki 10'dan büyük her bir tek sayıyı `"BANG!"` ile ve 10'dan küçük olan her bir tek sayıyı `"BOOM!"` ile değiştiren bir anlayış istiyoruz.
Bir sayı tek değilse, onu listemizden çıkarırız. Kolaylık sağlamak için, bu anlayışı bir fonksiyonun içine koyacağız, böylece onu kolayca yeniden kullanabiliriz.

~~~~ {.haskell: .ghci name="code"}
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]   
~~~~

Anlamanın son kısmı predicate'dir. `odd` sayı fonksiyonu, tek sayı için `True` ve çift sayı için `False` değerini döndürür. 
Öğe, yalnızca tüm predicate'ler `True` olarak değerlendirilirse listeye dahil edilir.

~~~~ {.haskell: .ghci name="code"}
ghci> boomBangs [7..13]  
["BOOM!","BOOM!","BANG!","BANG!"] 
~~~~

Birkaç predicate ekleyebiliriz. 10'dan 20'ye kadar 13, 15 veya 19 olmayan tüm sayıları isteseydik, yapardık:

~~~~ {.haskell: .ghci name="code"}
ghci> [ x | x <- [10..20], x /= 13, x /= 15, x /= 19]  
[10,11,12,14,16,17,18,20]  
~~~~

Sadece liste anlamalarında birden fazla predicate'imiz olmakla kalmaz (bir öğe, ortaya çıkan listeye dahil edilecek tüm predicate'leri karşılamalıdır),
aynı zamanda birkaç listeden de çıkarabiliriz. Birkaç listeden çizerken, anlamalar verilen listelerin tüm kombinasyonlarını üretir ve
ardından bunları sağladığımız çıktı fonksiyonu ile birleştirir. 4 uzunluklu iki listeden alınan bir anlayış tarafından üretilen bir listenin uzunluğu,
filtrelemememiz koşuluyla 16 olacaktır. `[2,5,10]` ve `[8,10,11]` olmak üzere iki listemiz varsa ve bu listelerdeki sayılar arasındaki tüm olası kombinasyonların
ürünlerini elde etmek istiyorsak, işte şunu yapardık.

~~~~ {.haskell: .ghci name="code"}
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11]]  
[16,20,22,40,50,55,80,100,110]   
~~~~

Beklendiği gibi, yeni listenin uzunluğu 9'dur. 50'den fazla olan tüm olası ürünleri istersek ne olur?

~~~~ {.haskell: .ghci name="code"}
ghci> [ x*y | x <- [2,5,10], y <- [8,10,11], x*y > 50]  
[55,80,100,110]  
~~~~

Destansı bir neşe için sıfatların bir listesini ve bir isim listesini birleştiren bir liste anlayışına ne dersiniz?

~~~~ {.haskell: .ghci name="code"}
ghci> let nouns = ["hobo","frog","pope"]  
ghci> let adjectives = ["lazy","grouchy","scheming"]  
ghci> [adjective ++ " " ++ noun | adjective <- adjectives, noun <- nouns]  
["lazy hobo","lazy frog","lazy pope","grouchy hobo","grouchy frog",  
"grouchy pope","scheming hobo","scheming frog","scheming pope"]   
~~~~

Biliyorum! Kendi `length` versiyonumuzu yazalım! Buna `length'` diyeceğiz.

~~~~ {.haskell: .ghci name="code"}
length' xs = sum [1 | _ <- xs] 
~~~~

`_` zaten listeden ne çıkaracağımızı umursamadığımız anlamına gelir, bu yüzden asla kullanmayacağımız bir değişken adı yazmak yerine sadece `_` yazıyoruz.
Bu fonksiyon, bir listenin her elemanını `1` ile değiştirir ve sonra bunu toplar. Bu, ortaya çıkan toplamın listemizin uzunluğu olacağı anlamına gelir.

Sadece dostça bir hatırlatma: stringler liste olduğundan, stringleri işlemek ve üretmek için liste anlamalarını kullanabiliriz.
İşte bir string alan ve ondan büyük harfler dışındaki her şeyi kaldıran bir fonksiyon.

~~~~ {.haskell: .ghci name="code"}
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]   
~~~~

Test ediliyor:

~~~~ {.haskell: .ghci name="code"}
ghci> removeNonUppercase "Hahaha! Ahahaha!"  
"HA"  
ghci> removeNonUppercase "IdontLIKEFROGS"  
"ILIKEFROGS"   
~~~~

Buradaki predicate'ler tüm işi yapar. Karakterin, yalnızca listenin bir öğesi `['A'..'Z']` ise yeni listeye dahil edileceğini söylüyor.
Listeler içeren listelerde çalışıyorsanız, iç içe geçmiş liste anlamaları da mümkündür. Listeyi düzleştirmeden tüm tek sayıları kaldıralım.

~~~~ {.haskell: .ghci name="code"}
ghci> let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  
ghci> [ [ x | x <- xs, even x ] | xs <- xxs]  
[[2,2,4],[2,4,6,8],[2,4,2,6,2,6]]  
~~~~

Birkaç satır boyunca liste anlayışları yazabilirsiniz. Dolayısıyla, GHCI içinde değilseniz, özellikle iç içe geçmişlerse,
daha uzun liste anlayışlarını birden çok satıra bölmek daha iyidir.


Demetler (Tuples)
-----------------

Bazı yönlerden, demetler listeler gibidir - birkaç değeri tek bir değerde saklamanın bir yoludur. Bununla birlikte, birkaç temel farklılık vardır.
Sayı listesi, sayıların listesidir. Bu onun türü ve içinde tek bir sayı mı yoksa sonsuz sayıda sayı mı olduğu önemli değil.
Bununla birlikte, demetler, tam olarak kaç değeri birleştirmek istediğinizi bildiğinizde ve türü, kaç bileşene sahip olduğuna ve bileşen türlerine bağlı olduğunda kullanılır.
Parantez ile gösterilirler ve bileşenleri virgülle ayrılmıştır.

Diğer bir önemli fark, homojen olmaları gerekmemesidir. Listeden farklı olarak, bir demet birkaç türden oluşan bir kombinasyon içerebilir.

Haskell'de iki boyutlu bir vektörü nasıl temsil edeceğimizi düşünün. Bir yol, bir liste kullanmaktır. Bu tür bir iş olurdu.
Öyleyse, iki boyutlu bir düzlemdeki bir şeklin noktalarını temsil etmek için bir listeye birkaç vektör koymak istersek ne olur? 
`[[1,2], [8,11], [4,5]]` gibi bir şey yapabiliriz. Bu yöntemle ilgili sorun, Haskell'in hala sayıları olan listelerin bir listesi olduğu için
sorun yaşamadığı `[[1,2],[8,11,5],[4,5]]` gibi şeyler de yapabilmemizdir, ancak bu bir anlam ifade etmiyor. Ancak iki boyutlu bir demet
(çift olarak da adlandırılır) kendi türüdür, yani bir listede birkaç çift ve sonra bir üçlü
(üç boyutlu bir demet) olamaz, bunun yerine onu kullanalım. Vektörleri köşeli parantezlerle çevrelemek yerine parantez kullanıyoruz: `[(1,2), (8,11), (4,5)]`.
Ya `[(1,2), (8,11,5), (4,5)]` gibi bir şekil yapmaya çalışırsak? Peki, bu hatayı alırdık:

~~~~ {.haskell: .ghci name="code"}
Couldn't match expected type `(t, t1)'  
against inferred type `(t2, t3, t4)'  
In the expression: (8, 11, 5)  
In the expression: [(1, 2), (8, 11, 5), (4, 5)]  
In the definition of `it': it = [(1, 2), (8, 11, 5), (4, 5)]  
~~~~

Bize aynı listede bir çift ve bir üçlü kullanmayı denediğimizi söylüyor, ki bunun olmaması gerekiyor. Ayrıca `[(1,2),("One",2)]` gibi bir liste yapamazsınız
çünkü listenin ilk öğesi bir çift sayıdır ve ikinci öğe bir string ve bir sayıdan oluşan bir çifttir. Demetler çok çeşitli verileri temsil etmek için de kullanılabilir.
Örneğin, Haskell'de birinin adını ve yaşını temsil etmek istersek, üçlü kullanabiliriz: `("Christopher", "Walken", 55)`.
Bu örnekte görüldüğü gibi, demetler ayrıca listeler içerebilir. 

Bazı veri parçalarının kaç bileşene sahip olması gerektiğini önceden bildiğiniz zaman demetleri kullanın. Demetler çok daha katıdır
çünkü her farklı boyuttaki demet kendi türüdür, bu nedenle bir demete bir öğe eklemek için genel bir fonksiyon yazamazsınız -
bir çifte eklemek için bir fonksiyon yazmanız gerekir, bir fonksiyon için üçlü, bir 4'lü gruba eklemek için bir fonksiyon, vb.

Tekil listeleri varken, tekil demet diye bir şey yoktur. Düşündüğünüzde pek bir anlam ifade etmiyor.
Bir tekil demeti sadece içerdiği değerdir ve bu nedenle bize hiçbir faydası olmaz.

Listeler gibi, demetler de bileşenleri karşılaştırılabilirse birbirleriyle karşılaştırılabilir. Yalnızca farklı boyutlardaki iki demeti karşılaştıramazsınız,
oysa farklı boyutlardaki iki listeyi karşılaştırabilirsiniz. Çiftler üzerinde çalışan iki kullanışlı fonksiyon:

`fst` bir çift içerisinden ilk elemanı alır.

~~~~ {.haskell: .ghci name="code"}
ghci> fst (8,11)  
8  
ghci> fst ("Wow", False)  
"Wow"  
~~~~

`snd` bir çifti alır ve ikinci bileşenini döndürür. Sürpriz!

~~~~ {.haskell: .ghci name="code"}
ghci> snd (8,11)  
11  
ghci> snd ("Wow", False)  
False   
~~~~

**Not**: Bu fonksiyonlar yalnızca çiftler üzerinde çalışır. Üçlü, 4'lü, 5'li vb demetler üzerinde çalışmayacaklar.
Biraz sonra demetlerden farklı şekillerde veri ayıklamayı gözden geçireceğiz.

Çiftlerin bir listesini oluşturan harika bir fonksiyon: `zip`. İki listeyi alır ve ardından eşleşen öğeleri çiftler halinde birleştirerek bunları
tek bir liste halinde sıkıştırır. Bu gerçekten basit bir fonksiyondur, ancak birçok kullanımı vardır. 
İki listeyi bir şekilde birleştirmek veya iki listeyi aynı anda dolaşmak istediğinizde özellikle kullanışlıdır. İşte bir gösterimi.

~~~~ {.haskell: .ghci name="code"}
ghci> zip [1,2,3,4,5] [5,5,5,5,5]  
[(1,5),(2,5),(3,5),(4,5),(5,5)]  
ghci> zip [1 .. 5] ["one", "two", "three", "four", "five"]  
[(1,"one"),(2,"two"),(3,"three"),(4,"four"),(5,"five")]  
~~~~

Öğeleri eşleştirir ve yeni bir liste oluşturur. İlk eleman birinciyle, ikincisi ikinciyle, vb gider. Çiftlerin içlerinde farklı türleri olabileceğinden, 
`zip`'in farklı türler içeren iki listeyi alıp bunları sıkıştırabileceğine dikkat edin. Listelerin uzunlukları uyuşmazsa ne olur?

~~~~ {.haskell: .ghci name="code"}
ghci> zip [5,3,2,6,2,7,2,5,4,6,6] ["im","a","turtle"]  
[(5,"im"),(3,"a"),(2,"turtle")]  
~~~~

Uzun liste, kısa olanın uzunluğuna uyacak şekilde kesilir. Haskell tembel olduğu için, sonsuz listelerle sonlu listeleri sıkıştırabiliriz:

~~~~ {.haskell: .ghci name="code"}
ghci> zip [1..] ["apple", "orange", "cherry", "mango"]  
[(1,"apple"),(2,"orange"),(3,"cherry"),(4,"mango")]  
~~~~

![pythag](../img/pythag.png)
İşte demetleri ve liste anlamalarını birleştiren bir problem: Tüm tarafları ve tüm tarafları 10'a eşit veya daha küçük olan tam sayıları olan
hangi dik üçgenin çevresi 24'tür? İlk olarak, kenarları 10'a eşit veya daha küçük olan tüm üçgenleri oluşturmayı deneyelim:

~~~~ {.haskell: .ghci name="code"}
ghci> let triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ]   
~~~~

Sadece üç listeden çizim yapıyoruz ve çıktı fonksiyonumuz onları üçlü olarak birleştiriyor. GHCI'da `triangles` yazarak bunu değerlendirirseniz,
kenarları 10'un altında veya ona eşit olan tüm olası üçgenlerin bir listesini alırsınız. Sonra, hepsinin dik üçgen olması gerektiği koşulunu ekleyeceğiz.
Ayrıca bu fonksiyonu, b tarafının hipotenüsten daha büyük olmadığını ve a tarafının b tarafından daha büyük olmadığını dikkate alarak değiştireceğiz.

~~~~ {.haskell: .ghci name="code"}
ghci> let rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2]   
~~~~

Neredeyse bitirdik. Şimdi, çevresi 24 olanları istediğimizi söyleyerek fonksiyonu değiştiriyoruz.

~~~~ {.haskell: .ghci name="code"}
ghci> let rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]  
ghci> rightTriangles'  
[(6,8,10)]  
~~~~

Ve cevabımız var! Bu, fonksiyonel programlamada yaygın bir modeldir. Bir çözüm kümesi alırsınız ve sonra bu çözümlere dönüşümler uygularsınız ve
doğru olanları elde edene kadar onları filtrelersiniz.

