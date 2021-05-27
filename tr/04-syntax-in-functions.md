Fonksiyonlarda Sözdizimi
========================

Desen Eşleştirme (Pattern matching)
-----------------------------------

![pattern](../img/pattern.png)
Bu bölüm Haskell'in bazı harika sözdizimsel yapılarını kapsayacak ve desen eşleştirmeyle başlayacağız. Desen eşleştirme, bazı verilerin uyması gereken
kalıpları belirleme ve ardından uyup uymadığını kontrol etme ve verileri bu modellere göre yapısızlaştırma işlemlerinden oluşur.

Fonksiyonları tanımlarken, farklı desenler için ayrı fonksiyon gövdeleri tanımlayabilirsiniz. Bu, basit ve okunabilir olan gerçekten düzgün bir koda götürür.
Herhangi bir veri türünde - sayılar, karakterler, listeler, demetler vb. Sağladığımız sayının yedi olup olmadığını kontrol eden gerçekten önemsiz bir fonksiyon yapalım.

~~~~ {.haskell: .ghci name="code"}
lucky :: (Integral a) => a -> String  
lucky 7 = "LUCKY NUMBER SEVEN!"  
lucky x = "Sorry, you're out of luck, pal!"   
~~~~

`lucky` dediğinizde, desenler yukarıdan aşağıya kontrol edilecek ve bir kalıba uyduğunda karşılık gelen fonksiyon gövdesi kullanılacaktır.
Bir sayının buradaki ilk kalıba uymasının tek yolu, 7 olmasıdır. Değilse, herhangi bir şeyle eşleşen ve onu `x`'e bağlayan ikinci düzene geçer.
Bu fonksiyon, bir if ifadesi kullanılarak da gerçekleştirilebilirdi. Peki ya 1'den 5'e kadar sayıları söyleyen ve
başka herhangi bir sayı için `"Not between 1 and 5"` diyen bir fonksiyon istersek? Desen eşleştirme olmadan, eğer öyleyse oldukça kıvrımlı bir ağaç yapmalıyız.
Ancak bununla birlikte:

~~~~ {.haskell: .ghci name="code"}
sayMe :: (Integral a) => a -> String  
sayMe 1 = "One!"  
sayMe 2 = "Two!"  
sayMe 3 = "Three!"  
sayMe 4 = "Four!"  
sayMe 5 = "Five!"  
sayMe x = "Not between 1 and 5"  
~~~~

Son kalıbı (tümünü yakalama) en üste taşırsak, her zaman `"Not between 1 and 5"` diyeceğini unutmayın, çünkü tüm sayıları yakalayacak ve düşme ve
başka desenler için kontrol edilme şansları olmayacaktı.

Daha önce uyguladığımız faktöryel fonksiyonunu hatırlıyor musunuz? Bir `n` sayısının faktöriyelini `product [1..n]` olarak tanımladık.
Ayrıca, matematikte genellikle tanımlandığı gibi, faktöryel bir fonksiyonu da yinelemeli olarak tanımlayabiliriz. 0'ın faktöriyelinin 1 olduğunu söyleyerek başlıyoruz.
Daha sonra, herhangi bir pozitif tamsayının faktöriyelinin, tamsayının ardılının faktöriyeli ile çarpımı olduğunu belirtiriz. 
İşte bunun Haskell terimlerine çevrilince nasıl görünüyor.

~~~~ {.haskell: .ghci name="code"}
factorial :: (Integral a) => a -> a  
factorial 0 = 1  
factorial n = n * factorial (n - 1)
~~~~

Bu, bir fonksiyonu özyinelemeli olarak ilk kez tanımlıyoruz. Haskell'de özyineleme önemlidir ve buna daha sonra daha yakından bakacağız.
Ama özetle, bu, diyelim ki 3'ün faktöriyelini elde etmeye çalışırsak olan şeydir. 3 * factorial 2'yi hesaplamaya çalışır.
2'nin faktöriyeli `2 * factorial 1` olduğundan şimdilik `3 * (2 * factorial 1)` var. `factorial 1`, `1 * factorial 0`, dolayısıyla `3 * (2 * (1 * factorial 0))` var.
Şimdi püf noktası geliyor - 0'ın faktöriyelini sadece 1 olarak tanımladık ve hepsini yakalama modelinden önce bu modelle karşılaştığı için, sadece 1'i döndürüyor.
Dolayısıyla, nihai sonuç `3 * (2 * (1 * 1))` ile eşdeğerdir. İkinci kalıbı ilkinin üstüne yazmış olsaydık, 0 dahil tüm sayıları yakalar ve hesaplamamız asla sona ermezdi.
Bu nedenle, kalıpları belirlerken sıra önemlidir ve her zaman önce en spesifik olanları, sonra daha genel olanları belirlemek en iyisidir.

Desen eşleştirme de başarısız olabilir. Bunun gibi bir fonksiyonu tanımlarsak:

~~~~ {.haskell: .ghci name="code"}
charName :: Char -> String  
charName 'a' = "Albert"  
charName 'b' = "Broseph"  
charName 'c' = "Cecil" 
~~~~

ve sonra onu beklemediğimiz bir girdi ile çağırmaya çalışın, olan şey şu:

~~~~ {.haskell: .ghci name="code"}
ghci> charName 'a'  
"Albert"  
ghci> charName 'b'  
"Broseph"  
ghci> charName 'h'  
"*** Exception: tut.hs:(53,0)-(55,21): Non-exhaustive patterns in function charName 
~~~~

Haklı olarak, kapsamlı olmayan kalıplarımız olduğundan şikayet ediyor. Kalıplar oluştururken, beklenmedik bir girdi alırsak programımızın çökmemesi için
her zaman tümünü yakalama kalıbı eklemeliyiz.

Desen eşleştirme, demetler üzerinde de kullanılabilir. Ya 2 boyutlu bir uzayda (çiftler halinde olan) iki vektörü alıp bunları birbirine ekleyen bir
fonksiyon yapmak istersek? İki vektörü bir araya getirmek için, x bileşenlerini ayrı ayrı ve sonra y bileşenlerini ayrı ayrı ekliyoruz.
Desen eşleştirmeyi bilmeseydik bunu şu şekilde yapardık:

~~~~ {.haskell: .ghci name="code"}
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors a b = (fst a + fst b, snd a + snd b)  
~~~~

Bu işe yarıyor, ama bunu yapmanın daha iyi bir yolu var. Fonksiyonu, desen eşleştirmesini kullanacak şekilde değiştirelim.

~~~~ {.haskell: .ghci name="code"}
addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)  
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)  
~~~~

Oraya gidiyoruz! Çok daha iyi. Bunun zaten her şeyi kapsayan bir model olduğunu unutmayın. `addVectors` türü (her iki durumda da)
`addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)` şeklindedir, dolayısıyla parametre olarak iki çift almamız garanti edilir.

`fst` ve `snd`, çiftlerin bileşenlerini çıkarır. Peki ya üçlüler? Peki, bunu yapan herhangi bir sağlanmış fonksiyon yok ama kendi fonksiyonumuzu yapabiliriz.

~~~~ {.haskell: .ghci name="code"}
first :: (a, b, c) -> a  
first (x, _, _) = x  
  
second :: (a, b, c) -> b  
second (_, y, _) = y  
  
third :: (a, b, c) -> c  
third (_, _, z) = z  
~~~~

`_` liste anlamalarında olduğu gibi aynı anlama gelir. Bu, o kısmın ne olduğu gerçekten umursamadığımız anlamına geliyor, bu yüzden sadece bir `_` yazıyoruz.

Bu bana şunu hatırlattı, aynı zamanda liste kavrayışlarında desen eşleştirme de yapabilirsiniz. Şuna bir bak:

~~~~ {.haskell: .ghci name="code"}
ghci> let xs = [(1,3), (4,3), (2,4), (5,3), (5,6), (3,1)]  
ghci> [a+b | (a,b) <- xs]  
[4,7,6,8,11,4]   
~~~~

Bir kalıp hata verirse diğerinden devam eder.

Listelerin kendisinde de patern matching kullanılabilir. Boş liste `[]` ile veya `:` içeren herhangi bir kalıp ve boş liste ile eşleştirebilirsiniz.
Ancak `[1,2,3]` `1: 2: 3: []` için sadece sözdizimsel şeker(syntactic sugar) olduğundan, önceki kalıbı da kullanabilirsiniz.
`x:xs` gibi bir kalıp, listenin başını `x`'e ve geri kalanını `xs`'ye bağlar, tek bir eleman olsa bile, `xs` boş bir liste olur.

`Not`: `x:xs` kalıbı, özellikle özyinelemeli fonksiyonlarda çok kullanılır. Ancak, içinde `:` bulunan modeller yalnızca 1 veya daha fazla uzunluktaki listelerle eşleşir.

Örneğin, ilk üç öğeyi değişkenlere ve listenin geri kalanını başka bir değişkene bağlamak istiyorsanız, `x:y:z:zs` gibi bir şey kullanabilirsiniz.
Yalnızca üç veya daha fazla öğesi olan listelerle eşleşir.

Artık listeyle eşleştirmeyi nasıl yapacağımızı bildiğimize göre, `head` fonksiyonu için kendi uygulamamızı yapalım.

~~~~ {.haskell: .ghci name="code"}
head' :: [a] -> a  
head' [] = error "Can't call head on an empty list, dummy!"  
head' (x:_) = x  
~~~~

Çalışıp çalışmadığını kontrol edelim:

~~~~ {.haskell: .ghci name="code"}
ghci> head' [4,5,6]  
4  
ghci> head' "Hello"  
'H' 
~~~~

Güzel! Birkaç değişkene bağlanmak istiyorsanız (bunlardan biri sadece `_` olsa ve aslında hiç bağlanmasa bile), onları parantez içine almamız gerektiğine dikkat edin.
Ayrıca kullandığımız `error` fonksiyonuna de dikkat edin. Bir string'i alır ve bu string'i ne tür bir hatanın oluştuğu hakkında bilgi olarak kullanarak 
bir çalışma zamanı hatası(runtime error) oluşturur. Programın çökmesine neden olur, bu yüzden onu çok fazla kullanmak iyi değildir.
Ancak boş bir listede `head`'i aramak bir anlam ifade etmiyor.

Listenin ilk unsurlarından bazılarını bize uygun(suz) İngilizce biçiminde anlatan önemsiz bir fonksiyon yapalım.

~~~~ {.haskell: .ghci name="code"}
tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  
~~~~

Bu fonksiyon güvenlidir, çünkü boş liste, tekli liste, iki öğeli bir liste ve ikiden fazla öğeli bir liste ile ilgilenir.
`(x:[])` ve `(x:y:[])`'nin `[x]` ve `[x, y]` olarak yeniden yazılabileceğini unutmayın (sözdizimsel şekeri olduğu için parantezlere ihtiyacımız yoktur).
`(x:y:_)` uzunluğunu 2 veya daha fazla olan herhangi bir listeyle eşleştiği için köşeli parantezlerle yeniden yazamayız.

Liste anlama özelliğini kullanarak kendi `length` fonksiyonumuzu zaten uyguladık. Şimdi bunu desen eşleştirme ve biraz özyineleme kullanarak yapacağız:

~~~~ {.haskell: .ghci name="code"}
length' :: (Num b) => [a] -> b  
length' [] = 0  
length' (_:xs) = 1 + length' xs  
~~~~

Bu, daha önce yazdığımız faktöryel fonksiyona benzer. Önce bilinen bir girdinin sonucunu tanımladık - boş liste. Bu aynı zamanda kenar koşulu olarak da bilinir.
Sonra ikinci düzende listeyi bir baş ve bir kuyruğa bölerek ayırırız. Uzunluğun 1 artı kuyruk uzunluğuna eşit olduğunu söylüyoruz. 
Kafayı eşleştirmek için `_` kullanıyoruz çünkü aslında ne olduğu umurumuzda değil. Ayrıca, bir listenin tüm olası kalıplarına dikkat ettiğimizi unutmayın.
İlk kalıp boş bir listeyle eşleşir ve ikincisi boş bir liste olmayan herhangi bir şeyle eşleşir.

Bakalım `"ham"`'a `length'` dersek ne olacak. İlk önce boş bir liste olup olmadığını kontrol edecektir. Öyle olmadığı için, ikinci desene geçer.
İkinci desenle eşleşiyor ve orada uzunluğun `1 + length' "am"` olduğunu söylüyor, çünkü onu bir baş ve bir kuyruğa ayırdık ve başını attık.
Tamam. `"am"`'nın `length'`'i de benzer şekilde `1 + length' "m"`'dir. Yani şu anda `1 + (1 + length' "m")` var. `length' "m"`, `1 + length' ""` 
(`1 + length' []` olarak da yazılabilir). Ve `length' []` `0` olarak tanımladık. Yani sonunda `1 + (1 + (1 + 0))` var.

`sum`'ı uygulayalım. Boş bir listenin toplamının 0 olduğunu biliyoruz. Bunu bir kalıp olarak yazıyoruz. Ayrıca bir listenin toplamının
baş artı listenin geri kalanının toplamı olduğunu da biliyoruz. Yani bunu yazarsak, şunu elde ederiz:

~~~~ {.haskell: .ghci name="code"}
sum' :: (Num a) => [a] -> a  
sum' [] = 0  
sum' (x:xs) = x + sum' xs  
~~~~

Bir de kalıp denen bir şey var. Bunlar, bir şeyi bir kalıba göre ayırmanın ve her şeye referans tutarken onu isimlere bağlamanın kullanışlı bir yoludur.
Bunu bir kalıbın önüne bir isim ve `@` koyarak yaparsınız. Örneğin, `xs@(x:y:ys)` deseni. Bu desen, `x:y:ys` ile tam olarak aynı şeyle eşleşecektir,
ancak tüm listeyi, `x:y:ys` fonksiyon gövdesine tekrar yazarak kendinizi tekrarlamak yerine `xs` aracılığıyla kolayca alabilirsiniz. İşte hızlı ve kirli bir örnek:

~~~~ {.haskell: .ghci name="code"}
capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]  
~~~~

~~~~ {.haskell: .ghci name="code"}
ghci> capital "Dracula"  
"The first letter of Dracula is D"  
~~~~

Normalde, fonksiyon gövdesinde her şeyi tekrar kullanmak zorunda olduğumuzda, daha büyük bir kalıpla eşleşirken kendimizi tekrar etmekten kaçınmak için 
kalıplar olarak kullanırız.

Bir şey daha - kalıp eşleşmelerinde `++` kullanamazsınız. `(xs ++ ys)` ile eşleştirmeyi denediyseniz, ilk sırada ne olurdu ve ikinci listede ne olurdu?
Pek mantıklı değil. Elemanları `(xs ++ [x, y, z])` veya sadece `(xs ++ [x])` ile eşleştirmek mantıklı olacaktır, ancak listelerin doğası gereği bunu yapamazsınız.

Muhafızlar, muhafızlar!
-----------------------

