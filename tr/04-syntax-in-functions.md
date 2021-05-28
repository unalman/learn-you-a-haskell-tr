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

![guards](../img/guards.png)
Kalıplar, bir değerin bir biçime uyduğundan emin olmanın ve onu parçalamanın bir yolu iken,
korumalar bir değerin bazı özelliklerinin (veya birkaçının) doğru mu yanlış mı olduğunu test etmenin bir yoludur. Bu bir if cümlesine çok benziyor ve çok benzer. 
Mesele şu ki, birkaç koşulunuz olduğunda gardiyanlar çok daha okunabilir ve kalıplarla gerçekten güzel oynuyorlar.

Sözdizimini açıklamak yerine, sadece içeri girelim ve korumaları kullanarak bir fonksiyon yapalım. [BMI](http://en.wikipedia.org/wiki/Body_mass_index)'ınıza (vücut kitle indeksi) bağlı olarak
sizi farklı şekilde azarlayan basit bir fonksiyon yapacağız. BMI'nız, kilonuzun boyunuzun karesine bölünmesiyle eşittir. 
BMI'nız 18,5'in altındaysa, zayıf olarak kabul edilirsiniz. 18,5 ile 25 arasında bir yerdeyse, normal kabul edilirsiniz. 25 ila 30 fazla kilolu ve 30'dan fazlası obez.
İşte fonksiyon (şu anda onu hesaplamayacağız, bu fonksiyon sadece bir BMI alır ve size söyler)

~~~~ {.haskell: .ghci name="code"}
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
~~~~

Korumalar, bir fonksiyonun adını ve parametrelerini izleyen pipe'larla gösterilir. Genellikle, biraz sağa doğru girintilenir ve sıralanırlar.
Bir koruma, temelde mantıksal bir ifadedir. `True` olarak değerlendirilirse, karşılık gelen fonksiyon gövdesi kullanılır. `False` olarak değerlendirilirse,
kontrol işlemi bir sonraki korumaya geçer ve bu böyle devam eder. Bu fonksiyonu `24.3` ile çağırırsak, önce bunun `18.5`'e eşit veya küçük olup olmadığını kontrol edecektir.
Çünkü öyle değil, bir sonraki korumaya düşüyor. Kontrol, ikinci koruma ile gerçekleştirilir ve 24.3, 25.0'dan küçük olduğu için ikinci string döndürülür.

Bu, zorunlu dillerde, büyükse büyük bir ağacı andırıyor, ancak bu çok daha iyi ve daha okunaklı. Büyük, aksi halde ağaçlar genellikle hoş karşılanmazken,
bazen bir problem, etrafından dolanamayacağınız kadar ayrı bir şekilde tanımlanır. Muhafızlar bunun için çok güzel bir alternatif.

Çoğu zaman, son koruma `otherwise`'dır. `otherwise` basitçe `otherwise = True` olarak tanımlanır ve her şeyi yakalar.
Bu modellere çok benzer, yalnızca girdinin bir modele uyup uymadığını kontrol ederler, ancak korumalar boolean koşullarını kontrol ederler.
Bir fonksiyonun tüm korumaları `False` olarak değerlendirilirse (ve bir `otherwise`'la tümünü kapsayan bir koruma sağlamadıysak), değerlendirme bir sonraki **kalıba** geçer.
Kalıplar ve muhafızlar böyle güzel bir şekilde birlikte oynar. Uygun koruma veya desen bulunmazsa, bir hata atılır.

Elbette istediğimiz kadar parametre alan fonksiyonlara sahip korumaları kullanabiliriz. Kullanıcının fonksiyonu çağırmadan önce kendi BMI'sini hesaplaması yerine,
bu fonksiyonu bir boy ve kilo alacak ve bizim için hesaplayacak şekilde değiştirelim.

~~~~ {.haskell: .ghci name="code"}
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                 = "You're a whale, congratulations!"  
~~~~

Bakalım şişman mıyım ...

~~~~ {.haskell: .ghci name="code"}
ghci> bmiTell 85 1.90  
"You're supposedly normal. Pffft, I bet you're ugly!"  
~~~~

Yaşasın! Şişman değilim! Ama Haskell az önce bana çirkin dedi. Her neyse!

Fonksiyon adından ve parametrelerinden hemen sonra, ilk korumadan önce `=` olmadığına dikkat edin. Pek çok yeni başlayanlar sözdizimi hataları alıyor
çünkü bazen oraya koyuyorlar.

Çok basit bir örnek daha: hadi kendi `max` fonksiyonumuzu uygulayalım. Hatırlarsanız, karşılaştırılabilecek iki şey alır ve daha büyük olanı döndürür.

~~~~ {.haskell: .ghci name="code"}
max' :: (Ord a) => a -> a -> a  
max' a b   
    | a > b     = a  
    | otherwise = b  
~~~~

Muhafızlar satır içi olarak da yazılabilir, ancak buna karşı bir tavsiyem var çünkü çok kısa fonksiyonlar için bile daha az okunabilir.
Ancak bunu göstermek için şu şekilde `max'` yazabiliriz:

~~~~ {.haskell: .ghci name="code"}
max' :: (Ord a) => a -> a -> a  
max' a b | a > b = a | otherwise = b  
~~~~

Ugh! Hiç de okunabilir değil! Devam edelim: Muhafızları kullanarak kendi `compare`'imizi uygulayalım.

~~~~ {.haskell: .ghci name="code"}
myCompare :: (Ord a) => a -> a -> Ordering  
a `myCompare` b  
    | a > b     = GT  
    | a == b    = EQ  
    | otherwise = LT  
~~~~

~~~~ {.haskell: .ghci name="code"}
ghci> 3 `myCompare` 2  
GT  
~~~~

**Not**: Fonksiyonları yalnızca geri işaretli infix olarak adlandırmakla kalmayabiliriz, aynı zamanda onları geri işaretler kullanarak da tanımlayabiliriz.
Bazen bu şekilde okumak daha kolaydır.

Where!?
--------

Önceki bölümde, bir BMI hesaplayıcı fonksiyonu ve şöyle bir tampon tanımladık:

~~~~ {.haskell: .ghci name="code"}
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | weight / height ^ 2 <= 18.5 = "You're underweight, you emo, you!"  
    | weight / height ^ 2 <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | weight / height ^ 2 <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise                   = "You're a whale, congratulations!"   
~~~~

Burada kendimizi üç kez tekrarladığımıza dikkat edin. Kendimizi üç kez tekrarlıyoruz. Programlama sırasında kendinizi (üç kez) tekrarlamak,
kafanıza tekme atmak kadar arzu edilir bir şeydir. Aynı ifadeyi üç kez tekrarladığımız için, onu bir kez hesaplayıp bir isme bağlayıp sonra
ifade yerine o ismi kullansak ideal olur. Pekala, fonksiyonumuzu şu şekilde değiştirebiliriz:

~~~~ {.haskell: .ghci name="code"}
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
~~~~

`where` keyword'ünü korumaların arkasına koyarız (genellikle en iyisi, pipe'lar girintili olduğu kadar onu girintilendirmektir) ve sonra birkaç isim veya fonksiyon tanımlarız.
Bu isimler gardiyanlar arasında görülebilir ve bize kendimizi tekrarlamak zorunda kalmama avantajı sağlar. BMI'yi biraz farklı hesaplamak istediğimize karar verirsek,
sadece bir kez değiştirmemiz gerekir. Ayrıca şeylere ad vererek okunabilirliği artırır ve buradaki `bmi` değişkenimiz gibi şeyler yalnızca bir kez hesaplandığından
programlarımızı daha hızlı hale getirebilir. Biraz denize düşebilir ve fonksiyonumuzu şu şekilde sunabiliriz:

~~~~ {.haskell: .ghci name="code"}
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  
~~~~

Bir fonksiyonun where bölümünde tanımladığımız isimler yalnızca o fonksiyon tarafından görülebilir, bu nedenle diğer fonksiyonların ad alanını kirletmeleri konusunda
endişelenmemize gerek yoktur. Tüm adların tek bir sütunda hizalandığına dikkat edin. Onları güzel ve düzgün bir şekilde hizalamazsak Haskell'in kafası karışır
çünkü o zaman hepsinin aynı bloğun parçası olduğunu bilmez.

*where* bağlamaları, farklı modellerin fonksiyon gövdeleri arasında paylaşılmaz. Bir fonksiyonun birkaç deseninin bazı paylaşılan adlara erişmesini istiyorsanız,
bunu genel olarak tanımlamanız gerekir.

Ayrıca **desen eşleştirmek** için where bağlamalarını da kullanabilirsiniz! Önceki fonksiyonumuzun where bölümünü şu şekilde yeniden yazabilirdik:

~~~~ {.haskell: .ghci name="code"}
...  
where bmi = weight / height ^ 2  
      (skinny, normal, fat) = (18.5, 25.0, 30.0)   
~~~~

Bir ad ve soyad aldığımız ve birine baş harflerini geri verdiğimiz oldukça önemsiz bir fonksiyon daha yapalım.

~~~~ {.haskell: .ghci name="code"}
initials :: String -> String -> String  
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."  
    where (f:_) = firstname  
          (l:_) = lastname    
~~~~

Bu desen eşleştirmesini doğrudan fonksiyonun parametrelerinde yapabilirdik (aslında daha kısa ve daha net olurdu) ama 
bu sadece bunu bağlamalarda da yapmanın mümkün olduğunu gösterir.

Sabitleri where bloklarında tanımladığımız gibi, fonksiyonları da tanımlayabilirsiniz. Sağlıklı programlama temamıza sadık kalarak,
ağırlık-boy çiftlerinin bir listesini alan ve BMI'ların bir listesini döndüren bir fonksiyon yapalım.

~~~~ {.haskell: .ghci name="code"}
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi w h | (w, h) <- xs]  
    where bmi weight height = weight / height ^ 2  
~~~~

Ve hepsi bu kadar! Bu örnekte `bmi`'ı bir fonksiyon olarak sunmak zorunda olmamızın nedeni, fonksiyonun parametrelerinden yalnızca bir BMI hesaplayamayacağımızdır.
Fonksiyona aktarılan listeyi incelemeliyiz ve oradaki her çift için farklı bir BMI var.

*where* bağlamaları da iç içe yerleştirilebilir. Bir fonksiyon yapmak ve onun *where* cümlesinde bazı yardımcı fonksiyonlar tanımlamak ve
sonra bu fonksiyonlara her biri kendi *where* cümlesine sahip yardımcı fonksiyonlar vermek yaygın bir deyimdir.


Let it be
---------

Let bağlamaları where bağlamalarına çok benzer. where bağlamaları, bir fonksiyonun sonundaki değişkenlere bağlanmanıza izin veren sözdizimsel bir yapıdır ve
tüm korumalar da dahil olmak üzere tüm fonksiyon onları görebilir. let bağlamaları, değişkenlere her yerde bağlanmanıza izin verir ve
ifadelerin kendileri olurlar, ancak çok yereldirler, bu nedenle korumalara yayılmazlar. Haskell'de değerleri adlara bağlamak için kullanılan herhangi bir yapı gibi,
desen eşleştirme için bağlamaların kullanılmasına izin verin. Onları iş başında görelim! Bize bir silindirin yüzey alanını yüksekliğine ve 
yarıçapına göre veren bir fonksiyonu bu şekilde tanımlayabiliriz:

~~~~ {.haskell: .ghci name="code"}
cylinder :: (RealFloat a) => a -> a -> a  
cylinder r h = 
    let sideArea = 2 * pi * r * h  
        topArea = pi * r ^2  
    in  sideArea + 2 * topArea  
~~~~
![letitbe](../img/letitbe.png)
Biçimi, `let <bindings> in <expression>`'dır. *let* bölümünde tanımladığınız isimlere *in* kısmından sonraki ifadeye erişilebilir.
Gördüğünüz gibi, bunu bir where bağlamı ile de tanımlayabilirdik. Not: İsimlerin de tek bir sütunda hizalandığına dikkat edin. Öyleyse ikisi arasındaki fark nedir?
Şimdilik öyle görünüyor ki *let* önce bağlamaları ve daha sonra bunları kullanan ifadeyi koyarken *where* tam tersi.

Aradaki fark, *let* bağlamaların ifadelerin kendileri olmasıdır. *where* bağlamaları yalnızca sözdizimsel yapılardır.
if ifadesini yaptığımızda ve if ifadesinin bir ifade olduğunun açıklandığını ve onu hemen hemen her yere sıkıştırabileceğinizi hatırlıyor musunuz?

~~~~ {.haskell: .ghci name="code"}
ghci> [if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"]  
["Woo", "Bar"]  
ghci> 4 * (if 10 > 5 then 10 else 0) + 2  
42  
~~~~

Bunu *let* bağlamalarıyla da yapabilirsiniz.

~~~~ {.haskell: .ghci name="code"}
ghci> 4 * (let a = 9 in a + 1) + 2  
42   
~~~~

local scope fonksiyonları tanıtmak için de kullanabilirsiniz.

~~~~ {.haskell: .ghci name="code"}
ghci> [let square x = x * x in (square 5, square 3, square 2)]  
[(25,9,4)]   
~~~~

Satır içi birkaç değişkene bağlanmak istiyorsak, onları sütunlara hizalayamayız. Bu yüzden onları noktalı virgülle ayırabiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)  
(6000000,"Hey there!")  
~~~~

Son bağlamadan sonra noktalı virgül koymak zorunda değilsiniz, ancak isterseniz yapabilirsiniz. Daha önce söylediğimiz gibi, *let* bağlamaları ile
desen eşleştirme yapabilirsiniz. Bir demeti bileşenlerden hızlı bir şekilde ayırmak ve bunları adlara ve benzerlerine bağlamak için çok kullanışlıdırlar.

~~~~ {.haskell: .ghci name="code"}
ghci> (let (a,b,c) = (1,2,3) in a+b+c) * 100  
600  
~~~~

Ayrıca *let* bağlamalarını liste anlamalarının içine de koyabilirsiniz. Ağırlık-yükseklik çiftlerinin listelerini hesaplamakla ilgili önceki örneğimizi, 
bir yardımcı fonksiyonu *where* ile tanımlamak yerine bir liste anlayışı içinde *let* kullanmak için yeniden yazalım.

~~~~ {.haskell: .ghci name="code"}
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]   
~~~~

Bir predicate yaptığımız gibi bir liste kavrayışının içine bir *let* ekleriz, ancak listeyi filtrelemez, yalnızca adlara bağlanır.
Bir liste kavrayışının içindeki *let*'te tanımlanan adlar, çıktı fonksiyonu (`|`'den önceki kısım) ve bağlamadan sonra gelen tüm predicate'ler ve bölümler tarafından görülebilir. Böylece fonksiyonumuzun yalnızca şişman insanların BMI'larını döndürmesini sağlayabiliriz:

~~~~ {.haskell: .ghci name="code"}
calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2, bmi >= 25.0]  
~~~~

`(w,h) <- xs` kısmında `bmi` adını kullanamayız çünkü *let* bağlamasından önce tanımlanmıştır.

İsimlerin görünürlüğü zaten orada önceden tanımlanmış olduğundan, onları liste anlamalarında kullandığımızda *let* bağının bir kısmını atladık
Bununla birlikte, bir predicate içinde bir *let in* bağlaması kullanabiliriz ve tanımlanan adlar yalnızca o predicate tarafından görülebilir.
*in* bölümü, fonksiyonları ve sabitleri doğrudan GHCi'da tanımlarken de çıkarılabilir. Bunu yaparsak, isimler tüm interaktif oturum boyunca görünür olacaktır.

~~~~ {.haskell: .ghci name="code"}
ghci> let zoot x y z = x * y + z  
ghci> zoot 3 9 2  
29  
ghci> let boot x y z = x * y + z in boot 3 4 2  
14  
ghci> boot  
<interactive>:1:0: Not in scope: `boot'  
~~~~

Eğer *let* bağlamalar o kadar havalıysa, neden *where* bağlamalar yerine onları her zaman kullanmıyorsunuz? Pekala, *let* bağlamaları ifadeler olduğundan ve
kapsamları açısından oldukça yerel olduklarından, korumalar arasında kullanılamazlar. Bazı insanlar *where* bağlamayı tercih ederler
çünkü isimler kullanıldıkları fonksiyondan sonra gelir. Bu şekilde, fonksiyon gövdesi, adına ve tür bildirimine daha yakındır ve daha okunaklı olan bazılarına daha yakındır.


Durum ifadeleri
---------------

Çoğu zorunlu dilin (C, C ++, Java, vb.) Büyük/küçük harf sözdizimi vardır ve bunlarda programladıysanız, muhtemelen ne hakkında olduğunu biliyorsunuzdur.
Bu, bir değişkeni almak ve sonra o değişkenin belirli değerleri için kod bloklarını çalıştırmak ve sonra değişkenin bir durum oluşturmadığımız bir değere sahip 
olması durumunda bir tümünü yakalama kod bloğu eklemekle ilgilidir.

Çoğu imperative(zorunlu) dilde (C, C++, Java vb) case sözdizimi vardır. Haskell bu kavramı alıyor ve bir artıyor.
Adından da anlaşılacağı gibi, durum ifadeleri, if else ifadeleri ve *let* bağlamaları gibi ifadelerdir.
İfadeleri yalnızca bir değişkenin değerinin olası durumlarına göre değerlendirmekle kalmaz, aynı zamanda desen eşleştirme de yapabiliriz.
Hmmm, bir değişkeni almak, onu eşleştirmek, kod parçalarını değerine göre değerlendirmek, bunu daha önce nerede duymuştuk?
Oh evet, fonksiyon tanımlarında parametrelerde desen eşleştirme! Şey, bu aslında durum ifadeleri için sözdizimsel şekerdir. 
Bu iki kod parçası aynı şeyi yapar ve birbirinin yerine kullanılabilir:

~~~~ {.haskell: .ghci name="code"}
head' :: [a] -> a  
head' [] = error "No head for empty lists!"  
head' (x:_) = x 
~~~~

~~~~ {.haskell: .ghci name="code"}
head' :: [a] -> a  
head' xs = case xs of [] -> error "No head for empty lists!"  
                      (x:_) -> x  
~~~~

Gördüğünüz gibi, durum ifadelerinin sözdizimi oldukça basittir:

~~~~ {.haskell: .ghci name="code"}
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
                   ...  
~~~~

`expression`, desenlerle eşleştirilir. Desen eşleştirme eylemi, beklenen ile aynıdır: İfadeyle eşleşen ilk desen kullanılır.
Tüm durum ifadesine denk gelirse ve uygun bir model bulunamazsa, bir çalışma zamanı hatası oluşur.

Fonksiyon parametrelerinde desen eşleştirme yalnızca fonksiyonları tanımlarken yapılabilirken, durum ifadeleri hemen hemen her yerde kullanılabilir. Örneğin:

~~~~ {.haskell: .ghci name="code"}
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list."  
~~~~

İfadenin ortasındaki bir şeyle desen eşleştirme için kullanışlıdırlar. Fonksiyon tanımlarında desen eşleştirme, durum ifadeleri için sözdizimsel şeker olduğundan,
bunu şu şekilde de tanımlayabilirdik:

~~~~ {.haskell: .ghci name="code"}
describeList :: [a] -> String  
describeList xs = "The list is " ++ what xs  
    where what [] = "empty."  
          what [x] = "a singleton list."  
          what xs = "a longer list."  
~~~~

