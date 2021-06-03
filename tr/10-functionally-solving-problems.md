İşlevsel Olarak Sorunları Çözme
===============================

Bu bölümde, birkaç ilginç soruna ve bunları olabildiğince zarif bir şekilde çözmek için işlevsel olarak nasıl düşüneceğimize bir göz atacağız.
Muhtemelen yeni konseptler sunmayacağız, sadece yeni edindiğimiz Haskell kaslarımızı esnetip kodlama becerilerimizi geliştireceğiz.
Her bölüm farklı bir problem sunacaktır. Önce sorunu tanımlayacağız, sonra onu çözmenin en iyi (veya en az kötü) yolunun ne olduğunu bulmaya çalışacağız.


Ters Lehçe notasyon hesaplayıcı
-------------------------------

Genellikle okulda matematiksel ifadeler yazdığımızda, bunları sonsuz bir şekilde yazarız. Örneğin, Haskell'de (`+`, ```elem```, vb.) tanıştığımız infix fonksiyonlar gibi
`10 - (4 + 3) * 2`. `+`, `*` ve `-` infix operatörleri yazıyoruz. Bu onu kullanışlı kılıyor çünkü biz insanlar olarak, böyle bir ifadeye bakarak zihnimizde kolayca ayrıştırabiliriz.
Bunun dezavantajı, önceliği belirtmek için parantez kullanmamız gerektiğidir.

[Ters Lehçe notasyonu](https://en.wikipedia.org/wiki/Reverse_Polish_notation), matematiksel ifadeleri yazmanın başka bir yoludur. Başlangıçta biraz tuhaf görünüyor, ama aslında anlaşılması ve 
kullanılması oldukça kolay çünkü parantezlere gerek yok ve bir hesap makinesine girmek çok kolay.
Çoğu modern hesap makinesi infix gösterimini kullanırken, bazı insanlar hala RPN hesap makinelerine yemin ediyor.
RPN'de önceki infix ifadesi şöyle görünür: `10 4 3 + 2 * -`. Bunun sonucunun ne olduğunu nasıl hesaplıyoruz? Bir yığın düşünün. İfadenin üzerinden soldan sağa gidin.
Bir sayıyla her karşılaşıldığında, onu yığına itin. Bir operatörle karşılaştığımızda, yığının en üstündeki iki sayıyı alın (ayrıca onları patlattığımızı söyleriz),
operatörü ve bu ikisini kullanın ve sonra elde edilen sayıyı yığına geri itin.
İfadenin sonuna geldiğinizde, eğer ifade iyi biçimlendirilmişse ve bu sayı sonucu temsil ediyorsa tek bir sayı kalmalıdır.

![rpn](../img/rpn.png)
Gelin birlikte `10 4 3 + 2 *` ifadesinin üzerinden geçelim! Önce `10`'u yığına basıyoruz ve yığın şimdi `10`. Bir sonraki öğe `4`, bu yüzden onu da yığına itiyoruz.
Yığın şimdi `10, 4`. Aynı şeyi `3` ile yapıyoruz ve yığın şimdi `10, 4, 3`. Ve şimdi, bir operatörle, yani `+`!
Yığıntan en üstteki iki sayıyı çıkarırız (yani şimdi yığın sadece `10`'dur), bu sayıları bir araya getiririz ve bu sonucu yığına aktarırız.
Yığın artık `10, 7`. `2`'yi yığına itiyoruz, şimdilik yığın `10, 7, 2`. Yine bir operatörle karşılaştık, öyleyse hadi `7` ve `2`'yi yığından çıkaralım,
onları çarpalım ve bu sonucu yığına aktaralım. `7` ve `2`'yi çarpmak bir `14` üretir, yani şu anda sahip olduğumuz yığın `10`, `14`'tür.
Son olarak, bir `-` var. Yığından `10` ve `14`'ü çıkarıyoruz, `10`'dan `14`'ü çıkarıyoruz ve geri itiyoruz.
Yığınının üzerindeki sayı şimdi `-4`'tür ve ifademizde daha fazla sayı veya operatör olmadığı için, bu bizim sonucumuzdur!

Artık herhangi bir RPN ifadesini elle nasıl hesaplayacağımızı bildiğimize göre, `"10 4 3 + 2 * -"` gibi bir RPN ifadesi içeren bir string'i
parametresi olarak alan bir Haskell fonksiyonunu nasıl yapabileceğimizi düşünelim ve sonucunu geri verelim.

Bu fonksiyonun türü ne olabilir? Parametre olarak bir string almasını ve sonucu olarak bir sayı üretmesini istiyoruz.
Yani muhtemelen `solveRPN :: (Num a) => String -> a` gibi bir şey olacaktır.

**Protip:** Uygulamayla ilgili olarak önce bir fonksiyonun tür bildiriminin ne olması gerektiğini düşünmek ve sonra onu yazmak gerçekten yardımcı olur.
Haskell'de, bir fonksiyonun tür bildirimi, çok güçlü tür sistemi nedeniyle bize fonksiyon hakkında çok şey anlatır.

![calculator](../img/calculator.png)
Güzel. Haskell'de bir soruna çözüm uygularken, bunu elle nasıl yaptığınızı tekrar düşünmek ve belki bundan bir fikir edinebilecek misiniz diye görmek de iyidir.
Burada bir boşlukla ayrılan her sayı veya operatörü tek bir öğe olarak ele aldığımızı görüyoruz.
Dolayısıyla, `["10","4","3","+","2","*","-"]` gibi bir öğe listesine `10 4 3 + 2 *` gibi bir string'i bölerek başlamamız bize yardımcı olabilir.

Sonra, kafamızdaki bu öğeler listesiyle ne yaptık? Soldan sağa gittik ve bunu yaparken bir yığın tuttuk. Önceki cümle size bir şey hatırlatıyor mu?
Unutmayın, [fold'larla](../tr/06-higher-order-functions.md#sadece-foldlar-ve-atlar) ilgili bölümde, bir listeyi soldan sağa ya da sağdan sola bir eleman eleman geçirdiğiniz ve bir sonuç oluşturduğunuz (biriktirdiğiniz)
hemen hemen her fonksiyonun (bir sayı, bir liste, bir yığın, ne olursa olsun) bir fold ile uygulanabilir.

 Bu durumda, bir sol fold kullanacağız çünkü listeyi soldan sağa doğru ilerliyoruz. Biriktirici değeri bizim yığınımız olacak ve bu nedenle,
 fold'un sonucu da bir yığın olacak, yalnızca gördüğümüz gibi, yalnızca bir öğeye sahip olacak.

Düşünülmesi gereken bir şey daha var, yığını nasıl temsil edeceğiz? Bir liste kullanmamızı öneriyorum.
Ayrıca, yığının en tepesini listenin başında tutmamızı öneriyorum. Bunun nedeni, bir listenin başına (başına) eklemenin, sonuna eklemekten çok daha hızlı olmasıdır.
Yani `10, 4, 3` gibi bir yığınımız varsa, bunu liste `[3,4,10]` olarak göstereceğiz.

Şimdi, fonksiyonunuzu kabaca çizmek için yeterli bilgiye sahibiz.
`"10 4 3 + 2 * -"` gibi bir string alacak ve `["10","4","3","+","2","*","-"]` elde etmek için `words` kullanarak onu bir öğe listesine bölecektir.
Sonra, bu listenin üzerinde bir sol fold yapacağız ve sonunda tek bir öğeye sahip bir yığın elde edeceğiz, yani `[-4]`.
Bu tek öğeyi listeden çıkarıyoruz ve bu bizim nihai sonucumuz! 

İşte bu fonksiyonun bir taslağı:

~~~~ {.haskell: .ghci name="code"}
import Data.List  
  
solveRPN :: (Num a) => String -> a  
solveRPN expression = head (foldl foldingFunction [] (words expression))  
    where   foldingFunction stack item = ...  
~~~~

İfadeyi alıp bir öğe listesine dönüştürüyoruz. Ardından, fold fonksiyonuna sahip bu öğeler listesini fold'luyoruz.
Başlangıç üreticisini temsil eden `[]`'ye dikkat edin. Tek bir öğe ile son yığını elde ettikten sonra,
öğeyi çıkarmak için o listeye `head` diyoruz ve ardından `read` uyguluyoruz.

Şimdi geriye kalan tek şey, `[4,10]` gibi bir yığını ve `"3"` gibi bir öğeyi alıp yeni bir yığın [3,4,10] döndüren bir fold fonksiyonu uygulamaktır.
Yığın `[4,10]` ve öğe `"*"` ise, `[40]` döndürmesi gerekecektir. Ama ondan önce, fonksiyonumuzu [noktasız stile](../tr/06-higher-order-functions.md#fonksiyon-bileşimi) çevirelim çünkü beni biraz korkutan pek çok parantez var:

~~~~ {.haskell: .ghci name="code"}
import Data.List  
  
solveRPN :: (Num a) => String -> a  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction stack item = ...  
~~~~

Ah, işte başlıyoruz. Çok daha iyi. Bu nedenle, fold fonksiyonu bir yığın ve bir öğe alır ve yeni bir yığın döndürür.
Bir yığının en iyi öğelerini elde etmek ve `"*"` ve `"-"` gibi operatörlerle pattern macthing uygulamak için desen eşleştirmeyi kullanacağız.

~~~~ {.haskell: .ghci name="code"}
solveRPN :: (Num a, Read a) => String -> a  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction xs numberString = read numberString:xs  
~~~~

Bunu dört kalıp olarak ortaya koyduk. Kalıplar yukarıdan aşağıya denenecek. Önce fold fonksiyon, geçerli öğenin `"*"` olup olmadığını görecek.
Eğer öyleyse, `[3,4,9,3]` gibi bir liste alacak ve sırasıyla ilk iki elemanı `x` ve `y`'yi çağıracaktır. Yani bu durumda, `x` `3` ve `y` `4` olur. `ys` `[9,3]` olacaktır.
Tıpkı `ys` gibi bir liste döndürecektir, sadece head çarpılmış `x` ve `y`'ye sahiptir. Böylece, en üstteki iki sayıyı yığından çıkarırız,
onları çarparız ve sonucu yığına geri göndeririz. Böylece, en üstteki iki sayıyı yığından çıkarırız, onları çarparız ve sonucu yığına geri göndeririz.
Öğe `"*"` değilse, desen eşleştirme düşecek ve `"+"` işaretlenecek ve bu böyle devam edecek.

Öğe operatörlerden hiçbiri değilse, bir sayıyı temsil eden bir string olduğunu varsayarız. Eğer bu bir sayı ise, ondan bir sayı almak ve önceki yığına dönmek için
bu string'te `read` çağırırız, ancak bu numara en üste itilir.

Ve bu kadar! Ayrıca, sayıyı almak için string'imizde `read` çağırdığımız için, fonksiyon bildirimine fazladan bir `Read a` sınıf kısıtlaması eklediğimizi fark ettik.
Dolayısıyla bu bildirim, sonucun `Num` ve `Read` tür sınıflarının (`Int`, `Float` vb. gibi) parçası olan herhangi bir türüde olabileceği anlamına gelir.

`["2", "3", "+"]` öğelerin listesi için, fonksiyonumuzu fold'lamaya soldan başlayacaktır. İlk yığın `[]` olacaktır.
folding fonksiyonunu yığın (türetici) olarak `[]` ve öğe olarak `"2"` ile çağırır. Bu öğe bir operatör olmadığı için okunacak ve `[]` başlığının başına eklenecektir.
Böylece yeni yığın şimdi `[2]` ve folding fonksiyonu yığın olarak `[2]` ve öğe olarak `["3"]` ile çağrılacak ve yeni bir `[3,2]` yığını oluşturacak.
Ardından, yığın olarak `[3,2]` ve öğe olarak `"+"` ile üçüncü kez çağrılır. Bu, bu iki sayının yığından çıkmasına, birbirine eklenmesine ve geri itilmesine neden olur.
Son yığın, döndürdüğümüz sayı olan `[5]` 'dir.

Fonksiyonumuzla oynayalım:

~~~~ {.haskell: .ghci name="code"}
ghci> solveRPN "10 4 3 + 2 * -"  
-4  
ghci> solveRPN "2 3 +"  
5  
ghci> solveRPN "90 34 12 33 55 66 + * - +"  
-3947  
ghci> solveRPN "90 34 12 33 55 66 + * - + -"  
4037  
ghci> solveRPN "90 34 12 33 55 66 + * - + -"  
4037  
ghci> solveRPN "90 3 -"  
87  
~~~~

Harika, işe yarıyor! Bu fonksiyonla ilgili güzel bir şey, diğer çeşitli operatörleri desteklemek için kolayca değiştirilebilmesidir.
Binary operatörler olmak zorunda bile değiller. Örneğin, yığından sadece bir sayı çıkaran ve logaritmasını geri iten bir operatör `"log"` oluşturabiliriz.
Yığından üç sayı çıkaran ve bir sonucu veya tüm sayıları çıkaran ve toplamlarını geri iten `"sum"` gibi işleçleri geri iten üçlü operatörler de yapabiliriz.

Fonksiyonumuzu birkaç operatör daha alacak şekilde değiştirelim. Basitlik uğruna, tür bildirimini bir dizi `Float` türü döndürecek şekilde değiştireceğiz.

~~~~ {.haskell: .ghci name="code"}
import Data.List  
  
solveRPN :: String -> Float  
solveRPN = head . foldl foldingFunction [] . words  
    where   foldingFunction (x:y:ys) "*" = (x * y):ys  
            foldingFunction (x:y:ys) "+" = (x + y):ys  
            foldingFunction (x:y:ys) "-" = (y - x):ys  
            foldingFunction (x:y:ys) "/" = (y / x):ys  
            foldingFunction (x:y:ys) "^" = (y ** x):ys  
            foldingFunction (x:xs) "ln" = log x:xs  
            foldingFunction xs "sum" = [sum xs]  
            foldingFunction xs numberString = read numberString:xs  
~~~~

Vay harika! `/` elbette bölmedir ve `**` kayan noktalı sayı(floating point exponentiation). Logaritma operatörü ile, doğal logaritmasını gerçekleştirmek için
yalnızca bir öğeye ihtiyacımız olduğundan, tek bir öğeye ve yığının geri kalanına karşı yalnızca desen eşleştirme yaparız.
sum operatörü ile, şimdiye kadarki yığının toplamı olan yalnızca bir öğesi olan bir yığın döndürürüz.

~~~~ {.haskell: .ghci name="code"}
ghci> solveRPN "2.7 ln"  
0.9932518  
ghci> solveRPN "10 10 10 10 sum 4 /"  
10.0  
ghci> solveRPN "10 10 10 10 10 sum 4 /"  
12.5  
ghci> solveRPN "10 2 ^"  
100.0  
~~~~

İfademize kayan noktalı sayıları ekleyebileceğimize dikkat edin çünkü `read` onları nasıl okuyacağını bilir.

~~~~ {.haskell: .ghci name="code"}
ghci> solveRPN "43.2425 0.5 ^"  
6.575903  
~~~~

Keyfi kayan noktalı RPN ifadelerini hesaplayabilen ve 10 satırda kolayca genişletme seçeneğine sahip bir fonksiyon yapmak oldukça harika.

Bu fonksiyonla ilgili dikkat edilmesi gereken bir nokta, gerçekten hataya dayanıklı olmamasıdır. Mantıklı olmayan girdi verildiğinde, her şeyi çökertir.
Monad'ları tanıdığımızda (korkutucu değiller, güven bana!) `solveRPN :: String -> Maybe Float` tür bildirimi ile bunun hataya dayanıklı bir versiyonunu yapacağız.
Şu anda bir tane yapabilirdik, ama bu biraz sıkıcı olurdu çünkü her adımda çok fazla `Nothing` kontrolü gerektirecekti.
Yine de meydan okumaya hazır hissediyorsanız, devam edip deneyebilirsiniz! İpucu: Bir okumanın başarılı olup olmadığını görmek için `reads`'i kullanabilirsiniz.


Heathrow'dan Londra'ya
------------------

Bir sonraki sorunumuz şudur: uçağınız az önce İngiltere'ye indi ve bir araba kiraladınız. Çok yakında bir toplantınız var ve Heathrow Havaalanı'ndan
Londra'ya olabildiğince hızlı (ama güvenli bir şekilde!) gitmelisiniz.

Heathrow'dan Londra'ya giden iki ana yol var ve bunları geçen birkaç bölgesel yol var.
Bir kavşaktan diğerine gitmek sabit bir zaman alır. Olabildiğince hızlı bir şekilde Londra'ya gitmeniz için en uygun yolu bulmak size kalmış!
Sol taraftan başlayıp diğer ana yola geçebilir veya ilerleyebilirsiniz.

![roads](../img/roads.png)
Resimde de görebileceğiniz gibi, bu durumda Heathrow'dan Londra'ya giden en kısa yol, ana yol B'den başlamak, karşıya geçmek,
A üzerinde ilerlemek, tekrar karşıya geçmek ve sonra B üzerinde iki kez ilerlemektir. Bu yoldan gidersek 75 dakikamızı alır.
Başka bir yol seçmiş olsaydık, bundan daha fazlasını alırdı.

İşimiz, bir yol sistemini temsil eden girdileri alan ve en kısa yolun ne olduğunu yazdıran bir program yapmaktır.
Bu durum için girdinin nasıl görüneceği aşağıda açıklanmıştır:

~~~~ {.haskell: .ghci name="code"}
50  
10  
30  
5  
90  
20  
40  
2  
25  
10  
8  
0  
~~~~

Giriş dosyasını zihinsel olarak ayrıştırmak için, onu üçlü olarak okuyun ve yol sistemini zihinsel olarak bölümlere ayırın.
Her bölüm bir A yolu, B yolu ve bir kavşak yolundan oluşur. Düzgün bir şekilde üçlere sığdırmak için, üzerinden geçmesi 0 dakika süren son bir geçiş kesiti
olduğunu söylüyoruz. Bunun nedeni, Londra'da olduğumuz sürece, Londra'da nereye varacağımız umurumuzda değil.

RPN hesap makinesi problemini çözerken yaptığımız gibi, bu problemi üç adımda çözeceğiz:

- Haskell'i bir dakikalığına unutun ve sorunu elle nasıl çözeceğimizi düşünün.
- Verilerimizi Haskell'de nasıl temsil edeceğimizi düşünün
- Haskell'de bu veriler üzerinde nasıl çalışılacağını öğrenin, böylece bir çözümde üretebiliriz

RPN hesap makinesi bölümünde, önce elle bir ifade hesaplarken, zihnimizde bir tür yığın tutacağımızı ve sonra ifadenin her seferinde bir öğenin üzerinden geçeceğimizi anladık.
Son olarak, bir çözüm üretmek için bir yığın tutarken string'ler listesinde gezinmek için bir sol fold kullandık.

Peki, Heathrow'dan Londra'ya giden en kısa yolu elle nasıl bulabiliriz? Pekala, resmin tamamına bakıp en kısa yolun ne olduğunu tahmin etmeye çalışabiliriz ve
umarım bunun doğru olduğunu tahmin ederiz. Bu çözüm çok küçük girdiler için işe yarıyor, ancak ya 10.000 bölümü olan bir yolumuz varsa? Eyvah!
Ayrıca çözümümüzün en uygun çözüm olduğunu kesin olarak söyleyemeyeceğiz, sadece oldukça emin olduğumuzu söyleyebiliriz.

O halde bu iyi bir çözüm değil. İşte yol sistemimizin basitleştirilmiş bir resmi:

![roads](../img/roads.png)

Pekala, A yolunda ilk kavşağa giden en kısa yolun (A üzerindeki ilk mavi nokta, A1 olarak işaretlenmiştir) ne olduğunu anlayabilir misin? Bu oldukça önemsiz.
Doğrudan A üzerinde ileri gitmenin mi daha kısa, yoksa B üzerinde ilerleyip karşıya geçmenin mi daha kısa olduğunu görüyoruz. 
Açıkçası, B üzerinden ilerlemek ve sonra karşıya geçmek daha ucuzdur çünkü bu 40 dakika sürer, oysa doğrudan A üzerinden gitmek 50 dakika sürer. Ya kavşak B1? Aynı şey.
Doğrudan B üzerinden gitmenin çok daha ucuz olduğunu görüyoruz (10 dakikalık bir ücrete tabi), çünkü A üzerinden gidip karşıya geçmek bize 80 dakika sürecek!

Artık A1'e giden en ucuz yolun ne olduğunu biliyoruz (B üzerinden gidin ve sonra karşıya geçin, bu yüzden 40 maliyetle `B, C` olduğunu söyleyeceğiz) ve
B1'e giden en ucuz yolun ne olduğunu biliyoruz (doğrudan şuradan gidin: `B`, yani bu sadece B, 10'a gidiyor). 
Her iki ana yoldaki bir sonraki kavşağa giden en ucuz yolu bilmek istiyorsak bu bilgi bize hiç yardımcı olur mu? Aman Tanrım, kesinlikle öyle!

A2'ye giden en kısa yolun ne olacağını görelim. A2'ye ulaşmak için, ya doğrudan A1'den A2'ye gideceğiz ya da B1'den ileri gideceğiz ve sonra karşıya geçeceğiz
(unutmayın, sadece ileri gidebiliriz ya da diğer tarafa geçebiliriz). Ve A1 ve B1'in maliyetini bildiğimiz için, A2'ye giden en iyi yolun ne olduğunu kolayca bulabiliriz.
A1'e ulaşmak 40'a ve sonra A1'den A2'ye ulaşmak 5'e mal oluyor, yani 45'lik bir maliyet için `B, C, A` oluyor.
B1'e ulaşmak sadece 10'a mal oluyor, ancak daha sonra B2'ye gitmek ve sonra geçiş yapmak 110 dakika daha alacaktı! Açıkçası, A2'ye giden en ucuz yol `B, C, A`'dır.
Aynı şekilde, B2'ye giden en ucuz yol da A1'den ileri gitmek ve sonra karşıya geçmek.

**Belki kendinize soruyorsunuz:** ama önce B1'den karşıya geçip sonra ilerleyerek A2'ye ulaşmaya ne dersiniz? Eh, A1'e giden en iyi yolu ararken B1'den A1'e geçişi
zaten ele almıştık, bu yüzden sonraki adımda da bunu hesaba katmak zorunda değiliz.

Artık A2 ve B2'ye giden en iyi yola sahip olduğumuza göre, sonuna ulaşana kadar bunu sonsuza kadar tekrar edebiliriz. A4 ve B4 için en iyi yolları bulduktan sonra,
daha ucuz olan en uygun yoldur!

Yani özünde, ikinci bölüm için, ilk başta yaptığımız adımı tekrarlıyoruz, sadece A ve B üzerindeki önceki en iyi yolları hesaba katıyoruz.
İlk adımda A ve B üzerindeki en iyi yolları da hesaba kattığımızı söyleyebiliriz, sadece ikisi de 0 maliyetle boş yollar.

İşte bir özet. Heathrow'dan Londra'ya en iyi yolu bulmak için şunu yapıyoruz: Önce, A ana yolundaki bir sonraki kavşağa giden en iyi yolun ne olduğunu görürüz.
İki seçenek doğrudan ilerler veya karşı yoldan başlar, ilerler ve sonra karşıdan karşıya geçer. Maliyeti ve yolu hatırlıyoruz.
B ana yolundaki bir sonraki kavşağa giden en iyi yolun ne olduğunu görmek için aynı yöntemi kullanıyoruz ve bunu hatırlıyoruz.
Daha sonra, önceki A kavşağından gidersek veya önceki B kavşağından geçersek, A üzerindeki bir sonraki kavşağa giden yolun daha ucuz olup olmadığını görürüz.
Daha ucuz yolu hatırlıyoruz ve ardından karşısındaki kavşak için de aynısını yapıyoruz. Bunu sonuna kadar her bölüm için yapıyoruz.
Sona vardığımızda, sahip olduğumuz iki yolun en ucuzu, en uygun yolumuzdur!

Yani özünde, A yolunda en kısa bir yolu ve B yolundaki en kısa yolu tutuyoruz ve sona ulaştığımızda, bu ikisinden daha kısa olan yolumuz oluyor.
Artık en kısa yolu elle nasıl bulacağımızı biliyoruz. Yeterli zamanınız, kağıdınız ve kaleminiz varsa,
herhangi bir sayıda bölüme sahip bir yol sisteminden en kısa yolu bulabilirdiniz.

Sonraki adım! Bu yol sistemini Haskell'in data türüyle nasıl temsil ederiz? Bir yol, başlangıç noktalarını ve kavşakları,
diğer kavşaklara işaret eden bir grafiğin düğümleri olarak düşünmektir. Başlangıç noktalarının aslında bir uzunluğa sahip bir yolla birbirine işaret ettiğini düşünürsek,
her kavşağın (veya node'un) diğer taraftaki node'a ve ayrıca kendi tarafındaki bir sonraki node'a işaret ettiğini görürüz. Son node'lar hariç,
sadece diğer tarafa işaret ediyorlar.

~~~~ {.haskell: .ghci name="code"}
data Node = Node Road Road | EndNode Road  
data Road = Road Int Node  
~~~~

Bir node ya normal bir node'lar ve diğer ana yola giden yol ve bir sonraki node'a giden yol veya yalnızca diğer ana yola giden yol hakkında bilgi içeren
bir son node hakkında bilgi içerir. Bir yol, ne kadar uzun olduğu ve hangi node'a işaret ettiği hakkında bilgi tutar.
Örneğin, A ana yolundaki yolun ilk bölümü, `a1`'in bir `Node x y` node'u olacağı `Road 50 a1` olacaktır; burada `x` ve `y`, B1 ve A2'yi gösteren yollardır.

Başka bir yol, ileriyi gösteren yol bölümleri için `Maybe`'yi kullanmaktır. Her düğümün karşı yola işaret eden bir yol bölümü vardır,
ancak yalnızca son olmayan node'ların ileriyi gösteren yol bölümleri vardır.

~~~~ {.haskell: .ghci name="code"}
data Node = Node Road (Maybe Road)  
data Road = Road Int Node  
~~~~

Bu, Haskell'deki yol sistemini temsil etmenin iyi bir yolu ve bu sorunu kesinlikle çözebiliriz, ama belki daha basit bir şey bulabiliriz?
Çözümümüzü elle düşünürsek, her zaman üç yol parçasının uzunluklarını aynı anda kontrol ettik: A yolu üzerindeki yol kısmı,
B yolu üzerindeki karşı tarafı ve bu iki kısma dokunan ve onları birbirine bağlayan C kısmı. A1 ve B1'e giden en kısa yolu ararken, sadece 50, 10 ve 30'luk uzunluklara sahip
ilk üç parçanın uzunluklarıyla uğraşmak zorunda kaldık. Buna bir bölüm diyeceğiz. Dolayısıyla bu örnek için kullandığımız yol sistemi kolaylıkla
dört bölüm olarak temsil edilebilir: `50, 10, 30, 5, 90, 20, 40, 2, 25` ve `10, 8, 0`.

Daha basit olmasa da, veri türlerimizi olabildiğince basit tutmak her zaman iyidir!

~~~~ {.haskell: .ghci name="code"}
data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)  
type RoadSystem = [Section] 
~~~~

Bu hemen hemen mükemmel! Bu kadar basit ve çözümümüzü uygulamak için mükemmel çalışacağını hissediyorum.
`Section`, üç yol bölümünün uzunlukları için üç tamsayı tutan basit bir cebirsel veri türüdür.
`RoadSystem`'in bir bölüm listesi olduğunu söyleyerek bir tür eşanlamlısı da tanıtıyoruz.

Bir yol bölümünü temsil etmek için üçlü `(Int, Int, Int)` de kullanabiliriz. Kendi cebirsel veri türlerinizi oluşturmak yerine demet kullanmak,
bazı küçük yerelleştirilmiş şeyler için iyidir, ancak bunun gibi şeyler için yeni bir tür oluşturmak genellikle daha iyidir.
Tür sistemine neyin ne olduğu hakkında daha fazla bilgi verir. 3D uzayda bir yol kesitini veya bir vektörü temsil etmek için `(Int, Int, Int)` kullanabiliriz ve
bu ikisi üzerinde çalışabiliriz, ancak bu onları karıştırmamıza izin verir. `Section` ve `Vector` veri türlerini kullanırsak,
karayolu sisteminin bir bölümüne yanlışlıkla bir vektör ekleyemeyiz.

Heathrow'dan Londra'ya giden yol sistemimiz artık şu şekilde gösterilebilir:

~~~~ {.haskell: .ghci name="code"}
heathrowToLondon :: RoadSystem  
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]  
~~~~

Şimdi tek yapmamız gereken daha önce Haskell'de bulduğumuz çözümü uygulamak.
Herhangi bir yol sistemi için en kısa yolu hesaplayan bir fonksiyonun tür bildirimi ne olmalıdır? Bir yol sistemini parametre olarak almalı ve bir yol döndürmelidir.
Bir yolu da liste olarak göstereceğiz. Sadece `A`, `B` veya `C`'nin bir numaralandırması olan bir `Label` türü sunalım. Ayrıca bir türü eşanlamlısı yapacağız: `Path`.

~~~~ {.haskell: .ghci name="code"}
data Label = A | B | C deriving (Show)  
type Path = [(Label, Int)]  
~~~~

Fonksiyonumuz, buna `optimalPath` adını vereceğiz, bu nedenle `optimalPath :: RoadSystem -> Path` tür bildirimine sahip olmalıdır.
Yol sistemi `heathrowToLondon` ile çağrılırsa, aşağıdaki yolu döndürmelidir:

~~~~ {.haskell: .ghci name="code"}
[(B,10),(C,30),(A,5),(C,20),(B,2),(B,8)]  
~~~~

Soldan sağa bölümlerle listenin üzerinden geçmemiz ve ilerledikçe A'da en uygun yolu ve B'de en iyi yolu tutmamız gerekecek.
Listede soldan sağa yürürken en iyi yolu biriktireceğiz. Bu neye benziyor? Ding Ding Ding! Doğru, SOL FOLD!

Çözümü elle yaparken defalarca tekrarladığımız bir adım vardı. Şimdiye kadar A ve B üzerindeki en uygun yolları ve A ve B üzerinde yeni en uygun yolları üretmek için
mevcut bölümü kontrol etmeyi içeriyordu. Örneğin, başlangıçta A ve B için en uygun yollar sırasıyla `[]` ve `[]` idi.
`Section 50 10 30`'u inceledik ve A1'e giden yeni en uygun yolun `[(B, 10), (C, 30)]` ve B1'e giden en uygun yolun `[(B, 10)]` olduğu sonucuna vardık.
Bu adıma bir fonksiyon olarak bakarsanız, bir çift yol ve bir bölüm alır ve yeni bir yol çifti oluşturur. Bu adıma bir fonksiyon olarak bakarsanız,
bir çift yol ve bir bölüm alır ve yeni bir yol çifti oluşturur. Türü `(Path, Path) -> Section -> (Path, Path)` şeklindedir.
Devam edelim ve bu fonksiyonu uygulayalım, çünkü yararlı olması zorunludur.

**İpucu:** yararlı olacaktır çünkü `(Path, Path) -> Section -> (Path, Path)`, `a -> b -> a` türüne sahip olması gereken bir sol fold için ikili fonksiyon olarak kullanılabilir.

~~~~ {.haskell: .ghci name="code"}
roadStep :: (Path, Path) -> Section -> (Path, Path)  
roadStep (pathA, pathB) (Section a b c) =   
    let priceA = sum $ map snd pathA  
        priceB = sum $ map snd pathB  
        forwardPriceToA = priceA + a  
        crossPriceToA = priceB + b + c  
        forwardPriceToB = priceB + b  
        crossPriceToB = priceA + a + c  
        newPathToA = if forwardPriceToA <= crossPriceToA  
                        then (A,a):pathA  
                        else (C,c):(B,b):pathB  
        newPathToB = if forwardPriceToB <= crossPriceToB  
                        then (B,b):pathB  
                        else (C,c):(A,a):pathA  
    in  (newPathToA, newPathToB)  
~~~~

![guycar](../img/guycar.png)
Burada neler oluyor? İlk olarak, A yolunda şimdiye kadarki en iyiye dayalı olarak A yolundaki en uygun fiyatı hesaplayın ve aynısını B için de yapıyoruz.
`sum $ map snd pathA` yaparız, dolayısıyla `pathA` `[(A, 100), (C, 20)]` gibi bir şeyse, `priceA` 120 olur. `forwardPriceToA`, A üzerindeki önceki kavşaktan doğrudan oraya gidersek, A üzerindeki bir sonraki kavşağa gidersek ödeyeceğimiz fiyattır. Önceki A'mızın en iyi fiyatı artı mevcut bölümün A kısmının uzunluğuna eşittir.
`crossPriceToA`, önceki B'den ileri gidip sonra karşıya geçerek bir sonraki A'ya gidersek ödeyeceğimiz fiyattır.
B'nin şimdiye kadarki en iyi fiyatı artı bölümün B uzunluğu artı bölümün C uzunluğu. `forwardPriceToB` ve `crossPriceToB`'yi aynı tarzda belirleriz.

Artık A ve B'ye giden en iyi yolun ne olduğunu bildiğimize göre, ona göre A ve B'ye giden yeni yolları yapmamız gerekiyor.
Sadece ileri giderek A'ya gitmek daha ucuzsa, `newPathToA`'yı `(A, a):pathA` olarak ayarladık. 
Temel olarak `Label` `A`'yı ve bölüm uzunluğunu `a`'yı şimdiye kadarki en uygun yol yolunun başına ekliyoruz.
Temel olarak, bir sonraki A kavşağına giden en iyi yolun önceki A kavşağına giden yol ve ardından A üzerinden ileriye doğru bir bölüm olduğunu söylüyoruz.
Unutmayın, `A` sadece bir etikettir, oysa `a`'nın bir `Int` türü vardır. Neden `pathA ++ [(A, a)]` yapmak yerine başa eklemeliyiz?
Bir listenin başlangıcına bir öğe eklemek (consing olarak da bilinir), onu sonuna eklemekten çok daha hızlıdır. 
Bu, bu fonksiyonla bir listeyi fold'ladığımızda yolun yanlış yönde olacağı anlamına gelir, ancak listeyi daha sonra tersine çevirmek kolaydır.
Bir sonraki A kavşağına B yolundan ileri gidip karşıya geçerek gitmek daha ucuzsa, o zaman `newPathToA`, B'ye giden ve daha sonra ileri giden ve A'ya geçen eski yoldur.
Aynı şeyi `newPathToB` için de yapıyoruz, sadece her şey yansıtılıyor.

Son olarak, bir çift halinde `newPathToA` ve `newPathToB` döndürüyoruz.

Bu fonksiyonu `heathrowToLondon`'un ilk bölümünde çalıştıralım. Birinci bölüm olduğu için, A ve B parametresindeki en iyi yollar bir çift boş liste olacaktır.

~~~~ {.haskell: .ghci name="code"}
ghci> roadStep ([], []) (head heathrowToLondon)  
([(C,30),(B,10)],[(B,10)])  
~~~~

Unutmayın, yollar ters çevrilmiştir, bu yüzden onları sağdan sola doğru okuyun. Buradan bir sonraki A'ya giden en iyi yolun B'den başlamak ve sonra A'ya geçmek olduğunu ve
bir sonraki B'ye giden en iyi yolun doğrudan B'deki başlangıç noktasından ileri gitmek olduğunu okuyabiliriz.

**Optimizasyon ipucu:** `priceA = sum $ map snd pathA` yaptığımızda, fiyatı her adımda yoldan hesaplıyoruz.
`roadStep`'i tam sayıların A ve B'deki en iyi fiyatı temsil ettiği `(Path, Path, Int, Int) -> Section -> (Path, Path, Int, Int)` fonksiyonu olarak uygularsak
bunu yapmak zorunda kalmayız.

Artık bir çift yol ve bir kesiti alan ve yeni bir optimal yol üreten bir fonksiyona sahip olduğumuza göre, bir bölüm listesi üzerinde kolayca bir sol fold yapabiliriz.
`roadStep`, `([], [])` ve ilk bölümle çağrılır ve bu bölüme bir çift en uygun yol döndürür. Ardından, bu yol çiftiyle ve sonraki bölümle çağrılır ve bu böyle devam eder.
Tüm bölümleri gezdiğimizde, bir çift optimal yolla baş başa kalırız ve bunlardan daha kısa olanı cevabımızdır. Bunu aklımızda tutarak `optimalPath`'i uygulayabiliriz.

~~~~ {.haskell: .ghci name="code"}
optimalPath :: RoadSystem -> Path  
optimalPath roadSystem = 
    let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem  
    in  if sum (map snd bestAPath) <= sum (map snd bestBPath)  
            then reverse bestAPath  
            else reverse bestBPath  
~~~~

Başlangıç üreticisinin bir çift boş yol olmasıyla birlikte `roadSystem` üzerine sol fold bıraktık (unutmayın, bu bir bölümler listesidir).
Bu fold'un sonucu bir çift yoldur, bu yüzden yolları kendileri elde etmek için çift üzerinde desen eşlemesi yaparız. 
Sonra bunlardan hangisinin daha ucuz olduğunu kontrol edip iade ediyoruz. Dönmeden önce, onu da tersine çeviriyoruz, çünkü şimdiye kadarki en uygun yollar,
eklemeyi tercih etmemizden dolayı tersine çevrildi.

Bunu test edelim!

~~~~ {.haskell: .ghci name="code"}
ghci> optimalPath heathrowToLondon  
[(B,10),(C,30),(A,5),(C,20),(B,2),(B,8),(C,0)] 
~~~~

Bu bizim elde etmemiz gereken sonuç! Harika! Beklediğimiz sonuçtan biraz farklıdır çünkü sonunda bir adım `(C, 0)` vardır,
bu da Londra'ya vardığımızda diğer yola geçeceğimiz anlamına gelir, ancak bu geçişin hiçbir maliyeti yoktur, bu hala doğru sonuç.

Buna dayalı olarak en uygun yolu bulan bir fonksiyona sahibiz, şimdi sadece standart girdiden bir yol sisteminin metinsel bir temsilini okumalı,
onu bir tür `RoadSystem`'e dönüştürmeli, bunu `optimalPath` fonksiyonumuz aracılığıyla çalıştırmalı ve yolu yazdırmalıyız.

Öncelikle, bir listeyi alıp aynı büyüklükte gruplara ayıran bir fonksiyon yapalım. Biz buna `groupsOf` diyeceğiz.
`[1..10]` parametresi için, `groupsOf 3`, `[[1,2,3], [4,5,6], [7,8,9], [10]]` döndürmelidir.

~~~~ {.haskell: .ghci name="code"}
groupsOf :: Int -> [a] -> [[a]]  
groupsOf 0 _ = undefined  
groupsOf _ [] = []  
groupsOf n xs = take n xs : groupsOf n (drop n xs)  
~~~~

Standart bir özyinelemeli(recursive) fonksiyon. `[1..10]` `xs` ve `3` `n` için bu, `[1,2,3] : groupOf 3 [4,5,6,7,8,9,10]`'a eşittir.
Özyineleme yapıldığında, listemizi üçlü gruplar halinde alıyoruz. Ve işte standart girdiden okuyan, ondan bir `RoadSystem` yapan ve en kısa yolu yazdıran `main` fonksiyonumuz:

~~~~ {.haskell: .ghci name="code"}
import Data.List  
  
main = do  
    contents <- getContents  
    let threes = groupsOf 3 (map read $ lines contents)  
        roadSystem = map (\[a,b,c] -> Section a b c) threes  
        path = optimalPath roadSystem  
        pathString = concat $ map (show . fst) path  
        pathPrice = sum $ map snd path  
    putStrLn $ "The best path to take is: " ++ pathString  
    putStrLn $ "The price is: " ++ show pathPrice  
~~~~

İlk olarak, tüm içeriği standart girişten alıyoruz. Daha sonra, `"50\n10\n30\n...` gibi bir şeyi `["50","10","30"..`'a dönüştürmek için içeriğimizle `lines` diyoruz ve
sonra `read` kullanarak bir sayı listesine dönüştürmek için eşleştiriyoruz. Bunun üzerine `groupsOf 3` diyoruz, böylece onu `3` uzunluğundaki listelerin bir listesine dönüştürüyoruz. Lambda'yı `(\[a,b,c] -> Section a b c)` bu liste listesi üzerinde eşliyoruz. Gördüğünüz gibi, lambda sadece 3 uzunluğunda bir liste alıyor ve onu bir bölüme dönüştürüyor. Yani `roadSystem` artık bizim yol sistemimiz ve hatta doğru türe sahip, yani `RoadSystem` (veya `[Section]`). Bununla `optimalPath` diyoruz ve sonra yolu ve fiyatı
güzel bir metinsel sunumda alıp yazdırıyoruz.

Aşağıdaki metni kaydediyoruz

~~~~ {.haskell: .ghci name="code"}
50  
10  
30  
5  
90  
20  
40  
2  
25  
10  
8  
0
~~~~

`paths.txt` adlı bir dosyaya ekleyin ve ardından programımıza gönderiyoruz.

~~~~ {.haskell: .ghci name="code"}
$ cat paths.txt | runhaskell heathrow.hs  
The best path to take is: BCACBBC  
The price is: 75  
~~~~

Tıkır tıkır çalışıyor! `Data.Random` modülüne ilişkin bilginizi, daha sonra yazdıklarımızla besleyebileceğiniz çok daha uzun bir yol sistemi oluşturmak için kullanabilirsiniz.
Yığın taşması ile karşılaşırsanız, `foldl` yerine `foldl'` kullanmayı deneyin, çünkü `foldl'` katıdır(strict).

