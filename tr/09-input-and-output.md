Girdi ve Çıktı
==============

![dognap](../img/dognap.png)
Haskell'in saf olan işlevsel dilleri (purely functional languages) olduğundan bahsetmiştik.
Zorunlu(imperative) illerde, genellikle bilgisayara yürütmesi için bir dizi adım vererek işleri hallettiğiniz halde,
fonksiyonel programlama (functional programming) daha çok şeyin ne olduğunu tanımlamaktır. Haskell'da bir fonksiyon, bir değişkenin içeriğini değiştirmek gibi
bazı durumu değiştiremez (bir fonksiyon durumu değiştirdiğinde, fonksiyonun yan etkileri(side-effect) olduğunu söyleriz).
Haskell'de bir fonksiyonun yapabileceği tek şey, ona verdiğimiz parametrelere göre bize bazı sonuçlar vermesidir. Bir fonksiyon aynı parametrelerle iki kez çağrılırsa,
aynı sonucu döndürmesi gerekir. Zorunlu bir dünyadan geldiğinizde bu biraz sınırlayıcı görünse de, aslında gerçekten harika olduğunu gördük.
Zorunlu bir dilde, sadece bazı sayıları sıkıştırması gereken basit bir fonksiyonun evinizi yakmayacağına,
köpeğinizi kaçırmayacağına ve bu sayıları sıkıştırırken bir patatesle arabanızı çizmeyeceğine dair hiçbir garantiniz yok.
Örneğin, bir binary search tree yaparken, bir ağacı yerinde değiştirerek bir ağaca bir öğe eklemedik. 
Binary search tree'sine ekleme fonksiyonumuz aslında yeni bir ağaç döndürdü, çünkü eskisini değiştiremez.

Fonksiyonların durumu değiştirememesi, programlarımız hakkında akıl yürütmemize yardımcı olduğu için iyi olsa da, bununla ilgili bir sorun var.
Bir fonksiyon dünyadaki hiçbir şeyi değiştiremezse, neyi hesapladığını bize nasıl söyler? Bize neyi hesapladığını söylemek için,
bir çıkış cihazının durumunu (genellikle ekranın durumunu) değiştirmesi gerekiyor,
bu da daha sonra beynimize giden ve zihnimizin durumunu değiştiren fotonları yayıyor, adamım.

Umutsuzluğa kapılma, her şey kaybolmaz. Haskell'in, programımızın saf(pure) olan kısmını ve programımızın saf olmayan (unpure) kısmını düzgün bir şekilde ayıran yan etkileri olan,
klavye ve ekranla konuşmak gibi tüm kirli işleri yapan fonksiyonlarla başa çıkmak için gerçekten akıllı bir sisteme sahip olduğu ortaya çıktı.
Bu iki parçayı ayırdığımızda, pure programımız hakkında akıl yürütmeye devam edebilir ve dış dünya ile verimli bir şekilde iletişim kurarken tembellik(laziness),
sağlamlık(robustness) ve modülerlik(modularity) gibi saflığın(purity) sunduğu tüm şeylerden yararlanabiliriz.


Hello, world!
-------------

![helloworld](../img/helloworld.png)
Şimdiye kadar, onları test etmek ve onlarla oynamak için fonksiyonlarımızı her zaman GHCI'ye yükledik.
Standart kütüphane fonksiyonlarını da bu şekilde inceledik. Ama şimdi, sekiz veya daha fazla bölümden sonra, nihayet ilk gerçek Haskell programımızı yazacağız!
Yaşasın! Ve tabii ki, eski güzel `"hello, world"` şakasını yapacağız.

**Hey!** Bu bölümün amaçları doğrultusunda, Haskell'i öğrenmek için bir unix-y ortamı kullandığınızı varsayacağım.
Windows kullanıyorsanız, Windows için Linux benzeri bir ortam olan [Cygwin'i](http://www.cygwin.com/) indirmenizi öneririm, A.K.A. tam ihtiyacınız olan şey.

Bu nedenle, başlangıç olarak, en sevdiğiniz metin düzenleyicide aşağıdakileri yapın:

~~~~ {.haskell: .ghci name="code"}
main = putStrLn "hello, world"  
~~~~

Sadece `main` adında bir isim tanımladık ve bunun içinde `putStrLn` adında `"hello, world"` parametresiyle bir fonksiyon çağırıyoruz.
Hemen hemen değirmen gibi görünüyor, ancak sadece birkaç dakika içinde göreceğimiz gibi değil. Bu dosyayı `helloworld.hs` olarak kaydedin.

Ve şimdi, daha önce hiç yapmadığımız bir şeyi yapacağız. Aslında programımızı derleyeceğiz! Çok heyecanlıyım!
Terminalinizi açın ve `helloworld.hs`'nin bulunduğu dizine gidin ve aşağıdakileri yapın:

~~~~ {.haskell: .ghci name="code"}
$ ghc --make helloworld  
[1 of 1] Compiling Main             ( helloworld.hs, helloworld.o )  
Linking helloworld ...  
~~~~

Tamam! Şansınız varsa, böyle bir şey elde edersiniz ve şimdi programınızı `./helloworld` yaparak çalıştırabilirsiniz.

~~~~ {.haskell: .ghci name="code"}
$ ./helloworld  
hello, world 
~~~~

Ve işte, terminale bir şeyler yazdıran ilk derlenmiş programımız. Ne kadar olağanüstü sıkıcı!

Ne yazdığımızı inceleyelim. Öncelikle `putStrLn` fonksiyonunun türüne bakalım.

~~~~ {.haskell: .ghci name="code"}
ghci> :t putStrLn  
putStrLn :: String -> IO ()  
ghci> :t putStrLn "hello, world"  
putStrLn "hello, world" :: IO ()  
~~~~

`putStrLn` türünü şu şekilde okuyabiliriz: `putStrLn` bir string alır ve sonuç türü `()` olan bir **I/O eylemi** döndürür (yani boş demet, birim olarak da bilinir).
Bir I/O eylemi, gerçekleştirildiğinde, bir yan etkiye sahip bir eylem gerçekleştirecek
(genellikle ya girdiden okuma ya da ekrana bir şeyler yazdırmadır) ve ayrıca içinde bir tür dönüş değeri içerecek bir şeydir.
Terminal'e bir string yazdırmak gerçekten herhangi bir anlamlı dönüş değerine sahip değildir, bu nedenle `()` kukla(dummy) değeri kullanılır.

Boş demet, bir `()` değeridir ve ayrıca bir `()` türüne sahiptir.

Peki, bir I/O eylemi ne zaman gerçekleştirilecek? İşte bu noktada `main` devreye giriyor.
Ona `main` adını verdiğimizde ve sonra programımızı çalıştırdığımızda bir I/O eylemi gerçekleştirilecek.

Tüm programınızın yalnızca bir I/O eylemi olması sınırlayıcı gibi görünüyor.
Bu nedenle, birkaç I/O eylemini tek bir işlemde birleştirmek için do sözdizimini kullanabiliriz. Aşağıdaki örneğe bir göz atın:

~~~~ {.haskell: .ghci name="code"}
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")  
~~~~

Ah, ilginç, yeni sözdizimi! Ve bu hemen hemen zorunlu bir program gibi okur. Derler ve denerseniz, muhtemelen beklediğiniz gibi davranacaktır.
do dediğimize ve ardından zorunlu bir programda yapacağımız gibi bir dizi adım belirlediğimize dikkat edin.
Bu adımların her biri bir I/O eylemidir. Onları do sözdizimi ile bir araya getirerek, onları tek bir I/O eylemine yapıştırdık.
Aldığımız eylemin bir tür `IO ()` içeriyor, çünkü bu, içerideki son I/O eyleminin türüdür.

Bundan dolayı, `main` her zaman `main :: IO something` şeklinde bir tür imzasına sahiptir, burada `something` somut bir türdür.
Geleneksel olarak, `main` için genellikle bir tür bildirimini belirtmiyoruz.

Daha önce karşılaşmadığımız ilginç bir şey de `name <- getLine` yazan üçüncü satırdır.
Görünüşe göre girişten bir satır okuyor ve bunu `name` adlı bir değişkene kaydediyor. Gerçekten öyle mi? Pekala, `getLine` türünü inceleyelim.

~~~~ {.haskell: .ghci name="code"}
ghci> :t getLine  
getLine :: IO String  
~~~~

![luggage](../img/luggage.png)
Aha tamam. `getLine`, `String` türünün bir sonuç türünü içeren bir I/O eylemidir. 
Bu mantıklı, çünkü kullanıcının terminal'a bir şey girmesini bekleyecek ve sonra bir şey bir string olarak temsil edilecek. `name <- getLine` adından ne haber?
Bu kod parçasını şu şekilde okuyabilirsiniz: **I/O eylemini `getLine` gerçekleştirin ve ardından sonuç değerini `name`'e bağlayın**.
`getLine`'ın bir tür `IO String`'i vardır, bu yüzden `name` bir `String` türüne sahip olacaktır.
Bir I/O eylemini, gerçek dünyaya açılan ve orada bir şeyler yapan (bir duvara grafiti yazmak gibi) 
ve belki bazı verileri geri getiren küçük ayaklı bir kutu olarak düşünebilirsiniz.
Verileri sizin için getirdikten sonra, kutuyu açmanın ve içindeki verileri almanın tek yolu `<-` yapısını kullanmaktır.
Ve bir I/O eyleminden veri alıyorsak, onu yalnızca başka bir I/O eyleminin içindeyken çıkarabiliriz.
Haskell, kodumuzun saf ve saf olmayan kısımlarını düzgün bir şekilde ayırmayı bu şekilde başarır.
`getLine` bir anlamda saf değildir, çünkü iki kez yapıldığında sonuç değerinin aynı olması garanti edilmez.
Bu yüzden `IO` type constructor ile bir nevi kusurludur ve bu verileri sadece I/O kodunda alabiliriz.
Ve I/O kodu da bozuk olduğu için, bozuk I/O verilerine bağlı olan herhangi bir hesaplama, bozuk bir sonuca sahip olacaktır.

Ben kusurlu dediğimde, bir I/O eyleminin içerdiği sonucu bir daha asla saf kodda kullanamayacağımız şekilde kusurlu demek istemiyorum.
Hayır, bir ada bağladığımızda bir I/O eylemi içindeki verileri geçici olarak kaldırırız.
`name <- getLine` yaptığımızda, `name` sadece normal bir string'tir çünkü kutunun içindekini temsil eder.
Diyelim ki isminizi (normal bir string) parametre olarak alan ve adınıza göre servetinizi ve tüm hayatınızın geleceğini
söyleyen gerçekten karmaşık bir fonksiyona sahip olabiliriz. Bunu yapabiliriz:

~~~~ {.haskell: .ghci name="code"}
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn $ "Read this carefully, because this is your future: " ++ tellFortune name   
~~~~

 ve `tellFortune`'un (veya `name`'e ilettiği fonksiyonlardan herhangi biri) I/O hakkında hiçbir şey bilmesine gerek yoktur,
 bu sadece normal bir `String -> String` fonksiyonu!
 
 Bu kod parçasına bir göz atın. Geçerli mi?

~~~~ {.haskell: .ghci name="code"}
nameTag = "Hello, my name is " ++ getLine   
~~~~

Hayır dediyseniz, gidip bir kurabiye yiyin. Evet dediyseniz, bir kase erimiş lav içiniz.
Şaka yapıyorum, yapma! Bunun işe yaramamasının nedeni, `++`'nın her iki parametresinin de aynı tür üzerinde liste olmasını gerektirmesidir.
Soldaki parametrenin bir `String` türü vardır (veya isterseniz `[Char]`), `getLine` ise bir `IO String` türüne sahiptir.
Bir `String`'i ve bir I/O eylemini birleştiremezsiniz. String türünden bir değer elde etmek için önce I/O eyleminden sonucu almalıyız ve
bunu yapmanın tek yolu başka bir I/O eylemi içinde `name <- getLine` gibi bir şey söylemektir.
Saf olmayan verilerle uğraşmak istiyorsak, bunu saf olmayan bir ortamda yapmalıyız.
Dolayısıyla, saf olmama kusuru ölümsüz bela gibi etrafa yayılır ve kodumuzun I/O bölümlerini olabildiğince küçük tutmak bizim yararımıza.

Gerçekleştirilen her I/O eyleminin içinde kapsüllenmiş(encapsulated) bir sonucu vardır.
Bu yüzden önceki örnek programımız da şöyle yazılabilirdi:

~~~~ {.haskell: .ghci name="code"}
main = do  
    foo <- putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")  
~~~~

Bununla birlikte, `foo` sadece `()` değerine sahip olacaktır, bu nedenle bunu yapmak biraz tartışmalı olacaktır. Son `putStrLn`'i hiçbir şeye bağlamadığımıza dikkat edin.
Bunun nedeni, bir *do* bloğunda, **son eylemin ilk ikisi gibi bir isme bağlanamamasıdır**. 
Monad'ların dünyasına girdiğimizde bunun neden bu kadar sonra olduğunu tam olarak göreceğiz. 
Şimdilik, bunu do bloğunun son eylemden değeri otomatik olarak çıkarması ve kendi sonucuna bağlaması şeklinde düşünebilirsiniz.

Son satır dışında, bir do bloğundaki bağlanmayan her satır da bir bind ile yazılabilir.
Yani `putStrLn "BLAH"`, `_ <- putStrLn "BLAH"` olarak yazılabilir. Ancak bu işe yaramaz, bu yüzden `putStrLn something` gibi
önemli bir sonuç içermeyen I/O eylemleri için `<-`'yi dışarıda bırakırız.

Yeni başlayanlar bazen bunu yaptığını düşünür

~~~~ {.haskell: .ghci name="code"}
name = getLine  
~~~~

Girdiden okuyacak ve ardından bunun değerini `name`'e bağlayacaktır. Pekala, olmayacak, tüm burada yapılanlar `getLine` I/O eylemine `name` denen farklı bir ad vermektir.
Unutmayın, bir I/O eyleminden değer elde etmek için, bunu başka bir I/O eylemi içinde `<-` ile bir ada bağlayarak gerçekleştirmeniz gerekir.

I/O eylemleri, yalnızca `main` adı verildiğinde veya *do* bloğu ile oluşturduğumuz daha büyük bir I/O eyleminin içindeyken gerçekleştirilir.
Birkaç I/O eylemini birbirine yapıştırmak için bir *do* bloğu da kullanabiliriz ve sonra bu I/O eylemini başka bir *do* bloğunda vb. kullanabiliriz.
Her iki durumda da, yalnızca sonunda `main` duruma düşerlerse gerçekleştirilecekler.

Oh, doğru, ayrıca I/O eylemlerinin gerçekleştirileceği bir durum daha var. GHCI'da bir I/O eylemi yazdığımızda ve geri dönüş tuşuna bastığımızda, bu gerçekleştirilecektir.

~~~~ {.haskell: .ghci name="code"}
ghci> putStrLn "HEEY"  
HEEY  
~~~~

GHCI'da bir numarayı yumrukladığımızda veya bir fonksiyonu çağırıp return tuşuna bastığımızda bile,
onu değerlendirecek (ihtiyaç duyduğu kadarıyla) ve ardından `show`'u çağıracak ve sonra bu string'i `putStrLn` kullanarak dolaylı olarak terminale yazdıracaktır.

*let* bağlarını hatırlıyor musun? Eğer hatırlamıyorsanız, bu [bölümü](../tr/04-syntax-in-functions.md#let-it-be) okuyarak hafızanızı tazeleyin.
İfade `let bindings in expression` şeklinde olmalılar, burada `bindings` ifadelere verilecek isimler ve `expression` onlara göre değerlendirilecek ifadedir.
Ayrıca liste anlamalarında kısmen gerekli olmadığını söyledik. Bunları, liste anlamalarında kullandığınız gibi, do bloklarında da kullanabilirsiniz. Şuna bir bak:

~~~~ {.haskell: .ghci name="code"}
main = putStrLn "hello, world"  
~~~~

~~~~ {.haskell: .ghci name="code"}
main = putStrLn "hello, world"  
~~~~

~~~~ {.haskell: .ghci name="code"}
main = putStrLn "hello, world"  
~~~~

~~~~ {.haskell: .ghci name="code"}
main = putStrLn "hello, world"  
~~~~







































































































































































































































































