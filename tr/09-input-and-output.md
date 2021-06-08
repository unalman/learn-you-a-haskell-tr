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
Bu nedenle, birkaç I/O eylemini tek bir işlemde birleştirmek için *do* sözdizimini kullanabiliriz. Aşağıdaki örneğe bir göz atın:

~~~~ {.haskell: .ghci name="code"}
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")  
~~~~

Ah, ilginç, yeni sözdizimi! Ve bu hemen hemen zorunlu bir program gibi okur. Derler ve denerseniz, muhtemelen beklediğiniz gibi davranacaktır.
*do* dediğimize ve ardından zorunlu bir programda yapacağımız gibi bir dizi adım belirlediğimize dikkat edin.
Bu adımların her biri bir I/O eylemidir. Onları *do* sözdizimi ile bir araya getirerek, onları tek bir I/O eylemine yapıştırdık.
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
import Data.Char  
  
main = do  
    putStrLn "What's your first name?"  
    firstName <- getLine  
    putStrLn "What's your last name?"  
    lastName <- getLine  
    let bigFirstName = map toUpper firstName  
        bigLastName = map toUpper lastName  
    putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"  
~~~~

*do* bloğundaki I/O eylemlerinin nasıl sıralandığını görüyor musunuz? Ayrıca *let*'in I/O eylemleriyle nasıl sıralandığına ve
*let*'in isimlerinin birbiriyle sıralandığına dikkat edin. Bu iyi bir uygulamadır çünkü Haskell'de girinti önemlidir.
Şimdi, `"John"` gibi bir şeyi `"JOHN"` gibi çok daha havalı bir string'e dönüştüren `map toUpper firstName` yaptık.
Bu büyük harfli string'i bir isme bağladık ve daha sonra terminale yazdırdığımız bir string'te kullandık.

Ne zaman `<-` kullanacağınızı ve *let* bağlamalarını ne zaman kullanacağınızı merak ediyor olabilirsiniz.
Unutmayın, `<-` (şimdilik) I/O eylemlerini gerçekleştirmek ve sonuçlarını adlara bağlamak içindir.
Ancak `map toUpper firstName` bir I/O eylemi değildir. Haskell'de saf bir ifadedir.
Dolayısıyla, I/O eylemlerinin sonuçlarını adlara bağlamak istediğinizde `<-` kullanın ve saf ifadeleri adlara bağlamak için *let* bağlamaları kullanabilirsiniz.
`let firstName = getLine` gibi bir şey yapsaydık, `getLine` I/O eylemini farklı bir ad olarak adlandırırdık ve
bunu gerçekleştirmek için yine de `<-` üzerinden çalıştırmamız gerekirdi.

Şimdi bir satırı sürekli okuyan ve aynı satırı ters çevrilmiş sözcüklerle yazdıran bir program yapacağız.
Boş bir satır girdiğimizde programın çalışması duracaktır. Program şudur:

~~~~ {.haskell: .ghci name="code"}
main = do   
    line <- getLine  
    if null line  
        then return ()  
        else do  
            putStrLn $ reverseWords line  
            main  
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words   
~~~~

Ne yaptığına dair bir fikir edinmek için, kodu gözden geçirmeden önce çalıştırabilirsiniz.

**Protip:** Bir programı çalıştırmak için ya onu derleyebilir `ghc --make helloworld` ve sonra üretilen çalıştırılabilir dosyayı `./helloworld` yazarak çalıştırabilirsiniz
veya şu şekilde `runhaskell` komutunu kullanabilirsiniz: `runhaskell helloworld.hs` ve program anında yürütülecektir.

İlk olarak, `reverseWords` fonksiyonuna bir göz atalım. Bu sadece `"hey there man"` gibi bir string alan ve ardından `["hey","there","man"]` gibi
bir kelime listesi oluşturmak için onunla `words` çağıran normal bir fonksiyondur. Sonra listedeki `reverse`'i eşleriz, `["yeh", "ereht", "nam"]` alırız ve 
sonra bunu `unwords` kullanarak tek bir string'e koyarız ve sonuç `"yeh ereht nam"` olur. Fonksiyon bileşimini nasıl kullandığımıza buradan bakın.
Fonksiyon bileşimi olmadan, `reverseWords st = unwords (map reverse (words st))` gibi bir şey yazmamız gerekir.

Peki ya `main`? Önce `getLine` bu satırı `line` olarak çağırarak terminalden bir hat alıyoruz. Ve şimdi, şartlı bir ifademiz var.
Haskell'de her if'in bir karşılığı olması gerektiğini, çünkü her ifadenin bir çeşit değeri olması gerektiğini unutmayın.
Bir koşul doğru olduğunda (bizim durumumuzda, girdiğimiz satır boştur), bir I/O eylemi gerçekleştiririz ve bu olmadığında,
diğerinin altındaki I/O eylemi gerçekleştirilir. Bu nedenle, bir I/O *do* bloğunda, `if condition then I/O action else I/O action` biçimine sahip olması gerekir.

Öncelikle else maddesinde ne olduğuna bir bakalım. Çünkü, else'ten sonra tam olarak bir I/O eylemine sahip olmamız gerekiyor,
iki I/O eylemini tek bir eylemde birleştirmek için bir *do* bloğu kullanıyoruz. Bu bölümü şu şekilde de yazabilirsiniz:

~~~~ {.haskell: .ghci name="code"}
else (do  
    putStrLn $ reverseWords line  
    main)  
~~~~

Bu, *do* bloğunun bir I/O eylemi olarak görülebileceğini daha açık hale getirir, ancak daha çirkindir. Her neyse, *do* bloğunun içinde,
`getLine`'dan aldığımız satıra `reverseWords` diyoruz ve sonra bunu terminale yazdırıyoruz. Bundan sonra, sadece `main` gerçekleştiririz.
Yinelemeli olarak adlandırılır ve sorun değil, çünkü `main`'in kendisi bir I/O eylemidir. Yani bir anlamda programın başlangıcına geri dönüyoruz.

Şimdi `null line` true olduğunda ne olur? Bundan sonra olan şey bu durumda yapılır. Yukarı bakarsak, `then return ()` yazdığını göreceğiz.
C, Java veya Python gibi zorunlu diller kullandıysanız, muhtemelen bu `return`'ün ne yaptığını bildiğinizi düşünüyorsunuz
ve bu gerçekten uzun paragrafı zaten atlamış olabilirsiniz. İşte olay şu:
**Haskell'deki `return`, diğer dillerin çoğundaki return gibi bir şey değildir!** Birçok insanın kafasını karıştıran aynı adı taşıması, ancak gerçekte oldukça farklı.
Zorunlu dillerde, `return` genellikle bir metodun veya alt yordamın(subroutine) yürütülmesini sona erdirir ve onu çağıran kişiye bir çeşit değer bildirmesini sağlar.
Haskell'de (özellikle I/O eylemlerinde), saf(pure) bir değerden bir I/O işlemi yapar. Daha önceki kutu benzetmesini düşünürseniz, bir değer alır ve bir kutuya sarar.
Ortaya çıkan I/O eylemi aslında hiçbir şey yapmaz, yalnızca sonucu olarak bu değeri kapsüllenir.
Yani bir I/O bağlamında, `return "haha"` bir `IO String` türüne sahip olacaktır. Saf bir değeri hiçbir şey yapmayan bir I/O eylemine dönüştürmenin amacı nedir?
Programımızı neden olması gerektiğinden daha fazla `IO` ile kirletelim? Boş bir giriş satırı olması durumunda gerçekleştirecek bazı I/O eylemlerine ihtiyacımız vardı.
Bu yüzden, `return ()` yazarak hiçbir şey yapmayan sahte bir I/O eylemi yaptık.

`return` kullanmak, I/O *do* bloğunun yürütmede sona ermesine veya buna benzer bir şeye neden olmaz.
Örneğin, bu program son satıra kadar oldukça mutlu bir şekilde devam edecektir:

~~~~ {.haskell: .ghci name="code"}
main = do  
    return ()  
    return "HAHAHA"  
    line <- getLine  
    return "BLAH BLAH BLAH"  
    return 4  
    putStrLn line  
~~~~

Tüm bu `return`'lerin yaptığı, kapsüllenmiş bir sonuç dışında gerçekten hiçbir şey yapmayan I/O eylemleri yapmaları ve bu sonucun bir isme bağlı olmadığı için atılmasıdır.
Bir şeyleri isimlere bağlamak için `return` ile `<-` kombinasyonunu kullanabiliriz.

~~~~ {.haskell: .ghci name="code"}
main = do  
    a <- return "hell"  
    b <- return "yeah!"  
    putStrLn $ a ++ " " ++ b 
~~~~

Gördüğünüz gibi, `return` `<-`'nin tam tersi. `return` bir değer alır ve onu bir kutuya sararken, <- bir kutu alır (ve gerçekleştirir) ve değeri ondan çıkararak bir isme bağlar.
Ancak bunu yapmak biraz fazlalıktır, özellikle de *do* bloklarındaki let bağlamalarını adlara bağlanmak için kullanabildiğiniz için:

~~~~ {.haskell: .ghci name="code"}
main = do  
    let a = "hell"  
        b = "yeah"  
    putStrLn $ a ++ " " ++ b  
~~~~

I/O *do* blokları ile uğraşırken, genellikle ya hiçbir şey yapmayan bir I/O eylemi oluşturmamız gerektiğinden ya da
*do* bloğundan oluşan I/O eylemini istemediğimiz için `return` kullanırız. Son eyleminin sonuç değerine sahip, ancak farklı bir sonuç değerine sahip olmasını istiyoruz,
bu nedenle her zaman istediğimiz sonucu içeren bir I/O eylemi yapmak için `return` kullanıyoruz ve bunu sonuna koyuyoruz.

Bir *do* bloğunun yalnızca bir I/O eylemi de olabilir. Bu durumda, I/O eylemini yazmakla aynı şeydir.
Bazı insanlar bu durumda `then do return ()` yazmayı tercih ederler çünkü diğerinin de bir anlamı vardır.

Dosyalara geçmeden önce, I/O ile uğraşırken yararlı olan bazı fonksiyonlara bir göz atalım.

`putStr`, parametre olarak bir string alan ve bu string'i terminale yazdıracak bir I/O eylemi döndürdüğü için `putStrLn`'e çok benzer;
`putStrLn` ise string'i yazdırdıktan sonra yalnızca `putStr` yeni bir satıra atlamaz.

~~~~ {.haskell: .ghci name="code"}
main = do   putStr "Hey, "  
            putStr "I'm "  
            putStrLn "Andy!"   
~~~~

~~~~ {.haskell: .ghci name="code"}
$ runhaskell putstr_test.hs  
Hey, I'm Andy!  
~~~~

Tür imzası `putStr :: String -> IO ()` olduğundan, sonuçta ortaya çıkan I/O eylemi içinde kapsüllenen sonuç birimdir.
Bozuk bir değer, bu yüzden onu bağlamanın bir anlamı yok.

`putChar` bir karakter alır ve onu terminale yazdıracak bir I/O eylemi döndürür.

~~~~ {.haskell: .ghci name="code"}
main = do   putChar 't'  
            putChar 'e'  
            putChar 'h' 
~~~~

~~~~ {.haskell: .ghci name="code"}
$ runhaskell putchar_test.hs  
teh  
~~~~

`putStr` aslında `putChar` yardımıyla yinelemeli olarak tanımlanır. `putStr`'nin kenar koşulu boş string'tir, bu nedenle boş bir string yazdırıyorsak,
`return ()` kullanarak hiçbir şey yapmayan bir I/O işlemi döndürmeniz yeterlidir.
Boş değilse, `putChar` yaparak string'in ilk karakterini yazdırın ve ardından `putStr` kullanarak yazdırın.

~~~~ {.haskell: .ghci name="code"}
putStr :: String -> IO ()  
putStr [] = return ()  
putStr (x:xs) = do  
    putChar x  
    putStr xs  
~~~~

Tıpkı saf kodda kullanabildiğimiz gibi, özyinelemeyi I/O'da nasıl kullanabileceğimize bakın. Tıpkı saf kodda olduğu gibi,
uç durumu tanımlıyoruz ve sonra sonucun gerçekte ne olduğunu düşünüyoruz. İlk önce ilk karakteri ve ardından string'in geri kalanını çıkaran bir eylemdir.

`print`, `Show`'un instance'ı olan herhangi bir türden bir değer alır (bu, onu bir string olarak nasıl temsil edeceğimizi bildiğimiz anlamına gelir),
onu string'leştirmek için bu değerle `show`'u çağırır ve sonra bu string'i terminale çıkarır. Temel olarak, sadece `putStrLn . show`.
Önce bir değer üzerinde `show` çalıştırır ve sonra bunu `putStrLn`'ye besler, bu da değerimizi yazdıracak bir I/O eylemi döndürür.

~~~~ {.haskell: .ghci name="code"}
main = do   print True  
            print 2  
            print "haha"  
            print 3.2  
            print [3,4,3]  
~~~~

~~~~ {.haskell: .ghci name="code"}
$ runhaskell print_test.hs  
True  
2  
"haha"  
3.2  
[3,4,3] 
~~~~

Gördüğünüz gibi çok kullanışlı bir fonksiyon. I/O eylemlerinin yalnızca `main` duruma düştüklerinde veya
GHCI isteminde onları değerlendirmeye çalıştığımızda nasıl gerçekleştirildiğinden nasıl bahsettiğimizi hatırlıyor musunuz?
Bir değer yazdığımızda (`3` veya `[1,2,3]` gibi) ve dönüş tuşuna bastığımızda, GHCI aslında bu değeri terminalimizde görüntülemek için `print` kullanır!

~~~~ {.haskell: .ghci name="code"}
ghci> 3  
3  
ghci> print 3  
3  
ghci> map (++"!") ["hey","ho","woo"]  
["hey!","ho!","woo!"]  
ghci> print (map (++"!") ["hey","ho","woo"])  
["hey!","ho!","woo!"]  
~~~~

string'leri yazdırmak istediğimizde, genellikle `putStrLn` kullanırız çünkü bunların etrafındaki tırnakları istemeyiz,
ancak diğer türler değerlerini terminale yazdırmak için en çok `print` kullanılır.

`getChar`, girişten(input) bir karakter okuyan bir I/O eylemidir. Bu nedenle, tür imzası `getChar :: IO Char`'dır, çünkü I/O eyleminin içerdiği sonuç bir `Char`'dır.
Arabelleğe(buffering) alma nedeniyle, kullanıcı dönüş anahtarını karıştırana kadar karakterlerin okunmasının gerçekten gerçekleşmeyeceğini unutmayın.

~~~~ {.haskell: .ghci name="code"}
main = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main  
        else return ()  
~~~~

Bu program bir karakteri okumalı ve sonra boşluk olup olmadığını kontrol etmelidir. Öyleyse, yürütmeyi durdurun ve değilse,
terminale yazdırın ve ardından aynı şeyi baştan yapın. Şey, öyle, sadece beklediğiniz şekilde değil. Şuna bir bak:

~~~~ {.haskell: .ghci name="code"}
$ runhaskell getchar_test.hs  
hello sir  
hello  
~~~~

İkinci satır giriştir. `hello sir` yazıyoruz ve ardından dönüş tuşuna basıyoruz. Arabelleğe alma nedeniyle, programın çalıştırılması, girilen her karakterden sonra değil,
return tuşuna bastığımızda başlayacaktır. Ama bir kez geri dönüş tuşuna bastığımızda, şimdiye kadar koyduğumuz şeye göre hareket eder.
Bir fikir edinmek için bu programla oynamayı deneyin!

`Control.Monad`'da `when` fonksiyonu bulunur (ona erişmek için `import Control.Monad` yapın).
Bu ilginç, çünkü *do* bloğunda bir kontrol akış ifadesi gibi görünüyor, ama aslında normal bir fonksiyon.
Bir boole değeri ve bu boole değeri `True` ise bir I/O eylemi alır, sağladığımız I/O eyleminin aynısını döndürür.
Ancak, `False` ise, `return ()` eylemini, yani hiçbir şey yapmayan bir I/O eylemini döndürür.
`getChar`'ı `when`'i kullanarak gösterdiğimiz önceki kod parçasını nasıl yeniden yazabileceğimiz aşağıda açıklanmıştır:

~~~~ {.haskell: .ghci name="code"}
import Control.Monad   
  
main = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main  
~~~~

Gördüğünüz gibi, `if something then do some I/O action else return ()` modelini kapsüllemek için yararlıdır.

`sequence`, I/O eylemlerinin bir listesini alır ve bu eylemleri birbiri ardına gerçekleştirecek bir I/O eylemi döndürür.
Bu I/O eyleminin içerdiği sonuç, gerçekleştirilen tüm I/O eylemlerinin sonuçlarının bir listesi olacaktır.
Tür imzası `sequence :: [IO a] -> IO [a]` şeklindedir. Bunu yapmak:

~~~~ {.haskell: .ghci name="code"}
main = do  
    a <- getLine  
    b <- getLine  
    c <- getLine  
    print [a,b,c]  
~~~~

Bunu yapmakla tamamen aynı:

~~~~ {.haskell: .ghci name="code"}
main = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs  
~~~~

Dolayısıyla `sequence [getLine, getLine, getLine]` `getLine`'ı üç kez gerçekleştirecek bir I/O eylemi yapar.
Bu eylemi bir isme bağlarsak, sonuç tüm sonuçların bir listesidir, yani bizim durumumuzda, kullanıcının istemde girdiği üç şeyin bir listesi.

`sequence` ile ortak bir model, `print` veya `putStrLn` gibi fonksiyonları listeler üzerine eşlediğimiz zamandır.
`map print [1,2,3,4]` yapmak bir I/O eylemi oluşturmaz. I/O eylemlerinin bir listesini oluşturacaktır, çünkü bu `[print 1, print 2, print 3, print 4]` yazmak gibidir.
Bu I/O eylemleri listesini bir I/O eylemine dönüştürmek istiyorsak, onu sıralamamız gerekir.

~~~~ {.haskell: .ghci name="code"}
ghci> sequence (map print [1,2,3,4,5])  
1  
2  
3  
4  
5  
[(),(),(),(),()]  
~~~~

Sonunda `[(), (), (), (), ()]` ile ne var? GHCI'de bir I/O eylemini değerlendirdiğimizde, gerçekleştirilir ve ardından sonucu yazdırılır, bu sonuç `()` değilse,
bu durumda yazdırılmaz. Bu nedenle GHCI'de `putStrLn "hehe"`'yi değerlendirmek sadece `hehe`'yi yazdırır (çünkü `putStrLn "hehe"` `()`'dir).
Ancak GHCI'de `getLine` yaptığımızda, bu I/O eyleminin sonucu yazdırılır, çünkü `getLine`'ın bir tür `IO String`'i vardır.

Bir liste üzerinde bir I/O eylemi döndüren bir fonksiyonun map'lenmesi ve ardından sıralanması çok yaygın olduğu için,
`mapM` ve `mapM_` yardımcı program fonksiyonları tanıtılmıştır. `mapM` bir fonksiyonu ve bir listeyi alır, fonksiyonu listenin üzerine map'ler ve ardından sıralar.
`mapM_` aynı şeyi yapar, ancak sonucu daha sonra atar. Sıralı I/O eylemlerimizin ne gibi bir sonuca sahip olduğunu umursamadığımızda genellikle `mapM_` kullanırız.

~~~~ {.haskell: .ghci name="code"}
ghci> mapM print [1,2,3]  
1  
2  
3  
[(),(),()]  
ghci> mapM_ print [1,2,3]  
1  
2  
3  
~~~~

`forever` bir I/O eylemi alır ve aldığı I/O eylemini sonsuza kadar tekrarlayan bir I/O eylemi döndürür. `Control.Monad`'da bulunur.
Bu küçük program, süresiz olarak kullanıcıdan bir miktar girdi isteyecek ve onu ona geri gönderecek, CAPSLOCKED:

~~~~ {.haskell: .ghci name="code"}
import Control.Monad  
import Data.Char  
  
main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l
~~~~

`forM` (`Control.Monad`'da bulunur) `mapM` gibidir, sadece parametrelerinin değiştirilmiş olmasıdır. İlk parametre listedir ve ikincisi,
daha sonra sıralanan bu liste üzerinde eşleme yapan fonksiyondur. Bu neden faydalıdır? Lambda ve notasyonların bazı yaratıcı kullanımıyla,
bunun gibi şeyler yapabiliriz:

~~~~ {.haskell: .ghci name="code"}
import Control.Monad  
  
main = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors  
~~~~

`(\a -> do ... )`, bir sayı alan ve bir I/O eylemi döndüren bir fonksiyondur. Onu parantez içine almalıyız,
aksi takdirde lambda son iki I/O eyleminin kendisine ait olduğunu düşünür. İç *do* bloğunda return color yaptığımıza dikkat edin.
Bunu, *do* bloğunun tanımladığı I/O eyleminin, içindeki rengimizin sonucunu alması için yapıyoruz.
Aslında bunu yapmak zorunda değildik, çünkü getLine zaten bunu içeriyor. `color <- getLine` ve ardından `return color` yapmak, sonucu `getLine` paketinden
çıkarmak ve sonra yeniden paketlemek, yani `getLine` yapmakla aynı şeydir. `ForM` (iki parametresiyle çağrılır) sonucu `colors` bağladığımız bir I/O eylemi üretir.
`colors` string'leri tutan normal bir listedir. Sonunda `mapM putStrLn colors` yaparak tüm bu `colors`'ı yazdırıyoruz.

`forM`'yi anlam olarak düşünebilirsiniz: bu listedeki her öğe için bir I/O eylemi yapın. Her bir I/O eyleminin ne yapacağı, eylemi yapmak için kullanılan öğeye bağlı olabilir.
Son olarak, bu eylemleri gerçekleştirin ve sonuçlarını bir şeye bağlayın. Bağlamak zorunda değiliz, sadece atabiliriz.

~~~~ {.haskell: .ghci name="code"}
$ runhaskell form_test.hs  
Which color do you associate with the number 1?  
white  
Which color do you associate with the number 2?  
blue  
Which color do you associate with the number 3?  
red  
Which color do you associate with the number 4?  
orange  
The colors that you associate with 1, 2, 3 and 4 are:  
white  
blue  
red  
orange  
~~~~

Aslında bunu `forM` olmadan da yapabilirdik, sadece `forM` ile daha okunabilir. Normalde, orada tanımladığımız bazı eylemleri *do*
gösterimini kullanarak eşlemek ve sıralamak istediğimizde `forM` yazarız. Aynı şekilde, son satırı `forM colors putStrLn` ile değiştirebilirdik.

Bu bölümde, girdi ve çıktının temellerini öğrendik. Ayrıca I/O eylemlerinin ne olduğunu, girdi ve çıktı yapmamızı nasıl sağladıklarını ve
gerçekte ne zaman gerçekleştirildiklerini de öğrendik. Tekrarlamak gerekirse, I/O eylemleri Haskell'deki diğer tüm değerler gibi değerlerdir.
Bunları fonksiyonlara parametre olarak aktarabiliriz ve fonksiyonlar sonuç olarak I/O eylemlerini döndürebilir.
Onları özel kılan şey, `main` fonksiyonuna girerlerse (veya bir GHCI satırıyla sonuçlanırlarsa), gerçekleştirilmeleridir.
İşte o zaman ekranınıza bir şeyler yazacaklar veya hoparlörleriniz aracılığıyla Yakety Sax'ı çalacaklar.
Her bir I/O eylemi, size gerçek dünyadan ne aldığını söyleyen bir sonucu da kapsayabilir.

`putStrLn` gibi bir fonksiyonu, bir string'i alıp ekrana yazdıran bir fonksiyon olarak düşünmeyin.
Bu I/O eylemi gerçekleştirildiğinde, terminalinize güzel şiirler basacaktır.


Dosyalar ve akışlar(streams)
----------------------------

![streams](../img/streams.png)
`getChar`, terminalden tek bir karakter okuyan bir I/O eylemidir. `getLine`, terminalden bir satırı okuyan bir I/O eylemidir.
Bu ikisi oldukça basittir ve çoğu programlama dilinin kendilerine paralel olan bazı fonksiyonları veya ifadeleri vardır.
Ama şimdi `getContents` ile tanışalım. `getContents`, standart girdiden bir dosya sonu karakteriyle karşılaşana kadar her şeyi okuyan bir I/O eylemidir.
Türü `getContents :: IO String` şeklindedir. `getContents` hakkında harika olan şey, tembel bir I/O yapmasıdır.
`foo <- getContents` yaptığımızda, tüm girdileri aynı anda okumaz, bellekte saklamaz ve sonra onu `foo`'ya bağlamaz. Hayır, tembel!
Şöyle der: "*Evet, evet, ilerledikçe, gerçekten ihtiyacınız olduğunda, terminalden girdiyi okuyacağım!*"
Bir programdan çıktıyı programımızın girdisine aktarırken(piping) getContents gerçekten kullanışlıdır.
Unix-y sistemlerde pipe'ların nasıl çalıştığını bilmiyorsanız, işte hızlı bir başlangıç.
Aşağıdaki küçük haiku içeren bir metin dosyası yapalım:

`getContents` gerçekten kullanışlıdır. Unix-y sistemlerde pipe'ların nasıl çalıştığını bilmiyorsanız, işte hızlı bir başlangıç.
Aşağıdaki küçük haiku içeren bir metin dosyası yapalım:

~~~~ {.haskell: .ghci name="code"}
I'm a lil' teapot  
What's with that airplane food, huh?  
It's so small, tasteless   
~~~~

Evet, haiku berbat, ne olacak? Herhangi bir iyi haiku öğreticisi bilen varsa, bana bildirin.

Şimdi, `forever` fonksiyonunu tanıtırken yazdığımız küçük programı hatırlayın. Kullanıcıdan bir satır istedi,
CAPSLOCK'ta ona geri verdi ve sonra bunu sonsuza kadar baştan yaptı. Sırf tüm yolu geri kaydırmanıza gerek kalmaması için, işte yine burada:

~~~~ {.haskell: .ghci name="code"}
import Control.Monad  
import Data.Char  
  
main = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l  
~~~~

Bu programı `capslocker.hs` veya başka bir şey olarak kaydedip derleyeceğiz. Ve sonra, metin dosyamızı doğrudan küçük programımıza beslemek için bir unix pipe'ı kullanacağız.
Kendisine argüman olarak verilen bir dosyayı yazdıran GNU cat programının yardımını kullanacağız. Şuna bak, booyaka!

~~~~ {.haskell: .ghci name="code"}
$ ghc --make capslocker   
[1 of 1] Compiling Main             ( capslocker.hs, capslocker.o )  
Linking capslocker ...  
$ cat haiku.txt  
I'm a lil' teapot  
What's with that airplane food, huh?  
It's so small, tasteless  
$ cat haiku.txt | ./capslocker  
I'M A LIL' TEAPOT  
WHAT'S WITH THAT AIRPLANE FOOD, HUH?  
IT'S SO SMALL, TASTELESS  
capslocker <stdin>: hGetLine: end of file  
~~~~

Gördüğünüz gibi, bir programın çıktısını (bizim durumumuzda cat olan) başka bir programın (*capslocker*) girişine pipe'lama `|` karakteri ile yapılır.
Yaptığımız şey, sadece capslocker çalıştırmaya, haiku'muzu terminale yazmaya ve ardından bir dosya sonu karakteri vermeye eşdeğerdir (bu genellikle Ctrl-D'ye basarak yapılır).
Bu, *cat haiku.txt* dosyasını çalıştırıp şunu söylemek gibidir: "Bekle, bunu terminale yazdırmayın, onun yerine *capslocker'a* söyleyin!".

Yani `forever` kullanımıyla esasen yaptığımız şey, girdiyi almak ve onu bir çıktıya dönüştürmektir.
Bu nedenle programımızı daha da kısaltmak ve daha iyi hale getirmek için `getContents`'i kullanabiliriz:

~~~~ {.haskell: .ghci name="code"}
import Data.Char  
  
main = do  
    contents <- getContents  
    putStr (map toUpper contents)  
~~~~

`getContents` I/O eylemini çalıştırıyoruz ve ürettiği string'i `contents` olarak adlandırıyoruz.
Daha sonra, bu string üzerinden `toUpper` ile map'lenir ve bunu terminale yazdırırız. string'ler temelde tembel olan listeler olduğundan ve `getContents` tembel olduğundan,
tüm içeriği bir kerede okumaya ve kapsüllü sürümü yazdırmadan önce belleğe depolamaya çalışmayacağını unutmayın.
Daha doğrusu, okuduğu gibi kapsüllü sürümü yazdıracaktır, çünkü gerçekten ihtiyaç duyduğunda yalnızca girdiden bir satır okuyacaktır.

~~~~ {.haskell: .ghci name="code"}
$ cat haiku.txt | ./capslocker  
I'M A LIL' TEAPOT  
WHAT'S WITH THAT AIRPLANE FOOD, HUH?  
IT'S SO SMALL, TASTELESS  
~~~~

Harika, işe yarıyor. Ya sadece capslocker'ı çalıştırırsak ve satırları kendimiz yazmaya çalışırsak?

~~~~ {.haskell: .ghci name="code"}
$ ./capslocker  
hey ho  
HEY HO  
lets go  
LETS GO   
~~~~

Ctrl-D'ye basarak bundan kurtulduk. Bayağı güzel! Gördüğünüz gibi, kapsüllü girdimizi satır satır bize geri yazdırıyor.
`getContents`'in sonucu `contents`'e bağlandığında, bellekte gerçek bir string olarak temsil edilmez, daha çok sonunda string'i üreteceği vaadine benzer.
`toUpper`'ı `contents` üzerine eşlediğimizde, bu aynı zamanda bu fonksiyonu nihai contents üzerinde eşleme vaadidir.
Ve son olarak `putStr` gerçekleştiğinde, önceki söze şöyle der: "*Hey, kapatılmış bir hatta ihtiyacım var!*".
Henüz herhangi bir satırı yok, bu yüzden `contents` diyor: "*Hey, terminalden gerçekten bir hat almaya ne dersin?*".
İşte o zaman getContents gerçekten terminalden okur ve koda somut bir şey üretmesini isteyen bir satır verir.
Bu kod daha sonra bu satır üzerinden `toUpper`'ı eşler ve onu, onu yazdıran `putStr`'a verir. Ve sonra `putStr` der ki:
"*Hey, bir sonraki satıra ihtiyacım var, hadi!*" ve bu, dosya sonu karakteri ile gösterilen başka girdi kalmayana kadar tekrar eder.

Biraz girdi alan ve sadece 10 karakterden kısa satırları yazdıran bir program yapalım. Gözlemleyin:

~~~~ {.haskell: .ghci name="code"}
main = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  
  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result  
~~~~

Programın I/O kısmını olabildiğince kısa hale getirdik. Programımızın bir girdi alması ve girdiye bağlı olarak bir çıktı çıktısı alması gerektiği için,
bunu girdi içeriklerini okuyarak, üzerlerinde bir fonksiyon çalıştırarak ve ardından fonksiyonun ne verdiğini yazdırarak uygulayabiliriz.

`shortLinesOnly` fonksiyonu şu şekilde çalışır: `"short\nlooooooooooooooong\nshort again"` gibi bir string alır.
Bu string'te ikisi kısa ve ortadaki uzun olmak üzere üç satır var. Bu string üzerinde `lines` fonksiyonunu çalıştırır ve onu
`["short", "looooooooooooooong", "short again"]` dönüştürür, sonra bunları `allLines` adına bağlarız.
Bu string listesi daha sonra filtrelenir, böylece listede yalnızca 10 karakterden kısa satırlar kalır ve `["short", "short again"]` üretir.
Ve son olarak, `unlines`, bu listeyi yeni satırla ayrılmış tek bir string'te birleştirir ve `"short\nshort again"` verir. Hadi bir deneyelim.

~~~~ {.haskell: .ghci name="code"}
i'm short  
so am i  
i am a loooooooooong line!!!  
yeah i'm long so what hahahaha!!!!!!  
short line  
loooooooooooooooooooooooooooong  
short  
~~~~

~~~~ {.haskell: .ghci name="code"}
$ ghc --make shortlinesonly  
[1 of 1] Compiling Main             ( shortlinesonly.hs, shortlinesonly.o )  
Linking shortlinesonly ...  
$ cat shortlines.txt | ./shortlinesonly  
i'm short  
so am i  
short  
~~~~

shortlines.txt içeriğini yalnızca shortlines çıktısına pipe'lıyoruz ve çıktı olarak yalnızca kısa satırları elde ediyoruz.

Girdiden bir string elde etme, onu bir fonksiyona dönüştürme ve sonra bunu çıktılama o kadar yaygındır ki bunu daha da kolaylaştıran,
`interact` adı verilen bir fonksiyon vardır. `interact`, parametre olarak `String -> String` türünde bir fonksiyonu alır ve bir girdi alacak,
üzerinde bu fonksiyonu çalıştıracak ve ardından fonksiyonun sonucunu yazdıracak bir I/O eylemi döndürür. Bunu kullanmak için programımızı değiştirelim.

~~~~ {.haskell: .ghci name="code"}
main = interact shortLinesOnly  
  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result  
~~~~

Sadece bunun çok daha az kodla elde edilebileceğini (daha az okunabilir olsa bile) göstermek ve 
fonksiyon oluşturma becerimizi göstermek için, bunu biraz daha gözden geçireceğiz.

~~~~ {.haskell: .ghci name="code"}
main = interact $ unlines . filter ((<10) . length) . lines  
~~~~

Vay canına, aslında bunu tek bir satıra indirdik, bu oldukça havalı!

`interact`, bazı içerikleri içlerine pipe'layan programları yapmak ve ardından bazı sonuçları çıkarmak için kullanılabilir veya
kullanıcıdan bir satır girdi alıyor gibi görünen programlar yapmak, bu satıra göre bazı sonuçları geri vermek için kullanılabilir ve sonra başka bir satır alın vb.
Aslında ikisi arasında gerçek bir ayrım yok, bu sadece kullanıcının bunları nasıl kullanacağına bağlı.

Devamlı olarak bir satırı okuyan ve ardından satırın palindrom olup olmadığını söyleyen bir program yapalım.
`getLine`'ı bir satırı okumak, kullanıcıya palindrom olup olmadığını söylemek ve ardından `main`'i baştan çalıştırmak için kullanabiliriz.
Ancak `interact`'ı kullanırsak daha kolay. `interact` özelliğini kullanırken, bazı girdileri istenen çıktıya dönüştürmek için ne yapmanız gerektiğini düşünün.
Bizim durumumuzda, girdinin her satırını `"palindrome"` veya `"not palindrome"` ile değiştirmeliyiz.
Bu yüzden `"elephant\nABCBA\nwhatever"` gibi bir şeyi `"not a palindrome\npalindrome\nnot a palindrome"` haline getiren bir fonksiyon yazmalıyız. Hadi bunu yapalım!

~~~~ {.haskell: .ghci name="code"}
respondPalindromes contents = unlines (map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") (lines contents))  
    where   isPalindrome xs = xs == reverse xs  
~~~~

Bunu noktasız yazalım.

~~~~ {.haskell: .ghci name="code"}
respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines  
    where   isPalindrome xs = xs == reverse xs  
~~~~

Oldukça basit. Önce `"elephant\nABCBA\nwhatever"` gibi bir şeyi `["elephant", "ABCBA", "whatever"]` haline dönüştürür ve
ardından lambdayı bunun üzerine eşleyerek `["not a palindrome", "palindrome", "not a palindrome"]` ve ardından `unlines`,
bu listeyi yeni satırla ayrılmış tek bir string'de birleştirir. Şimdi yapabiliriz

~~~~ {.haskell: .ghci name="code"}
main = interact respondPalindromes  
~~~~

Bunu test edelim:

~~~~ {.haskell: .ghci name="code"}
$ runhaskell palindromes.hs  
hehe  
not a palindrome  
ABCBA  
palindrome  
cookie  
not a palindrome   
~~~~

Büyük bir girdi string'ini diğerine dönüştüren bir program yapmış olsak da, bunu satır satır yapan bir program yapmışız gibi davranıyor.
Bunun nedeni Haskell'in tembel olması ve sonuç string'inin ilk satırını yazdırmak istemesidir, ancak henüz girişin ilk satırına sahip olmadığı için yapamaz.
Yani biz ona ilk girdi satırını verir vermez çıktının ilk satırını yazdırır. Satır sonu karakteri vererek programdan çıkıyoruz.

Bu programı, içine bir dosya aktararak da kullanabiliriz. Diyelim ki bu dosya bizde:

~~~~ {.haskell: .ghci name="code"}
dogaroo  
radar  
rotor  
madam  
~~~~

ve bunu `word.txt` olarak kaydediyoruz. Bunu programımıza bağlayarak elde ettiğimiz şey bu:

~~~~ {.haskell: .ghci name="code"}
$ cat words.txt | runhaskell palindromes.hs  
not a palindrome  
palindrome  
palindrome  
palindrome  
~~~~

Yine, programımızı çalıştırmış ve kelimeleri standart girdiye kendimiz koymuş gibi aynı çıktıyı elde ederiz.
`palindromes.hs`'nin girdisini görmüyoruz çünkü girdi bizden değil, bizden değil, dosyadan geliyor.

Şimdi muhtemelen I/O'nun ne kadar tembel çalıştığını ve bunu kendi yararımıza nasıl kullanabileceğimizi görüyorsunuz.
Verilen bir girdi için çıktının ne olması gerektiğini düşünebilir ve bu dönüşümü yapmak için bir fonksiyon yazabilirsiniz.
Tembel I/O'da, girişten kesinlikle olması gerekene kadar hiçbir şey yenmez çünkü şu anda yazdırmak istediğimiz şey bu girişe bağlıdır.

Şimdiye kadar, terminale bir şeyler yazdırarak ve ondan okuyarak I/O ile çalıştık. Peki ya dosyaları okuma ve yazma? Bir bakıma biz zaten bunu yapıyoruz.
Terminalden okumayı düşünmenin bir yolu, (biraz özel) bir dosyadan okumak gibi olduğunu hayal etmektir. 
Aynısı terminale yazmak için de geçerli, bir tür dosyaya yazmak gibi.
Bu iki dosyayı sırasıyla standart çıktı ve standart girdi anlamına gelen `stdout` ve `stdin` olarak adlandırabiliriz.
Bunu aklımızda tutarak, dosyalara yazmanın ve dosyalardan okumanın standart çıktıya yazmaya ve standart girdiden okumaya çok benzediğini göreceğiz.

Avril Lavigne'nin 1 numaralı kız arkadaşından bir mısra içeren girlfriend.txt adlı dosyayı açan ve
sadece terminale yazdıran gerçekten basit bir programla başlayacağız. İşte *girlfriend.txt*:

~~~~ {.haskell: .ghci name="code"}
Hey! Hey! You! You!   
I don't like your girlfriend!   
No way! No way!   
I think you need a new one!  
~~~~

Ve işte programımız:

~~~~ {.haskell: .ghci name="code"}
import System.IO  
  
main = do  
    handle <- openFile "girlfriend.txt" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle  
~~~~

Çalıştırarak beklenen sonucu elde ederiz:

~~~~ {.haskell: .ghci name="code"}
$ runhaskell girlfriend.hs  
Hey! Hey! You! You!  
I don't like your girlfriend!  
No way! No way!  
I think you need a new one!  
~~~~

Bu satır satır üzerinden geçelim. İlk satır, dikkatimizi çekmek için sadece dört ünlemdir. İkinci satırda Avril, şu anki romantik partnerimizden hoşlanmadığını söylüyor.
Üçüncü satır, bu onaylanmamayı vurgulamaya hizmet ederken, dördüncü satır, yeni bir kız arkadaş bulmamız gerektiğini öne sürüyor.

Ayrıca programı satır satır gözden geçirelim! Programımız, bir *do* bloğu ile birbirine yapıştırılmış birkaç I/O eylemidir.
*do* bloğunun ilk satırında, `openFile` adında yeni bir fonksiyon görüyoruz. Bu, tür imzasıdır: `openFile :: FilePath -> IOMode -> IO Handle`.
Bunu yüksek sesle okursanız, şunu belirtir: `openFile` bir dosya yolunu ve bir `IOMode` alır ve bir dosyayı açacak ve
sonuç olarak dosyanın ilişkili tutamacını kapsülleyecek bir I/O eylemi döndürür.

`FilePath`, `String` için yalnızca bir [tür eşanlamlısıdır(type synonyms)](../tr/08-making-our-own-types-and-typeclasses.md#tür-eşanlamlıları), basitçe şu şekilde tanımlanır:

~~~~ {.haskell: .ghci name="code"}
type FilePath = String  
~~~~

`IOMode`, şu şekilde tanımlanan bir türdür:

~~~~ {.haskell: .ghci name="code"}
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode  
~~~~

Tıpkı haftanın günleri için yedi olası değeri temsil eden türümüz gibi, bu tür, açık dosyamızla ne yapmak istediğimizi temsil eden bir numaralandırmadır.
Çok basit. Sadece bu türün `IOMode` olduğunu ve `IO Mode`'u olmadığını unutmayın. `IO Mode`, sonucu olarak bir tür `Mode` değeri olan bir I/O eyleminin türü olabilir,
ancak `IOMode` yalnızca basit bir numaralandırmadır.

Son olarak, belirtilen dosyayı belirtilen modda açacak bir I/O eylemi döndürür. Bu eylemi bir şeye bağlarsak, bir `Handle` elde ederiz.
`Handle` türünde bir değer dosyamızın nerede olduğunu gösterir. Bu tanıtıcıyı kullanacağız, böylece hangi dosyadan okuyacağımızı bileceğiz.
Bir dosyayı okumak ama bu okumayı bir handle'a bağlamamak aptalca olurdu çünkü dosyayla hiçbir şey yapamayız. Yani bizim durumumuzda, işlemek için `handle`'a bağladık.

Sonraki satırda `hGetContents` adlı bir fonksiyon görüyoruz. Bir `Handle`'ı alır, böylece içeriği hangi dosyadan alacağını bilir ve bir `IO String` döndürür - 
sonuç olarak dosyanın içeriğini tutan bir I/O işlemi. Bu fonksiyon, `getContents`'e çok benzer. Tek fark, `getContents`'in standart girdiden (yani terminalden) 
otomatik olarak okuyacağı, `hGetContents`'in hangi dosyadan okunacağını söyleyen bir dosya handle'ı almasıdır.
Diğer tüm açılardan aynı şekilde çalışırlar. Ve tıpkı `getContents` gibi, `hGetContents` dosyayı bir kerede okumaya ve hafızada saklamaya çalışmaz,
ancak gerektiği gibi okuyacaktır. Bu gerçekten harika çünkü `contents`'i dosyanın tüm içeriği olarak değerlendirebiliriz, ancak gerçekten belleğe yüklenmez.
Yani bu gerçekten çok büyük bir dosya olsaydı, `hGetContents`'i yapmak hafızamızı boğmazdı, ancak gerektiğinde dosyadan sadece ihtiyaç duyduğu şeyi okurdu.

Bir dosyayı tanımlamak için kullanılan handle ile dosyanın contents'i arasındaki farka dikkat edin, programımızda `handle` ve `contents` olarak sınırlandırılmıştır.
handle, dosyamızın ne olduğunu bildiğimiz bir şeydir. Tüm dosya sisteminizin gerçekten büyük bir kitap olduğunu ve
her dosyanın kitabın bir bölümü olduğunu hayal ediyorsanız, handle, bir bölümü şu anda nerede okuduğunuzu (veya yazdığınızı) gösteren bir yer imidir,
oysa contents asıl bölümdür.

`putStr contents` ile içeriği standart çıktıya yazdırırız ve ardından bir handle alan ve dosyayı kapatan bir I/O eylemi döndüren `hClose` yaparız.
Dosyayı `openFile` ile açtıktan sonra kendiniz kapatmalısınız! 

Az önce yaptığımız şeyi yapmanın bir başka yolu da `withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a` tür imzasına sahip `withFile` fonksiyonunu kullanmaktır.
Bir dosyaya giden yolu, bir `IOMode`'u alır ve sonra bir handle alan ve bazı I/O eylemi döndüren bir fonksiyonu alır.
Döndürdüğü şey, o dosyayı açacak, dosya ile istediğimiz bir şeyi yapacak ve sonra onu kapatacak bir I/O eylemidir.
Döndürülen son I/O eyleminde kapsüllenen sonuç, verdiğimiz fonksiyonun döndürdüğü I/O eyleminin sonucuyla aynıdır.
Bu biraz karmaşık gelebilir, ancak gerçekten basit, özellikle de lambda'ya aşina olanlar için, işte `withFile` kullanmak için yeniden yazılmış önceki örneğimiz:

~~~~ {.haskell: .ghci name="code"}
import System.IO     
    
main = do     
    withFile "girlfriend.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle     
        putStr contents)  
~~~~

Gördüğünüz gibi, önceki kod parçasına çok benziyor. `(\handle -> ...)` bir handle alan ve bir I/O eylemi döndüren fonksiyondur ve
genellikle bu şekilde, bir lambda ile yapılır. Sadece yapmak için bir I/O eylemi yapmak ve sonra dosyayı kapatmak yerine bir I/O eylemi döndüren bir fonksiyon
almasının nedeni, ona ileteceğimiz I/O eylemi hangi dosyada çalıştırılacağını bilmez. Bu şekilde `withFile` dosyayı açar ve ardından handle'ı ona verdiğimiz fonksiyona geçirir.
Bu fonksiyondan bir I/O eylemi alır ve daha sonra aynı ona benzer bir I/O eylemi yapar, ancak daha sonra dosyayı kapatır.
İşte `withFile` fonksiyonu ile kendimiz nasıl yapabiliriz:

~~~~ {.haskell: .ghci name="code"}
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path mode f = do  
    handle <- openFile path mode   
    result <- f handle  
    hClose handle  
    return result  
~~~~

![edd](../img/edd.png)
Sonucun bir I/O eylemi olacağını biliyoruz, bu yüzden bir *do* ile başlayabiliriz. Önce dosyayı açıp ondan bir handle alıyoruz.
Ardından, tüm işi yapan I/O eylemini geri almak için fonksiyonumuza `handle` uygularız. Bu eylemi `result`'a bağlarız, handle'ı kapatır ve ardından `return result` yaparız.
`f`'den aldığımız I/O eyleminde özetlenen sonucu `return`'lediğimizde, bunu I/O eylemimizin `f handle`'ndan aldığımızla aynı sonucu kapsayacak şekilde yaparız.
Öyleyse, `f handle`, standart girdiden birkaç satırı okuyacak ve bunları bir dosyaya yazacak ve sonuç olarak okuduğu satır sayısını kapsayacak bir eylem döndürürse,
bunu `withFile'` ile kullandıysak, Sonuç olarak ortaya çıkan I/O eylemi de sonuç olarak okunan satır sayısına sahip olacaktır.

Tıpkı `getContents` gibi çalışan `hGetContents`'imiz olduğu gibi, ancak belirli bir dosya için `hGetLine`, `hPutStr`, `hPutStrLn`, `hGetChar` ve bunun gibileri de mevcut.
Aynı *h* harfi olmadan berzerleri gibi çalışırlar, sadece parametre olarak bir handle alırlar ve standart girdi veya standart çıktı üzerinde çalışmak yerine
o belirli dosya üzerinde çalışırlar. Örnek: `putStrLn`, bir string'i alan ve bu string'i terminale yazdıracak ve ondan sonra
yeni bir satır yazdıracak bir I/O eylemi döndüren bir fonksiyondur. `hPutStrLn` bir handle ve bir string alır ve bu string'i handle'la ilişkili dosyaya yazacak ve
ardından yeni bir satır koyacak bir I/O eylemi döndürür. Aynı şekilde, `hGetLine` bir handle alır ve dosyasından bir satırı okuyan bir I/O eylemi döndürür.

Dosyaları yüklemek ve ardından content'lerini string'ler olarak ele almak o kadar yaygındır ki, işimizi daha da kolaylaştırmak için şu üç güzel küçük fonksiyona sahibiz:

`readFile`, `readFile :: FilePath -> IO String` tür imzasına sahiptir. Unutmayın, `FilePath` sadece `String` için süslü bir isimdir.
`readFile` bir dosyanın yolunu alır ve o dosyayı okuyacak (tabii ki tembel olarak) ve içeriğini bir string olarak bir şeye bağlayacak bir I/O eylemi döndürür.
Genellikle `openFile` yapmaktan ve onu bir handle'a bağlamaktan ve sonra `hGetContents` yapmaktan daha kullanışlıdır.
Önceki örneğimizi `readFile` ile şöyle yazabilirdik:

~~~~ {.haskell: .ghci name="code"}
import System.IO  
  
main = do  
    contents <- readFile "girlfriend.txt"  
    putStr contents  
~~~~

Dosyamızı tanımlayacak bir handle alamadığımız için, onu manuel olarak kapatamayız, bu nedenle Haskell bunu `readFile` kullandığımızda bizim için yapar.

`writeFile`, `writeFile :: FilePath -> String -> IO ()` türüne sahiptir. Bir dosyaya giden yolu ve bu dosyaya yazmak için bir string'i alır ve
yazmayı yapacak bir I/O eylemi döndürür. Böyle bir dosya zaten mevcutsa, üzerine yazılmadan önce sıfır uzunluğa kadar bastırılacaktır.
*girlfriend.txt* dosyasını CAPSLOCKED sürüme nasıl çevireceğiniz ve *girlfriendcaps.txt* dosyasına nasıl yazacağınız aşağıda açıklanmıştır:

~~~~ {.haskell: .ghci name="code"}
import System.IO     
import Data.Char  
    
main = do     
    contents <- readFile "girlfriend.txt"     
    writeFile "girlfriendcaps.txt" (map toUpper contents)  
~~~~

~~~~ {.haskell: .ghci name="code"}
$ runhaskell girlfriendtocaps.hs  
$ cat girlfriendcaps.txt  
HEY! HEY! YOU! YOU!  
I DON'T LIKE YOUR GIRLFRIEND!  
NO WAY! NO WAY!  
I THINK YOU NEED A NEW ONE!  
~~~~

`appendFile`, tıpkı `writeFile` gibi bir tür imzasına sahiptir, yalnızca `appendFile`, zaten varsa, dosyayı sıfır uzunluğa kesmez, ancak ona bir şeyler ekler.

Diyelim ki, yapmamız gereken satır başına bir görev olan bir *todo.txt* dosyamız var.
Şimdi standart girişten bir satır alan ve bunu yapılacaklar listemize ekleyen bir program yapalım.

~~~~ {.haskell: .ghci name="code"}
import System.IO     
    
main = do     
    todoItem <- getLine  
    appendFile "todo.txt" (todoItem ++ "\n")
~~~~

~~~~ {.haskell: .ghci name="code"}
$ runhaskell appendtodo.hs  
Iron the dishes  
$ runhaskell appendtodo.hs  
Dust the dog  
$ runhaskell appendtodo.hs  
Take salad out of the oven  
$ cat todo.txt  
Iron the dishes  
Dust the dog  
Take salad out of the oven  
~~~~

Her satırın sonuna `"\n"` eklememiz gerekiyordu çünkü `getLine` bize sonunda bir yeni satır karakteri vermiyor.

Ooh, bir şey daha. `contents <- hGetContents` handle işleminin tüm dosyanın aynı anda okunmasına ve bellekte depolanmasına neden olmadığını konuştuk.
I/O tembel, bunu yapmak için: 

~~~~ {.haskell: .ghci name="code"}
main = do   
    withFile "something.txt" ReadMode (\handle -> do  
        contents <- hGetContents handle  
        putStr contents)  
~~~~

Aslında dosyadan çıktıya bir pipe bağlamak gibidir. Listeleri streams olarak düşünebileceğiniz gibi, dosyaları akışlar(streams) olarak da düşünebilirsiniz.
Bu, her seferinde bir satırı okuyacak ve ilerledikçe terminale yazdıracaktır. Öyleyse soruyor olabilirsiniz, o zaman bu pipe ne kadar geniş?
Disk'e ne sıklıkla erişilecek? Metin dosyaları için varsayılan arabelleğe alma genellikle satır arabelleğe almadır.
Bu, dosyanın tek seferde okunacak en küçük kısmının bir satır olduğu anlamına gelir. Bu nedenle bu durumda aslında bir satırı okur,
çıktıya yazdırır, sonraki satırı okur, yazdırır vb. Binary dosyalar için, varsayılan arabelleğe alma genellikle blok arabelleğe(block-buffering) almadır.
Bu, dosyayı yığın(chunk) halinde okuyacağı anlamına gelir. Yığın boyutu, işletim sisteminizin harika olduğunu düşündüğü bir boyuttur.

`hSetBuffering` fonksiyonunu kullanarak arabelleğe almanın tam olarak nasıl yapıldığını kontrol edebilirsiniz.
Bir handle ve `BufferMode` alır ve arabelleğe almayı ayarlayan bir I/O eylemi döndürür. `BufferMode` basit bir numaralandırma veri türüdür ve
tutabileceği olası değerler şunlardır: `NoBuffering`, `LineBuffering` veya `BlockBuffering (Maybe Int)`.
`Maybe Int`, chunk'ın byte cinsinden ne kadar büyük olması gerektiğini gösterir. `Nothing` ise, işletim sistemi chunk boyutunu belirler.
`NoBuffering`, her seferinde bir karakter okunacağı anlamına gelir. `NoBuffering` genellikle bir arabellekleme modu olarak berbattır çünkü disk'e çok fazla erişmesi gerekir.

İşte önceki kod parçamız, sadece satır satır okumuyor, tüm dosyayı 2048 byte'lık chunk'lar halinde okuyor.

~~~~ {.haskell: .ghci name="code"}
main = do   
    withFile "something.txt" ReadMode (\handle -> do  
        hSetBuffering handle $ BlockBuffering (Just 2048)  
        contents <- hGetContents handle  
        putStr contents)  
~~~~

Dosyaları daha büyük parçalar halinde okumak, disk erişimini en aza indirmek istiyorsak veya dosyamız aslında yavaş bir ağ kaynağıysa yardımcı olabilir.

Ayrıca bir handle alan ve handle ile ilişkilendirilmiş dosyanın arabelleğini temizleyecek(flush) bir I/O eylemi döndüren bir funciton olan `hFlush`'u da kullanabiliriz.
Line-buffering yaptığımızda, arabellek her satırdan sonra temizlenir. Block-buffering yaptığımızda, bir chunk okuduktan sonra. Bir handle'ı kapattıktan sonra da temizlenir.
Bu, bir satırsonu karakterine ulaştığımızda, okuma (veya yazma) mekanizmasının şimdiye kadarki tüm verileri raporladığı anlamına gelir.
Ancak şimdiye kadar okunan verilerin raporlanmasını zorlamak için `hFlush` kullanabiliriz.
Temizlemeden sonra, veriler aynı anda çalışan diğer programlar tarafından kullanılabilir.

Şunun gibi block-buffered bir dosya okuduğunuzu düşünün: klozetiniz, içinde bir galon su olduktan sonra kendini sifonu çekecek şekilde ayarlanmıştır.
Böylece su dökmeye başlarsınız ve galon işaretine ulaşıldığında, bu su otomatik olarak sifonu çeker ve o ana kadar döktüğünüz sudaki veriler okunur.
Ancak tuvaletteki düğmeye basarak sifonu elle de çekebilirsiniz. Bu, tuvaletin sifonunu çeker ve tuvaletin içindeki tüm su (veriler) okunur.
Fark etmediyseniz, tuvaletin sifonunu çekmek `hFlush` için bir metafordur. Bu, analoji standartlarını programlayarak çok iyi bir benzetme değil,
ancak esas için yıkanabilecek gerçek bir dünya nesnesi istedim.

*todo.txt*'deki yapılacaklar listemize yeni bir öğe eklemek için bir program yaptık, şimdi bir öğeyi kaldırmak için bir program yapalım.
Kodu yapıştıracağım ve sonra programın üzerinden birlikte geçeceğiz, böylece gerçekten kolay olduğunu göreceksiniz.
`System.Directory`'den birkaç yeni fonksiyon ve `System.IO`'dan bir yeni fonksiyon kullanacağız, ancak hepsi açıklanacak.

Her neyse, işte bir öğeyi *todo.txt*'den kaldırma programı:

~~~~ {.haskell: .ghci name="code"}
import System.IO  
import System.Directory  
import Data.List  
  
main = do        
    handle <- openFile "todo.txt" ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let todoTasks = lines contents     
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks     
    putStrLn "These are your TO-DO items:"  
    putStr $ unlines numberedTasks  
    putStrLn "Which one do you want to delete?"     
    numberString <- getLine     
    let number = read numberString     
        newTodoItems = delete (todoTasks !! number) todoTasks     
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile "todo.txt"  
    renameFile tempName "todo.txt"  
~~~~

İlk başta, *todo.txt* dosyasını okuma modunda açıp `handle`'ını handle'a bağlarız.

Ardından, `System.IO` - `openTempFile`'dan daha önce karşılaşmadığımız bir fonksiyonu kullanıyoruz. Adı oldukça açıklayıcıdır.
Geçici bir dizine giden yolu ve bir dosya için şablon adını alır ve geçici bir dosya açar.
Kullandığımız `"."` geçici dizin için, çünkü `.` hemen hemen her işletim sistemindeki mevcut dizini gösterir.
Geçici dosya için şablon adı olarak `"temp"` kullandık, bu da geçici dosyanın geçici olarak adlandırılacağı ve bazı rastgele karakterlerin olacağı anlamına gelir.
Geçici dosyayı oluşturan bir I/O eylemi döndürür ve bu I/O eyleminin bir değer çifti olması sonucunu verir: geçici dosyanın adı ve bir handle.
Sadece *todo2.txt* adında normal bir dosya veya bunun gibi bir şey açabilirdik, ancak `openTempFile` kullanmak daha iyi bir uygulamadır,
böylece muhtemelen herhangi bir şeyin üzerine yazmadığınızı bilirsiniz. 

Geçerli dizini almak için `getCurrentDirectory` kullanmamamız ve daha sonra onu `openTempFile`'a geçirmememizin nedeni,
bunun yerine `"."` `openTempFile` çünkü. unix benzeri sistem ve Windows üzerindeki mevcut dizini ifade eder

Ardından, todo.txt içeriğini `contents`'e bağlıyoruz. Ardından, bu string'i her string bir satır olacak şekilde bir string listesine bölün.
Yani `todoTasks` artık `["Iron the dishes", "Dust the dog", "Take salad out of the oven"]`.
0'dan itibaren sayıları ve bu listeyi 3 gibi bir sayı alan ve `"hey"` gibi bir string'i alan ve `"3 - hey"` döndüren bir fonksiyon içeren listeyi sıkıştırıyoruz,
bu nedenle `numberedTasks`  `["0 - Iron the dishes", "1 - Dust the dog" ...` olur. Bu string listesini `unlines` ile tek bir satırsonu ile ayrılmış
string olarak birleştiririz ve bu string'i terminale yazdırırız. Bunu yapmak yerine `mapM putStrLn numberedTasks` da yapabileceğimizi unutmayın.

Kullanıcıya hangisini silmek istediğini sorar ve bir numara girmesini bekleriz. Diyelim ki `"Dust the dog"` olan `1` numarayı silmek istiyorlar, böylece `1`'i yumruklasınlar.
numberString artık `"1"` oldu ve bir sayı istediğimiz için, bir string'e değil, `1`'i elde etmek ve onu `number`'a bağlamak için `read`'i çalıştırıyoruz.

`delete` ve `!!`'i unutmayın `Data.List` içindeki fonksiyonlar. `!!` index'e sahip bir listeden bir öğe döndürür ve
`delete`, bir listedeki bir öğenin ilk oluşumunu siler ve bu oluşum olmadan yeni bir liste döndürür.
`(todoTasks !! number)` (sayı artık `1`'dir) `"Dust the dog"` döndürür. 
`todoTasks`'i `"Dust the dog"`'un ilk ortaya çıkışı olmadan `newTodoItems`'a bağlarız ve ardından açtığımız geçici dosyaya
yazmadan önce bunu `unlines` ile tek bir string'de birleştiririz. Eski dosya artık değiştirilmemiştir ve geçici dosya,
sildiğimiz dışında eski dosyanın yaptığı tüm satırları içerir.

Bundan sonra hem orijinal hem de geçici dosyaları kapatıyoruz ve daha sonra orijinali,
görebileceğiniz gibi, bir dosyanın yolunu açan ve onu silen `removeFile` ile kaldırıyoruz. Eski *todo.txt* dosyasını sildikten sonra,
geçici dosyayı *todo.txt* olarak yeniden adlandırmak için `renameFile`'ı kullanırız. Dikkatli olun, `removeFile` ve `renameFile` (ikisi de bu arada `System.Directory`'de bulunur)
dosya yollarını handle'ları değil parametreleri olarak alırlar.

Ve işte bu! Bunu daha da az satırda yapabilirdik, ancak var olan dosyaların üzerine yazmamaya çok dikkat ettik ve işletim sisteminden
bize geçici dosyamızı nereye koyabileceğimizi söylemesini istedik. Bunu bir deneyelim!

~~~~ {.haskell: .ghci name="code"}
$ runhaskell deletetodo.hs  
These are your TO-DO items:  
0 - Iron the dishes  
1 - Dust the dog  
2 - Take salad out of the oven  
Which one do you want to delete?  
1  
  
$ cat todo.txt  
Iron the dishes  
Take salad out of the oven  
  
$ runhaskell deletetodo.hs  
These are your TO-DO items:  
0 - Iron the dishes  
1 - Take salad out of the oven  
Which one do you want to delete?  
0  
  
$ cat todo.txt  
Take salad out of the oven   
~~~~


Komut satırı argümanları
----------------------

![arguments](../img/arguments.png)
Bir terminalde çalışan bir komut dosyası veya uygulama yapmak istiyorsanız, komut satırı argümanlarıyla uğraşmak hemen hemen bir gerekliliktir.
Neyse ki, Haskell'in standart kitaplığı, bir programın komut satırı argümanlarını almanın güzel bir yoluna sahiptir.

Önceki bölümde, to-do listemize bir to-do öğesi eklemek için bir program ve bir öğeyi kaldırmak için bir program yaptık. Aldığımız yaklaşımla ilgili iki sorun var.
Birincisi, to-do dosyamızın adını kodumuza gömdük. Dosyanın todo.txt olarak adlandırılacağına ve kullanıcının hiçbir zaman birkaç to-do listesi yönetmeye
ihtiyaç duymayacağına karar verdik.

Bunu çözmenin bir yolu, kullanıcıya her zaman to-do listesi olarak hangi dosyayı kullanmak istediklerini sormaktır.
Kullanıcının hangi öğeyi silmek istediğini bilmek istediğimizde bu yaklaşımı kullandık. Çalışıyor, ancak o kadar iyi değil,
çünkü kullanıcının programı çalıştırmasını, programın bir şey sormasını beklemesini ve sonra bunu programa söylemesini gerektiriyor.
Buna etkileşimli program denir ve etkileşimli komut satırı programlarının zor kısmı şudur - 
bu programın yürütülmesini toplu komut dosyası gibi otomatikleştirmek isterseniz ne olur? 
Bir programla etkileşime giren bir toplu komut dosyası yapmak, yalnızca bir programı veya birkaçını çağıran bir toplu komut dosyası oluşturmaktan daha zordur.

Bu nedenle, bazen programın çalıştırıldığında kullanıcıya sormasını sağlamak yerine,
kullanıcının programı çalıştırırken ne istediğini programa söylemesini sağlamak daha iyidir.
Ve kullanıcının programı çalıştırırken ne yapmasını istediklerini komut satırı argümanları aracılığıyla söylemekten daha iyi bir yolu var!

`System.Environment` modülünün iki harika I/O eylemi vardır. Bunlardan biri, bir `getArgs :: IO [String]` türüne sahip olan ve
programın birlikte çalıştırıldığı argümanları alacak ve içerdiği sonuç olarak argümanların bulunduğu bir listeye sahip olacak bir I/O eylemi olan `getArgs`'dır.
`getProgName`, `getProgName :: IO String` türüne sahiptir ve program adını içeren bir I/O eylemidir.

İşte bu ikisinin nasıl çalıştığını gösteren küçük bir program:

~~~~ {.haskell: .ghci name="code"}
import System.Environment   
import Data.List  
  
main = do  
   args <- getArgs  
   progName <- getProgName  
   putStrLn "The arguments are:"  
   mapM putStrLn args  
   putStrLn "The program name is:"  
   putStrLn progName  
~~~~

`getArgs` ve `progName`'i `args` ve `progName`'e bağlarız. `The arguments are:` deriz ve sonra `args`'daki her argüman için `putStrLn` yaparız. Bunu `arg-test` olarak derleyelim.

~~~~ {.haskell: .ghci name="code"}
$ ./arg-test first second w00t "multi word arg"  
The arguments are:  
first  
second  
w00t  
multi word arg  
The program name is:  
arg-test  
~~~~

Güzel. Bu bilgiyle donanmış, bazı harika komut satırı uygulamaları oluşturabilirsiniz. Aslında, devam edip bir tane yapalım.
Önceki bölümde, görev eklemek için ayrı bir program ve bunları silmek için ayrı bir program yaptık.
Şimdi, bunu tek bir programda birleştireceğiz, ne yaptığı komut satırı argümanlarına bağlı olacaktır.
Ayrıca, sadece todo.txt değil, farklı dosyalarda da çalışabilmesi için yapacağız.

Biz buna basitçe yapmak diyeceğiz ve o üç farklı şey yapabilecektir (haha!):

- Görevleri görüntüleyin
- Görev ekleyin
- Görevleri silin

Şu anda olası kötü girdilerle çok fazla ilgilenmeyeceğiz.

Programımız, *todo.txt* dosyasına` Find the magic sword of power` görevini eklemek istersek,
`todo add todo.txt "Find the magic sword of power"` terminalimize yumruk atmamız gerekecek şekilde yapılacaktır.
Görevleri görüntülemek için sadece `todo view todo.txt` yapacağız ve index'i 2 olan görevi kaldırmak için `todo remove todo.txt 2` yapacağız.

Bir dispatch association listesi oluşturarak başlayacağız. Key'ler olarak komut satırı argümanlarına ve karşılık gelen değerler olarak
fonksiyonlara sahip basit bir association listesi olacak. Tüm bu fonksiyonlar `[String] -> IO ()` türünde olacaktır.
Argüman listesini bir parametre olarak alacaklar ve görüntüleme, ekleme, silme vb. işlemleri gerçekleştiren bir I/O eylemi döndüreceklerdir.

~~~~ {.haskell: .ghci name="code"}
import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
  
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]  
~~~~

Henüz `main`, `add`, `view` ve `remove`'u tanımlamadık, o halde `main` ile başlayalım:

~~~~ {.haskell: .ghci name="code"}
main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args  
~~~~

İlk önce argümanları alırız ve onları bağlarız `(command:args)`. Desen eşleştirmesinden hatırlarsanız, bu, ilk argümanın `command`'a ve
geri kalanının `args`'a bağlanacağı anlamına gelir. Programımızı `todo add todo.txt "Spank the monkey"` olarak adlandırırsak,
`command` `add` olacaktır ve `args` `["todo.xt", "Spank the monkey"]` olacaktır.

Sonraki satırda, dispatch listesinde komutumuzu arıyoruz. `add`, `add`'e işaret ettiğinden, sonuç olarak `Just add` alırız.
Fonksiyonumuzu `Maybe` seçeneğinden çıkarmak için tekrar desen eşleştirme kullanırız. Komutumuz dispatch listesinde değilse ne olur?
Öyleyse arama `Nothing` döndürür, ancak çok fazla incelikle başarısızlıkla ilgilenmeyeceğimizi söyledik,
bu nedenle desen eşleştirme başarısız olacak ve programımız bir uyum sağlayacaktır.

Son olarak, argüman listesinin geri kalanıyla action fonksiyonumuzu çağırıyoruz.
Bu, bir öğe ekleyen, bir öğe listesi görüntüleyen veya bir öğeyi silen bir I/O eylemi döndürecektir ve bu eylem `main` *do* bloğunun
bir parçası olduğu için gerçekleştirilecektir. Şimdiye kadar somut örneğimizi takip edersek ve `action` fonksiyonumuz `add` ise, `args`
(yani `["todo.txt", "Spank the monkey"]`) ile çağrılır ve bir I/O eylemi döndürür bu, todo.txt dosyasına `"Spank the monkey"`'i ekler.

Harika! Artık geriye kalan tek şey `add`, `view` ve `remove` fonksiyonlarını uygulamaktır. `add` ile başlayalım:

~~~~ {.haskell: .ghci name="code"}
add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")  
~~~~

Programımızı `todo add todo.txt "Spank the monkey"` gibi çağırırsak, `add`, `main` bloktaki ilk desen eşleşmesinde `command`'a bağlanırken,
`["todo.txt", "Spank the monkey"]` gönderim listesinden aldığımız fonksiyona geçirilecektir. Bu nedenle, şu anda kötü bir girdi ile uğraşmadığımız için,
hemen bu iki öğeli bir listeye göre pattern match yapıyoruz ve bu satırı dosyanın sonuna ekleyen bir I/O eylemi ve bir satırsonu karakteri ile birlikte döndürüyoruz.

Sonra, liste görüntüleme funciton'ını uygulayalım. Öğeleri bir dosyada görüntülemek istiyorsak, `todo view todo.txt` yaparız.
Dolayısıyla, ilk desen eşleşmesinde, `command` `"view"` ve `args` `["todo.txt"]` olacaktır.

~~~~ {.haskell: .ghci name="code"}
view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks  
~~~~

Programda zaten aynı şeyi yaptık, yalnızca görevleri görüntülerken görevleri sildik, böylece kullanıcı silmek için birini seçebilsin,
sadece burada sadece görevleri gösteriyoruz.

Ve son olarak, `remove`'u uygulayacağız. Yalnızca görevleri silen programa çok benzeyecek, bu nedenle buradaki bir öğeyi silmenin nasıl çalıştığını anlamıyorsanız,
o programın altındaki açıklamaya bakın. Temel fark, todo.txt dosyasını kodlamıyor olmamız, onu bir argüman olarak almamızdır.
Ayrıca kullanıcıdan görev numarasını silmesini istemiyoruz, bunu bir argüman olarak alıyoruz.

~~~~ {.haskell: .ghci name="code"}
remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName  
~~~~

Dosyayı `fileName`'e göre açtık ve geçici bir dosya açtık, kullanıcının silmek istediği dizinin bulunduğu satırı sildik, bunu geçici dosyaya yazdık,
orijinal dosyayı kaldırdık ve geçici dosyayı tekrar `fileName` olarak yeniden adlandırdık.

İşte tüm görkemiyle, aynı anda tüm program!

~~~~ {.haskell: .ghci name="code"}
import System.Environment   
import System.Directory  
import System.IO  
import Data.List  
  
dispatch :: [(String, [String] -> IO ())]  
dispatch =  [ ("add", add)  
            , ("view", view)  
            , ("remove", remove)  
            ]  
   
main = do  
    (command:args) <- getArgs  
    let (Just action) = lookup command dispatch  
    action args  
  
add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")  
  
view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks  
  
remove :: [String] -> IO ()  
remove [fileName, numberString] = do  
    handle <- openFile fileName ReadMode  
    (tempName, tempHandle) <- openTempFile "." "temp"  
    contents <- hGetContents handle  
    let number = read numberString  
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks  
    hPutStr tempHandle $ unlines newTodoItems  
    hClose handle  
    hClose tempHandle  
    removeFile fileName  
    renameFile tempName fileName  
~~~~

![salad](../img/salad.png)
Çözümümüzü özetlemek gerekirse: Komutlardan bazı komut satırı argümanlarını alan ve bir I/O eylemi döndüren fonksiyonlara eşleyen bir dispatch association yaptık.
Komutun ne olduğunu görüyoruz ve buna göre uygun fonksiyonu dispatch listesinden alıyoruz. Uygun olanı yapacak bir I/O eylemini geri almak için bu fonksiyonu
komut satırı argümanlarının geri kalanıyla birlikte çağırıyoruz ve sonra sadece bu eylemi gerçekleştiriyoruz!

Diğer dillerde, bunu büyük bir switch case ifadesiyle veya başka bir şeyle uygulamış olabiliriz, ancak daha yüksek dereceli fonksiyonlar kullanmak,
bize uygun fonksiyonu vermesi için yalnızca dispatch listesini söylememize ve ardından bu fonksiyona bazı komut satırı argümanları için bize bir I/O eylemi vermesini
söylememize olanak tanır.

Uygulamamızı deneyelim!

~~~~ {.haskell: .ghci name="code"}
$ ./todo view todo.txt  
0 - Iron the dishes  
1 - Dust the dog  
2 - Take salad out of the oven  
  
$ ./todo add todo.txt "Pick up children from drycleaners"  
  
$ ./todo view todo.txt  
0 - Iron the dishes  
1 - Dust the dog  
2 - Take salad out of the oven  
3 - Pick up children from drycleaners  
  
$ ./todo remove todo.txt 2  
  
$ ./todo view todo.txt  
0 - Iron the dishes  
1 - Dust the dog  
2 - Pick up children from drycleaners  
~~~~

Bununla ilgili bir başka harika şey, ekstra işlevsellik eklemenin kolay olmasıdır.
Dispach association listesine bir girdi ekleyin ve ilgili fonksiyona uygulayın ve gülüyorsunuz!
Alıştırma olarak, bir dosya ve bir görev numarası alacak ve bu görevi to-do listesinin en üstüne çıkaran bir I/O eylemi
döndürecek bir `bump` fonksiyona uygulamayı deneyebilirsiniz.

Hatalı girdi durumunda (örneğin, birisi `todo UP YOURS HAHAHAHA` çalıştırırsa), bir hata olduğunu bildiren bir I/O eylemi yaparak bu programın biraz daha düzgün
başarısız olmasını sağlayabilirsiniz (örneğin, `errorExit :: IO ()`) ve ardından olası hatalı girdileri kontrol edin ve hatalı girdi varsa,
hata raporlama I/O eylemini gerçekleştirin. Başka bir yol da, yakında karşılaşacağımız istisnaları kullanmaktır.


Rastgelelik
----------


![random](../img/random.png)
Çoğu zaman programlama sırasında rasgele veri almanız gerekir. Belki de bir zarın atılması gereken bir oyun yapıyorsun veya
programınızı test etmek için bazı test verileri oluşturmanız gerekiyor. Programlama sırasında rastgele veriler için birçok kullanım vardır.
Aslında, sözde rasgele(pseudo-random), çünkü hepimiz biliyoruz ki tek gerçek rastlantısallık kaynağı, bir elinde peynir,
diğerinde poposu olan tek tekerlekli bisiklet üzerindeki bir maymun. Bu bölümde Haskell'in görünüşte rastgele veriler oluşturmasını nasıl sağlayacağımızı inceleyeceğiz.

Diğer programlama dillerinin çoğunda, size rastgele bir sayı veren fonksiyonlara sahipsiniz. Bu fonksiyonu her çağırdığınızda, (umarız) farklı bir rastgele sayı alırsınız.
Haskell'de nasıl? Unutma, Haskell pure functional language'dir. Bunun anlamı, referans şeffaflığına sahip olmasıdır.
Bunun anlamı, bir fonksiyonun aynı parametreler iki kez verilirse, aynı sonucu iki kez üretmesi gerektiğidir.
Bu gerçekten harika çünkü programlar hakkında farklı düşünmemize izin veriyor ve gerçekten ihtiyacımız olana kadar değerlendirmeyi ertelememizi sağlıyor.
Bir fonksiyonu çağırırsam, sonuçları bana vermeden önce komik şeyler yapmayacağından emin olabilirim. Önemli olan tek şey sonuçlarıdır.
Ancak bu, rastgele sayılar elde etmeyi biraz zorlaştırır. Böyle bir fonksiyona sahipsem:

~~~~ {.haskell: .ghci name="code"}
randomNumber :: (Num a) => a  
randomNumber = 4  
~~~~

Rastgele sayı fonksiyonu olarak pek kullanışlı değil çünkü `4`'ün tamamen rastgele olduğunu garanti edebilirim, çünkü onu belirlemek için bir zar kullandım.

Diğer diller görünüşte rastgele sayılar nasıl oluşturur? Bilgisayarınızdan şu anki saat, farenizi ne kadar ve nereye hareket ettirdiğiniz ve
bilgisayarınızın arkasında ne tür sesler çıkardığınız gibi çeşitli bilgileri alıyorlar ve buna dayanarak gerçekten rastgele görünen bir sayı veriyorlar.
Bu faktörlerin kombinasyonu (bu rastgelelik) muhtemelen herhangi bir anda farklıdır, bu nedenle farklı bir rastgele sayı elde edersiniz.

Ah. Yani Haskell'de rastgele bir sayı yapabiliriz, o zaman rasgeleliği parametresi olarak alan ve buna dayalı olarak bir sayı (veya başka bir veri türü)
döndüren bir fonksiyon yaparsak.

`System.Random` modülüne girin. Rastgelelik ihtiyacımızı karşılayan tüm fonksiyonlara sahiptir.
Şimdi, export ettiği fonksiyonlardan birine, yani `random`'a girelim. İşte türü: `random :: (RandomGen g, Random a) => g -> (a, g)`. Whoa!
Bu tür bildiriminde bazı yeni tür sınıfıları burada! `RandomGen` tür sıfını, rastgelelik kaynağı olarak hareket edebilen türler içindir.
`Random` tür sınıfı, rastgele değerler alabilen şeyler içindir. Bir boolean değeri, `True` veya `False` gibi rastgele bir değer alabilir.
Bir sayı ayrıca çok sayıda farklı rasgele değer alabilir. Bir fonksiyon rastgele bir değer alabilir mi? Sanmıyorum, muhtemelen hayır!
`random`'ın tür bildirimini İngilizceye çevirmeye çalışırsak, şöyle bir şey elde ederiz:
rastgele bir üretici alır (bu bizim rastgelelik kaynağımızdır) ve rastgele bir değer ve yeni bir rastgele üretici döndürür.
Neden rastgele bir değerin yanı sıra yeni bir üretici(generator) de döndürüyor? Pekala, birazdan göreceğiz.

`random` fonksiyonumuzu kullanmak için, bu rastgele üreticilerden birini elimize almalıyız.
`System.Random` modülü, `RandomGen` tür sınıfının bir instance'ı olan `StdGen` adlı havalı bir türü export eder.
Ya manuel olarak bir `StdGen` yapabiliriz ya da sisteme bize çok sayıda rastgele şeyden yola çıkarak bir tane vermesini söyleyebiliriz.

Manuel olarak rastgele bir üretici oluşturmak için `mkStdGen` fonksiyonunu kullanın. `mkStdGen :: Int -> StdGen` türüne sahiptir.
Bir tamsayı alır ve buna göre bize rastgele bir üretici verir. Tamam o zaman, (neredeyse rastgele) bir sayı elde etmek için rastgele ve `mkStdGen`'i
birlikte kullanmayı deneyelim. Tamam o zaman, (neredeyse rastgele) bir sayı elde etmek için `random` ve `mkStdGen`'i birlikte kullanmayı deneyelim.

~~~~ {.haskell: .ghci name="code"}
ghci> random (mkStdGen 100)  
~~~~

~~~~ {.haskell: .ghci name="code"}
<interactive>:1:0:  
    Ambiguous type variable `a' in the constraint:  
      `Random a' arising from a use of `random' at <interactive>:1:0-20  
    Probable fix: add a type signature that fixes these type variable(s)  
~~~~

Bu ne? Ah, doğru, `random` fonksiyonu, `Random` tür sınıfının parçası olan herhangi bir türden bir değer döndürebilir,
bu yüzden Haskell'e ne tür bir tür istediğimizi bildirmeliyiz. Ayrıca bir çiftte rastgele bir değer ve rastgele bir oluşturucu döndürdüğünü de unutmayalım.

~~~~ {.haskell: .ghci name="code"}
ghci> random (mkStdGen 100) :: (Int, StdGen)  
(-1352021624,651872571 1655838864)  
~~~~

En sonunda! Biraz rastgele görünen bir sayı! Demetin ilk bileşeni bizim sayımızdır, ikinci bileşen ise yeni rastgele oluşturucumuzun metinsel bir temsilidir.
Aynı rastgele oluşturucu ile tekrar `random` dersek ne olur?

~~~~ {.haskell: .ghci name="code"}
ghci> random (mkStdGen 100) :: (Int, StdGen)  
(-1352021624,651872571 1655838864)  
~~~~

Elbette. Aynı parametreler için aynı sonuç. Öyleyse ona parametre olarak farklı bir rastgele oluşturucu vermeyi deneyelim.

~~~~ {.haskell: .ghci name="code"}
ghci> random (mkStdGen 949494) :: (Int, StdGen)  
(539963926,466647808 1655838864)  
~~~~

Pekala, harika, harika, farklı bir numara. Bu fonksiyonlardan farklı türleri geri almak için tür ek açıklamasını kullanabiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> random (mkStdGen 949488) :: (Float, StdGen)  
(0.8938442,1597344447 1655838864)  
ghci> random (mkStdGen 949488) :: (Bool, StdGen)  
(False,1485632275 40692)  
ghci> random (mkStdGen 949488) :: (Integer, StdGen)  
(1691547873,1597344447 1655838864)  
~~~~

Üç kez bozuk para atmayı simüle eden bir fonksiyon yapalım. Eğer `random` rastgele bir değerle birlikte yeni bir oluşturucu döndürmediyse,
bu fonksiyonun bir parametre olarak üç rastgele oluşturucu almasını ve ardından her biri için jeton atışı döndürmesini sağlamalıyız.
Ancak bu kulağa yanlış geliyor çünkü bir üretici `Int` türünde rastgele bir değer üretebiliyorsa (farklı değerlerin yükünü alabilir),
üç jeton atışı yapabilmelidir (tam olarak sekiz kombinasyon alabilir). Dolayısıyla, yeni bir üreticiyi bir değerle birlikte `random`
döndürmenin gerçekten işe yaradığı yer burasıdır.

Bir madeni parayı basit bir `Bool` ile temsil edeceğiz. `True` yazıdır(tails), `False` turadır(heads).

~~~~ {.haskell: .ghci name="code"}
threeCoins :: StdGen -> (Bool, Bool, Bool)  
threeCoins gen =   
    let (firstCoin, newGen) = random gen  
        (secondCoin, newGen') = random newGen  
        (thirdCoin, newGen'') = random newGen'  
    in  (firstCoin, secondCoin, thirdCoin)   
~~~~

Bir jeton ve yeni bir üretici elde etmek için parametre olarak aldığımız üretici ile `random` diyoruz.
Sonra tekrar çağırıyoruz, ancak bu sefer yeni üreticimizle ikinci parayı almak için. Üçüncü para için de aynısını yapıyoruz.
Her seferinde aynı üreticiyle çağırsaydık, tüm paralar aynı değere sahip olurdu ve sonuç olarak sadece `(False, False, False)` veya
`(True, True, True)` elde edebilirdik.

~~~~ {.haskell: .ghci name="code"}
ghci> threeCoins (mkStdGen 21)  
(True,True,True)  
ghci> threeCoins (mkStdGen 22)  
(True,False,True)  
ghci> threeCoins (mkStdGen 943)  
(True,False,True)  
ghci> threeCoins (mkStdGen 944)  
(True,True,True)  
~~~~

`random gen :: (Bool, StdGen)` yapmak zorunda olmadığımıza dikkat edin. Bunun nedeni, fonksiyonun tür bildiriminde boole istediğimizi zaten belirtmiş olmamızdır.
Bu nedenle Haskell, bu durumda bir boole değeri istediğimiz sonucuna varabilir.

Peki ya dört jeton çevirmek istersek? veya beş? Pekala, bir üretici alan ve bu üreticiye bağlı olarak sonsuz bir değer dizisi döndüren `randoms` adlı bir fonksiyon var.

~~~~ {.haskell: .ghci name="code"}
ghci> take 5 $ randoms (mkStdGen 11) :: [Int]  
[-1807975507,545074951,-1015194702,-1622477312,-502893664]  
ghci> take 5 $ randoms (mkStdGen 11) :: [Bool]  
[True,True,True,True,False]  
ghci> take 5 $ randoms (mkStdGen 11) :: [Float]  
[7.904789e-2,0.62691015,0.26363158,0.12223756,0.38291094]  
~~~~

Neden `randoms` yeni bir oluşturucu ve bir liste getirmiyor? `randoms` fonksiyonunu şu şekilde çok kolay bir şekilde uygulayabiliriz:

~~~~ {.haskell: .ghci name="code"}
randoms' :: (RandomGen g, Random a) => g -> [a]  
randoms' gen = let (value, newGen) = random gen in value:randoms' newGen  
~~~~

Özyinelemeli bir tanım. Mevcut üreticiden rastgele bir değer ve yeni bir üretici alıyoruz ve ardından head değeri olarak değeri ve
tail olarak yeni üreticiye dayalı rastgele sayıları olan bir liste yapıyoruz. Potansiyel olarak sonsuz sayıda sayı üretebilmemiz gerektiğinden,
yeni rastgele üreticiye geri veremeyiz.

Sonlu bir sayı akışı üreten bir fonksiyon ve bunun gibi yeni bir üretici yapabiliriz:

~~~~ {.haskell: .ghci name="code"}
finiteRandoms :: (RandomGen g, Random a, Num n) => n -> g -> ([a], g)  
finiteRandoms 0 gen = ([], gen)  
finiteRandoms n gen =   
    let (value, newGen) = random gen  
        (restOfList, finalGen) = finiteRandoms (n-1) newGen  
    in  (value:restOfList, finalGen)  
~~~~

Yine, özyivelemeli bir tanım. Diyoruz ki 0 numara istiyorsak boş bir liste ve bize verilen üreticiyi döndürüyoruz.
Diğer herhangi bir sayıdaki rastgele değerler için, önce bir rastgele sayı ve yeni bir oluşturucu elde ederiz. Bu head olacak.
O zaman tail'in yeni oluşturucu ile üretilen *n - 1* sayı olacağını söylüyoruz. Ardından, head ve listenin geri kalanını ve
*n - 1* rastgele sayıları elde ederek elde ettiğimiz son oluşturucuyu döndürürüz.

Ya bir çeşit aralıkta rastgele bir değer istiyorsak? Şimdiye kadarki tüm rastgele tam sayılar aşırı derecede büyük ya da küçüktü.
Ya bir zar atmak istersek? Bu amaçla `randomR` kullanıyoruz. `randomR :: (RandomGen g, Random a) :: (a, a) -> g -> (a, g)` türünde bir tür vardır,
yani bir tür `random` gibi, yalnızca ilk parametresi olarak alır alt ve üst sınırları ve üretilen nihai değeri belirleyen bir çift değer bu sınırlar içinde olacaktır.

~~~~ {.haskell: .ghci name="code"}
ghci> randomR (1,6) (mkStdGen 359353)  
(6,1494289578 40692)  
ghci> randomR (1,6) (mkStdGen 35935335)  
(3,1250031057 40692)  
~~~~

Ayrıca, tanımlı aralıklarımız içinde rastgele değerler akışı üreten `randomR`'lar da vardır. Şuna bir bak:

~~~~ {.haskell: .ghci name="code"}
ghci> take 10 $ randomRs ('a','z') (mkStdGen 3) :: [Char]  
"ndkxbvmomg"  
~~~~

Güzel, süper gizli bir şifre gibi görünüyor.

Kendinize soruyor olabilirsiniz, bu bölümün I/O ile ne alakası var? Şu ana kadar I/O ile ilgili hiçbir şey yapmadık.
Şimdiye kadar rastgele sayı üreticimizi her zaman rastgele bir tamsayı ile yaparak manuel olarak yaptık.
Sorun şu ki, bunu gerçek programlarımızda yaparsak, her zaman aynı rastgele sayıları döndürürler ki bu bizim için iyi değildir.
Bu nedenle `System.Random`, bir tür `IO StdGen`'e sahip `getStdGen` I/O eylemini sunar. Programınız başladığında,
sistemden iyi bir rasgele sayı oluşturucu ister ve bunu küresel bir oluşturucu olarak saklar. getStdGen, onu bir şeye bağladığınızda size küresel rastgele oluşturucu getirir.

İşte rastgele bir string oluşturan basit bir program.

~~~~ {.haskell: .ghci name="code"}
import System.Random  
  
main = do  
    gen <- getStdGen  
    putStr $ take 20 (randomRs ('a','z') gen)  
~~~~

~~~~ {.haskell: .ghci name="code"}
$ runhaskell random_string.hs  
pybphhzzhuepknbykxhe  
$ runhaskell random_string.hs  
eiqgcxykivpudlsvvjpg  
$ runhaskell random_string.hs  
nzdceoconysdgcyqjruo  
$ runhaskell random_string.hs  
bakzhnnuzrkgvesqplrx  
~~~~

Yine de dikkatli olun, sadece `getStdGen`'i iki kez gerçekleştirmek sistemden aynı global oluşturucuyu iki kez isteyecektir. Eğer bunu yaparsan:

~~~~ {.haskell: .ghci name="code"}
import System.Random  
  
main = do  
    gen <- getStdGen  
    putStrLn $ take 20 (randomRs ('a','z') gen)  
    gen2 <- getStdGen  
    putStr $ take 20 (randomRs ('a','z') gen2)  
~~~~

Aynı string'i iki kez yazdıracaksınız! İki farklı uzunlukta 20 string'i elde etmenin bir yolu,
sonsuz bir akış oluşturmak ve ardından ilk 20 karakteri alıp bunları bir satırda yazdırmak ve ardından 20 karakterlik ikinci seti alıp ikinci satıra yazdırmaktır.
Bunun için `Data.List`'teki `splitAt` fonksiyonunu kullanabiliriz; bu, bir listeyi bir index'e böler ve ilk bölümü birinci bileşen,
ikinci bölümü ikinci bileşen olarak içeren bir demet döndürür.

~~~~ {.haskell: .ghci name="code"}
import System.Random  
import Data.List  
  
main = do  
    gen <- getStdGen  
    let randomChars = randomRs ('a','z') gen  
        (first20, rest) = splitAt 20 randomChars  
        (second20, _) = splitAt 20 rest  
    putStrLn first20  
    putStr second20  
~~~~

Başka bir yol da, mevcut rastgele oluşturucumuzu iki oluşturucuya ayıran `newStdGen` eylemini kullanmaktır.
Global rastgele oluşturucuyu bunlardan biriyle günceller ve sonuç olarak diğerini de kapsüller.

~~~~ {.haskell: .ghci name="code"}
import System.Random  
  
main = do     
    gen <- getStdGen     
    putStrLn $ take 20 (randomRs ('a','z') gen)     
    gen' <- newStdGen  
    putStr $ take 20 (randomRs ('a','z') gen') 
~~~~

`newStdGen`'i bir şeye bağladığımızda sadece yeni bir rastgele oluşturucu elde etmekle kalmayız, aynı zamanda global olan da güncellenir,
yani tekrar `getStdGen` yaparsak ve onu bir şeye bağlarsak, `gen` ile aynı olmayan bir oluşturucu elde ederiz.

İşte kullanıcının hangi sayıyı düşündüğünü tahmin etmesini sağlayacak küçük bir program.

~~~~ {.haskell: .ghci name="code"}
import System.Random  
import Control.Monad(when)  
  
main = do  
    gen <- getStdGen  
    askForNumber gen  
  
askForNumber :: StdGen -> IO ()  
askForNumber gen = do  
    let (randNumber, newGen) = randomR (1,10) gen :: (Int, StdGen)  
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number   
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        askForNumber newGen  
~~~~

![jackofdiamonds](../img/jackofdiamonds.png)
Bir rastgele sayı üretici alan ve kullanıcıyı bir sayı için uyaran ve doğru tahmin edip etmediğini söyleyen bir I/O eylemi döndüren `askForNumber` fonksiyonunu yaparız.
Bu fonksiyonda, parametre olarak aldığımız oluşturucuya bağlı olarak önce rastgele bir sayı ve yeni bir üretici oluşturup onlara `randNumber` ve `newGen` diyoruz.
Üretilen sayının `7` olduğunu varsayalım. Sonra kullanıcıya hangi sayıyı düşündüğümüzü tahmin etmesini söyleriz. `getLine` gerçekleştiririz ve sonucunu `numberString`'e bağlarız. Kullanıcı `7` girdiğinde, `numberString` `"7"` olur. Daha sonra, kullanıcının girdiği string'in boş bir string olup olmadığını kontrol etmek için `when` kullanırız.
Eğer öyleyse, programı etkin bir şekilde sonlandıran boş bir `return ()` I/O eylemi gerçekleştirilir. Değilse, orada *do* block işleminden oluşan eylem gerçekleştirilir.
Bir sayıya dönüştürmek için `numberString`'de `read`'i kullanırız, bu nedenle `number` artık `7`'dir.

**Affedersiniz!** Eğer kullanıcı bize burada `read`'in okuyamayacağı bir girdi verirse (`"haha"` gibi), programımız çirkin bir hata mesajı ile çökecektir.
Programınızın hatalı girdilerde çökmesini istemiyorsanız, bir girdiyi okuyamadığında boş bir liste döndüren `reads`'i kullanın.
Başarılı olduğunda, tek bileşen olarak istediğimiz değere sahip bir demet içeren tekli bir liste ve öteki durumda bitiremediği bir string döndürür.

Girdiğimiz sayının rastgele oluşturulan sayıya eşit olup olmadığını kontrol edip kullanıcıya uygun mesajı veriyoruz. Ve sonra özyinelemeli olarak `askForNumber` diyoruz,
sadece bu sefer sahip olduğumuz yeni oluşturucuyla, bu da bize tıpkı yaptığımız gibi bir I/O eylemi veriyor,
sadece farklı bir oluşturucuya bağlı ve biz onu gerçekleştiriyoruz.

`main`, sistemden rastgele bir oluşturucu alıp ilk eylemi elde etmek için onunla `askForNumber`'ı çağırmaktan ibarettir.

İşte uygulamalı programımız!

~~~~ {.haskell: .ghci name="code"}
$ runhaskell guess_the_number.hs  
Which number in the range from 1 to 10 am I thinking of? 4  
Sorry, it was 3  
Which number in the range from 1 to 10 am I thinking of? 10  
You are correct!  
Which number in the range from 1 to 10 am I thinking of? 2  
Sorry, it was 4  
Which number in the range from 1 to 10 am I thinking of? 5  
Sorry, it was 10  
Which number in the range from 1 to 10 am I thinking of?  
~~~~

Aynı programı yapmanın başka bir yolu da şudur:

~~~~ {.haskell: .ghci name="code"}
import System.Random  
import Control.Monad(when)  
  
main = do  
    gen <- getStdGen  
    let (randNumber, _) = randomR (1,10) gen :: (Int, StdGen)     
    putStr "Which number in the range from 1 to 10 am I thinking of? "  
    numberString <- getLine  
    when (not $ null numberString) $ do  
        let number = read numberString  
        if randNumber == number  
            then putStrLn "You are correct!"  
            else putStrLn $ "Sorry, it was " ++ show randNumber  
        newStdGen  
        main  
~~~~

Önceki sürüme çok benziyor, sadece bir üreteci alan ve sonra kendisini yeni güncellenmiş oluşturucu ile özyinelemeli olarak çağıran bir fonksiyon yapmak yerine,
tüm işi main olarak yapıyoruz. Kullanıcıya tahminlerinde doğru olup olmadıklarını söyledikten sonra global oluşturucuyu güncelliyoruz ve ardından tekrar main diyoruz.
Her iki yaklaşım da geçerlidir, ancak main de daha az işlem yaptığı ve ayrıca bize kolayca yeniden kullanabileceğimiz bir fonksiyon sağladığı için ilkini daha çok seviyorum.


Bytestrings
-----------

![chainchomp](../img/chainchomp.png)
Listeler havalı ve kullanışlı bir veri yapısıdır. Şimdiye kadar, onları hemen hemen her yerde kullandık.
Bunlar üzerinde çalışan çok sayıda fonksiyon vardır ve Haskell'in tembelliği, listeleri filtering ve mapping için diğer dillerin döngülerini değiştirmemize izin verir,
çünkü değerlendirme yalnızca gerçekten ihtiyaç duyulduğunda gerçekleşir, bu nedenle sonsuz listeler gibi şeyler
(ve sonsuz listelerin sonsuz listeleri bile!) bizim için sorun değil.
Bu nedenle listeler, standart girişten okurken veya dosyalardan okurken akışları temsil etmek için de kullanılabilir.
Sadece ihtiyaç duyulduğunda erişilebilse bile, bir dosyayı açıp bir string olarak okuyabiliriz.

Ancak, dosyaları string'ler olarak işlemenin bir dezavantajı vardır: yavaş olma eğilimindedir. Bildiğiniz gibi `String`, `[Char]` ile eşanlamlı bir türdür.
`Char`'ların sabit bir boyutu yoktur, çünkü Unicode'dan bir karakteri temsil etmek için birkaç byte gerekir.
Dahası, listeler gerçekten tembeldir. `[1,2,3,4]` gibi bir listeniz varsa, sadece tamamen gerekli olduğunda değerlendirilecektir.
Yani tüm liste bir tür liste vaadidir. `[1,2,3,4]`'ün `1: 2: 3: 4: []` için sözdizimsel şeker olduğunu unutmayın.
Listenin ilk öğesi zorla değerlendirildiğinde (örneğin yazdırarak), listenin geri kalanı `2: 3: 4: []` hala sadece bir liste vaadidir ve bu böyle devam eder.
Bu nedenle listeleri, bir sonraki öğenin gerçekten gerekli olduğunda ve onunla birlikte, ondan sonraki öğenin vaadine teslim edileceğine dair sözler olarak düşünebilirsiniz.
Basit bir sayı listesinin bir dizi vaat olarak işlenmesinin dünyadaki en verimli şey olmayabileceği sonucuna varmak büyük bir zihinsel adım gerektirmez.

Bu ek yük bizi çoğu zaman rahatsız etmiyor, ancak büyük dosyaları okurken ve onları işlerken bir sorun haline geliyor. Bu nedenle Haskell'de **bytestrings** var.
Bytestrings benzer listelerdir, yalnızca her eleman bir byte (veya 8 bit) boyutundadır. Tembellikle başa çıkma biçimleri de farklı.

Bytestrings'in iki çeşidi vardır: strict ve layz olanlar. Strict bytestrings `Data.ByteString`'de bulunur ve laziness tamamen ortadan kaldırır.
Söz konusu hiçbir promise yok; strict bytestring, bir array'deki bir dizi byte'ı temsil eder. Sonsuz strict bytestrings gibi şeylere sahip olamazsınız.
Strict bytestring'in ilk byte'ını değerlendirirseniz, onu bir bütün olarak değerlendirmelisiniz.
Bunun tersi, daha az ek yükün olmasıdır, çünkü dahil olan hiçbir şey (promise için teknik terim) yoktur.
Dezavantajı, hafızanızı daha hızlı doldurmalarıdır çünkü aynı anda hafızaya okunurlar.

Diğer bytestrings çeşidi `Data.ByteString.Lazy`'de bulunur. Lazy'ler, ancak listeler kadar lazy değiller.
Daha önce de söylediğimiz gibi, bir listede elemanlar kadar thunks vardır. Bazı amaçlar için onları yavaşlatan da budur.
Lazy bytestrings farklı bir yaklaşım benimsiyor - chunk'lar halinde saklanıyorlar (thunks ile karıştırılmamalıdır!), Her yığın 64K boyutundadır.
Ondan sonra, diğer chunk'lar için sadece bir promise. Lazy bytestrings, 64K boyutunda strict bytestrings listesi gibidir.
Bu harika çünkü bellek kullanımının fırlamasına neden olmayacak ve 64K muhtemelen CPU'nuzun L2 önbelleğine tam olarak sığacak.

`Data.ByteString.Lazy` [belgelerine](https://hackage.haskell.org/package/bytestring-0.11.1.0/docs/Data-ByteString-Lazy.html) bakarsanız, `Data.List`'teki ile aynı ada sahip birçok fonksiyona sahip olduğunu görürsünüz,
yalnızca tür imzalarında `[a]` yerine `ByteString` ve bunlarda `a` yerine `Word8` bulunur.
Aynı isme sahip fonksiyonlar, çoğunlukla listelerde çalışan fonksiyonlarla aynı fonksiyonu görür.
İsimler aynı olduğu için, bir script içinde qualified import yapacağız ve sonra bu script bytestrings ile oynamak için GHCI'ye yükleyeceğiz.

~~~~ {.haskell: .ghci name="code"}
import qualified Data.ByteString.Lazy as B  
import qualified Data.ByteString as S  
~~~~

`B`'nin lazy bytestring türleri ve fonksiyonları varken, `S`'nin strict olanları vardır. Çoğunlukla lazy versiyonu kullanacağız.

`pack` fonksiyonu `pack :: [Word8] -> ByteString` tür imzasına sahiptir. Bunun anlamı, `Word8` türünde bir byte listesi alması ve bir `ByteString` döndürmesidir.
Bunu, lazy olan bir listeyi almak ve daha az lazy hale getirmek olarak düşünebilirsiniz, böylece yalnızca 64K aralıklarla lazy olur.

Bu `Word8` türü ile anlaşma nedir? Evet, tıpkı `Int` gibi, sadece 0-255 gibi çok daha küçük bir aralığı var. 8 bit'lik bir sayıyı temsil eder.
Ve tıpkı `Int` gibi, `Num` tür sınıfında. Örneğin, 5 değerinin polimorfik olduğunu biliyoruz, çünkü herhangi bir sayı türü gibi davranabilir.
Eh, aynı zamanda `Word8` türünü de alabilir. Eh, aynı zamanda Word8 türünü de alabilir.

~~~~ {.haskell: .ghci name="code"}
ghci> B.pack [99,97,110]  
Chunk "can" Empty  
ghci> B.pack [98..120]  
Chunk "bcdefghijklmnopqrstuvwx" Empty  
~~~~

Gördüğünüz gibi, genellikle `Word8` için çok fazla endişelenmenize gerek yok, çünkü yazı sistemi sayıların bu türü seçmesini sağlayabilir.
`336` gibi büyük bir sayıyı `Word8` olarak kullanmaya çalışırsanız, sadece `80`'e sarılır.

`ByteString`'e yalnızca bir avuç değer sığdırdık, böylece tek bir parçaya sığarlar. `Empty`, listeler için `[]` gibidir.

`unpack`, `pack`'in ters fonksiyondur. Bir bytestring alır ve onu bir byte listesine dönüştürür.

`fromChunks`, strict bytestrings listesini alır ve bunu lazy bytestring'e dönüştürür. `toChunks` bir lazy bytestring alır ve bunu strict olanların listesine dönüştürür.

~~~~ {.haskell: .ghci name="code"}
ghci> B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack [46,47,48]]  
Chunk "()*" (Chunk "+,-" (Chunk "./0" Empty))  
~~~~

Bu, çok sayıda küçük strict bytestrings'iniz varsa ve bunları önce bellekte tek bir büyük strict bytestrings içinde birleştirmeden
verimli bir şekilde işlemek istiyorsanız iyidir.

`:`'nin bytestring sürümüne `cons` denir. Bir byte ve bir bytestring alır ve byte'ı başa koyar.
Yine de lazy'dir, bu yüzden bytestring'deki ilk chunk dolu olmasa bile yeni bir chunk oluşturacaktır.
Bu nedenle, bir bytestring'in başlangıcına çok fazla byte ekleyecekseniz `cons`, `cons'` un katı(strict) versiyonunu kullanmak daha iyidir.

~~~~ {.haskell: .ghci name="code"}
ghci> B.cons 85 $ B.pack [80,81,82,84]  
Chunk "U" (Chunk "PQRT" Empty)  
ghci> B.cons' 85 $ B.pack [80,81,82,84]  
Chunk "UPQRT" Empty  
ghci> foldr B.cons B.empty [50..60]  
Chunk "2" (Chunk "3" (Chunk "4" (Chunk "5" (Chunk "6" (Chunk "7" (Chunk "8" (Chunk "9" (Chunk ":" (Chunk ";" (Chunk "<"  
Empty))))))))))  
ghci> foldr B.cons' B.empty [50..60]  
Chunk "23456789:;<" Empty  
~~~~

Gördüğünüz gibi `empty`, boş bir bytestring yapar. `cons` ve `cons'` arasındaki farkı görüyor musunuz? `foldr` ile boş bir bytestring ile başladık ve
ardından sağdaki sayılar listesinin üzerinden geçerek her sayıyı bytestring'in başına ekledik. 
`cons` kullandığımızda, her byte için bir chunk elde ettik ve bu da amacımıza aykırıdır.

Aksi takdirde, bytestring modülleri `Data.List`'tekilere benzer bir fonksiyon yüküne sahiptir;
`head`, `tail`, `init`, `null`, `length`, `map`, `reverse`, `foldl`, `foldr`, `concat`, `takeWhile`, `filter` , vb.

Aynı adı taşıyan ve `System.IO`'da bulunan bazı fonksiyonlarla aynı davranan fonksiyonlara da sahiptir, yalnızca `String`'ler `ByteStrings` ile değiştirilir.
Örneğin, `System.IO`'daki `readFile` fonksiyonu `readFile :: FilePath -> IO String` türüne sahipken,
bytestring modüllerinden `readFile` `readFile :: FilePath -> IO ByteString` türüne sahiptir. 
Dikkat edin, katı bytestrings kullanıyorsanız ve bir dosyayı okumaya çalışırsanız, bir kerede onu belleğe okuyacaktır!
Lazy bytestrings ile, onu düzgün chunk'lar halinde okuyacaktır.

Komut satırı argümanları olarak iki dosya adı alan ve ilk dosyayı ikinci dosyaya kopyalayan basit bir program yapalım.
`System.Directory`'nin `copyFile` adında bir fonksiyonu olduğunu unutmayın, ancak yine de kendi dosya kopyalama fonksiyonumuzu ve programımızı uygulayacağız.

~~~~ {.haskell: .ghci name="code"}
import System.Environment  
import qualified Data.ByteString.Lazy as B  
  
main = do  
    (fileName1:fileName2:_) <- getArgs  
    copyFile fileName1 fileName2  
  
copyFile :: FilePath -> FilePath -> IO ()  
copyFile source dest = do  
    contents <- B.readFile source  
    B.writeFile dest contents  
~~~~

İki `FilePath` alan (unutmayın, `FilePath` sadece `String` ile eşanlamlıdır) ve bytestring kullanarak bir dosyayı diğerine kopyalayacak bir I/O eylemi döndüren
kendi fonksiyonumuzu oluşturuyoruz. `main` fonksiyonda, daha sonra gerçekleştirilen I/O eylemini elde etmek için argümanları alır ve onlarla fonksiyonumuzu çağırırız.

~~~~ {.haskell: .ghci name="code"}
$ runhaskell bytestringcopy.hs something.txt ../../something.txt  
~~~~

Bytestrings kullanmayan bir programın aynen böyle görünebileceğine dikkat edin, tek fark, `readFile` ve `writeFile` yerine `B.readFile` ve `B.writeFile` kullanmamızdır.
Çoğu zaman, normal string'leri kullanan bir programı, yalnızca gerekli içe aktarmaları yaparak ve ardından nitelenmiş modül adlarını
bazı fonksiyonların önüne koyarak bytestrings kullanan bir programa dönüştürebilirsiniz. 
Bazen, yazdığınız fonksiyonları string'ler üzerinde çalışacak şekilde dönüştürmeniz gerekir, böylece bytestrings üzerinde çalışırlar, ancak bu zor değildir.

Çok fazla veriyi string'lere okuyan bir programda daha iyi performansa ihtiyaç duyduğunuzda, yan testlere bir şans verin,
sizin tarafınızdan çok az çabayla bazı iyi performans artışları elde etme olasılığınız vardır. 
Genelde programları normal string'ler kullanarak yazıyorum ve ardından performans tatmin edici değilse bunları bytestrings'e dönüştürüyorum.


İstisnalar
---------

Tüm dillerin prosedürleri, fonksiyonları ve bir şekilde başarısız olabilecek kod parçaları vardır. Bu sadece hayatın bir gerçeği.
Farklı dillerin bu başarısızlıkları ele almak için farklı yolları vardır. C'de, döndürülen bir fonksiyonun normal bir değer gibi değerlendirilmemesi gerektiğini
belirtmek için genellikle bazı anormal dönüş değerleri (-1 veya boş gösterici gibi) kullanırız.
Öte yandan Java ve C#, başarısızlığı gidermek için istisnalar(exceptions) kullanma eğilimindedir. 
Bir istisna atıldığında, kontrol akışı bir miktar temizleme yapan tanımladığımız bir koda atlar ve sonra belki istisnayı yeniden fırlatır,
böylece başka bir hata işleme kodu başka şeylerle ilgilenebilir.

Haskell'in çok iyi bir tür sistemi var. Cebirsel veri yapıları `Maybe` ve `Either` gibi türlere izin verir ve bu türlerin değerlerini orada
olabilecek veya olmayabilecek sonuçları temsil etmek için kullanabiliriz. C'de, örneğin başarısızlık durumunda `-1` döndürmek tamamen bir konvansiyon meselesidir.
Sadece insanlar için özel bir anlamı vardır. Dikkatli olmazsak, bu anormal değerleri sıradan değerler olarak ele alabiliriz ve sonra bunlar,
kodumuzda hasara ve dehşete neden olabilir. Haskell'in tür sistemi bize bu açıdan çok ihtiyaç duyulan bir güvenlik sağlar.
Bir `a -> Maybe b` fonksiyonu, `Just` ile sarılmış bir `b` üretebileceğini veya `Nothing` döndürebileceğini açıkça belirtir.
Tür, sadece `a -> b`'den farklıdır ve bu iki fonksiyonu birbirinin yerine kullanmaya çalışırsak, derleyici bize şikayet edecektir.

Başarısız hesaplamaları destekleyen ifade edici türlere sahip olmasına rağmen, Haskell hala istisnalar için desteğe sahiptir, çünkü I/O bağlamlarında daha mantıklıdırlar.
Dış dünya ile uğraşırken pek çok şey ters gidebilir çünkü bu çok güvenilmezdir. Örneğin, bir dosyayı açarken birçok şey ters gidebilir.
Dosya kilitli olabilir, hiç bulunmayabilir veya sabit disk sürücüsü veya orada hiç bir şey olmayabilir. 
Bu nedenle, böyle bir hata oluştuğunda kodumuzun bazı hata işleme kısmına atlayabilmek iyidir.

Tamam, yani I/O kodu (yani saf olmayan(impure) kod) istisnalar atabilir. Mantıklı. Peki ya saf (pure) kod? Eh, istisnalar da atabilir.
`div` ve `head` fonksiyonunu düşünün. Sırasıyla `(Integral a) => a -> a -> a` ve `[a] -> a` türleri vardır.
Dönüş türlerinde `Maybe` veya `Eiter` yoktur ve yine de ikisi de başarısız olabilir!
Sıfıra bölmeye çalışırsanız yüzünüzde `div` patlar ve boş bir liste verdiğinizde `head` öfke nöbeti geçirir.

~~~~ {.haskell: .ghci name="code"}
ghci> 4 `div` 0  
*** Exception: divide by zero  
ghci> head []  
*** Exception: Prelude.head: empty list  
~~~~

Saf kod istisnalar atabilir, ancak bunlar yalnızca kodumuzun I/O kısmında yakalanabilirler (main'e giren bir do bloğunun içindeyken).
Bunun nedeni, herhangi bir şeyin saf kodda ne zaman (veya değerlendirilip değerlendirilmeyeceğini) bilmemenizdir,
çünkü tembeldir ve iyi tanımlanmış bir yürütme sırasına sahip değildir, oysa I/O kodu vardır.

Daha önce, programımızın I/O bölümünde olabildiğince az zaman geçirmemiz gerektiğinden bahsetmiştik.
Programımızın mantığı çoğunlukla saf fonksiyonlarımızda bulunmalıdır, çünkü bunların sonuçları sadece fonksiyonların çağrıldığı parametrelere bağlıdır.
Saf fonksiyonlarla uğraşırken, yalnızca bir fonksiyonun ne döndürdüğünü düşünmeniz gerekir, çünkü başka bir şey yapamaz.
Bu hayatınızı kolaylaştırır. I/O'da biraz mantık kullanamak gerekli olsa da (dosya açma ve benzeri), tercihen minimumda tutulmalıdır.
Saf fonksiyonlar varsayılan olarak tembeldir, bu da ne zaman değerlendirileceklerini bilmediğimiz ve gerçekten önemli olmaması gerektiği anlamına gelir.
Bununla birlikte, saf fonksiyonlar istisnalar atmaya başladığında, değerlendirildikleri zaman önemlidir.
Bu nedenle, kodumuzun I/O kısmında yalnızca saf fonksiyonlardan atılan istisnaları yakalayabiliriz. 
Ve bu kötü, çünkü I/O bölümünü olabildiğince küçük tutmak istiyoruz. Ancak, bunları kodumuzun I/O kısmında yakalayamazsak programımız çöküyor. Çözüm?
İstisnaları ve saf kodu karıştırmayın. Haskell'in güçlü tür sisteminden yararlanın ve başarısız olmuş olabilecek sonuçları temsil etmek için
`Either` ve `Maybe` gibi türleri kullanın.

Bu yüzden şimdilik sadece I/O istisnalarının nasıl kullanılacağına bakacağız. 
I/O istisnaları, `main`'in parçası olan bir I/O eyleminde dış dünya ile iletişim kurarken bir şeyler ters gittiğinde ortaya çıkan istisnalardır.
Örneğin, bir dosyayı açmayı deneyebiliriz ve sonra dosyanın silinmiş veya başka bir şey olduğu ortaya çıkar.
Adı kendisine komut satırı argümanı olarak verilen bir dosyayı açan ve bize dosyanın kaç satırı olduğunu söyleyen bu programa bir göz atın.

~~~~ {.haskell: .ghci name="code"}
import System.Environment  
import System.IO  
  
main = do (fileName:_) <- getArgs  
          contents <- readFile fileName  
          putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"
~~~~

Çok basit bir program. `getArgs` I/O eylemini gerçekleştiriyoruz ve listedeki ilk string'i `fileName`'e bağlarız.
Daha sonra bu dosyanın içeriğini `contents` olarak adlandırıyoruz. Son olarak, satırların bir listesini almak için bu içeriklere `lines` uygularız ve
ardından bu listenin length'ini alır ve bu sayının string temsilini elde etmek için onu `show`'a veririz.
Beklendiği gibi çalışıyor, ancak ona var olmayan bir dosyanın adını verdiğimizde ne oluyor?

~~~~ {.haskell: .ghci name="code"}
$ runhaskell linecount.hs i_dont_exist.txt  
linecount.hs: i_dont_exist.txt: openFile: does not exist (No such file or directory)  
~~~~

Aha, GHC'den bize dosyanın olmadığını söyleyen bir hata alıyoruz. Programımız çöküyor. Ya dosya yoksa daha güzel bir mesaj yazdırmak istersek?
Bunu yapmanın bir yolu, dosyayı açmaya çalışmadan önce `System.Directory`'deki `doesFileExist` fonksiyonunu kullanarak dosyanın var olup olmadığını kontrol etmektir.

~~~~ {.haskell: .ghci name="code"}
import System.Environment  
import System.IO  
import System.Directory  
  
main = do (fileName:_) <- getArgs  
          fileExists <- doesFileExist fileName  
          if fileExists  
              then do contents <- readFile fileName  
                      putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
              else do putStrLn "The file doesn't exist!"  
~~~~

`fileExists <- doesFileExist fileName` yaptık çünkü `doesFileExist` bir `doesFileExist :: FilePath -> IO Bool` türüne sahip,
bu da dosyanın var olup olmadığını bize bildiren bir boole değerine sahip bir I/O eylemi döndürdüğü anlamına gelir.
`doFileExist`'i doğrudan bir if ifadesinde kullanamayız.

Buradaki başka bir çözüm, istisnaları kullanmak olacaktır. Bunları bu bağlamda kullanmak tamamen kabul edilebilir.
Var olmayan bir dosya, I/O'dan kaynaklanan bir istisnalardır, bu nedenle onu I/O'da yakalamak iyi ve zekidir.

İstisnaları kullanarak bunun üstesinden gelmek için, `System.IO.Error`'daki `catch` fonksiyonundan yararlanacağız.
Türü `catch :: IO a -> (IOError -> IO a) -> IO a` şeklindedir. İki parametre alır. İlki bir I/O eylemidir. Örneğin, bir dosyayı açmaya çalışan bir I/O eylemi olabilir.
İkincisi, sözde handler'dır. `catch`'e geçirilen ilk I/O eylemi bir I/O istisnayı atarsa, bu istisna handler'a iletilir ve bu da ne yapılacağına karar verir.
Dolayısıyla, nihai sonuç, ilk parametreyle aynı şekilde davranacak veya ilk I/O eylemi bir istisna atarsa handler'ın söylediği şeyi yapacak bir I/O eylemidir.

![puppy](../img/puppy.png)
Java veya Python gibi dillerde *try-catch* bloklarına aşinaysan, `catch` fonksiyonu onlara benzer. İlk parametre denenecek şeydir,
tıpkı diğer zorunlu dillerdeki *try* bloğundaki şeyler gibi. İkinci parametre, tıpkı çoğu catch bloğunun istisnalar alması gibi,
daha sonra ne olduğunu görmek için inceleyebileceğiniz bir istisna alan handler'dır. handler, bir istisna atılırsa çağrılır.

Handler, bir I/O istisnanın oluştuğunu belirten bir değer olan `IOError` türünde bir değer alır. Ayrıca, atılan istisnanın türü ile ilgili bilgileri de taşır.
Bu türün nasıl uygulandığı, dilin kendisinin uygulanmasına bağlıdır; bu, `IO something` türündeki değerlerle desen eşleştiremeyeceğimiz gibi,
`IOError` türündeki değerleri bunlarla desen eşleştirerek inceleyemeyeceğimiz anlamına gelir.

Öyleyse yeni arkadaşımız `catch`'i kullanalım!

~~~~ {.haskell: .ghci name="code"}
import System.Environment  
import System.IO  
import System.IO.Error  
  
main = toTry `catch` handler  
              
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
  
handler :: IOError -> IO ()  
handler e = putStrLn "Whoops, had some trouble!"  
~~~~

Her şeyden önce, bunu bir infix fonksiyonu olarak kullanabilmemiz için etrafına geri işaretler koyduğunu göreceksiniz, çünkü iki parametre alıyor.
Bir infix fonksiyonu olarak kullanmak onu daha okunaklı hale getirir. Dolayısıyla, ``toTry `catch` handler``, türüne iyi uyan ``catch toTry handler`` ile aynıdır.
`toTry`, gerçekleştirmeye çalıştığımız I/O eylemidir ve `handler`, bir `IOError`'u alıp bir istisna durumunda yürütülecek bir eylemi döndüren fonksiyondur.

Şuna bir bakalım:

~~~~ {.haskell: .ghci name="code"}
$ runhaskell count_lines.hs i_exist.txt  
The file has 3 lines!  
  
$ runhaskell count_lines.hs i_dont_exist.txt  
Whoops, had some trouble!    
~~~~

Handler'da, ne tür bir `IOError`'a sahip olduğumuzu kontrol etmedik. Her türlü hata için sadece `"Whoops, had some trouble!"` diyoruz.
Sadece bir handler'da her tür istisnayı yakalamak, diğer birçok dilde olduğu gibi Haskell'de kötü bir uygulamadır.
Ya bizim catch içine almak istemediğimiz başka bir istisna olursa, programı yarıda kesmek gibi bir şey olursa?
Bu nedenle, genellikle diğer dillerde de aynı şeyi yapacağız: Ne tür bir istisnamız olduğunu kontrol edeceğiz.
Yakalamak için beklediğimiz türden bir istisna ise, işimizi yaparız. Değilse, bu istisnayı vahşi doğaya geri atıyoruz.
Programımızı yalnızca mevcut olmayan bir dosyanın neden olduğu istisnaları yakalayacak şekilde değiştirelim.

~~~~ {.haskell: .ghci name="code"}
import System.Environment  
import System.IO  
import System.IO.Error  
  
main = toTry `catch` handler  
              
toTry :: IO ()  
toTry = do (fileName:_) <- getArgs  
           contents <- readFile fileName  
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"  
  
handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | otherwise = ioError e  
~~~~

Yalnızca belirli bir I/O istisna grubunu yakalamak için değiştirdiğimiz istisna dışında her şey aynı kalır.
Burada `System.IO.Error`'dan iki yeni fonksiyon kullandık - `isDoesNotExistError` ve `ioError`. `isDoesNotExistError`, `IOError` üzerinde bir predicate'dir;
bu, bir `IOError` alıp `True` veya `False` döndüren bir fonksiyon olduğu anlamına gelir, yani bir tür `isDoesNotExistError :: IOError -> Bool`.
Mevcut olmayan bir dosyanın neden olduğu bir hata olup olmadığını görmek için handler'ımıza iletilen istisnada kullanırız.
Burada [guard](../tr/04-syntax-in-functions.md#muhafızlar-muhafızlar) sözdizimini kullanıyoruz, ancak *if else* de kullanabilirdik. Mevcut olmayan bir dosyadan kaynaklanmıyorsa, `ioError` fonksiyonu ile handler tarafından iletilen istisnayı yeniden atarız. Bir tür `ioError :: IOException -> IO a` vardır,
bu nedenle bir `IOError` alır ve onu fırlatacak bir I/O eylemi üretir. I/O eyleminin bir tür `IO a` vardır, çünkü aslında hiçbir zaman bir sonuç vermez,
bu nedenle `IO anything` olarak fonksiyon görebilir.

Bu nedenle, bir do bloğu ile yapıştırdığımız `toTry` I/O eyleminde atılan istisna, mevcut bir dosyadan kaynaklanmaz,
`toTry `catch` handler` bunu yakalar ve sonra yeniden atar. Oldukça havalı, ha?

`IOError` üzerinde hareket eden birkaç tahmin vardır ve bir koruma `True` olarak değerlendirilmezse, değerlendirme bir sonraki korumaya düşer.
`IOError` üzerinde etkili olan predicate'ler şunlardır:

- isAlreadyExistsError
- isDoesNotExistError
- isAlreadyInUseError
- isFullError
- isEOFError
- isIllegalOperation
- isPermissionError
- isUserError

Bunların çoğu oldukça açıklayıcıdır. `isUserError`, kodumuzdan istisnalar oluşturmak ve bunları bir string'te donatmak için kullanılan istisnayı yapmak için
`userError` fonksiyonunu kullandığımızda `True` olarak değerlendirilir. Örneğin, `ioError $ userError "remote computer unplugged!"` yapabilirsiniz,
ancak `userError` ile istisnaları kendiniz atmak yerine olası hataları ifade etmek için `Either` ve `Maybe` gibi türleri kullanmanız tercih edilir.

Böylece şuna benzer bir handler'ınız olabilir:

~~~~ {.haskell: .ghci name="code"}
handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | isFullError e = freeSomeSpace  
    | isIllegalOperation e = notifyCops  
    | otherwise = ioError e   
~~~~

`notifyCops` ve `freeSomeSpace`, tanımladığınız bazı I/O eylemleridir. Ölçütlerinizden herhangi birine uymuyorsa istisnaları yeniden attığınızdan emin olun,
aksi takdirde programınızın olmaması gereken bazı durumlarda sessizce başarısız olmasına neden olursunuz.

`System.IO.Error` ayrıca, hataya neden olan dosyanın handler'ının ne olduğu veya dosya adının ne olduğu gibi bazı öznitelikler için
istisnalarımızı sormamızı sağlayan fonksiyonları da export eder. Bunlar `ioe` ile başlar ve dökümantasyonda [tam listesini](https://downloads.haskell.org/~ghc/6.10.1/docs/html/libraries/base/System-IO-Error.html#3) görebilirsiniz.
Hatamıza neden olan dosya adını yazdırmak istediğimizi varsayalım. `getArgs`'dan aldığımız `fileName`'i yazdıramayız,
çünkü handler'a sadece `IOError` geçirilir ve handler başka hiçbir şey bilmiyor. Bir fonksiyon yalnızca çağrıldığı parametrelere bağlıdır.
Bu nedenle, `ioeGetFileName :: IOError -> Maybe FilePath` türüne sahip `ioeGetFileName` fonksiyonunu kullanabiliriz.
Parametre olarak bir `IOError` alır ve belki bir `FilePath` döndürür (bu sadece `String` ile eşanlamlıdır, unutmayın, bu yüzden aynı şeydir).
Temel olarak yaptığı şey, eğer yapabiliyorsa `IOError`'dan dosya yolunu çıkarmaktır. Oluşan istisnadan sorumlu olan dosya yolunu yazdırmak için programımızı değiştirelim.

~~~~ {.haskell: .ghci name="code"}
import System.Environment     
import System.IO     
import System.IO.Error     
    
main = toTry `catch` handler     
                 
toTry :: IO ()     
toTry = do (fileName:_) <- getArgs     
           contents <- readFile fileName     
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"     
    
handler :: IOError -> IO ()     
handler e     
    | isDoesNotExistError e =   
        case ioeGetFileName e of Just path -> putStrLn $ "Whoops! File does not exist at: " ++ path  
                                 Nothing -> putStrLn "Whoops! File does not exist at unknown location!"  
    | otherwise = ioError e     
~~~~

`isDoesNotExistError`'ın `True` olduğu guard'da, `ioeGetFileName`'i `e` ile çağırmak için bir *case expression* kullandık ve
ardından döndürdüğü `Maybe` değeriyle desen eşleştirme yaptık. *case expression'larını* kullanmak, genellikle yeni bir fonksiyon getirmeden
bir şeye karşı pattern matching istediğinizde kullanılır.

Tüm I/O bölümünüzde `catch` istisnaları için tek bir handler kullanmanız gerekmez.
I/O kodunuzun belirli kısımlarını `catch` ile kaplayabilir veya birkaçını `catch` ile kaplayabilir ve bunlar için farklı handler'lar kullanabilirsiniz, şöyle:

~~~~ {.haskell: .ghci name="code"}
main = do toTry `catch` handler1  
          thenTryThis `catch` handler2  
          launchRockets  
~~~~

Burada, `toTry` handler olarak `handler1`'i kullanır ve `thenTryThis` `handler2`'yi kullanır. `launchRockets`, `catch` parametresi değildir,
bu nedenle hangisi istisnalar atabilirse, `launchRockets` kendi istisnalarını handle edebilmek için dahili olarak `catch` kullanmadığı sürece programımızı çökertebilir.
Elbette `toTry`, `thenTryThis` ve `launchRockets`, *do* sözdizimi kullanılarak birbirine yapıştırılmış ve varsayımsal olarak başka bir yerde tanımlanmış I/O eylemleridir.
Bu, diğer dillerin *try-catch* bloklarına benzerdir; burada tüm programınızı tek bir *try-catch* ile çevreleyebilirsiniz veya daha ince bir yaklaşım kullanabilir
ve ne tür bir hata işlemenin nerede gerçekleştiğini kontrol etmek için kodunuzun farklı bölümlerinde farklı olanları kullanabilirsiniz.

Artık I/O istisnalarıyla nasıl başa çıkacağınızı biliyorsunuz! Saf koddan istisnalar atmak ve bunlarla uğraşmak burada ele alınmamıştır, çünkü, dediğimiz gibi,
Haskell, hataları yakalamak için I/O'ya geri dönmekten çok daha iyi yöntemler sunar. Başarısız olabilecek I/O eylemlerini birbirine yapıştırırken bile,
türlerinin `IO (Either a b)` gibi bir şey olmasını tercih ederim, yani bunlar normal I/O eylemleri ancak gerçekleştirildiklerinde ortaya çıktıkları sonuç
`Either a b` türü yani `Left a` veya `Right b`.

