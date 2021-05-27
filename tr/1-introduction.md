Başlangıç
============

Bu eğitim hakkında
------------------

**Learn You a Haskell for Great Good**'a hoş geldiniz. Bunu okuyorsanız, Haskell öğrenmek istiyorsunuz. Pekala, doğru yere geldiniz, ama önce bu eğitim hakkında biraz konuşalım.

Bunu yazmaya karar verdim çünkü kendi Haskell bilgimi pekiştirmek istedim ve Haskell'de yeni olan insanların benim bakış açımdan öğrenmelerine yardımcı olabileceğimi düşündüm.
Haskell hakkında internette dolaşan epeyce öğretici var. Haskell'de işe başladığımda, tek bir kaynaktan öğrenmedim. 
Bunu birkaç farklı öğretici ve makale okuyarak öğrendim çünkü her biri bir şeyi diğerinden farklı bir şekilde açıkladı.
Çeşitli kaynaklardan geçerek parçaları bir araya getirebildim ve hepsi yerine oturdu. Bu, Haskell'i öğrenmek için başka bir yararlı kaynak ekleme girişimidir,
böylece sevdiğiniz birini bulma şansınız artar.

![bird](../img/bird.png)

Bu eğitim, zorunlu(imperative) programlama dillerinde (C, C ++, Java, Python…) deneyimi olan ancak daha önce fonksiyonel bir dilde programlanmamış (Haskell, ML, OCaml…) kişilere yöneliktir.
Her ne kadar önemli bir programlama deneyiminiz olmasa bile, sizin gibi zeki bir kişinin Haskell'i takip edip öğrenebileceğine bahse girerim.

Freenode ağındaki #haskell kanalı, sıkışmış hissediyorsanız soru sormak için harika bir yerdir. Oradaki insanlar yeni başlayanlara karşı son derece iyi, sabırlı ve anlayışlılar.

Haskell'i nihayet kavramadan önce yaklaşık 2 kez öğrenmeyi başaramadım çünkü hepsi bana çok tuhaf geldi ve anlamadım. Ama sonra bir kez "tıklandı" ve
ilk engelin üstesinden geldikten sonra, oldukça yumuşak bir seyir oldu. Sanırım söylemeye çalıştığım şey şu: Haskell harika ve programlamayla ilgileniyorsanız, 
ilk başta tuhaf görünse bile gerçekten öğrenmelisiniz. Haskell'i öğrenmek, ilk kez programlamayı öğrenmek gibidir - eğlencelidir!
Sizi farklı düşünmeye zorluyor, bu da bizi bir sonraki bölüme getiriyor ...

Peki Haskell nedir?
-------------------

![fx](../img/fx.png)
Haskell, **saf olan fonksiyonel bir programlama dilidir**. Zorunlu dillerde, bilgisayara bir dizi görev vererek işleri halledersiniz ve sonra bunları çalıştırır.
Bunları yürütürken durumu değiştirebilir. Örneğin, değişken `a`'yı 5'e ayarlarsınız ve sonra bazı şeyler yapar ve sonra başka bir şeye ayarlarsınız.
Birkaç kez bazı eylemler yapmak için kontrol akış yapılarına sahipsiniz. saf fonksiyonel programlamada(purely functional programming),
bilgisayara ne yapması gerektiğini söylemezsiniz, bunun yerine ona ne olduğunu söylersiniz. Bir sayının faktöriyeli, 1'den o sayıya kadar olan tüm sayıların çarpımıdır;
bir sayı listesinin toplamı, ilk sayı artı diğer tüm sayıların toplamıdır ve bu böyle devam eder. Bunu fonksiyonlar biçiminde ifade edersiniz.
Ayrıca bir değişkeni bir şeye ve daha sonra başka bir şeye ayarlayamazsınız. Eğer `a`'nın 5 olduğunu söylerseniz, daha sonra başka bir şey olduğunu söyleyemezsiniz 
çünkü az önce 5 olduğunu söylediniz. Nesin sen, bir tür yalancı mı? Yani saf fonksiyonel dillerde, bir fonksiyonun hiçbir yan etkisi(side-effect) yoktur.
Bir fonksiyonun yapabileceği tek şey, bir şeyi hesaplamak ve sonuç olarak döndürmektir. İlk başta, bu biraz sınırlayıcı gibi görünse de aslında çok güzel sonuçları var:
Eğer bir fonksiyon aynı parametrelerle iki kez çağrılırsa, aynı sonucu döndürmesi garantidir. Buna referans şeffaflık denir ve
yalnızca derleyicinin programın davranışı hakkında akıl yürütmesine izin vermekle kalmaz, aynı zamanda bir fonksiyonun doğru olduğunu kolayca çıkarmanıza
(ve hatta kanıtlamanıza) ve ardından basit fonksiyonları birbirine yapıştırarak daha karmaşık fonksiyonlar oluşturmanıza olanak tanır.

![lazy](../img/lazy.png)Haskell **tembeldir(lazy)**. Bu, özellikle aksi belirtilmedikçe, Haskell'in size gerçekten bir sonuç göstermeye zorlanana kadar fonksiyonları yürütmeyeceği ve
bir şeyleri hesaplamayacağı anlamına gelir. Bu, referans şeffaflığı ile uyumludur ve programları,
**veriler üzerindeki bir dizi dönüşüm** olarak düşünmenize olanak tanır. Ayrıca sonsuz veri yapıları gibi harika şeylere izin verir.
Diyelim ki, `xs = [1,2,3,4,5,6,7,8]` sayılarının değişmez bir listesi ve her elemanı 2 ile çarpan ve ardından yeni bir liste döndüren bir `doubleMe` fonksiyonu var.
Listemizi zorunlu bir dilde 8 ile çarpmak isteseydik ve `doubleMe (doubleMe (doubleMe (xs)))` yapsaydık, muhtemelen listeden bir kez geçer ve bir kopya oluşturur ve
sonra onu döndürürdü. Sonra listeden iki kez daha geçer ve sonucu döndürürdü. Tembel bir dilde, sonucu size göstermeye zorlamadan bir listede `doubleMe`'yi çağırmak,
program size "Evet, evet, daha sonra yaparım!" diyecektir. Ama sonucu görmek istediğinizde, ilk `doubleMe` ikinciye sonucu şimdi istediğini söyler!
İkincisi, üçüncü ve üçüncüye gönülsüzce 1'in çarpılmış halini, yani 2 verdiğini söylüyor. İkincisi bunu alır ve ilkine 4 verir.
İlki bunu görür ve size ilk elemanın 8 olduğunu söyler. Bu nedenle, listeden yalnızca bir kez geçer ve yalnızca gerçekten ihtiyacınız olduğunda.
Bu şekilde, tembel bir dilden bir şey istediğinizde, sadece bazı başlangıç verilerini alabilir ve
sonunda istediğiniz şeye benzemesi için onu verimli bir şekilde dönüştürün ve onarın.

![boat](../img/boat.png)Haskell **statik türlüdür(statically typed)**. Programınızı derlediğinizde, derleyici hangi kod parçasının bir sayı olduğunu,
hangi kodun bir string olduğunu vb. şeyleri bilir. Bu, derleme sırasında birçok olası hatanın yakalandığı anlamına gelir. Bir sayı ve string'i bir araya getirmeye çalışırsanız,
derleyici size sızlanacaktır. Haskell, **tür çıkarımına(type inference)** sahip çok iyi bir tür sistemi kullanır.
Bu, her kod parçasını bir türle açık bir şekilde etiketlemeniz gerekmediği anlamına gelir, çünkü tür sistemi bu konuda akıllıca çok şey çözebilir.
Eğer `a = 5 + 4` derseniz, Haskell'e a'nın bir sayı olduğunu söylemenize gerek yok, bunu kendi başına anlayabilir. Tür çıkarımı, kodunuzun daha genel olmasını da sağlar.
Yaptığınız bir fonksiyon iki parametre alır ve bunları bir araya getirirse ve bunların türünü açıkça belirtmezseniz,
fonksiyon sayılar gibi davranan herhangi iki parametre üzerinde çalışacaktır.

Haskell **zarif ve özlüdür**. Haskell programları çok sayıda üst düzey kavram kullandığı için genellikle zorunlu(imperative) eşdeğerlerinden daha kısadır.
Daha kısa programların bakımı, uzun programlara göre daha kolaydır ve daha az hataya sahiptir.

Haskell, bazı **gerçekten zeki adamlar** (doktoralı) tarafından yapıldı. Haskell üzerindeki çalışmalar, 1987'de bir araştırmacı komitesinin harika
bir dil tasarlamak için bir araya gelmesiyle başladı. 2003 yılında, dilin kararlı bir sürümünü tanımlayan Haskell Raporu yayınlandı.

Dalmak için neye ihtiyacın var?
-------------------------------

Bir metin düzenleyici ve bir Haskell derleyicisi. Muhtemelen favori metin düzenleyicinizi zaten yüklediniz, bu yüzden bununla zaman kaybetmeyeceğiz.
Bu eğitimin amaçları doğrultusunda, en yaygın olarak kullanılan Haskell derleyicisi olan GHC'yi kullanacağız.
Başlamanın en iyi yolu, temelde Haskell olan ve temel parçalarla birlikte gelen [Haskell Platformunu](http://hackage.haskell.org/platform/) indirmektir.

GHC bir Haskell betiğini alabilir (genellikle bir .hs uzantısına sahiptirler) ve derleyebilir, ancak aynı zamanda betiklerle etkileşimli olarak
etkileşime girmenize izin veren etkileşimli bir moda da sahiptir. Etkileşimli olarak. Yüklediğiniz komut dosyalarından fonksiyonları çağırabilirsiniz ve
sonuçlar hemen görüntülenir. Öğrenmek için, her değişiklik yaptığınızda derlemekten ve ardından programı komut isteminden çalıştırmaktan çok daha kolay ve hızlıdır.
Etkileşimli mod, komut isteminize `ghci` yazarak çalıştırılır. `myfunctions.hs` adlı bir dosyada bazı fonksiyonlar tanımladıysanız,
bu fonksiyonları `:l myfunctions` yazarak yüklersiniz ve sonra `myfunctions.hs`'nin `ghci`'ın çağrıldığı klasörde olması koşuluyla onlarla oynayabilirsiniz.
.hs betiğini değiştirirseniz, `:l myfunctions`'ı tekrar çalıştırın veya `:r` yapın; bu, geçerli betiği yeniden yüklediği için eşdeğerdir.
Bir şeyler üzerinde çalışırken benim için olağan iş akışı, bir .hs dosyasında bazı fonksiyonları tanımlamak, yüklemek ve bunlarla uğraşmak ve
sonra .hs dosyasını değiştirmek, yeniden yüklemek vb. Bu aynı zamanda burada yapacağımız şey.
