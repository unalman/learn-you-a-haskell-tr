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
Unutmayın, [fold](../tr/06-higher-order-functions.md#sadece-foldlar-ve-atlar)'larla ilgili bölümde, bir listeyi soldan sağa ya da sağdan sola bir eleman eleman geçirdiğiniz ve bir sonuç oluşturduğunuz (biriktirdiğiniz)
hemen hemen her fonksiyonun (bir sayı, bir liste, bir yığın, ne olursa olsun) bir fold ile uygulanabilir.





















































































































































































































