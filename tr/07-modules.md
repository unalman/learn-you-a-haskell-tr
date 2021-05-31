Modüller
========


Modüller yükleniyor
-------------------

![modules](../img/modules.png)
Haskell modülü, ilgili fonksiyonların, türlerin ve tür sınıflarının bir koleksiyonudur. 
Haskell programı, ana modülün diğer modülleri yüklediği ve daha sonra bir şeyler yapmak için içlerinde tanımlanan fonksiyonları kullandığı bir modüller koleksiyonudur.
Birkaç modüle bölünmüş kodun oldukça fazla avantajı vardır. Bir modül yeterince genelse, dışa aktardığı fonksiyonlar çok sayıda farklı programda kullanılabilir.
Kendi kodunuz birbirine çok fazla güvenmeyen bağımsız modüllere ayrılırsa (gevşek bir şekilde bağlı olduklarını da söyleriz), bunları daha sonra yeniden kullanabilirsiniz.
Her biri bir çeşit amaca sahip olan birkaç parçaya bölünerek kod yazma işinin tamamını daha yönetilebilir hale getirir.

Haskell standart kütüphanesi modüllere ayrılmıştır, her biri bir şekilde ilişkili olan ve bazı ortak amaca hizmet eden fonksiyonlar ve türleri içerir.
Listeleri işlemek için bir modül, eşzamanlı programlama(concurrency programming) için bir modül, karmaşık sayılarla uğraşmak için bir modül vb.
Şimdiye kadar ele aldığımız tüm fonksiyonlar, türler ve tür sınıfları, varsayılan olarak içe aktarılan Prelude modülünün bir parçasıydı.
Bu bölümde, birkaç yararlı modülü ve sahip oldukları fonksiyonları inceleyeceğiz. Ama önce, modülleri nasıl içe aktaracağımızı(import) göreceğiz.

Haskell betiğindeki modülleri içe aktarmanın sözdizimi `import <module name>` şeklindedir. Bu, herhangi bir fonksiyonu tanımlamadan önce yapılmalıdır, 
bu nedenle içe aktarmalar genellikle dosyanın en üstünde yapılır. Elbette bir komut dosyası birkaç modülü içe aktarabilir.
Her import ifadesini ayrı bir satıra koyun. Listelerle çalışmak için bir dizi yararlı fonksiyonu olan Data.List modülünü içe aktaralım ve 
bir listenin kaç benzersiz öğeye sahip olduğunu bize söyleyen bir fonksiyon oluşturmak için dışa aktardığı bir fonksiyonu kullanalım.
`Data.List`'i içe aktardığınızda, Data.List'in dışa aktardığı tüm fonksiyonlar genel ad alanında(namespace) kullanılabilir hale gelir,
bu da onları komut dosyasının neresinden çağırabileceğiniz anlamına gelir.

~~~~ {.haskell: .ghci name="code"}
import Data.List  
  
numUniques :: (Eq a) => [a] -> Int  
numUniques = length . nub  
~~~~

`Data.List`'i içe aktardığınızda, `Data.List`'in dışa aktardığı tüm fonksiyonlar genel ad alanında(namespace) kullanılabilir hale gelir, 
yani bunları komut dosyasının her yerinden çağırabilirsiniz. `nub`, `Data.List`'te tanımlanan ve bir liste alan ve yinelenen öğeleri ayıklayan bir fonksiyondur.
`length . nub` komutunu yaparak `length` ve `nub`'u oluşturmak, `\xs -> length (nub xs)` ile eşdeğer olan bir fonksiyon üretir.

GHCI kullanırken modüllerin fonksiyonlarını global ad alanına da koyabilirsiniz. 
GHCI'daysanız ve `Data.List` tarafından dışa aktarılan fonksiyonları çağırabilmek istiyorsanız, şunu yapın:

~~~~ {.haskell: .ghci name="code"}
ghci> :m + Data.List   
~~~~

GHCI içindeki birkaç modülden isimleri yüklemek istiyorsak, birkaç kez `:m +` yapmak zorunda değiliz, sadece birkaç modülü aynı anda yükleyebiliriz.

~~~~ {.haskell: .ghci name="code"}
ghci> :m + Data.List Data.Map Data.Set   
~~~~

Ancak, zaten bir modülü içe aktaran bir komut dosyası yüklediyseniz, ona erişmek için `:m +` kullanmanız gerekmez.

Bir modülden yalnızca birkaç fonksiyona ihtiyacınız varsa, yalnızca bu fonksiyonları seçerek içe aktarabilirsiniz.
`Data.List`'ten sadece `nub` ve `sort` fonksiyonlarını içe aktarmak isteseydik, şunu yapardık:

~~~~ {.haskell: .ghci name="code"}
import Data.List (nub, sort)    
~~~~

Ayrıca bir modülün birkaç seçili olanlar dışında tüm fonksiyonlarını içe aktarmayı da seçebilirsiniz. 
Bu, genellikle birkaç modülün aynı adı taşıyan fonksiyonları dışa aktardığında ve rahatsız edici olanlardan kurtulmak istediğinizde yararlıdır.
Diyelim ki `nub` adında kendi fonksiyonlarımız var ve `nub` fonksiyonu dışındaki tüm fonksiyonları `Data.List`'ten içe aktarmak istiyoruz:

~~~~ {.haskell: .ghci name="code"}
import Data.List hiding (nub)   
~~~~

İsim çatışmalarıyla baş etmenin bir başka yolu da qualified import yapmaktır. Değerleri anahtara göre aramak için bir veri yapısı(data structure) sunan `Data.Map` modülü, 
`filter` veya `null` gibi `Prelude` fonksiyonlarıyla aynı adı taşıyan bir dizi fonksiyonu dışa aktarır. 
Dolayısıyla `Data.Map`'i içeri aktardığımızda ve ardından `filter` çağırdığımızda Haskell hangi fonksiyonu kullanacağını bilemeyecek. Bunu şu şekilde çözüyoruz:

~~~~ {.haskell: .ghci name="code"}
import qualified Data.Map  
~~~~

`Data.Map`'in `filter` fonksiyonuna referans vermek istiyorsak, `Data.Map.filter` yapmamız gerekir, oysa sadece `filter` hala hepimizin bildiği ve
sevdiği normal filter'ı ifade eder.
Ancak bu modüldeki her fonksiyonun önüne `Data.Map` yazmak biraz sıkıcıdır. Bu nedenle, qualified import daha kısa bir adla yeniden adlandırabiliriz:

~~~~ {.haskell: .ghci name="code"}
import qualified Data.Map  
~~~~
























































































