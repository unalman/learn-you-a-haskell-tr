Bir Avuç Monad
==============

Functor'lardan ilk bahsettiğimizde, eşlenebilecek değerler için faydalı bir kavram olduklarını gördük. Ardından, belirli veri türlerinin değerlerini bağlamlara
sahip değerler olarak görmemize ve bu bağlamların anlamını korurken bu değerler üzerinde normal fonksiyonları kullanmamıza olanak tanıyan
applicative functor'ları tanıtarak bu kavramı bir adım öteye taşıdık.

Bu bölümde, applicative functor'ları güçlendiren monadlar hakkında bilgi edineceğiz, tıpkı applicative functor'ların yalnızca güçlendirilmiş functor'lar olduğu gibi.

![smugpig](../img/smugpig.png)
`Functor`'larla başladığımızda, fonksiyonları çeşitli veri türleri üzerinden eşlemenin mümkün olduğunu gördük. Bu amaçla Functor tür sınıfının tanıtıldığını ve
bize şu soruyu sorduğunu gördük: `a -> b` türünde bir fonksiyonumuz ve `f a` veri türünde bir fonksiyonumuz olduğunda,
`f b` ile sonuçlanacak şekilde bu fonksiyonu veri türü üzerinde nasıl eşleştiririz? Bir şeyi bir `Maybe a`, bir list `[a]`, bir `IO a` vb. Üzerine nasıl eşleştireceğimizi gördük.
`r -> b` türü fonksiyonları elde etmek için `a -> b` fonksiyonunu `r -> a` türündeki diğer fonksiyonların üzerine nasıl eşleştireceğimizi bile gördük.
Bir fonksiyonu bazı veri türleri üzerinde nasıl eşleyeceğimiz sorusuna cevap vermek için tek yapmamız gereken `fmap` türüne bakmaktı:

~~~~ {.haskell: .ghci name="code"}
fmap :: (Functor f) => (a -> b) -> f a -> f b  
~~~~

Ve sonra uygun `Functor` instance'ını yazarak veri türümüz için çalışmasını sağlayın.

Sonra functor'larda olası bir iyileşme gördük ve dedik ki, ya bu `a -> b` fonksiyonu zaten bir functor değerinin içine sarılmışsa? Mesela, `Just (*3)` varsa,
bunu `Just 5`'e nasıl uygularız? Ya ona Just 5 yerine Nothing uygulamak istemiyorsak?  Veya `[(*2), (+4)]`'e sahipsek, bunu `[1,2,3]`'e nasıl uygularız? Bu nasıl çalışır? Bunun için,
aşağıdaki türe yanıt almak istediğimiz `Applicative` tür sınıfı tanıtıldı:

~~~~ {.haskell: .ghci name="code"}

~~~~

~~~~ {.haskell: .ghci name="code"}

~~~~

~~~~ {.haskell: .ghci name="code"}

~~~~

~~~~ {.haskell: .ghci name="code"}

~~~~

~~~~ {.haskell: .ghci name="code"}

~~~~

~~~~ {.haskell: .ghci name="code"}

~~~~





































































































































































































































































