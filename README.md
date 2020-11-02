# Tugas 5 Pemrograman Fungsional Gasal 2020/2021

### Pelaksana

Kevin Christian Chandra (NPM: 1706039976)

### Sumber

GitHub: [masonwr/Lambda-Interpreter: Lambda Interpreter Implemented In haskell](https://github.com/masonwr/Lambda-Interpreter)

### Deskripsi Pekerjaan

Saya melakukan modifikasi terhadap lambda interpreter dari [Winston Riley](https://github.com/masonwr) ini agar dapat menerima bilangan cacah, operator penjumlahan, dan operator perkalian, kemudian dievaluasi sebagai ekspresi lambda __Church’s Numeral__. Sebelum modifikasi oleh saya, interpreter ini belum bisa menerima angka dan operator aritmetika (akan muncul pesan tidak bisa _parse_). Saat ini hanya dapat menerima input bilangan 1 digit, operator penjumlahan sebagai operator _infix_, dan operator perkalian sebagai operator _prefix_. Input bilangan lebih dari 1 digit akan diinterpretasi sebagai aplikasi dari bilangan 1 digit kepada bilangan 1 digit lainnya.

#### Modifikasi input

Saya melakukan perubahan pada [__src/LambdaParser.hs__](./src/LambdaParser.hs) karena di sini operasi-operasi interpretasi dilakukan. Saya menambah satu “_layer_” lagi sebelum string input di-parse. _Layer_ ini akan mengubah semua bilangan, operator ‘+’, dan operator ‘*’ menjadi string ekspresi lambda menurut Church’s Numeral. Saya menggunakan metode ini karena paling sederhana dilakukan.

Pada __LambdaParser.hs__, saya tambahkan dahulu fungsi __numberToChurch__ dan __arithmeticToChurch__ untuk mengubah atom bilangan atau operator aritmetika ke dalam string ekspresi lambda. Fungsi konversi ini akan di-_reuse_ pada fungsi-fungsi replace. Ada __replaceNumberToChurch__ untuk replace 1 jenis bilangan (seluruh kemunculannya) menjadi bentuk Church’s Numeral-nya dan mengembalikan string input yang angkanya sudah dikonversi. Begitu pula __replacePlusToChurch__ dan __replaceMultToChurch__. Khusus untuk angka, saya bungkus lagi dalam __replaceNumbersToChurches__ yang akan mengkonversi angka yang berbeda-beda (daftar angka yang ditemukan sebagai list) dibandingkan __replaceNumberToChurch__ yang hanya mengubah 1 jenis angka. Kemudian seluruh fungsi replace ini saya bungkus lagi dalam __replaceArithmeticsToChurches__ untuk menjalankan semuanya sekaligus. Fungsi __replaceArithmeticsToChurches__ ini saya panggil dalam __parseLambda__ untuk menggantikan string input yang tadinya langsung di-_parse_.

#### Modifikasi Output

Untuk menampilkan kembali digit pada output, saya modifikasi [src/Terms.hs](./src/Terms.hs). Pertama saya tambahkan ___constructor___ baru pada tipe data Term, yaitu ```Digit Int```. Saya juga tambahkan Digit ke implementasi __Show__-nya. Kemudian saya tambahkan 1 layer setelah __eval__, yaitu untuk mengecek apakah _term_ output berupa Church's Numeral dan konversikan ke digit jika iya. Saya bagi jadi 3 _level_, yaitu __firstLvl..__, __sndLvl..__, dan __bodyLvl..__ karena bentuk ekspresi Church's Numeral adalah ```λfirstVar.λsndVar.body```. Pada _level_ pertama, jika berupa abstraksi maka lanjut cek _level_ kedua. Pada _level_ kedua, jika berupa abstraksi lagi maka lanjut cek _level_ ketiga (_body_). Pada level ini, rekursi terus apakah memenuhi bentuk _body_ Church's Numeral dan jumlahkan jumlah variabel ```firstVar``` yang berurutan. Hasil penjumlahan adalah bilangan digitnya. Jika ada pengecekan gagal selama rekursi maka langsung kembalikan _term_ awal (berarti bukan Church's Numeral).

Output akan menampilkan bilangan digit jika seluruh ekspresinya adalah Church's Numeral. Jika hanya substring atau subterm dari ekspresi yang merupakan Church's Numeral, akan tetap ditampilkan sebagai ekspresi lambda. Hal ini mengingat saya memperbolehkan input berupa aplikasi campuran antara Church's Numeral dengan ekspresi lambda biasa sehingga hasil akhir dapat berupa ekspresi lambda apapun.
```
\x.\y.(x (y))
akan menghasilkan: 1

\z.\x.\y.(x (y))
akan menghasilkan: (λz. (λx. (λy. (x y))))
tidak menghasilkan: (λz. 1)

\x.\y.(x (\t.t))
bukan bilangan, akan menghasilkan: (λx. (λy. (x (λt. t))))
```

### Cara Menggunakan

Cara menjalankan interpreter ini dapat dilihat di bawah ini atau pada README original di bagian bawah (namun _clone_ repositori yang ini).
``` shell
> git clone https://gitlab.cs.ui.ac.id/kevin.christian71/funpro2020-tugas5interpreter.git
> cd funpro2020-tugas5interpreter
> make
> make run
stack exec lambda-interpreter-exe
Welcome to a basic lambda interpreter.
\x.y x
(λx. (y x))
(\x.x x)(\y.y)
(λy. y)
```

Contoh input dan output lengkap penggunaan interpreter ini dapat dilihat pada file [__Input-Examples.txt__](./Input-Examples.txt). Bentuk dasar input kurang lebih mirip dengan sintaksis lambda calculus, yaitu: ```\x.\y.(x y)``` dan semacamnya. Backslash ‘\’ digunakan sebagai pengganti simbol lambda. Contoh input-output sederhana juga dapat dilihat pada README original di bawah, namun pada __Input-Examples.txt__ ada lebih banyak contoh. Poin penting dalam penggunaan di sini adalah penggunaan tanda kurung untuk membatasi _scope_.

Beberapa kasus tidak perlu kurung seperti aplikasi antara dua variabel, asalkan diberikan spasi pemisah:
```
\x.x y
\x.(x)y
keduanya menghasilkan: (λx. (x y))
```


Namun jika ekspresi sebelah kiri (ekspresi yang diberi aplikasi) bukan variabel sederhanya, selalu harus diberi tanda kurung (dan tidak perlu spasi pemisah):
```
(\x.y x)(\t.t)
menghasilkan: (y (λt. t))
```

Berikut contoh penerapan Church’s Numeral:
```
1
+0
keduanya menghasilkan: 1

2+5
menghasilkan: 7

*32
menghasilkan: 6

3 8
menghasilkan efek perpangkatan 8^3: 512
```


<br/>
<br/>

### README Original Sumber

# lambda-interpreter

Very primitive lambda interpreter.

Parser by [dmringo](https://github.com/dmringo).

With [stack](https://docs.haskellstack.org/en/stable/README/) installed:

``` shell
> git clone https://github.com/masonwr/Lambda-Interpreter.git
> cd Lambda-Interpreter
> make
> make run
stack exec lambda-interpreter-exe
Welcome to a basic lambda interpreter.
\x.y
(λx. y)
(\x.x x)(\y.y)
(λy. y)
```
