/*         ***           THIS PROGRAM IS BROUGHT TO YOU BY SANGAT-DORA           ***         */

List of authors :
1. Fathan Adi Pranaya / 13511027
2. Yogi Salomo Mangontang Pratama / 13511059
3. Habibie Faried / 13511069
4. Iskandar Setiadi / 13511073

Assumption:
1. Always exist coins and ladies in this maze (coins , ladies > 0)

------------------------------------------------Single Player Section------------------------------------------------
1. Rule startG
Rule ini digunakan untuk memulai permainan secara manual / single player mode.

2. Rules moveup / movedown / moveleft / moveright
Rules ini digunakan untuk menggerakan Dora ke 4 arah yang dimungkinan oleh permainan.
Setiap gerakan ini yang menentukan petak selanjutnya yang akan dilalui oleh Dora.

3. Rule statistik (untuk single player & AI mode)
Rule ini digunakan untuk  mencetak statistik dari permainan yang berlangsung.

4. Rule printlocation
Rule ini digunakan dalam mode single player untuk mencetak posisi Dora saat ini.

-----------------------------------------------------AI Section-----------------------------------------------------
1) Selayang Pandang
AI adalah sebuah bot / robot yang dapat menentukan keputusan sendiri berdasarkan
algoritma yang dibentuk oleh programmer. Jadi, dia adalah program kecil yang dapat
mengambil keputusan sendiri tanpa bantuan user.

2) Algoritma AI

Pada kesempatan kali ini, AI-nya Dora sang penjelajah menggunakan DFS-Runut Balik disertai
dengan sensor detection. Pada algoritma DFS, simpul yang terdalam terlebih dahulu yang akan dikunjungi
jika mati, maka dia akan mundur satu langkah sebelumnya dan mencari simpul terdalam lain di arah yang berbeda.

Pada awalnya, DFS pada AI ini akan mengecek atas lalu kanan lalu bawah lalu kiri. Jika tidak ada yang bisa
dilewati, maka algoritma backtrack (Runut-balik) akan terpanggil. Lalu AI akan melakukan DFS kembali pada
arah yang berbeda. Algoritma ini juga dilengkapi 4 sensor pada spek tubes. 4 sensor ini akan mengecek apakah
ada coin atau lady di sekitarnya. 

Sehingga, keputusan pengambilan langkah dengan DFS-Backtrack akan terpengaruh dengan adanya sensor. Jika
sensor bonus (coin dan lady) menyala. Maka, kemungkinan yang akan menjadi solusi adalah arah menuju coin dan lady
tersebut, walaupun DFS-Backtrack menyatakan arah yang berbeda, tetapi pada algoritma ini akan mengubah keputusan
DFS. Akan tetapi, jika AI menggunakan arah sensor lalu menuju arah yang salah (PIT atau buntu), maka akan 
diambil keputusan sensor lain atau keputusan DFS.

Semua algoritma dilakukan secara boolean-rekursif. Dengan kondisi seperti itu, diharapkan solusi akan terbangun 
apabila basisnya mencapai TRUE. Walaupun tidak menggunakan DFS Murni, namun dengan sifat rekursif yang dimiliki
oleh DFS tentu dapat membangun solusi yang dibentuk oleh algoritma lainnya (Backtrack + Sensor).


3) Pemanggilan AI
ketikkan "solve." (tanpa tanda kutip). Lalu tekan enter, maka akan keluar hasil penjelajahannya si Dora hingga
finish. Disertai dengan total movement, ladies, coin dan swamp yang dilewati. Langkah yang menuju arah PIT tidak
akan menjadi sebuah solusi. Sehingga, yang ditampilkan adalah langkah yang paling benar menuju finish.

---------------------------------------------------Print Section--------------------------------------------------
1. Rule printG
Rule ini akan mencetak semua facts dari permainan kedalam sebuah file eksternal bernama 'MyOutput.txt'.
File eksternal ini kemudian digunakan oleh aplikasi GUI untuk menganimasikan jalannya permainan.

2. Rule printPassed
Rule ini secara otomatis akan dieksekusi saat AI dijalankan. Kegunaan dari rules ini adalah mencetak
urutan langkah jalannya AI kedalam sebuah file eksternal bernama 'out.pl', yang kemudian dapat digunakan
untuk mensimulasikan ulang permainan dari titik Start sampai Finish.


@version April 2013, All Rights Reserved - Institute Technology of Bandung
Informatika - STEI ITB