# pw-block-gen
Generate a 2D block of random characters for password management.  For when a password manager is not practical.

## Usage

1. Pick a starting position, perhaps based off the site or account name or both.
2. Move by some offset.
3. Pick a direction and/or pattern and select password characters until desired password length reached.

Asks for a generation key so that the same block can be regenerated.

Example output:
```
  ABCDEFGHIJKLMNOPQRSTUVWXYZ
  --------------------------
A|rx^thePIAmifyEb#sN_qADKvmA
B|+MYWk#BxJX$YkbbevrDH##I=%Z
C|_iRLvhHAUf+NuYAVJBf@jAb@v~
D|w*Ek-@Rkr@zz!uBMO-%SoV!NML
E|Cn$RpIc@$Aorqu=TT&@XEM@JRN
F|uAq#pYa=JT$%zlHymhEUF!QGX=
G|i!As$FTn&K=@or@sEoLToeFYWI
H|fx$R+pHWsP&ukgxj-~eoslP$FG
I|*VA^@!uMGoJZmFMEU#+^VxjV*c
J|laSeaVlKfNDHzW-SNAevGxLF$$
K|FBquE%XxkTDjRQpG~uw_yAVF!$
L|TAR_oTkTygHvr%ejTzAF+-Qtbh
M|HlTMykvKnyO~MDC+sg~QacQaaz
N|PraTl-&@znfLBXGw-GnxkmgUXB
O|~GfCNZxIvxjhpO$ipULvZzeAeJ
P|Uij=ehaBylqdXlXA$NCYjK!#Eg
Q|d*UhANgQOrU$MaRZwxshU=$vsv
R|h&S%rD$WrQ&b^LhqNTHLhfK^RY
S|i~jmAVDcSsU&%bWy&^hHo-tkYK
T|TiyrehSDQJYggWpMdt*ulUsSeq
U|WPVvtj%*Fdhm!s^tlF=Z&GwhFE
V|yr*Fc$wexvqqikCOHGbcOE^a!W
W|pj$eMe$*@u!RE^dDEnyw*Dg@EB
X|oTKDoUXaGk-mPXbUkRPFv!ZKgg
Y|nCzEo^ae!jxQr_mT_~KJVDSnAu
Z|^nADvKx!d@s@z#cuIA!&XptuMF

  A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
  ---------------------------------------------------
A|_ s L j V * 0 N b G & T N + Z 8 & D 1 A A ~ F g 6 P|A
B|J R J = E y 1 x 9 * $ + k % 5 o b c j H e I # x A U|B
C|m 2 i ! b 6 s q A p 4 j Y + P L E Q E C e % F 7 K N|C
D|V S 9 k b @ = Y 1 & k p V f m 7 a b P y j V ! N M r|D
E|W 2 T M p D c W 9 v o c - u 7 E u 7 7 X a 2 C p @ o|E
F|k 5 g # T k + d p k f K E g s N 6 Q 9 g v 6 6 L y &|F
G|W L q @ f 5 + = W K @ H # G = i 9 8 G 4 ~ X g k H #|G
H|9 = k M u E x W n A C z Y g 7 # v t I I & A 0 Y v V|H
I|D 6 K G @ r O s w y J % r U d z v N Y c m 2 j Q N V|I
J|% 4 * ~ a G A - T X X W p C g ~ t K j q 1 W m g a E|J
K|- h U k k v e W z J * t W ! J _ y z ^ 1 t b 6 - Q k|K
L|+ 0 H L y f E $ o P @ K 6 l X S u E % A a F V t b m|L
M|2 U f 7 # J 5 0 x y T # C N R u M l j L f L V f a E|M
N|b 1 u $ l U @ & u W Y ! V N G c U r 7 s u c 5 v # !|N
O|* V 4 x 3 v W y % n e ^ O a 9 R 4 % r Z K E N b j J|O
P|K s y i # 1 E L y v g @ * P 8 F k t & z N q V D + v|P
Q|7 y b G - y b B T c q k i J @ v m d & Q F H $ 5 d q|Q
R|_ R S Z r X O M m ! 7 5 _ G V 5 y 4 W c h k q 1 @ $|R
S|d o j Q q 1 t r 8 d F x 5 F i y d h c 7 I g t a f g|S
T|a x D _ 3 Q j o w Y Y v v d $ W 2 3 # a Z % x S 8 P|T
U|n 0 m - S o v N g d G 6 m 2 1 j b - 7 F i c ^ ! v 9|U
V|e B t K _ J _ * x v 0 v n + 2 9 = G P Q + k _ k ^ i|V
W|z X O ~ M X k t C a B = p _ 2 ~ p i D w I j 5 & 4 !|W
X|D u l # 3 b y T c u v r K N g - a C q v P h U q A v|X
Y|s C J f S w 4 3 B I @ _ m ^ Q O ! 3 - E ^ X t W l T|Y
Z|V 7 A j F Z d r C & R 7 + ~ V O N % V C ~ E 8 k s P|Z
  ---------------------------------------------------
  A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
```

## Build Instructions

1. `stack build`
2. `stack install`
