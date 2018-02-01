##############################################################################
##
#W  nicemap.tst                Groupoids Package                 Chris Wensley
##
#Y  Copyright (C) 2000-2018, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 

gap> a4 := Group( (1,2,3), (2,3,4) );;
gap> SetName( a4, "a4" );
gap> Ga4 := SinglePieceGroupoid( a4, [-9,-8,-7] );;
gap> Display( Ga4 );
single piece groupoid: 
  objects: [ -9, -8, -7 ]
    group: a4 = <[ (1,2,3), (2,3,4) ]>
gap> a4 := Ga4!.magma;; 
gap> ga4 := GeneratorsOfGroup( a4 );;
gap> obs := ObjectList( Ga4 );;
gap> ro := obs[1];; 
gap> nobs := Length( obs );; 
gap> AGa4 := AutomorphismGroupOfGroupoid( Ga4 );; 
gap> Size( AGa4 ); 
20736
gap> nob := NiceObject(AGa4);; 
gap> nmon := NiceMonomorphism(AGa4);;
gap> aut := AutomorphismGroup( Ga4 );;
gap> genaut := GeneratorsOfGroup(aut);;
gap> Length(genaut); 
8
gap> w := genaut[1]; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,4,2) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ] ]
gap> w1 := ImageElm( nmon, w ); 
f4*f5*f6
gap> x := genaut[2]; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,3,2) : -9 -> -9], [(1,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ] ]
gap> x1 := ImageElm( nmon, x ); 
f3*f9*f10*f11
gap> y := genaut[3]; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -8 -> -8], [(2,3,4) : -8 -> -8], [() : -8 -> -7], 
      [() : -8 -> -9] ] ]
gap> y1 := ImageElm( nmon, y ); 
f2
gap> z := genaut[4]; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -8 -> -8], [(2,3,4) : -8 -> -8], [() : -8 -> -9], 
      [() : -8 -> -7] ] ]
gap> z1 := ImageElm( nmon, z ); 
f1*f2
gap> u := genaut[5]; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [(1,2,3) : -9 -> -8], 
      [() : -9 -> -7] ] ]
gap> u1 := ImageElm( nmon, u ); 
f5*f11
gap> v := genaut[6]; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [(2,3,4) : -9 -> -8], 
      [() : -9 -> -7] ] ]
gap> v1 := ImageElm( nmon, v ); 
f5^2
gap> s := genaut[7]; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [(1,2,3) : -9 -> -7] ] ]
gap> s1 := ImageElm( nmon, s ); 
f4*f9
gap> t := genaut[8]; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [(2,3,4) : -9 -> -7] ] ]
gap> t1 := ImageElm( nmon, t ); 
f4^2

gap> aperm := GroupoidAutomorphismByObjectPerm( Ga4, [-8,-9,-7] ); 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -8 -> -8], [(2,3,4) : -8 -> -8], [() : -8 -> -9], 
      [() : -8 -> -7] ] ]
gap> a := GroupHomomorphismByImages( a4, a4, 
>             [ (1,2,3), (2,3,4) ], [ (1,2,4), (1,3,4) ] );;
gap> aauto := GroupoidAutomorphismByGroupAuto( Ga4, a );
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,4) : -9 -> -9], [(1,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ] ]
gap> arays := GroupoidAutomorphismByRayShifts( Ga4, [ (), (1,4,3), (1,4,2) ] ); 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [(1,4,3) : -9 -> -8], 
      [(1,4,2) : -9 -> -7] ] ]

gap> L := [ w, x, y, z, u, v, s, t ];; 
gap> L1 := [ w1, x1, y1, z1, u1, v1, s1, t1 ];; 
gap> K := [ 1, 2, 3, 4, 5, 6, 7, 8 ];; 

gap> j := 0;;
gap> ok := true;; 
gap> while ok and j<8 do 
>        j := j+1; 
>        k := 0;
>        while ok and k<8 do 
>            k := k+1; 
>            e := L[j] * L[k]; 
>            e1 := L1[j] * L1[k]; 
>            ime := ImageElm( nmon, e ); 
>            ok := e1 = ime; 
>            Print( "e1 = ", e1, "\n" );
>            Print( [j,k], ":  ", ok, "\n" ); 
>        od; 
>    od;
e1 = f4^2*f5^2*f6^2
[ 1, 1 ]:  true
e1 = f3*f4^2*f5^2*f6^2*f9*f10*f11
[ 1, 2 ]:  true
e1 = f2*f4*f5*f6
[ 1, 3 ]:  true
e1 = f1*f2*f4*f5*f6
[ 1, 4 ]:  true
e1 = f4*f5^2*f6*f11
[ 1, 5 ]:  true
e1 = f4*f6
[ 1, 6 ]:  true
e1 = f4^2*f5*f6*f9
[ 1, 7 ]:  true
e1 = f5*f6
[ 1, 8 ]:  true
e1 = f3*f4*f5*f6*f7*f8*f11*f12
[ 2, 1 ]:  true
e1 = <identity> of ...
[ 2, 2 ]:  true
e1 = f2*f3*f9*f10*f11
[ 2, 3 ]:  true
e1 = f1*f2*f3*f9*f10*f11
[ 2, 4 ]:  true
e1 = f3*f5*f9*f10*f12
[ 2, 5 ]:  true
e1 = f3*f5^2*f9*f10*f12
[ 2, 6 ]:  true
e1 = f3*f4*f8*f9*f10*f11
[ 2, 7 ]:  true
e1 = f3*f4^2*f8*f9*f10*f11
[ 2, 8 ]:  true
e1 = f2*f4*f5*f6
[ 3, 1 ]:  true
e1 = f2*f3*f9*f10*f11
[ 3, 2 ]:  true
e1 = f2^2
[ 3, 3 ]:  true
e1 = f1
[ 3, 4 ]:  true
e1 = f2*f5*f11
[ 3, 5 ]:  false

gap> q := L[j];; 
gap> r := L[k];; 
gap> q1 := L1[j];;
gap> r1 := L1[k];;
gap> qr := q*r;
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -8 -> -8], [(1,4,3) : -8 -> -8], [(1,3,2) : -8 -> -7], 
      [(1,3,2) : -8 -> -9] ] ]
gap> qr1 := q1*r1; 
f2*f5*f11

#### make some basic checks 
gap> a := Arrow( Ga4, (), -9, -9 );;
gap> b := Arrow( Ga4, (), -9, -8 );;
gap> c := Arrow( Ga4, (1,2,3), -8, -8 );;
gap> d := Arrow( Ga4, (2,3,4), -8, -7 );;
gap> e := Arrow( Ga4, (1,2)(3,4), -7, -7 );;
gap> f := Arrow( Ga4, (1,4,2), -7, -9 );;
gap> A := [ a, b, c, d, e, f ]; 
[ [() : -9 -> -9], [() : -9 -> -8], [(1,2,3) : -8 -> -8], 
  [(2,3,4) : -8 -> -7], [(1,2)(3,4) : -7 -> -7], [(1,4,2) : -7 -> -9] ]
gap> Aw := List( A, k -> ImageElm( w, k ) ); 
[ [() : -9 -> -9], [() : -9 -> -8], [(1,4,2) : -8 -> -8], 
  [(2,3,4) : -8 -> -7], [(1,4)(2,3) : -7 -> -7], [(1,3,4) : -7 -> -9] ]
gap> Az := List( A, k -> ImageElm( z, k ) ); 
[ [() : -8 -> -8], [() : -8 -> -9], [(1,2,3) : -9 -> -9], 
  [(2,3,4) : -9 -> -7], [(1,2)(3,4) : -7 -> -7], [(1,4,2) : -7 -> -8] ]
gap> Awz := List( Aw, k -> ImageElm( z, k ) ); 
[ [() : -8 -> -8], [() : -8 -> -9], [(1,4,2) : -9 -> -9], 
  [(2,3,4) : -9 -> -7], [(1,4)(2,3) : -7 -> -7], [(1,3,4) : -7 -> -8] ]
gap> Azw := List( Az, k -> ImageElm( w, k ) ); 
[ [() : -8 -> -8], [() : -8 -> -9], [(1,4,2) : -9 -> -9], 
  [(2,3,4) : -9 -> -7], [(1,4)(2,3) : -7 -> -7], [(1,3,4) : -7 -> -8] ]
gap> Awz = Azw; 
true
gap> wz := w*z;; 
gap> B := List( A, k -> ImageElm( wz, k ) ); 
[ [() : -8 -> -8], [() : -8 -> -9], [(1,4,2) : -9 -> -9], 
  [(2,3,4) : -9 -> -7], [(1,4)(2,3) : -7 -> -7], [(1,3,4) : -7 -> -8] ]
gap> Awz = B; 
true
gap> ut := u*t; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [(1,2,3) : -9 -> -8], 
      [(2,3,4) : -9 -> -7] ] ] 
gap> wzut := wz*ut; 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(2,4,3) : -8 -> -8], [(1,4,3) : -8 -> -8], [(1,3,2) : -8 -> -9], 
      [(1,4,2) : -8 -> -7] ] ]
gap> Aut := List( A, k -> ImageElm( ut, k ) ); 
[ [() : -9 -> -9], [(1,2,3) : -9 -> -8], [(1,2,3) : -8 -> -8], 
  [(1,2)(3,4) : -8 -> -7], [(1,3)(2,4) : -7 -> -7], [(1,4,3) : -7 -> -9] ]
gap> Awzut := List( Awz, k -> ImageElm( ut, k ) ); 
[ [() : -8 -> -8], [(1,3,2) : -8 -> -9], [(1,4,2) : -9 -> -9], 
  [(2,4,3) : -9 -> -7], [(1,2)(3,4) : -7 -> -7], [() : -7 -> -8] ]
gap> C := List( A, k -> ImageElm( wzut, k ) ); 
[ [() : -8 -> -8], [(1,3,2) : -8 -> -9], [(1,4,2) : -9 -> -9], 
  [(2,4,3) : -9 -> -7], [(1,2)(3,4) : -7 -> -7], [() : -7 -> -8] ]
gap> Awzut = C; 
true
gap> Autwz := List( Aut, k -> ImageElm( wz, k ) ); 
[ [() : -8 -> -8], [(1,4,2) : -8 -> -9], [(1,4,2) : -9 -> -9], 
  [(1,4)(2,3) : -9 -> -7], [(1,2)(3,4) : -7 -> -7], [(1,3,2) : -7 -> -8] ]
gap> Autwz = C; 
false
gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 

#############################################################################
##
#E  nicemap.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
