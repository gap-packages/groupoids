##############################################################################
##
#W  mwohom.tst                    Gpd Package                    Chris Wensley
##
##  version 1.31, 09/11/2014   
##
#Y  Copyright (C) 2000-2014, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

## SubSection 2.1.1
gap> gpd_infolevel_saved := InfoLevel( InfoGpd );; 
gap> SetInfoLevel( InfoGpd, 0 );;
gap> tup1 := 
>      [ Tuple([m1,m2]), Tuple([m2,m1]), Tuple([m3,m4]), Tuple([m4,m3]) ];; 
gap> f1 := GeneralMappingByElements( m, m, tup1 ); 
<general mapping: m -> m >
gap> IsMagmaHomomorphism( f1 ); 
true
gap> hom1 := MagmaWithObjectsHomomorphism( M78, M78, f1, [-8,-7] ); 
magma with objects homomorphism : M78 -> M78
[ [ <mapping: m -> m >, [ -8, -7 ] ] ]
gap> [ Source( hom1 ), Range( hom1 ) ]; 
[ M78, M78 ]
gap> b87;
[m4 : -8 -> -7]
gap> im1 := ImageElm( hom1, b87 );
[m3 : -8 -> -7]
gap> i56 := IsomorphismNewObjects( M78, [-5,-6] ); 
magma with objects homomorphism : [ [ IdentityMapping( m ), [ -5, -6 ] ] ]
gap> M65 := Range( i56);; 
gap> SetName( M65, "M65" ); 
gap> j56 := InverseGeneralMapping( i56 );; 
gap> ImagesOfObjects( j56 ); 
[ -7, -8 ]
gap> ib87 := ImageElm( i56, b87 );
[m4 : -5 -> -6]
gap> comp := j56 * hom1;
magma with objects homomorphism : M65 -> M78
[ [ <mapping: m -> m >, [ -7, -8 ] ] ]
gap> ImageElm( comp, ib87 );
[m3 : -8 -> -7]
gap> M4 := UnionOfPieces( [ M78, M65 ] );;
gap> images := [ PieceImages( hom1 )[1], PieceImages( j56 )[1] ]; 
[ [ <mapping: m -> m >, [ -8, -7 ] ], [ IdentityMapping( m ), [ -7, -8 ] ] ]
gap> map4 := HomomorphismToSinglePiece( M4, M78, images ); 
magma with objects homomorphism : [ [ <mapping: m -> m >, [ -8, -7 ] ], 
  [ IdentityMapping( m ), [ -7, -8 ] ] ]
gap> ImageElm( map4, b87 ); 
[m3 : -8 -> -7]
gap> ImageElm( map4, ib87 );
[m4 : -8 -> -7]

## Section 2.2, Homomorphisms of semigroups and monoids with objects 
gap> t2 := Transformation( [2,2,4,1] );; 
gap> s2 := Transformation( [1,1,4,4] );;
gap> r2 := Transformation( [4,1,3,3] );; 
gap> sgp2 := Semigroup( [ t2, s2, r2 ] );;
gap> SetName( sgp2, "sgp<t2,s2,r2>" );
gap> ##  apparently no method for transformation semigroups available for: 
gap> ##  nat := NaturalHomomorphismByGenerators( sgp, sgp2 );  so we use: 
gap> ##  in the function flip below t is a transformation on [1..n] 
gap> flip := function( t ) 
>     local i, j, k, L, L1, L2, n; 
>     n := DegreeOfTransformation( t );  
>     L := ImageListOfTransformation( t ); 
>     if IsOddInt( n ) then 
>         n := n+1; 
>         L1 := Concatenation( L, [n] );
>     else 
>         L1 := L; 
>     fi;
>     L2 := ShallowCopy( L1 );
>     for i in [1..n] do 
>         if IsOddInt(i) then j:=i+1; else j:=i-1; fi; 
>         k := L1[j]; 
>         if IsOddInt(k) then L2[i]:=k+1; else L2[i]:=k-1; fi; 
>     od; 
>     return( Transformation( L2 ) ); 
> end;; 
gap> smap := MappingByFunction( sgp, sgp2, flip );; 
gap> ok := RespectsMultiplication( smap ); 
true
gap> [ t, Image( smap, t ) ]; 
[ Transformation( [ 1, 1, 2, 3 ] ), Transformation( [ 2, 2, 4, 1 ] ) ]
gap> [ s, Image( smap, s ) ]; 
[ Transformation( [ 2, 2, 3, 3 ] ), Transformation( [ 1, 1, 4, 4 ] ) ]
gap> [ r, Image( smap, r ) ]; 
[ Transformation( [ 2, 3, 4, 4 ] ), Transformation( [ 4, 1, 3, 3 ] ) ]
gap> SetName( smap, "smap" ); 
gap> T123 := SemigroupWithObjects( sgp2, [-13,-12,-11] );; 
gap> shom := MagmaWithObjectsHomomorphism( S123, T123, smap, [-11,-12,-13] );; 
gap> it12 := ImageElm( shom, t12 );;  [ t12, it12 ]; 
[ [Transformation( [ 1, 1, 2, 3 ] ) : -1 -> -2], 
  [Transformation( [ 2, 2, 4, 1 ] ) : -13 -> -12] ]
gap> is23 := ImageElm( shom, s23 );;  [ s23, is23 ]; 
[ [Transformation( [ 2, 2, 3, 3 ] ) : -2 -> -3], 
  [Transformation( [ 1, 1, 4, 4 ] ) : -12 -> -11] ]
gap> ir31 := ImageElm( shom, r31 );;  [ r31, ir31 ]; 
[ [Transformation( [ 2, 3, 4, 4 ] ) : -3 -> -1], 
  [Transformation( [ 4, 1, 3, 3 ] ) : -11 -> -13] ]

## Section 2.3, Homomorphisms from several pieces

## SubSection 2.3.1
gap> N4 := UnionOfPieces( [ M78, T123 ] );; 
gap> h14 := HomomorphismByUnionNC( N1, N4, [ hom1, shom ] ); 
magma with objects homomorphism : 
[ magma with objects homomorphism : M78 -> M78
    [ [ <mapping: m -> m >, [ -8, -7 ] ] ], magma with objects homomorphism : 
    [ [ smap, [ -11, -12, -13 ] ] ] ]
gap> IsInjectiveOnObjects( h14 );
true
gap> IsSurjectiveOnObjects( h14 );
true
gap> IsBijectiveOnObjects( h14 ); 
true
gap> ImageElm( h14, t12 );
[Transformation( [ 2, 2, 4, 1 ] ) : -13 -> -12]
gap> h45 := IsomorphismNewObjects( N4, [ [-103,-102,-101], [-108,-107] ] );
magma with objects homomorphism : 
[ magma with objects homomorphism : 
    [ [ IdentityMapping( m ), [ -108, -107 ] ] ], 
  magma with objects homomorphism : 
    [ [ IdentityMapping( sgp<t2,s2,r2> ), [ -103, -102, -101 ] ] ] ]
gap> N5 := Range( h45 );;  SetName( N5, "N5" ); 
gap> h15 := h14 * h45;
magma with objects homomorphism : 
[ magma with objects homomorphism : [ [ <mapping: m -> m >, [ -108, -107 ] ] ]
    , magma with objects homomorphism : [ [ smap, [ -101, -102, -103 ] ] ] ]
gap> ImageElm( h15, t12 ); 
[Transformation( [ 2, 2, 4, 1 ] ) : -103 -> -102]
gap> SetInfoLevel( InfoGpd, gpd_infolevel_saved );;  

##############################################################################
##
#E  mwohom.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
