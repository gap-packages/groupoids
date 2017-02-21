##############################################################################
##
#W  gpdhom.tst                    Gpd Package                    Chris Wensley
##
#Y  Copyright (C) 2000-2017, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

gap> gpd_infolevel_saved := InfoLevel( InfoGpd );; 
gap> SetInfoLevel( InfoGpd, 0 );; 

###  Section 5.1

## SubSection 5.1.1
gap> gend12 := [ (15,16,17,18,19,20), (15,20)(16,19)(17,18) ];; 
gap> d12 := Group( gend12 );; 
gap> Gd12 := Groupoid( d12, [-37,-36,-35,-34] );;
gap> SetName( d12, "d12" );  
gap> SetName( Gd12, "Gd12" );
gap> s3 := Subgroup( d12, [ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ] );;
gap> Gs3 := SubgroupoidByPieces( Gd12, [ [ s3, [-36,-35,-34] ] ] );;
gap> SetName( s3, "s3" );  
gap> SetName( Gs3, "Gs3" );
gap> gend8 := GeneratorsOfGroup( d8 );;
gap> imhd8 := [ ( ), (15,20)(16,19)(17,18) ];;
gap> hd8 := GroupHomomorphismByImages( d8, s3, gend8, imhd8 );;
gap> homd8 := GroupoidHomomorphism( Gd8, Gs3, hd8 ); 
groupoid homomorphism : Gd8 -> Gs3
[ [ GroupHomomorphismByImages( d8, s3, [ (1,2,3,4), (1,3) ], 
        [ (), (15,20)(16,19)(17,18) ] ), [ -36, -35, -34 ], [ (), (), () ] ] ]
gap> e2; ImageElm( homd8, e2 );
[(1,3) : -8 -> -7]
[(15,20)(16,19)(17,18) : -35 -> -34]
gap> incGs3 := InclusionMappingGroupoids( Gd12, Gs3 );; 
gap> ihomd8 := homd8 * incGs3;; 
gap> IsBijectiveOnObjects( ihomd8 );
false
gap> Display( ihomd8 );
  groupoid mapping: [ Gd8 ] -> [ Gd12 ]
 root homomorphism: [ [ (1,2,3,4), (1,3) ], [ (), (15,20)(16,19)(17,18) ] ]
 images of objects: [ -36, -35, -34 ]
image elts of rays: [ (), (), () ]
gap> e2; ImageElm( ihomd8, e2 );
[(1,3) : -8 -> -7]
[(15,20)(16,19)(17,18) : -35 -> -34]
gap> c6d := Subgroup( d12, [ (15,16,17,18,19,20) ] );; 
gap> rays := [ (), (16,20)(17,19), (15,17)(18,20), (15,19)(16,18) ];; 
gap> Gc6d := SubgroupoidWithRays( Gd12, c6d, rays ); 
single piece groupoid with rays: < Group( [ (15,16,17,18,19,20) ] ), 
[ -37, -36, -35, -34 ], [ (), (16,20)(17,19), (15,17)(18,20), (15,19)(16,18) 
 ] >
gap> iso1 := IsomorphismNewObjects( Gd12, [-7,-6,-5,-4] ); 
groupoid homomorphism : 
[ [ IdentityMapping( d12 ), [ -7, -6, -5, -4 ], [ (), (), (), () ] ] ]
gap> iso2 := IsomorphismStandardGroupoid( Gc6d, [-7,-6,-5,-4] ); 
groupoid homomorphism : 
[ [ IdentityMapping( Group( [ (15,16,17,18,19,20) ] ) ), [ -7, -6, -5, -4 ], 
      [ (), (), (), () ] ] ]

## SubSection 5.2.1
gap> hc6 := GroupHomomorphismByImages( c6, s3, 
>            [(5,6,7)(8,9)], [(15,16)(17,20)(18,19)] );;
gap> Fs3 := FullSubgroupoid( Gs3, [ -35 ] );; 
gap> SetName( Fs3, "Fs3" ); 
gap> homc6 := GroupoidHomomorphism( Gc6, Fs3, hc6 );;
gap> incFs3 := InclusionMappingGroupoids( Gs3, Fs3 );; 
gap> ihomc6 := homc6 * incFs3; 
groupoid homomorphism : Gc6 -> Gs3
[ [ GroupHomomorphismByImages( c6, s3, [ (5,6,7)(8,9) ], 
        [ (15,16)(17,20)(18,19) ] ), [ -35 ], [ () ] ] ]
gap> idGs3 := IdentityMapping( Gs3 );;
gap> V3 := ReplaceOnePieceInUnion( U3, 1, Gs3 ); 
groupoid with 3 pieces:
[ Gs3, Gd8, Gc6 ]
gap> images3 := [ PieceImages( idGs3 )[1], 
>              PieceImages( homd8 )[1], 
>              PieceImages( ihomc6 )[1] ];; 
gap> homV3 := HomomorphismToSinglePiece( V3, Gs3, images3 );; 
gap> Display( homV3 );         
homomorphism to single piece magma with pieces:
(1): [ Gs3 ] -> [ Gs3 ]
magma mapping: [ [ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ], 
  [ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ] ]
   object map: [ -36, -35, -34 ] -> [ -36, -35, -34 ]
(2): [ Gd8 ] -> [ Gs3 ]
magma mapping: [ [ (1,2,3,4), (1,3) ], [ (), (15,20)(16,19)(17,18) ] ]
   object map: [ -9, -8, -7 ] -> [ -36, -35, -34 ]
(3): [ Gc6 ] -> [ Gs3 ]
magma mapping: [ [ (5,6,7)(8,9) ], [ (15,16)(17,20)(18,19) ] ]
   object map: [ -6 ] -> [ -35 ]

## Section 5.3, Homomorphisms with more than one piece 

## SubSection 5.3.1
gap> isoq8 := IsomorphismNewObjects( Gq8, [-38,-37] ); 
groupoid homomorphism : 
[ 
  [ IdentityMapping( q8 ), [ -38, -37 ], 
      [ <identity> of ..., <identity> of ... ] ] ]
gap> Gq8b := Range( isoq8 );; 
gap> SetName( Gq8b, "Gq8b" ); 
gap> V4 := UnionOfPieces( [ V3, Gq8 ] ); 
groupoid with 4 pieces:
[ Gs3, Gq8, Gd8, Gc6 ]
gap> SetName( V4, "V4" ); 
gap> Vs3q8b := UnionOfPieces( [ Gs3, Gq8b ] );; 
gap> SetName( Vs3q8b, "Vs3q8b" ); 
gap> hom4 := HomomorphismByUnion( V4, Vs3q8b, [ homV3, isoq8 ] );; 
gap> PiecesOfMapping( hom4 );
[ groupoid homomorphism : Gq8 -> Gq8b
    [ [ IdentityMapping( q8 ), [ -38, -37 ], 
          [ <identity> of ..., <identity> of ... ] ] ], 
  groupoid homomorphism : 
    [ [ IdentityMapping( s3 ), [ -36, -35, -34 ], [ (), (), () ] ], 
      [ GroupHomomorphismByImages( d8, s3, [ (1,2,3,4), (1,3) ], 
            [ (), (15,20)(16,19)(17,18) ] ), [ -36, -35, -34 ], 
          [ (), (), () ] ], 
      [ GroupHomomorphismByImages( c6, s3, [ (5,6,7)(8,9) ], 
            [ (15,16)(17,20)(18,19) ] ), [ -35 ], [ () ] ] ] ]

## Section 5.4, Groupoid automoprphisms 

## SubSection 5.4.1
gap> a4 := Subgroup( s4, [(1,2,3),(2,3,4)] );; 
gap> SetName( a4, "a4" ); 
gap> gensa4 := GeneratorsOfGroup( a4 );; 
gap> Ga4 := SubgroupoidByPieces( Gs4, [ [a4, [-15,-13,-11]] ] ); 
single piece groupoid: < a4, [ -15, -13, -11 ] >
gap> SetName( Ga4, "Ga4" ); 
gap> aut1 := GroupoidAutomorphismByObjectPerm( Ga4, [-13,-11,-15] );; 
gap> Display( aut1 ); 
  groupoid mapping: [ Ga4 ] -> [ Ga4 ]
 root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (1,2,3), (2,3,4) ] ]
 images of objects: [ -13, -11, -15 ]
image elts of rays: [ (), (), () ]
gap> h2 := GroupHomomorphismByImages( a4, a4, gensa4, [(2,3,4), (1,3,4)] );; 
gap> aut2 := GroupoidAutomorphismByGroupAuto( Ga4, h2 );; 
gap> Display( aut2 ); 
  groupoid mapping: [ Ga4 ] -> [ Ga4 ]
 root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (2,3,4), (1,3,4) ] ]
 images of objects: [ -15, -13, -11 ]
image elts of rays: [ (), (), () ]
gap> im3 := [(), (1,3,2), (2,4,3)];; 
gap> aut3 := GroupoidAutomorphismByRayShifts( Ga4, im3 );; 
gap> Display( aut3 ); 
  groupoid mapping: [ Ga4 ] -> [ Ga4 ]
 root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (1,2,3), (2,3,4) ] ]
 images of objects: [ -15, -13, -11 ]
image elts of rays: [ (), (1,3,2), (2,4,3) ]
gap> aut123 := aut1*aut2*aut3;; 
gap> Display( aut123 ); 
  groupoid mapping: [ Ga4 ] -> [ Ga4 ]
 root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (2,3,4), (1,3,4) ] ]
 images of objects: [ -13, -11, -15 ]
image elts of rays: [ (), (1,4,3), (1,2,3) ]
gap> inv123a := InverseGeneralMapping( aut123 );; 
gap> Display( inv123a ); 
  groupoid mapping: [ Ga4 ] -> [ Ga4 ]
 root homomorphism: [ [ (2,3,4), (1,3,4) ], [ (1,2,3), (2,3,4) ] ]
 images of objects: [ -11, -15, -13 ]
image elts of rays: [ (), (1,2,4), (1,3,4) ]
gap> inv1 := InverseGeneralMapping( aut1 );; 
gap> inv2 := InverseGeneralMapping( aut2 );; 
gap> inv3 := InverseGeneralMapping( aut3 );;
gap> inv123b := inv3*inv2*inv1;;
gap> Display( inv123b ); 
  groupoid mapping: [ Ga4 ] -> [ Ga4 ]
 root homomorphism: [ [ (1,2,3), (2,3,4) ], [ (1,2,4), (1,2,3) ] ]
 images of objects: [ -11, -15, -13 ]
image elts of rays: [ (), (1,2,4), (1,2,3) ]
gap> ok := inv123a = inv123b;
false
gap> AGa4 := AutomorphismGroupOfGroupoid( Ga4 ); 
<group with 8 generators>
gap> AGgens := GeneratorsOfGroup( AGa4);; 
gap> NGa4 := NiceObject( AGa4 );; 
gap> MGa4 := NiceMonomorphism( AGa4 );; 
gap> Size( AGa4 ); 
20736
gap> SetName( AGa4, "AGa4" ); 
gap> SetName( NGa4, "NGa4" ); 
gap> ##  cannot test images of AGgens because of random variations 
gap> ##  Now do some tests!
gap> mgi := MappingGeneratorsImages( MGa4 );; 
gap> autgen := mgi[1];; 
gap> pcgen := mgi[2];;
gap> ngen := Length( autgen );; 
gap> ForAll( [1..ngen], i -> Order(autgen[i]) = Order(pcgen[i]) ); 
true

## SubSection 5.4.2
gap> Hs3 := HomogeneousDiscreteGroupoid( s3, [ -13..-10] ); 
homogeneous, discrete groupoid: < s3, [ -13 .. -10 ] >
gap> aut4 := GroupoidAutomorphismByObjectPerm( Hs3, [-12,-10,-11,-13] ); 
groupoid homomorphism : morphism from a homogeneous discrete groupoid:
[ -13, -12, -11, -10 ] -> [ -12, -10, -11, -13 ]
object homomorphisms:
IdentityMapping( s3 )
IdentityMapping( s3 )
IdentityMapping( s3 )
IdentityMapping( s3 )

gap> gens3 := GeneratorsOfGroup( s3 );; 
gap> g1 := gens3[1];; 
gap> g2 := gens3[2];; 
gap> b1 := GroupHomomorphismByImages( s3, s3, gens3, [g1, g2^g1 ] );; 
gap> b2 := GroupHomomorphismByImages( s3, s3, gens3, [g1^g2, g2 ] );; 
gap> b3 := GroupHomomorphismByImages( s3, s3, gens3, [g1^g2, g2^(g1*g2) ] );; 
gap> b4 := GroupHomomorphismByImages( s3, s3, gens3, [g1^(g2*g1), g2^g1 ] );; 
gap> aut5 := GroupoidAutomorphismByGroupAutos( Hs3, [b1,b2,b3,b4] ); 
groupoid homomorphism : morphism from a homogeneous discrete groupoid:
[ -13, -12, -11, -10 ] -> [ -13, -12, -11, -10 ]
object homomorphisms:
GroupHomomorphismByImages( s3, s3, 
[ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ], 
[ (15,17,19)(16,18,20), (15,18)(16,17)(19,20) ] )
GroupHomomorphismByImages( s3, s3, 
[ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ], 
[ (15,19,17)(16,20,18), (15,20)(16,19)(17,18) ] )
GroupHomomorphismByImages( s3, s3, 
[ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ], 
[ (15,19,17)(16,20,18), (15,16)(17,20)(18,19) ] )
GroupHomomorphismByImages( s3, s3, 
[ (15,17,19)(16,18,20), (15,20)(16,19)(17,18) ], 
[ (15,19,17)(16,20,18), (15,18)(16,17)(19,20) ] )

gap> AHs3 := AutomorphismGroupOfGroupoid( Hs3 ); 
<group with 4 generators>
gap> Size( AHs3 );
31104
gap> genAHs3 := GeneratorsOfGroup( AHs3 ); 
[ groupoid homomorphism : morphism from a homogeneous discrete groupoid:
    [ -13, -12, -11, -10 ] -> [ -13, -12, -11, -10 ]
    object homomorphisms:
    ConjugatorAutomorphism( s3, (15,19,17)(16,20,18) )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    , groupoid homomorphism : morphism from a homogeneous discrete groupoid:
    [ -13, -12, -11, -10 ] -> [ -13, -12, -11, -10 ]
    object homomorphisms:
    ConjugatorAutomorphism( s3, (15,20)(16,19)(17,18) )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    , groupoid homomorphism : morphism from a homogeneous discrete groupoid:
    [ -13, -12, -11, -10 ] -> [ -12, -11, -10, -13 ]
    object homomorphisms:
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    , groupoid homomorphism : morphism from a homogeneous discrete groupoid:
    [ -13, -12, -11, -10 ] -> [ -12, -13, -11, -10 ]
    object homomorphisms:
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
    IdentityMapping( s3 )
     ]
gap> nobAHs3 := NiceObject( AHs3 );; 
gap> nmonAHs3 := NiceMonomorphism( AHs3 );;
gap> w := genAHs3[1];; 
gap> w1 := ImageElm( nmonAHs3, w );; 
gap> x := genAHs3[2];; 
gap> x1 := ImageElm( nmonAHs3, x );; 
gap> y := genAHs3[3];; 
gap> y1 := ImageElm( nmonAHs3, y );; 
gap> z := genAHs3[4];; 
gap> z1 := ImageElm( nmonAHs3, z );; 
gap> u := z*w*y*x*z; 
groupoid homomorphism : morphism from a homogeneous discrete groupoid:
[ -13, -12, -11, -10 ] -> [ -11, -13, -10, -12 ]
object homomorphisms:
IdentityMapping( s3 )
ConjugatorAutomorphism( s3, (15,19,17)(16,20,18) )
IdentityMapping( s3 )
ConjugatorAutomorphism( s3, (15,20)(16,19)(17,18) )

gap> u1 := z1*w1*y1*x1*z1; 
(1,2,4,3)(5,17,23,16,8,20,26,13)(6,18,24,15,7,19,25,14)(9,21,27,12,10,22,28,
11)
gap> imu := ImageElm( nmonAHs3, u );; 
gap> u1 = imu;
true
gap> SetInfoLevel( InfoGpd, gpd_infolevel_saved );;  

#############################################################################
##
#E  gpdhom.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
