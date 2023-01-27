##############################################################################
##
#W  homs.tst                   Groupoids Package                 Chris Wensley
##
#Y  Copyright (C) 2000-2023, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 
gap> #
gap> # add definitions from autos.tst 
gap> a4gens := [ (1,2,3), (2,3,4) ];;
gap> a4 := Group( a4gens );;
gap> SetName( a4, "a4" );;
gap> obs := [-9,-8,-7];;
gap> Ga4 := SinglePieceGroupoid( a4, obs );;
gap> #
gap> s4 := Group( (1,2,3,4), (3,4) );; 
gap> SetName( s4, "s4" );;
gap> a4gens := [ (1,2,3), (2,3,4) ];;
gap> a4 := Subgroup( s4, a4gens );;
gap> SetName( a4, "a4" );;
gap> SG := SinglePieceGroupoid( s4, [-4,-3,-2,-1] );
single piece groupoid: < s4, [ -4, -3, -2, -1 ] >
gap> G := SubgroupoidWithRays( SG, a4, [ (), (1,4), (2,4), (3,4) ] );;
gap> Display(G);
single piece groupoid with rays having: 
supergroupoid: single piece groupoid: < s4, [ -4, -3, -2, -1 ] >
      objects: [ -4, -3, -2, -1 ]
   root group: a4 = <[ (1,2,3), (2,3,4) ]>
         rays: [ (), (1,4), (2,4), (3,4) ]
gap> SH := SinglePieceGroupoid( s4, [-9,-8,-7,-6,-5] );
single piece groupoid: < s4, [ -9, -8, -7, -6, -5 ] >
gap> H := SubgroupoidWithRays( SH, a4, 
>           [ (), (2,3,4), (1,3,4), (1,2,4), (1,2,3) ] );;
gap> Display(H);
single piece groupoid with rays having: 
supergroupoid: single piece groupoid: < s4, [ -9, -8, -7, -6, -5 ] >
      objects: [ -9, -8, -7, -6, -5 ]
   root group: a4 = <[ (1,2,3), (2,3,4) ]>
         rays: [ (), (2,3,4), (1,3,4), (1,2,4), (1,2,3) ]
gap> SJ := SinglePieceGroupoid( s4, [-19,-18,-17,-16,-15] );
single piece groupoid: < s4, [ -19, -18, -17, -16, -15 ] >
gap> J := SubgroupoidWithRays( SJ, a4, 
>           [ (), (2,3), (1,3), (1,2), (1,2,3) ] );;
gap> Display(J);
single piece groupoid with rays having: 
supergroupoid: single piece groupoid: < s4, [ -19, -18, -17, -16, -15 ] >
      objects: [ -19, -18, -17, -16, -15 ]
   root group: a4 = <[ (1,2,3), (2,3,4) ]>
         rays: [ (), (2,3), (1,3), (1,2), (1,2,3) ]
gap> autgh := GroupHomomorphismByImages( a4, a4, a4gens, 
>               [ (1,3,2), (1,3,4) ] );; 
gap> obsgh := [ -7, -6, -5, -8 ];; 
gap> raygh := [(),(1,2)(3,4),(1,3)(2,4),(1,4)(2,3)];;
gap> gh := GroupoidHomomorphism( G, H, autgh, obsgh, raygh ); 
groupoid homomorphism : 
[ [ [(1,2,3) : -4 -> -4], [(2,3,4) : -4 -> -4], [(1,4) : -4 -> -3], 
      [(2,4) : -4 -> -2], [(3,4) : -4 -> -1] ], 
  [ [(1,3,2) : -7 -> -7], [(1,3,4) : -7 -> -7], [(1,2)(3,4) : -7 -> -6], 
      [(1,3)(2,4) : -7 -> -5], [(1,4)(2,3) : -7 -> -8] ] ]
gap> x1 := Arrow( G, (1,3,4,2), -2, -4 );;
gap> x2 := ImageElm( gh, x1 );          
[(1,4,3) : -5 -> -7]
gap> authj := GroupHomomorphismByImages( a4, a4, a4gens, [(1,4,3),(2,4,3)] );; 
gap> obshj := [-15,-18,-19,-16,-17];; 
gap> rayhj := [(),(1,4),(1,2,4),(3,4),(1,3,2,4)];; 
gap> hj := GroupoidHomomorphism( H, J, authj, obshj, rayhj ); 
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [(2,3,4) : -9 -> -8], 
      [(1,3,4) : -9 -> -7], [(1,2,4) : -9 -> -6], [(1,2,3) : -9 -> -5] ], 
  [ [(1,4,3) : -15 -> -15], [(2,4,3) : -15 -> -15], [(1,4) : -15 -> -18], 
      [(1,2,4) : -15 -> -19], [(3,4) : -15 -> -16], [(1,3,2,4) : -15 -> -17] 
     ] ]
gap> x3 := ImageElm( hj, x2 );
[(1,4,2,3) : -17 -> -19]
gap> gj := gh*hj; 
groupoid homomorphism : 
[ [ [(1,2,3) : -4 -> -4], [(2,3,4) : -4 -> -4], [(1,4) : -4 -> -3], 
      [(2,4) : -4 -> -2], [(3,4) : -4 -> -1] ], 
  [ [(1,4,2) : -19 -> -19], [(2,3,4) : -19 -> -19], [(1,2,3,4) : -19 -> -16], 
      [(1,4) : -19 -> -17], [(3,4) : -19 -> -18] ] ]
gap> jk := IsomorphismNewObjects( SJ, [-14,-13,-12,-11,-10]);;
gap> SK := Image( jk );
single piece groupoid: < s4, [ -14, -13, -12, -11, -10 ] >
gap> x4 := ImageElm( jk, x3 );
[(1,4,2,3) : -12 -> -14]
gap> genJ := GeneratorsOfGroupoid( J );
[ [(1,2,3) : -19 -> -19], [(2,3,4) : -19 -> -19], [(2,3) : -19 -> -18], 
  [(1,3) : -19 -> -17], [(1,2) : -19 -> -16], [(1,2,3) : -19 -> -15] ]
gap> resJ := RestrictedMappingGroupoids( jk, J );
groupoid homomorphism : 
[ [ [(1,2,3) : -19 -> -19], [(2,3,4) : -19 -> -19], [(2,3) : -19 -> -18], 
      [(1,3) : -19 -> -17], [(1,2) : -19 -> -16], [(1,2,3) : -19 -> -15] ], 
  [ [(1,2,3) : -14 -> -14], [(2,3,4) : -14 -> -14], [(2,3) : -14 -> -13], 
      [(1,3) : -14 -> -12], [(1,2) : -14 -> -11], [(1,2,3) : -14 -> -10] ] ]
gap> K := Image( resJ );
single piece groupoid with rays: < Group( [ (1,2,3), (2,3,4) ] ), 
[ -14, -13, -12, -11, -10 ], [ (), (2,3), (1,3), (1,2), (1,2,3) ] >
gap> gk := gj*jk;
groupoid homomorphism : 
[ [ [(1,2,3) : -4 -> -4], [(2,3,4) : -4 -> -4], [(1,4) : -4 -> -3], 
      [(2,4) : -4 -> -2], [(3,4) : -4 -> -1] ], 
  [ [(1,4,2) : -14 -> -14], [(2,3,4) : -14 -> -14], [(1,2,3,4) : -14 -> -11], 
      [(1,4) : -14 -> -12], [(3,4) : -14 -> -13] ] ]
gap> x4 = ImageElm( gk, x1 );
true
gap> ## 
gap> ##  now a second batch of constructions based on d12/s3d 
gap> ##
gap> d12 := Group( (1,2,3,4,5,6), (1,3)(4,6) );; 
gap> SetName( d12, "d12" );;
gap> s3dgens := [ (1,3,5)(2,4,6), (1,3)(4,6) ];;
gap> s3d := Subgroup( d12, s3dgens );;
gap> SetName( s3d, "s3d" );;
gap> DA := SinglePieceGroupoid( d12, [-24,-23,-22,-21] );
single piece groupoid: < d12, [ -24, -23, -22, -21 ] >
gap> A := SubgroupoidWithRays( DA, s3d, 
>           [(),(1,2)(3,6)(4,5),(1,4)(2,3)(5,6),(1,6)(2,5)(3,4)] );;
gap> Display(A);
single piece groupoid with rays having: 
supergroupoid: single piece groupoid: < d12, [ -24, -23, -22, -21 ] >
      objects: [ -24, -23, -22, -21 ]
   root group: s3d = <[ (1,3,5)(2,4,6), (1,3)(4,6) ]>
         rays: [ (), (1,2)(3,6)(4,5), (1,4)(2,3)(5,6), (1,6)(2,5)(3,4) ]
gap> DB := SinglePieceGroupoid( d12, [-29,-28,-27,-26] );
single piece groupoid: < d12, [ -29, -28, -27, -26 ] >
gap> B := SubgroupoidWithRays( DB, s3d, 
>           [ (), (1,2,3,4,5,6), (1,4)(2,5)(3,6), (1,6,5,4,3,2) ] );;
gap> Display(B);
single piece groupoid with rays having: 
supergroupoid: single piece groupoid: < d12, [ -29, -28, -27, -26 ] >
      objects: [ -29, -28, -27, -26 ]
   root group: s3d = <[ (1,3,5)(2,4,6), (1,3)(4,6) ]>
         rays: [ (), (1,2,3,4,5,6), (1,4)(2,5)(3,6), (1,6,5,4,3,2) ]
gap> DC := SinglePieceGroupoid( d12, [-39,-38,-37,-36] );
single piece groupoid: < d12, [ -39, -38, -37, -36 ] >
gap> C := SubgroupoidWithRays( DC, s3d, 
>           [ (), (1,6,5,4,3,2), (1,6)(2,5)(3,4), (2,6)(3,5) ] );;
gap> Display(C);
single piece groupoid with rays having: 
supergroupoid: single piece groupoid: < d12, [ -39, -38, -37, -36 ] >
      objects: [ -39, -38, -37, -36 ]
   root group: s3d = <[ (1,3,5)(2,4,6), (1,3)(4,6) ]>
         rays: [ (), (1,6,5,4,3,2), (1,6)(2,5)(3,4), (2,6)(3,5) ]
gap> autab := GroupHomomorphismByImages( s3d, s3d, s3dgens, 
>               [ (1,5,3)(2,6,4), (1,5)(2,4) ] );; 
gap> obsab := [ -27, -29, -26, -28 ];; 
gap> rayab := [(),(1,2,3,4,5,6),(1,5)(2,4),(2,6)(3,5)];;
gap> ab := GroupoidHomomorphism( A, B, autab, obsab, rayab ); 
groupoid homomorphism : 
[ [ [(1,3,5)(2,4,6) : -24 -> -24], [(1,3)(4,6) : -24 -> -24], 
      [(1,2)(3,6)(4,5) : -24 -> -23], [(1,4)(2,3)(5,6) : -24 -> -22], 
      [(1,6)(2,5)(3,4) : -24 -> -21] ], 
  [ [(1,5,3)(2,6,4) : -27 -> -27], [(1,5)(2,4) : -27 -> -27], 
      [(1,2,3,4,5,6) : -27 -> -29], [(1,5)(2,4) : -27 -> -26], 
      [(2,6)(3,5) : -27 -> -28] ] ]
gap> y1 := Arrow( A, (1,5,3)(2,6,4), -23, -22 );;
gap> y2 := ImageElm( ab, y1 );                  
[(1,4)(2,3)(5,6) : -29 -> -26]
gap> autbc := GroupHomomorphismByImages( s3d, s3d, s3dgens, 
>               [ (1,3,5)(2,4,6), (2,6)(3,5) ] );; 
gap> obsbc := [ -36, -37, -39, -38 ];; 
gap> raybc := [ (), (1,6,5,4,3,2), (1,5,3)(2,6,4), (1,2)(3,6)(4,5) ];; 
gap> bc := GroupoidHomomorphism( B, C, autbc, obsbc, raybc ); 
groupoid homomorphism : 
[ [ [(1,3,5)(2,4,6) : -29 -> -29], [(1,3)(4,6) : -29 -> -29], 
      [(1,2,3,4,5,6) : -29 -> -28], [(1,4)(2,5)(3,6) : -29 -> -27], 
      [(1,6,5,4,3,2) : -29 -> -26] ], 
  [ [(1,3,5)(2,4,6) : -36 -> -36], [(2,6)(3,5) : -36 -> -36], 
      [(1,6,5,4,3,2) : -36 -> -37], [(1,5,3)(2,6,4) : -36 -> -39], 
      [(1,2)(3,6)(4,5) : -36 -> -38] ] ]
gap> y3 := ImageElm( bc, y2 );
[(1,6,5,4,3,2) : -36 -> -38]
gap> ac := ab*bc; 
groupoid homomorphism : 
[ [ [(1,3,5)(2,4,6) : -24 -> -24], [(1,3)(4,6) : -24 -> -24], 
      [(1,2)(3,6)(4,5) : -24 -> -23], [(1,4)(2,3)(5,6) : -24 -> -22], 
      [(1,6)(2,5)(3,4) : -24 -> -21] ], 
  [ [(1,5,3)(2,6,4) : -39 -> -39], [(1,5)(2,4) : -39 -> -39], 
      [() : -39 -> -36], [(1,4)(2,5)(3,6) : -39 -> -38], 
      [(1,4)(2,3)(5,6) : -39 -> -37] ] ]
gap> cd := IsomorphismNewObjects( DC, [-34..-31]);;
gap> DD := Image( cd );
single piece groupoid: < d12, [ -34 .. -31 ] >
gap> genC := GeneratorsOfGroupoid( C );
[ [(1,3,5)(2,4,6) : -39 -> -39], [(1,3)(4,6) : -39 -> -39], 
  [(1,6,5,4,3,2) : -39 -> -38], [(1,6)(2,5)(3,4) : -39 -> -37], 
  [(2,6)(3,5) : -39 -> -36] ]
gap> y4 := ImageElm( cd, y3 );
[(1,6,5,4,3,2) : -31 -> -33]
gap> imC := List( genC, g -> ImageElm( cd, g ) );
[ [(1,3,5)(2,4,6) : -34 -> -34], [(1,3)(4,6) : -34 -> -34], 
  [(1,6,5,4,3,2) : -34 -> -33], [(1,6)(2,5)(3,4) : -34 -> -32], 
  [(2,6)(3,5) : -34 -> -31] ]
gap> D := SinglePieceSubgroupoidByGenerators( DD, imC );
single piece groupoid with rays: < Group( [ (1,3,5)(2,4,6), (1,3)(4,6) ] ), 
[ -34, -33, -32, -31 ], [ (), (1,6,5,4,3,2), (1,6)(2,5)(3,4), (2,6)(3,5) ] >
gap> ad := ac*cd;
groupoid homomorphism : 
[ [ [(1,3,5)(2,4,6) : -24 -> -24], [(1,3)(4,6) : -24 -> -24], 
      [(1,2)(3,6)(4,5) : -24 -> -23], [(1,4)(2,3)(5,6) : -24 -> -22], 
      [(1,6)(2,5)(3,4) : -24 -> -21] ], 
  [ [(1,5,3)(2,6,4) : -34 -> -34], [(1,5)(2,4) : -34 -> -34], 
      [() : -34 -> -31], [(1,4)(2,5)(3,6) : -34 -> -33], 
      [(1,4)(2,3)(5,6) : -34 -> -32] ] ]
gap> y4 = ImageElm( ad, y1 ); 
true
gap> ##
gap> ## now combine the various groupoids into unions 
gap> ##
gap> SGDA := UnionOfPieces( [ SG, DA ] );; 
gap> SetName( SGDA, "SGDA" ); 
gap> GA := UnionOfPieces( [ G, A ] );; 
gap> SetName( GA, "GA" ); 
gap> SHDB := UnionOfPieces( [ SH, DB ] );; 
gap> SetName( SHDB, "SHBB" ); 
gap> HB := UnionOfPieces( [ H, B ] );; 
gap> SetName( HB, "HB" ); 
gap> SJDC := UnionOfPieces( [ SJ, DC ] );; 
gap> SetName( SJDC, "SJDC" ); 
gap> JC := UnionOfPieces( [ J, C ] );; 
gap> SetName( JC, "JC" ); 
gap> SKDD := UnionOfPieces( [ SK, DD ] );; 
gap> SetName( SKDD, "SKDD" ); 
gap> KD := UnionOfPieces( [ K, D ] );; 
gap> SetName( KD, "KD" ); 
gap> ghab := HomomorphismByUnion( GA, HB, [ gh, ab ] );;
gap> ImageElm( ghab, x1 ) = x2;
true
gap> ImageElm( ghab, y1 ) = y2;
true
gap> hjbc := HomomorphismByUnion( HB, JC, [ hj, bc ] );;
gap> ImageElm( hjbc, x2 ) = x3;
true
gap> ImageElm( hjbc, y2 ) = y3;
true
gap> gjac := HomomorphismByUnion( GA, JC, [ gj, ac ] );;
gap> ghab*hjbc = gjac;
true
gap> jkcd := HomomorphismByUnion( SJDC, SKDD, [ jk, cd ] );;
gap> ImageElm( jkcd, x3 ) = x4;
true
gap> ImageElm( jkcd, y3 ) = y4;
true
gap> resJC := RestrictedMappingGroupoids( jkcd, JC );
groupoid homomorphism from several pieces : 
groupoid homomorphism : 
[ [ [(1,3,5)(2,4,6) : -39 -> -39], [(1,3)(4,6) : -39 -> -39], 
      [(1,6,5,4,3,2) : -39 -> -38], [(1,6)(2,5)(3,4) : -39 -> -37], 
      [(2,6)(3,5) : -39 -> -36] ], 
  [ [(1,3,5)(2,4,6) : -34 -> -34], [(1,3)(4,6) : -34 -> -34], 
      [(1,6,5,4,3,2) : -34 -> -33], [(1,6)(2,5)(3,4) : -34 -> -32], 
      [(2,6)(3,5) : -34 -> -31] ] ]
groupoid homomorphism : 
[ [ [(1,2,3) : -19 -> -19], [(2,3,4) : -19 -> -19], [(2,3) : -19 -> -18], 
      [(1,3) : -19 -> -17], [(1,2) : -19 -> -16], [(1,2,3) : -19 -> -15] ], 
  [ [(1,2,3) : -14 -> -14], [(2,3,4) : -14 -> -14], [(2,3) : -14 -> -13], 
      [(1,3) : -14 -> -12], [(1,2) : -14 -> -11], [(1,2,3) : -14 -> -10] ] ]

gap> resJ=RestrictedMappingGroupoids( jkcd, J );
true
gap> gkad := HomomorphismByUnion( GA, KD, [ gk, ad ] );;
gap> ImageElm( gkad, x1 ) = x4;
true
gap> ImageElm( gkad, y1 ) = y4;
true
gap> gl43 := SpecialLinearGroup( 4, 3 );;
gap> Ggl43 := SinglePieceGroupoid( gl43, [ -35..-31 ] );;
gap> SetName( gl43, "gl43" );  SetName( Ggl43, "Ggl43" );
gap> sl43 := NormalSubgroups( gl43 )[3];;
gap> gengl43 := GeneratorsOfGroup( gl43 );; 
gap> Hgl43 := SubgroupoidWithRays( Ggl43, sl43, 
>              [One(gl43),gengl43[2],gengl43[2]^3,gengl43[2]^5,gengl43[2]^7] );; 
gap> isoHgl43 := IsomorphismPermGroupoid( Hgl43 );;
gap> PHgl43 := Image( isoHgl43 );
single piece groupoid with rays: < Group( 
[ (28,37,46)(29,38,47)(30,39,48)(31,40,49)(32,41,50)(33,42,51)(34,43,52)
    (35,44,53)(36,45,54)(55,73,64)(56,74,65)(57,75,66)(58,76,67)(59,77,68)
    (60,78,69)(61,79,70)(62,80,71)(63,81,72), 
  ( 2, 7,10,55, 3, 4,19,28)( 5,25,37,56, 9,13,73,30)( 6,22,46,29, 8,16,64,57)
    (11,61,12,58,21,31,20,34)(14,79,39,59,27,40,74,36)(15,76,48,32,26,43,65,63
     )(17,70,66,60,24,49,47,35)(18,67,75,33,23,52,38,62)(41,80,45,68,81,42,77,
     54)(44,71,72,69,78,51,50,53) ] ), [ -35 .. -31 ], 
[ (), ( 2, 7,10,55, 3, 4,19,28)( 5,25,37,56, 9,13,73,30)( 6,22,46,29, 8,16,64,
     57)(11,61,12,58,21,31,20,34)(14,79,39,59,27,40,74,36)(15,76,48,32,26,43,
     65,63)(17,70,66,60,24,49,47,35)(18,67,75,33,23,52,38,62)(41,80,45,68,81,
     42,77,54)(44,71,72,69,78,51,50,53), 
  ( 2,55,19, 7, 3,28,10, 4)( 5,56,73,25, 9,30,37,13)( 6,29,64,22, 8,57,46,16)
    (11,58,20,61,21,34,12,31)(14,59,74,79,27,36,39,40)(15,32,65,76,26,63,48,43
     )(17,60,47,70,24,35,66,49)(18,33,38,67,23,62,75,52)(41,68,77,80,81,54,45,
     42)(44,69,50,71,78,53,72,51), 
  ( 2, 4,10,28, 3, 7,19,55)( 5,13,37,30, 9,25,73,56)( 6,16,46,57, 8,22,64,29)
    (11,31,12,34,21,61,20,58)(14,40,39,36,27,79,74,59)(15,43,48,63,26,76,65,32
     )(17,49,66,35,24,70,47,60)(18,52,75,62,23,67,38,33)(41,42,45,54,81,80,77,
     68)(44,51,72,53,78,71,50,69), 
  ( 2,28,19, 4, 3,55,10, 7)( 5,30,73,13, 9,56,37,25)( 6,57,64,16, 8,29,46,22)
    (11,34,20,31,21,58,12,61)(14,36,74,40,27,59,39,79)(15,63,65,43,26,32,48,76
     )(17,35,47,49,24,60,66,70)(18,62,38,52,23,33,75,67)(41,54,77,42,81,68,45,
     80)(44,53,50,51,78,69,72,71) ] >

gap> q8 := QuaternionGroup( 8 );;
gap> Gq8 := Groupoid( q8, [ -28, -27 ] );;
gap> SetName( q8, "q8" );  SetName( Gq8, "Gq8" );
gap> N2 := Subgroup( q8, [ q8.2] );; 
gap> SetName( N2, "N2" );
gap> Hq8 := SubgroupoidWithRays( Gq8, N2, [ One(q8), q8.1 ] );; 
gap> SetName( Hq8, "Hq8" );
gap> Uq8gl43 := UnionOfPieces( [ Hq8, Hgl43 ] );;
gap> isoUq8gl43 := IsomorphismPermGroupoid( Uq8gl43 );; 
gap> g := GeneratorsOfGroupoid( Uq8gl43 )[3];; 
gap> ImageElm( isoUq8gl43, g );
[( 2, 7,10,55, 3, 4,19,28)( 5,25,37,56, 9,13,73,30)( 6,22,46,29, 8,16,64,57)
(11,61,12,58,21,31,20,34)(14,79,39,59,27,40,74,36)(15,76,48,32,26,43,65,63)
(17,70,66,60,24,49,47,35)(18,67,75,33,23,52,38,62)(41,80,45,68,81,42,77,54)
(44,71,72,69,78,51,50,53) : -35 -> -34]
gap> iso := IsomorphismPcGroupoid( Ga4 );         
groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [f1*f3 : -9 -> -9], [f1^2 : -9 -> -9], [<identity> of ... : -9 -> -8], 
      [<identity> of ... : -9 -> -7] ] ]
gap> a5 := Group( (1,2,3,4,5), (1,2,3) );; 
gap> Ga5 := Groupoid( a5, [-8..-6] );;
gap> iso := IsomorphismPcGroupoid( Ga5 ); 
fail

## make this part of hom-test.tst independent of other tests 
gap> s4 := Group( (1,2,3,4), (3,4) );; 
gap> d8 := Subgroup( s4, [ (1,2,3,4), (1,3) ] );;
gap> SetName( s4, "s4" );  SetName( d8, "d8" ); 
gap> Gs4 := SinglePieceGroupoid( s4, [-15 .. -11] );; 
gap> Gd8 := Groupoid( d8, [-9,-8,-7] );;
gap> c6 := Group( (5,6,7)(8,9) );;
gap> SetName( c6, "c6" );
gap> Gc6 := DomainWithSingleObject( c6, -6 );;
gap> SetName( Gs4, "Gs4" );  SetName( Gd8, "Gd8" );  SetName( Gc6, "Gc6" );  
gap> U3 := UnionOfPieces( [ Gc6, Gd8, Gs4 ] );;
gap> isoU3 := IsomorphismPcGroupoid( U3 ); 
groupoid homomorphism from several pieces : 
groupoid homomorphism : 
[ [ [(1,2,3,4) : -15 -> -15], [(3,4) : -15 -> -15], [() : -15 -> -14], 
      [() : -15 -> -13], [() : -15 -> -12], [() : -15 -> -11] ], 
  [ [f1*f2*f3*f4 : -15 -> -15], [f1 : -15 -> -15], 
      [<identity> of ... : -15 -> -14], [<identity> of ... : -15 -> -13], 
      [<identity> of ... : -15 -> -12], [<identity> of ... : -15 -> -11] ] ]
groupoid homomorphism : 
[ [ [(1,2,3,4) : -9 -> -9], [(1,3) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [f2 : -9 -> -9], [f1*f3 : -9 -> -9], [<identity> of ... : -9 -> -8], 
      [<identity> of ... : -9 -> -7] ] ]
groupoid homomorphism : 
[ [ [(5,6,7)(8,9) : -6 -> -6] ], [ [f1*f2^2 : -6 -> -6] ] ]

gap> iso1 := IsomorphismNewObjects( Ga4, [-19,-18,-17] );; 
gap> Ha4 := Image( iso1 );; 
gap> iso2 := IsomorphismNewObjects( Ga4, [-29,-28,-27] );; 
gap> Ja4 := Image( iso2 );;
gap> iso3 := IsomorphismNewObjects( Ha4, [-29,-28,-27] );;
gap> Ja4 = Image( iso3 );
true
gap> GHa4 := UnionOfPieces( [ Ga4, Ha4 ] );; 
gap> iso23 := HomomorphismByUnion( GHa4, Ja4, [iso3,iso2] );;
gap> ImageElm( iso23, Arrow( Ga4, (1,2,3), -7, -8 ) ); 
[(1,2,3) : -27 -> -28]

gap> ##
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
