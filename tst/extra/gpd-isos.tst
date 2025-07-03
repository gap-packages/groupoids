############################################################################
##
#W  gpd-isos.tst               Groupoids Package               Chris Wensley
##

gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 
gap> s3a := Group( (1,2,3), (2,3) );; 
gap> s3b := Group( (4,6,8)(5,7,9), (4,9)(5,8)(6,7) );;
gap> s3c := Group( (4,6,8)(5,7,9), (5,9)(6,8) );;
gap> ida := IdentityMapping( s3a );; 
gap> isoab := IsomorphismGroups( s3a, s3b );; 
gap> isoac := IsomorphismGroups( s3a, s3c );;
gap> isos1 := [ ida, isoab, isoac ];; 
gap> G1 := GroupoidByIsomorphisms( s3a, [-3,-2,-1], isos1 );; 
gap> SetName( G1, "G1" ); 
gap> gens1 := GeneratorsOfGroupoid( G1 );;
gap> x1 := ImageElm( isos1[2], (1,2,3) );;
gap> a1 := Arrow( G1, [ (1,2,3), x1 ], -3, -2 );;
gap> y1 := ImageElm( isos1[2], (2,3) );;
gap> z1 := ImageElm( isos1[3], (2,3) );;
gap> b1 := Arrow( G1, [ y1, z1 ], -2, -1 );;
gap> c1 := a1*b1;;

gap> isopc := IsomorphismPcGroup( s3a );; 
gap> s3p := Image( isopc );; 
gap> f2 := FreeGroup( 2 );; 
gap> s3f := f2/[ f2.1^3, f2.2^2, (f2.1*f2.2)^2 ];; 
gap> isofp := GroupHomomorphismByImages(s3a,s3f,[(1,2,3),(2,3)],[s3f.1,s3f.2]);;
gap> isos2 := [ ida, isopc, isofp ];;
gap> G2 := GroupoidByIsomorphisms( s3a, [-7,-6,-5], isos2 );; 
gap> SetName( G2, "G2" );
gap> gens2 := GeneratorsOfGroupoid( G2 );;
gap> x2 := ImageElm( isos2[2], (1,2) );
f1*f2
gap> a2 := Arrow( G2, [ (1,2), x2 ], -7, -6 );
[[ (1,2), f1*f2 ] : -7 -> -6]
gap> y2 := ImageElm( isos2[2], (2,3) );
f1
gap> z2 := ImageElm( isos2[3], (2,3) );
f2^-1
gap> b2 := Arrow( G2, [ y2, z2 ], -6, -5 );
[[ f1, f2^-1 ] : -6 -> -5]
gap> c2 := a2*b2;
[[ (1,3,2), f1^2 ] : -7 -> -5]

gap> hom12 := GroupoidHomomorphismFromSinglePiece( G1, G2, gens1, gens2 );; 
gap> rgh12 := RootGroupHomomorphism( hom12 );;
gap> ia1 := ImageElm( hom12, a1 );; 
gap> ib1 := ImageElm( hom12, b1 );; 
gap> ic1 := ImageElm( hom12, c1 );; 

gap> d1 := Arrow( G1, [ x1, x1 ], -2, -2 );; 
gap> d2 := Arrow( G1, [ (4,7)(5,6)(8,9), (4,7)(5,6)(8,9) ], -2, -2 );; 
gap> d3 := Arrow( G1, [ (), () ], -2, -1 );; 
gap> d4 := Arrow( G1, [ (), () ], -2, -3 );; 

gap> d12 := ImageElm( hom12, d1 ); 
[[ f2, f2 ] : -6 -> -6]
gap> d22 := ImageElm( hom12, d2 ); 
[[ f1*f2^2, f1*f2^2 ] : -6 -> -6]
gap> d32 := ImageElm( hom12, d3 ); 
[[ <identity> of ..., <identity ...> ] : -6 -> -5]
gap> d42 := ImageElm( hom12, d4 ); 
[[ <identity> of ..., () ] : -6 -> -7]
gap> gens22 := [ d12, d22, d32, d42 ];; 
gap> hom122 := GroupoidHomomorphismFromSinglePiece( G1, G2, gens1, gens22 );;
gap> rgh122 := RootGroupHomomorphism( hom122 );;
gap> ia12 := ImageElm( hom122, a1 ); 
[[ f2, f1 ] : -6 -> -5]
gap> ib12 := ImageElm( hom122, b1 ); 
[[ f2^-1*f1^2, (1,3) ] : -5 -> -7]
gap> ic12 := ImageElm( hom122, c1 ); 
[[ f1*f2, (1,2) ] : -6 -> -7]

gap> s3 := Group( (11,22,33), (22,33) );; 
gap> G0 := SinglePieceGroupoid( s3, [-10,-9,-8] );;
gap> SetName( G0, "G0" ); 
gap> gens0 := GeneratorsOfGroupoid( G0 );; 
gap> hom01 := GroupoidHomomorphismFromSinglePiece( G0, G1, gens0, gens1 );; 
gap> rgh01 := RootGroupHomomorphism( hom01 );;
gap> a0 := Arrow( G0, (11,22,33), -10, -9 );;
gap> b0 := Arrow( G0, (22,33), -9, -8 );;
gap> c0 := a0*b0;; 
gap> ia01 := ImageElm( hom01, a0 ); 
[[ (1,2,3), (4,6,8)(5,7,9) ] : -3 -> -2]
gap> ib01 := ImageElm( hom01, b0 ); 
[[ (4,9)(5,8)(6,7), (5,9)(6,8) ] : -2 -> -1]
gap> ic01 := ImageElm( hom01, c0 ); 
[[ (1,3), (4,8)(5,7) ] : -3 -> -1]

gap> hom022 := GroupoidHomomorphismFromSinglePiece( G0, G2, gens0, gens22 );; 
gap> rgh022 := RootGroupHomomorphism( hom022 );;
gap> ia022 := ImageElm( hom022, a0 ); 
[[ f2, f1 ] : -6 -> -5]
gap> ib022 := ImageElm( hom022, b0 ); 
[[ f2^-1*f1^2, (1,3) ] : -5 -> -7]
gap> ic022 := ImageElm( hom022, c0 ); 
[[ f1*f2, (1,2) ] : -6 -> -7]

gap> hom10 := GroupoidHomomorphismFromSinglePiece( G1, G0, gens1, gens0 );; 
gap> rgh10 := RootGroupHomomorphism( hom10 );;
gap> ia10 := ImageElm( hom10, a1 ); 
[(11,22,33) : -10 -> -9]
gap> ib10 := ImageElm( hom10, b1 ); 
[(22,33) : -9 -> -8]
gap> ic10 := ImageElm( hom10, c1 ); 
[(11,33) : -10 -> -8]

gap> gens00 := [ Arrow( G0, (11,22,33), -9, -9 ), 
>                Arrow( G0, (22,33), -9, -9 ), 
>                Arrow( G0, (), -9, -8 ), 
>                Arrow( G0, (), -9, -10 ) ];; 
gap> hom200 := GroupoidHomomorphismFromSinglePiece( G2, G0, gens2, gens00 );; 
gap> rgh200 := RootGroupHomomorphism( hom200 );;
gap> a22 := Arrow( G2, [ (1,2,3), s3p.2 ], -7, -6 );;
gap> ia22 := ImageElm( hom200, a22 ); 
[(11,22,33) : -9 -> -8]
gap> b22 := Arrow( G2, [ s3p.1, s3f.2 ], -6, -5 );;
gap> ib22 := ImageElm( hom200, b22 ); 
[(22,33) : -8 -> -10]
gap> c22 := a22 * b22; 
[[ (1,3), f2^-1*f1^2 ] : -7 -> -5]
gap> ic22 := ImageElm( hom200, c22 ); 
[(11,33) : -9 -> -10]
gap> ##
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
