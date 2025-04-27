############################################################################
##
#W  auto-gpd.tst               Groupoids Package               Chris Wensley
##

gap> gpd_infolevel_saved := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );; 
gap> d8 := Group( (1,2,3,4), (3,4) );;
gap> SetName( d8, "d8" );
gap> Gd8 := Groupoid( d8, [-9,-8,-7] );
single piece groupoid: < d8, [ -9, -8, -7 ] >
gap> SetName( Gd8, "Gd8" );
gap> AGd8 := AutomorphismGroupOfGroupoid( Gd8 );;
gap> Dd8 := HomogeneousDiscreteGroupoid( d8, [-6..-4] );
homogeneous, discrete groupoid: < d8, [ -6 .. -4 ] >
gap> GeneratorsOfGroupoid( Dd8 );
[ [(1,2,3,4) : -6 -> -6], [(3,4) : -6 -> -6], [(1,2,3,4) : -5 -> -5], 
  [(3,4) : -5 -> -5], [(1,2,3,4) : -4 -> -4], [(3,4) : -4 -> -4] ]
gap> KnownAttributesOfObject( Dd8 );
[ "Pieces", "GeneratorsOfGroupoid" ]
gap> ADd8 := AutomorphismGroupOfGroupoid( Dd8 );
<group with 4 generators>
gap> Hd8 := HomogeneousGroupoid( Gd8, 
>           [ [-20,-19,-18], [-30,-29,-28], [-40,-39,-38] ] );
homogeneous groupoid with 3 pieces:
1:  single piece groupoid: < d8, [ -40, -39, -38 ] >
2:  single piece groupoid: < d8, [ -30, -29, -28 ] >
3:  single piece groupoid: < d8, [ -20, -19, -18 ] >
gap> SetName( Hd8, "Hd8" ); 
gap> AHd8 := AutomorphismGroupoidOfGroupoid( Hd8 );;
gap> AHd8!.objects;
[ [ -40, -39, -38 ], [ -30, -29, -28 ], [ -20, -19, -18 ] ]
gap> AHd8!.magma;  
<group with 8 generators>
gap> AHd8!.rays; 
[ groupoid homomorphism : 
    [ [ [(1,2,3,4) : -40 -> -40], [(3,4) : -40 -> -40], [() : -40 -> -39], 
          [() : -40 -> -38] ], 
      [ [(1,2,3,4) : -40 -> -40], [(3,4) : -40 -> -40], [() : -40 -> -39], 
          [() : -40 -> -38] ] ], groupoid homomorphism : 
    [ [ [(1,2,3,4) : -40 -> -40], [(3,4) : -40 -> -40], [() : -40 -> -39], 
          [() : -40 -> -38] ], 
      [ [(1,2,3,4) : -30 -> -30], [(3,4) : -30 -> -30], [() : -30 -> -29], 
          [() : -30 -> -28] ] ], groupoid homomorphism : 
    [ [ [(1,2,3,4) : -40 -> -40], [(3,4) : -40 -> -40], [() : -40 -> -39], 
          [() : -40 -> -38] ], 
      [ [(1,2,3,4) : -20 -> -20], [(3,4) : -20 -> -20], [() : -20 -> -19], 
          [() : -20 -> -18] ] ] ]

gap> s3a := Group( (1,2,3), (2,3) );; 
gap> s3b := Group( (4,6,8)(5,7,9), (4,9)(5,8)(6,7) );;
gap> s3c := Group( (4,6,8)(5,7,9), (5,9)(6,8) );;
gap> G1 := SinglePieceGroupoid( s3a, [-3,-2,-1] );; 
gap> SetName( G1, "G1" ); 
gap> A1 := AutomorphismGroupOfGroupoid( G1 );;
gap> SetName( A1, "A1" ); 
gap> one1 := One( A1 );; 
gap> G2 := SinglePieceGroupoid( s3b, [-6,-5,-4] );; 
gap> SetName( G2, "G2" ); 
gap> A2 := AutomorphismGroupOfGroupoid( G2 );;
gap> SetName( A1, "A1" ); ;
gap> one2 := One( A2 );; 
gap> G3 := SinglePieceGroupoid( s3c, [-9,-8,-7] );; 
gap> SetName( G3, "G3" ); 
gap> A3 := AutomorphismGroupOfGroupoid( G3 );; 
gap> SetName( A1, "GA1" ); 
gap> one3 := One( A3 );;
gap> G123 := UnionOfPieces( [ G1, G2, G3 ] );; 
gap> SetName( G123, "G123" ); 
gap> A123 := AutomorphismGroupoidOfGroupoid( G123 );; 
gap> SetName( A123, "A123" ); 
gap> ##
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
