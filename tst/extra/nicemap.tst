############################################################################
##
#W  nicemap.tst                Groupoids Package               Chris Wensley
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
gap> AGa4 = AutomorphismGroup( Ga4 );
true
gap> nob := NiceObject( AGa4 );; 
gap> StructureDescription( nob );
"(((A4 x A4 x A4) : C2) : C3) : C2"
gap> gennob := GeneratorsOfGroup( nob );
[ f1, f2, f3, f4, f5, f6, f7, f8, f9, f10, f11, f12 ]
gap> nmon := NiceMonomorphism( AGa4 );
MappingByFunction( <group of size 20736 with 
8 generators>, (((A4 x A4 x A4) : C2) : C3) : C2, function( alpha ) ... end )
gap> IsPcGroup( Range( nmon ) );
true

gap> #
gap> SetInfoLevel( InfoGroupoids, gpd_infolevel_saved );; 
