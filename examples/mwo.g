############################################################################# 
## 
#W  mwo.g                  GAP4 package `groupoids'             Chris Wensley 
##
#Y  Copyright (C) 2000-2017, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  

SetInfoLevel( InfoGroupoids, 1 );
## TraceImmediateMethods( true );


Print( "\n==============================================================\n");
Print(   "<<<<       testing examples in the groupoids manual       >>>>\n" );
Print(   "<<<< functions for magmas with objects and their mappings >>>>\n" );
Print( "\n==============================================================\n\n");


Print("\n========================  MAGMA  ============================\n\n");

### Section 1.1.1 : Magma with objects ###

tm := [[1,2,4,3],[1,2,4,3],[3,4,2,1],[3,4,2,1]]; 
Display( tm );
m := MagmaByMultiplicationTable(tm); 
SetName( m, "m" ); 
m1 := MagmaElement(m,1); 
m2 := MagmaElement(m,2); 
m3 := MagmaElement(m,3); 
m4 := MagmaElement(m,4); 
Print( "One(m) = ", One(m), "\n\n" ); 
M78 := MagmaWithObjects( m, [-8,-7] ); 
Print( "M78 = ", M78, "\n" ); 
SetName( M78, "M78" ); 
Print( "Is M78 associative, commutative? ", 
    [ IsAssociative(M78), IsCommutative(M78) ], "\n\n" ); 
Print( KnownTruePropertiesOfObject( M78 ), "\n\n" ); 
Print( "M78 has root object and object list ", 
           [ RootObject( M78 ), ObjectList( M78 ) ], "\n" ); 

### Section 1.1.2 : multiplicative element with objects ###

a78 := Arrow( M78, m2, -7, -8 ); 
Print( "a78 = ", a78, "\n" );
Print( "a78 in M78 ? ", a78 in M78, "\n" );
b87 := Arrow( M78, m4, -8, -7 ); 
Print( "b87 = ", b87, "\n" );
ba := b87*a78; 
ab := a78*b87;
Print( "[ba,ab,a78^2,ba^2,ba^3] = ", [ba,ab,a78^2,ba^2,ba^3], "\n" );

### Section 1.1.3 : IsSinglePiece ###

Print( "\nIsSinglePiece( M78 ) ? ", IsSinglePiece( M78 ), "\n" ); 
Print( "IsDirectProductWithCompleteDigraph( M78 ) ? ", 
        IsDirectProductWithCompleteDigraph( M78 ), "\n" ); 
Print( "IsDiscrete( M78 ) ? ", IsDiscrete( M78 ), "\n" ); 

Print("\n\n====================  SEMIGROUP  ==========================\n\n");

### Section 1.2.1 : Semigroup with objects ###

t := Transformation( [1,1,2,3] ); 
s := Transformation( [2,2,3,3] );
r := Transformation( [2,3,4,4] ); 
sgp := Semigroup( t, s, r ); 
SetName( sgp, "sgp<t,s,r>" ); 
S123 := SemigroupWithObjects( sgp, [-3,-2,-1] ); 
Print( "S123 = ", S123, "\n" ); 
Print( "Is S123 associative, commutative? ", 
    [ IsAssociative(S123), IsCommutative(S123) ], "\n\n" ); 
Print( KnownTruePropertiesOfObject( S123 ), "\n\n" ); 
Print( CategoriesOfObject( S123 ), "\n\n" ); 

t12 := Arrow( S123, t, -1, -2 ); 
s23 := Arrow( S123, s, -2, -3 ); 
r31 := Arrow( S123, r, -3, -1 ); 
ts13 := t12 * s23;
sr21 := s23 * r31;
rt32 := r31 * t12;
tsr1 := ts13 * r31;
Print( "[t12,s23,r31] = ", [t12,s23,r31], "\n" );
Print( "[ts13,sr21,rt32,tsr1] = ", [ts13,sr21,rt32,tsr1], "\n" );


Print("\n=========================  MONOID  =========================\n\n");

### Section 1.3.1 : Monoid with objects ###

fm := FreeMonoid( 2, "f" ); 
em := One( fm );
gm := GeneratorsOfMonoid( fm ); 
mon := fm/[ [gm[1]^3,em], [gm[1]*gm[2],gm[2]] ]; 
M49 := MonoidWithObjects( mon, [-9,-4] ); 
Print( "M49 = ", M49, "\n" ); 
Print( "Is M49 associative, commutative? ", 
    [ IsAssociative(M49), IsCommutative(M49) ], "\n\n" ); 
Print( KnownTruePropertiesOfObject( M49 ), "\n\n" ); 
Print( CategoriesOfObject( M49 ), "\n\n" ); 
genM := GeneratorsOfMonoidWithObjects( M49 ); 
Print( "M49 has generators ", genM, "\n" ); 
g1 := genM[1];; g2 := genM[2];; g3 := genM[3];; g4 := genM[4];; 
Print( "g4*g2*g1*g3 = ", g4*g2*g1*g3, "\n" ); 


Print("\n================ GROUP AS SINGLE OBJECT GROUPOID ============\n\n");

### Section 1.4.1 : domain with single object ###

d8 := Group( (1,2,3,4), (1,3) );
SetName( d8, "d8" );
D0 := DomainWithSingleObject( d8, 0 ); 
Print( "D0 = ", D0, "\n" ); 
Print( KnownTruePropertiesOfObject( D0 ), "\n\n" ); 

p0 := Arrow( D0, (1,2,3,4), 0, 0 ); 
q0 := Arrow( D0, (1,3), 0, 0 ); 
Print( "[p0,q0,p0*q0] = ", [p0,q0,p0*q0], "\n\n" ); 
Print( "size of D0 is ", Size( D0 ), "\n" ); 


Print("\n=============  MORE THAN ONE CONSTITUENT  ===================\n\n"); 

### Section 1.4.2 : union of pieces ###

N1 := UnionOfPieces( [ M78, S123 ] ); 
Print( "N1 = ", N1, "\n" ); 
N2 := UnionOfPieces( [ M49, D0 ] );  
Print( "N2 = ", N2, "\n" ); 
N3 := UnionOfPieces( [ N1, N2] );  
Print( "N3 = ", N3, "\n" ); 
Print( "N3 has ", Length(GeneratorsOfMagmaWithObjects(N3)), " generators\n" );

## this should fail since object sets are not disjoint: 
N4 := UnionOfPieces( [ S123, DomainWithSingleObject(d8,-2) ] );  
Print( "N4 = fail? ", N4=fail, "\n\n" ); 
