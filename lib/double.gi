############################################################################# 
## 
#W  double.gi                 GAP4 package `groupoids'          Chris Wensley 
##  
##  This file contains the implementations for basic double groupoids 
##  

#####################  MULT SQUARE WITH OBJECTS  ########################### 

############################################################################ 
## 
#M  SquareOfArrows( <dmwo>, <elt>, <down>, <left>, <up>, <right> ) 
#M  SquareOfArrowsNC( <dmwo>, <elt>, <down>, <left>, <up>, <right> ) 
##
InstallMethod( SquareOfArrowsNC, 
    "for double groupoid, element, up, left, right and down", true,  
    [ IsDoubleGroupoid, IsMultiplicativeElement, 
      IsObject, IsObject, IsObject, IsObject ], 0, 
function( dg, m, u, l, r, d ) 

    local elt, fam;

    fam := IsGroupoidElementFamily; 
    elt := Objectify( IsDoubleGroupoidElementType, [ dg, m, u, l, r, d ] );
    return elt; 
end ); 

InstallMethod( SquareOfArrows, 
    "for double groupoid with objects, element, up, left, right and down", 
    true, [ IsBasicDoubleGroupoid, IsMultiplicativeElement, 
            IsObject, IsObject, IsObject, IsObject ], 0, 
function( dmwo, e, u, l, r, d ) 

    local  gpd, ok, piece, loop, sq; 

    gpd := dmwo!.groupoid;     
    ok := ForAll( [u,l,r,d], x -> x in gpd ) 
             and ( TailOfArrow( u ) = TailOfArrow( l ) ) 
             and ( HeadOfArrow( u ) = TailOfArrow( r ) ) 
             and ( HeadOfArrow( l ) = TailOfArrow( d ) ) 
             and ( HeadOfArrow( r ) = HeadOfArrow( d ) ); 
    if not ok then 
        Info( InfoGroupoids, 1, "the four arrows do not form a square" );
        return fail; 
    else 
        if IsSinglePiece( gpd ) then 
            piece := dmwo; 
        else 
            piece := PieceOfObject( gpd, TailOfArrow( d ) ); 
        fi; 
        loop := d^-1 * l^-1 * u * r; 
        ok := ( e = loop![2] ); 
        if not ok then 
            Info( InfoGroupoids, 2, "here" ); 
            Info( InfoGroupoids, 1, "element ", e, 
                               " <> boundary element ", loop![2] ); 
            return fail; 
        fi; 
        sq := SquareOfArrowsNC( dmwo, e, u, l, r, d ); 
        return sq; 
    fi; 
end );

InstallOtherMethod( SquareOfArrows, 
    "for double groupoid with objects, up, left, right and down", 
    true, [ IsDoubleGroupoid, IsObject, IsObject, IsObject, IsObject ], 0, 
function( dmwo, u, l, r, d )

    local  gpd, ok, piece, loop, m, sq; 

    gpd := dmwo!.groupoid;     
    ok := ForAll( [u,l,r,d], x -> x in gpd ) 
             and ( TailOfArrow( u ) = TailOfArrow( l ) ) 
             and ( HeadOfArrow( u ) = TailOfArrow( r ) ) 
             and ( HeadOfArrow( l ) = TailOfArrow( d ) ) 
             and ( HeadOfArrow( r ) = HeadOfArrow( d ) ); 
    if not ok then 
        Info( InfoGroupoids, 1, "the four arrows do not form a square" );
        return fail; 
    else 
        if IsSinglePiece( gpd ) then 
            piece := dmwo; 
        else 
            piece := PieceOfObject( gpd, TailOfArrow( d ) ); 
        fi;
        m := ( d^-1 * l^-1 * u * r )![2];
        sq := SquareOfArrowsNC( dmwo, m, u, l, r, d ); 
        return sq; 
    fi; 
end );

#############################################################################
## 
#M  ElementOfSquare
#M  DownArrow
#M  LeftArrow 
#M  UpArrow 
#M  DownArrow
#M  BoundaryOfSquare 
##
InstallMethod( DoubleGroupoidOfSquare, "generic method for double gpd element", 
    true, [ IsDoubleGroupoidElement ], 0, s -> s![1] ); 

InstallMethod( ElementOfSquare, "generic method for double groupoid element", 
    true, [ IsDoubleGroupoidElement ], 0, s -> s![2] ); 

InstallMethod( UpArrow, "generic method for double groupoid element", 
    true, [ IsDoubleGroupoidElement ], 0, s -> s![3] ); 

InstallMethod( LeftArrow, "generic method for double groupoid element", 
    true, [ IsDoubleGroupoidElement ], 0, e -> e![4] ); 

InstallMethod( RightArrow, "generic method for double groupoid element", 
    true, [ IsDoubleGroupoidElement ], 0, s -> s![5] ); 

InstallMethod( DownArrow, "generic method for double groupoid element", 
    true, [ IsDoubleGroupoidElement ], 0, s -> s![6] ); 

InstallMethod( BoundaryOfSquare, "generic method for double groupoid element", 
    true, [ IsDoubleGroupoidElement ], 0,
    s -> s![6]^(-1) * s![4]^(-1) * s![3] * s![5] );

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj 
##  . . . . . . . . . . . . . . . . . . for elements in a double groupoid  
##
InstallMethod( String, "for a square in a double groupoid", true, 
    [ IsMultiplicativeElementWithObjects ], 0, 
function( e ) 
    return( STRINGIFY( "[", String( e![2] ), " : ", String( e![3] ), 
                       " -> ", String( e![4] ), "]" ) ); 
end );

InstallMethod( ViewString, "for a square in a double groupoid", true, 
    [ IsDoubleGroupoidElement ], 0, String ); 

InstallMethod( PrintString, "for a square in a double groupoid", true, 
    [ IsDoubleGroupoidElement ], 0, String ); 

InstallMethod( ViewObj, "for a square in a double groupoid", true, 
    [ IsDoubleGroupoidElement ], 0, PrintObj ); 

InstallMethod( PrintObj, "for a square in a double groupoid",
    [ IsDoubleGroupoidElement ],
function ( sq ) 
    local up, lt, rt, dn, es, spaces, dashes,
          upstr, ltstr, rtstr, dnstr, uplen, ltlen, rtlen, dnlen, esstr, eslen,
          toplen, midlen, botlen, maxlen, topgap, midgap, botgap; 
    up := UpArrow( sq );
    lt := LeftArrow( sq ); 
    rt := RightArrow( sq ); 
    dn := DownArrow( sq );
    es := ElementOfSquare( sq );
    spaces := "                                                  ";
    dashes := "--------------------------------------------------";
    upstr := [ String( up![2] ), String( up![3] ), String( up![4] ) ];
    uplen := List( upstr, s -> Length(s) );
    ltstr := [ String( lt![2] ), String( lt![3] ), String( lt![4] ) ];
    ltlen := List( ltstr, s -> Length(s) );
    rtstr := [ String( rt![2] ), String( rt![3] ), String( rt![4] ) ];
    rtlen := List( rtstr, s -> Length(s) );
    dnstr := [ String( dn![2] ), String( dn![3] ), String( dn![4] ) ];
    dnlen := List( dnstr, s -> Length(s) );
    esstr := String( es );
    eslen := Length( esstr );
    toplen := uplen[2]+2 + 6 + uplen[1] + 6 + uplen[3]+2;
    midlen := ltlen[1] + 6 + eslen + 6 + rtlen[1];
    botlen := dnlen[2]+2 + 6 + dnlen[1] + 6 + dnlen[3]+2;
    maxlen := Maximum( [ toplen, midlen, botlen ] );
    topgap := 4;
    midgap := 8;
    botgap := 4;
    if ( maxlen = midlen ) then
        topgap := 4 + Int( (maxlen - toplen)/2 );
        botgap := 4 + Int( (maxlen - botlen)/2 );
    elif ( maxlen = botlen ) then
        topgap := topgap + Int( (botlen-toplen)/2 );
        midgap := midgap - 2 + Int( (botlen-midlen)/2 );
    elif ( maxlen = toplen ) then
        botgap := botgap + Int( (toplen-botlen)/2 );
        midgap := midgap - 2 + Int( (toplen-midlen)/2 );
    fi;
    Print( "[", up![3], "] ", dashes{[1..topgap]}, " ", up![2],
           " ", dashes{[1..topgap-1]}, "> [", up![4], "]\n" );
    Print( "  |  ", spaces{[1..maxlen-11]}, "   |\n" );
    Print( lt![2], spaces{[1..midgap]}, es, spaces{[1..midgap]}, rt![2], "\n" ); 
    Print( "  V  ", spaces{[1..maxlen-11]}, "   V\n" ); 
    Print( "[", dn![3], "] ", dashes{[1..botgap]}, " ", dn![2],
           " ", dashes{[1..botgap-1]}, "> [", dn![4], "]" );
end );

InstallMethod( ViewObj, "for an element in a magma with objects",
    [ IsMultiplicativeElementWithObjects ], PrintObj );

#############################################################################
##
#M  IsCommutingSquare
##
InstallMethod( IsCommutingSquare, "for a square in a double groupoid", true,
    [ IsDoubleGroupoidElement ], 0,
function( sq )
    return sq![4] * sq![6] = sq![3] * sq![5];
end );

############################################################################# 
## 
#M  HorizontalProduct( s1, s2 ) 
##      . . . . horizantal composition of squares in a basic double groupoid 
## 
InstallMethod( HorizontalProduct, 
    "for two squares in a basic double groupoid", true, 
    [ IsDoubleGroupoidElement, IsDoubleGroupoidElement ], 0, 
function( s1, s2 ) 

    local m; 

    ## elements are composable? 
    if not ( s1![5] = s2![4] ) then 
        Info( InfoGroupoids, 1, "right arrow of s1 <> left arrow of s2" ); 
        return fail; 
    fi; 
    if not ( s1![1] = s2![1] ) then 
        Info( InfoGroupoids, 1, "s1 and s2 are in different double groupoids" ); 
        return fail; 
    fi; 
    m := s1![2]^s2![6]![2] * s2![2];
    return SquareOfArrowsNC( s1![1], m, s1![3]*s2![3], s1![4], 
                                        s2![5], s1![6]*s2![6] ); 
end );

############################################################################# 
## 
#M  VerticalProduct( s1, s2 ) 
##  . . . . . . . vertical composition of squares in a basic double groupoid 
## 
InstallMethod( VerticalProduct, "for two squares in a basic double groupoid", 
    true, [ IsDoubleGroupoidElement, IsDoubleGroupoidElement ], 0, 
function( s1, s2 ) 

    local m; 

    ## elements are composable? 
    if not ( s1![6] = s2![3] ) then 
        Info( InfoGroupoids, 1, "down arrow of s1 <> up arrow of s2" ); 
        return fail; 
    fi; 
    if not ( s1![1] = s2![1] ) then 
        Info( InfoGroupoids, 1, "s1 and s2 are in different double groupoids" ); 
        return fail; 
    fi; 
    m := s2![2] * s1![2]^s2![5]![2]; 
    return SquareOfArrowsNC( s1![1], m, s1![3], s1![4]*s2![4], 
                                        s1![5]*s2![5], s2![6] ); 
end );

############################################################################# 
## 
#M  HorizontalIdentities( s ) 
## 
InstallMethod( HorizontalIdentities, "for a square in a basic double groupoid", 
    true, [ IsDoubleGroupoidElement ], 0, 
function( s ) 

    local b, c, gpd, u, v, w, x, one, ui, vi, wi, xi, lti, rti; 

    b := LeftArrow( s );
    c := RightArrow( s );
    gpd := (s![1])!.groupoid;
    u := b![3];  w := b![4];  v := c![3];  x := c![4];
    one := One( b![2] );
    ui := Arrow( gpd, one, u, u );
    vi := Arrow( gpd, one, v, v );
    wi := Arrow( gpd, one, w, w );
    xi := Arrow( gpd, one, x, x );
    lti := SquareOfArrowsNC( s![1], one, ui, b, b, wi );
    rti := SquareOfArrowsNC( s![1], one, vi, c, c, xi );
    return [ lti, rti ];
end );

############################################################################# 
## 
#M  VerticalIdentities( s ) 
## 
InstallMethod( VerticalIdentities, "for a square in a basic double groupoid", 
    true, [ IsDoubleGroupoidElement ], 0, 
function( s ) 

    local a, d, gpd, u, v, w, x, one, ui, vi, wi, xi, upi, dni; 

    a := UpArrow( s );
    d := DownArrow( s );
    gpd := (s![1])!.groupoid;
    u := a![3];  v := a![4];  w := d![3];  x := d![4];
    one := One( a![2] );
    ui := Arrow( gpd, one, u, u );
    vi := Arrow( gpd, one, v, v );
    wi := Arrow( gpd, one, w, w );
    xi := Arrow( gpd, one, x, x );
    upi := SquareOfArrowsNC( s![1], one, a, ui, vi, a );
    dni := SquareOfArrowsNC( s![1], one, d, wi, xi, d );
    return [ upi, dni ];
end );

############################################################################# 
## 
#M  HorizontalInverse( s ) 
## 
InstallMethod( HorizontalInverse, "for a square in a basic double groupoid", 
    true, [ IsDoubleGroupoidElement ], 0, 
function( s ) 

    local a, b, c, d, gpd, bdy;

    a := UpArrow( s );
    b := LeftArrow( s );
    c := RightArrow( s );
    d := DownArrow( s );
    gpd := (s![1])!.groupoid;
    bdy := d * c^-1 * a^-1 * b;
    return SquareOfArrowsNC( s![1], bdy![2], a^-1, c, b, d^-1 );
end );

############################################################################# 
## 
#M  VerticalInverse( s ) 
## 
InstallMethod( VerticalInverse, "for a square in a basic double groupoid", 
    true, [ IsDoubleGroupoidElement ], 0, 
function( s ) 

    local a, b, c, d, gpd, bdy;

    a := UpArrow( s );
    b := LeftArrow( s );
    c := RightArrow( s );
    d := DownArrow( s );
    gpd := (s![1])!.groupoid;
    bdy := a^-1 * b * d * c^-1;
    return SquareOfArrowsNC( s![1], bdy![2], d, b^-1, c^-1, a );
end );

############################################################################# 
## 
#M  TransposedSquare( sq ) 
##
InstallMethod( TransposedSquare, "for a square in a double groupoid", true, 
    [ IsDoubleGroupoidElement ], 0,
function( sq )
    local dg, a, b, c, d, m, tsq; 

    dg := DoubleGroupoidOfSquare( sq );
    a := UpArrow( sq );
    b := LeftArrow( sq );
    c := RightArrow( sq );
    d := DownArrow( sq );
    m := BoundaryOfSquare( sq )![2];
    return SquareOfArrowsNC( dg, m^-1, b, a, d, c ); 
end );

InstallMethod( IsClosedUnderTransposition, "for a square in a double groupoid",
    true, [ IsDoubleGroupoidElement ], 0,
function( sq )
    local tsq; 

    tsq := TransposedSquare( sq );
    return sq = tsq;
end );

#############################################################################
##
#M  \in( <sq>, <dg> ) . . . . . . . test if a square is in a double groupoid 
##

InstallMethod( \in, "for a square in a standard double groupoid", true, 
    [ IsDoubleGroupoidElement, IsDoubleGroupoid and IsSinglePiece ], 0,
function( sq, dg ) 
    return ( sq![1] = dg );
end ); 

InstallMethod( \in, "for a square in a double groupoid with pieces", true, 
    [ IsDoubleGroupoidElement, IsDoubleGroupoid and HasPieces ], 0,
function( sq, dg )
    local u, p; 

    u := sq![3]![3];
    p := PieceOfObject( dg, u );
    if p = fail then 
        return false; 
    else 
        return sq in p; 
    fi;
end );

#############################################################################
##
#M  \=( <dg1>, <dg2> ) . . . . . . . . . . test equality of double groupoids 
##

InstallMethod( \=, "for two double groupoids", true, 
    [ IsDoubleGroupoid, IsDoubleGroupoid ], 0,
function( dg1, dg2 ) 

    local p1, p2, i;

    if ( IsSinglePiece( dg1 ) and IsSinglePiece( dg2 ) ) then 
        return ( ( dg1!.objects = dg2!.objects ) 
             and ( dg1!.groupoid = dg2!.groupoid ) ); 
    elif ( IsSinglePiece( dg1 ) or IsSinglePiece( dg2 ) ) then 
        return false; 
    else 
        p1 := Pieces( dg1 ); 
        p2 := Pieces( dg2 ); 
        if not ( Length( p1 ) = Length( p2 ) ) then 
            return false; 
        else 
            for i in [1..Length( p1 )] do 
                if ( p1[i] <> p2[i] ) then 
                    return false; 
                fi; 
            od; 
            return true; 
        fi; 
    fi;
end ); 

#############################################################################
##
#M  Size 
##
InstallOtherMethod( Size, "generic method for a double groupoid", true,
    [ IsDoubleGroupoid ], 0,
function( dg )

    local gpd, gp, obs, p, s;

    gpd := dg!.groupoid;
    gp := gpd!.magma;
    obs := dg!.objects;
    if IsBasicDoubleGroupoid( dg ) then 
        return Size( gp )^4 * Length( obs )^4; 
    else 
        TryNextMethod();  
    fi;
end );

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj 
##  . . . . . . . . . . . . . . . . . . . . . . . . . . for a double groupoid 
##
InstallMethod( String, "for a double groupoid", true, [ IsDoubleGroupoid ], 0, 
function( mwo ) 
    return( STRINGIFY( "groupoid" ) ); 
end );

InstallMethod( ViewString, "for a double groupoid", true, 
    [ IsDoubleGroupoid ], 0, String ); 

InstallMethod( PrintString, "for a double groupoid", true, 
    [ IsDoubleGroupoid ], 0, String ); 

InstallMethod( ViewObj, "for a double groupoid", true, 
    [ IsDoubleGroupoid ], 0, PrintObj ); 

InstallMethod( ViewObj, "for a double groupoid", true, 
    [ IsDoubleGroupoid and IsSinglePiece ], 0,   
function( dgpd )
    Print( "single piece double groupoid with:\n" );
    Print( " groupoid = ", dgpd!.groupoid, "\n" ); 
    Print( "    group = ", dgpd!.groupoid!.magma, "\n" ); 
    Print( "  objects = ", dgpd!.objects ); 
end );

InstallMethod( PrintObj, "for a double groupoid", true, 
    [ IsDoubleGroupoid and IsSinglePiece ], 0, 
function( dgpd )
    Print( "single piece double groupoid with:\n" );
    Print( " groupoid = ", dgpd!.groupoid, "\n" ); 
    Print( "    group = ", dgpd!.groupoid!.magma, "\n" ); 
    Print( "  objects = ", dgpd!.objects, "\n" ); 
end );

InstallMethod( ViewObj, "for more than one piece", true, 
    [ IsDoubleGroupoid and IsPiecesRep ], 10,   
function( ddwo )

    local i, pieces, np; 

    pieces := Pieces( ddwo ); 
    np := Length( pieces ); 
    Print( "double groupoid having ", np, " pieces :-\n" ); 
    for i in [1..np] do 
        Print( i, ": ", pieces[i] ); 
        if HasName( pieces[i] ) then 
            Print( "\n" ); 
        fi; 
    od; 
end ); 

InstallMethod( PrintObj, "for more than one piece", true, 
    [ IsDoubleGroupoid and IsPiecesRep ], 0,   
function( dg )

    local i, pieces, np; 

    pieces := Pieces( dg ); 
    np := Length( pieces ); 
    Print( "domain with objects having ", np, " pieces :-\n" ); 
    for i in [1..np] do 
        Print( pieces[i] ); 
        if HasName( pieces[i] ) then 
            Print( "\n" ); 
        fi; 
    od; 
end ); 

#############################################################################
##
#M  Display( <dg> ) . . . . . . . . . . . . . display a magma with objects
##
InstallMethod( Display, "for a double groupoid", true, [ IsDoubleGroupoid ], 0, 
function( dg )
    
    local comp, c, i, m, len;

    if IsSinglePiece( dg ) then 
        if IsDirectProductWithCompleteDigraph( dg ) then 
            Print( "Single constituent magma with objects: " );
            if HasName( dg ) then
                Print( dg );
            fi;
            Print( "\n" ); 
            Print( "  objects: ", dg!.objects, "\n" );
            m := dg!.magma;
            Print( "    magma: " );
            if HasName( m ) then
                Print( m, " = <", GeneratorsOfMagma( m ), ">\n" );
            else
                Print( m, "\n" );
            fi;
        else
            TryNextMethod(); 
        fi; 
    else
        comp := Pieces( dg );
        len := Length( comp );
        Print( "Magma with objects with ", len, " pieces:\n" );
        for i in [1..len] do
            c := comp[i];
            if IsDirectProductWithCompleteDigraph( c ) then 
                Print( "< objects: ", c!.objects, "\n" );
                m := c!.magma;
                Print( "    magma: " );
                if HasName( m ) then
                    Print( m, " = <", GeneratorsOfMagma( m ), "> >\n" );
                else
                    Print( m, " >\n" );
                fi;
            else 
                TryNextMethod(); 
            fi; 
        od;
    fi;
end );

###################  DOUBLE DOMAIN WITH OBJECTS   ########################## 

#############################################################################
##
#F  DoubleGroupoid( <pieces> )          double groupoid as list of pieces 
#F  DoubleGroupoid( <gpd> )             basic single piece double groupoid 
#F  DoubleGroupoid( <gpd> )             when the groupoid has pieces
#O  DoubleGroupoidWithTrivialGroup( <obs> )
##
InstallGlobalFunction( DoubleGroupoid, function( arg )

    local nargs, gpd, pieces, len, L, dg;

    nargs := Length( arg ); 
    # list of pieces
    if ( ( nargs = 1 ) and IsList( arg[1] ) ) then
        if ForAll( arg[1], IsDoubleGroupoid ) then
            Info( InfoGroupoids, 2, "ByUnion" );
            return UnionOfPieces( arg[1] );
        elif ForAll( arg[1], IsGroupoid ) then
            pieces := arg[1];
            len := Length( pieces );
            if ( len = 1 ) then
                Info( InfoGroupoids, 2, "SinglePieceBasicDoubleGroupoid" );
                return SinglePieceBasicDoubleGroupoid( gpd );
            else
                L := List( pieces, p -> SinglePieceBasicDoubleGroupoid( p ) );
                dg := UnionOfPieces( L );
                return dg;
            fi;
        fi;
    #  groupoid
    elif ( ( nargs = 1 ) and IsGroupoid( arg[1] ) ) then
        gpd := arg[1];
        pieces := Pieces( gpd );
        len := Length( pieces );
        if ( len = 1 ) then
            Info( InfoGroupoids, 2, "SinglePieceBasicDoubleGroupoid" );
            return SinglePieceBasicDoubleGroupoid( gpd );
        else
            L := List( pieces, p -> SinglePieceBasicDoubleGroupoid( p ) );
            dg := UnionOfPieces( L );
            return dg;
        fi;
    else
        return fail;
    fi;
end );

InstallMethod( SinglePieceBasicDoubleGroupoid, "for a groupoid", true, 
    [ IsGroupoid ], 0, 
function( gpd ) 

    local dgpd; 

    dgpd := rec( groupoid := gpd, objects := ObjectList( gpd ) ); 
    ObjectifyWithAttributes( dgpd, IsDoubleGroupoidType, 
        IsSinglePiece, true, 
        IsAssociative, true, 
        IsCommutative, IsCommutative( gpd!.magma ), 
        IsBasicDoubleGroupoid, true, 
        GroupoidOfDoubleGroupoid, gpd );
    return dgpd; 
end ); 

InstallMethod( DoubleGroupoidWithTrivialGroup, "for a list of objects",
    true, [ IsList ], 0, 
function( obs ) 

    local gp, gpd, dg; 

    gp := Group( () );
    gpd := SinglePieceGroupoid( gp, obs );
    dg := SinglePieceBasicDoubleGroupoid( gpd );
    return dg; 
end ); 

InstallMethod( DoubleGroupoidWithSingleObject, "for a group and an object",
    true, [ IsGroup, IsObject ], 0, 
function( gp, obj ) 

    local gpd, dg; 

    gpd := MagmaWithSingleObject( gp, obj );
    dg := SinglePieceBasicDoubleGroupoid( gpd );
    return dg; 
end ); 

############################################################################
##
#M  GroupoidOfDoubleGroupoid( <src> <rng> <hom> ) 
##
InstallMethod( GroupoidOfDoubleGroupoid, "for a double groupoid", true,
    [ IsDoubleGroupoid ], 0,
function( dg )
    local pieces, gpds;
    if ( HasIsSinglePiece( dg ) and IsSinglePiece( dg ) ) then
        return dg!.groupoid;
    else
        pieces := Pieces( dg );
        gpds := List( pieces, p -> p!.groupoid );
        return UnionOfPieces( gpds );
    fi;
end );


############################################################################
##
#M  DoubleGroupoidHomomorphism( <src> <rng> <hom> ) . . from a groupoid hom
##
InstallMethod( DoubleGroupoidHomomorphism, "for a groupoid homomorphism",
    true, [ IsDoubleGroupoid, IsDoubleGroupoid, IsGroupoidHomomorphism ], 0,
function( src, rng, hom )

    local sgpd, rgpd, map;

    sgpd := src!.groupoid;
    rgpd := rng!.groupoid;
    if not ( sgpd = Source( hom ) ) and ( rgpd = Range( hom ) ) then 
        Error( "hom not a map between the underlying groupoids" );
    fi;
    map := rec(); 
    ObjectifyWithAttributes( map, DoubleGroupoidHomomorphismType, 
        Source, src, 
        Range, rng, 
        UnderlyingGroupoidHomomorphism, hom, 
        IsGeneralMappingWithObjects, true, 
        IsGroupWithObjectsHomomorphism, true, 
        RespectsMultiplication, true ); 
    return map; 
end );

InstallMethod( PrintObj, "for a double groupoid homomorphism", true,
    [ IsDoubleGroupoidHomomorphism ], 0, 
function ( hom ) 

    local h; 

    if HasPiecesOfMapping( hom ) then 
        Print( "double groupoid homomorphism from several pieces : \n" );
        for h in PiecesOfMapping( hom ) do 
            Print( h, "\n" ); 
        od; 
    else 
        Print( "double groupoid homomorphism : " );
        if ( HasName( Source(hom) ) and HasName( Range(hom) ) ) then 
            Print( Source(hom), " -> ", Range(hom), "\n" ); 
        else 
            Print( "based on ", UnderlyingGroupoidHomomorphism( hom ), "\n" ); 
        fi; 
    fi;
end ); 

############################################################################
##
#M  Display
##
InstallMethod( Display, "for double groupoid homomorphism", true,
    [ IsDoubleGroupoidHomomorphism ], 0,
function ( map ) 
    Print( "double groupoid homomorphism: " ); 
    Print( "[ ", Source(map), " ] -> [ ", Range(map), " ]\n" ); 
    Print( "with underlying groupoid homomorphism:\n" ); 
    Display( UnderlyingGroupoidHomomorphism( map ) );
end );

############################################################################
##
#M  ImageElm
##
InstallOtherMethod( ImageElm, "for a double groupoid homomorphism and a square", 
    true, [ IsDoubleGroupoidHomomorphism, IsDoubleGroupoidElement ], 0,
function ( hom, sq )

    local mor, up, lt, rt, dn, e, isq;

    if not ( sq in Source( hom ) ) then 
        Error( "the square sq is not in the source of hom" ); 
    fi; 
    mor := UnderlyingGroupoidHomomorphism( hom );
    up := ImageElm( mor, UpArrow( sq ) );
    lt := ImageElm( mor, LeftArrow( sq ) );
    rt := ImageElm( mor, RightArrow( sq ) );
    dn := ImageElm( mor, DownArrow( sq ) );
    e := dn![2]^-1 * lt![2]^-1 * up![2] * rt![2];
    isq := SquareOfArrowsNC( Range( hom ), e, up, lt, rt, dn );
    return isq;
end ); 

############################################################################
##
#E double.gi . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  
