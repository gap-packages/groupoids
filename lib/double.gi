############################################################################# 
## 
#W  double.gi                 GAP4 package `groupoids'          Chris Wensley 
##
#Y  Copyright (C) 2000-2023, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file contains the implementations for basic double groupoids 
##  

#####################  MULT SQUARE WITH OBJECTS  ########################### 

############################################################################# 
## 
#M  SquareOfArrows( <dmwo>, <elt>, <down>, <left>, <up>, <right> ) 
#M  SquareOfArrowsNC( <elt>, <down>, <left>, <up>, <right> ) 
##
InstallMethod( SquareOfArrowsNC, 
    "for element, up, left, right and down", true,  
    [ IsMultiplicativeElement, IsObject, IsObject, IsObject, IsObject ], 
    0, 
function( e, u, l, r, d ) 

    local elt, fam;

    fam := IsGroupoidElementFamily; 
    elt := Objectify( IsDoubleGroupoidElementType, [ e, u, l, r, d ] );
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
        ok := ( e = loop![1] ); 
        if not ok then 
Print( "[e,loop![1]] = ", [e,loop![1]], "\n" ); 
            Info( InfoGroupoids, 2, "here" ); 
            Info( InfoGroupoids, 1, "element ", e, 
                               " <> boundary element ", loop![1] ); 
            return fail; 
        fi; 
        sq := SquareOfArrowsNC( e, u, l, r, d ); 
        SetBoundaryOfSquare( sq, e ); 
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
InstallMethod( ElementOfSquare, "generic method for double groupoid element", 
    true, [ IsDoubleGroupoidElement ], 0, e -> e![1] ); 

InstallMethod( UpArrow, "generic method for double groupoid element", 
    true, [ IsDoubleGroupoidElement ], 0, e -> e![2] ); 

InstallMethod( LeftArrow, "generic method for double groupoid element", 
    true, [ IsDoubleGroupoidElement ], 0, e -> e![3] ); 

InstallMethod( RightArrow, "generic method for double groupoid element", 
    true, [ IsDoubleGroupoidElement ], 0, e -> e![4] ); 

InstallMethod( DownArrow, "generic method for double groupoid element", 
    true, [ IsDoubleGroupoidElement ], 0, e -> e![5] ); 


#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj 
##  . . . . . . . . . . . . . . . . . . for elements in a double groupoid  
##
InstallMethod( String, "for a square in a double groupoid", true, 
    [ IsMultiplicativeElementWithObjects ], 0, 
function( e ) 
    return( STRINGIFY( "[", String( e![1] ), " : ", String( e![2] ), 
                       " -> ", String( e![3] ), "]" ) ); 
end );

InstallMethod( ViewString, "for a square in a double groupoid", true, 
    [ IsDoubleGroupoidElement ], 0, String ); 

InstallMethod( PrintString, "for a square in a double groupoid", true, 
    [ IsDoubleGroupoidElement ], 0, String ); 

InstallMethod( ViewObj, "for a square in a double groupoid", true, 
    [ IsDoubleGroupoidElement ], 0, PrintObj ); 

InstallMethod( PrintObj, "for a square in a double groupoid",
    [ IsDoubleGroupoidElement ],
function ( e ) 
    local up, lt, rt, dn; 
    up := UpArrow( e ); 
    lt := LeftArrow( e ); 
    rt := RightArrow( e ); 
    dn := DownArrow( e ); 
    Print( "[", up![2], "] ---- ", up![1], " ---> [", up![3], "]\n" );
    Print( "  |                         |\n" );
    Print( lt![1], "    ", e![1], "    ", rt![1], "\n" ); 
    Print( "  V                         V\n" ); 
    Print( "[", dn![2], "] ---- ", dn![1], " ---> [", dn![3], "]" );
end );

InstallMethod( ViewObj, "for an element in a magma with objects",
    [ IsMultiplicativeElementWithObjects ], PrintObj );

############################################################################# 
## 
#M  UpDownProduct( dmwo, s1, s2 ) 
##  . . . . . . . vertical composition of squares in a basic double groupoid 
## 
InstallMethod( UpDownProduct, "for two squares in a basic double groupoid", 
    true, [ IsBasicDoubleGroupoid, IsDoubleGroupoidElement, 
            IsDoubleGroupoidElement], 0, 
function( dgpd, s1, s2 ) 

    local m; 

    ## elements are composable? 
    if not ( ( s1![5] = s2![2] ) and 
             ( FamilyObj( s1![1] ) = FamilyObj( s2![1] ) ) ) then 
        Info( InfoGroupoids, 1, "down arrow of s1 <> up arrow of s2" ); 
        return fail; 
    fi; 
    m := s2![1] * s1![1]^s2![4]![1]; 
    return SquareOfArrowsNC( m, s1![2], s1![3]*s2![3], 
                                s1![4]*s2![4], s2![5] ); 
end );

############################################################################# 
## 
#M  LeftRightProduct( dmwo, s1, s2 ) 
##      . . . . horizantal composition of squares in a basic double groupoid 
## 
InstallMethod( LeftRightProduct, 
    "for two squares in a basic double groupouid", true, 
    [ IsBasicDoubleGroupoid, IsDoubleGroupoidElement, 
      IsDoubleGroupoidElement], 0, 
function( dgpd, s1, s2 ) 

    local m; 

    ## elements are composable? 
    if not ( ( s1![4] = s2![3] ) and 
             ( FamilyObj( s1![1] ) = FamilyObj( s2![1] ) ) ) then 
        Info( InfoGroupoids, 1, "right arrow of s1 <> left arrow of s2" ); 
        return fail; 
    fi; 
    m := s1![1]^s2![5]![1] * s1![1];
    return SquareOfArrowsNC( m, s1![2]*s2![2], s1![3], 
                                s2![4], s1![5]*s2![5] ); 
end );

############################################################################# 
## 
#M  \^( e, p ) . . . . . . power (inverse) of element in a magma with objects 
## 
InstallMethod( \^, "for an element in a double groupoid and a PosInt", 
    true, [ IsMultiplicativeElementWithObjects, IsPosInt ], 0, 
function( e, p ) 
    ##  should be able to invert an identity element 
    ##  groupoids use their own method 
    if ( e![3] = e![2] ) then 
        return ArrowNC( false, e![1]^p, e![2], e![3] ); 
    else 
        return fail; 
    fi;  
end );

#############################################################################
##
#M  \in( <elt>, <mwo> ) . . . . test if an element is in a magma with objects 
##

InstallMethod( \in, "for mwo element and a standard magma with objects", true, 
    [ IsMultiplicativeElementWithObjects, 
      IsMagmaWithObjects and IsSinglePiece ], 0,
function( e, dmwo ) 

    local obs; 

    obs := dmwo!.objects; 
    if not ( (e![2] in obs) and (e![3] in obs) ) then 
        return false; 
    fi; 
    if ( HasIsDirectProductWithCompleteDigraph( dmwo ) 
         and IsDirectProductWithCompleteDigraph( dmwo ) ) then 
        return (e![1] in dmwo!.magma);
    else 
        Error( "dmwo not a double groupoid" ); 
    fi; 
end ); 

InstallMethod( \in, "for mwo element and a union of constituents", true, 
    [ IsMultiplicativeElementWithObjects, IsMagmaWithObjects and HasPieces ], 0,
function( e, mwo )
    return e in PieceOfObject( mwo, e![2] ); 
end );

#############################################################################
##
#M  Size 
##
InstallOtherMethod( Size, "generic method for a magma with objects", true,
    [ IsDoubleGroupoid ], 0,
function( dmwo )

    local p, s;

    if ( HasIsDirectProductWithCompleteDigraph( dmwo ) and 
            IsDirectProductWithCompleteDigraph( dmwo ) ) then 
        return Size( dmwo!.magma ) * Length( dmwo!.objects )^2; 
    elif ( HasIsDiscreteDomainWithObjects( dmwo ) and 
              IsDiscreteDomainWithObjects( dmwo ) ) then 
        return Size( dmwo!.magma ) * Length( dmwo!.objects ); 
    elif ( HasIsSinglePieceDomain( dmwo ) and 
              IsSinglePieceDomain( dmwo ) ) then 
        return Size( dmwo!.magma ) * Length( dmwo!.objects )^2;
Print("reached here\n");
    elif HasPieces( dmwo ) then 
        s := 0; 
        for p in Pieces( dmwo ) do 
            s := s + Size(p); 
        od;
        return s; 
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

InstallMethod( ViewString, "for an element in a magma with objects", true, 
    [ IsMultiplicativeElementWithObjects ], 0, String ); 

InstallMethod( PrintString, "for an element in a magma with objects", true, 
    [ IsMultiplicativeElementWithObjects ], 0, String ); 

InstallMethod( ViewObj, "for an element in a magma with objects", true, 
    [ IsMultiplicativeElementWithObjects ], 0, PrintObj ); 

InstallMethod( ViewObj, "for a double groupoid", true, 
    [ IsDoubleGroupoid and IsSinglePiece ], 0,   
function( dgpd )
    Print( " groupoid = ", dgpd!.groupoid, "\n" ); 
    Print( "    group = ", dgpd!.group, "\n" ); 
    Print( "  objects = ", dgpd!.objects, "\n" ); 
end );

InstallMethod( PrintObj, "for a double groupoid", true, 
    [ IsDoubleGroupoid and IsSinglePiece ], 0, 
function( dgpd )
    Print( " groupoid = ", dgpd!.groupoid, "\n" ); 
    Print( "    group = ", dgpd!.group, "\n" ); 
    Print( "  objects = ", dgpd!.objects, "\n" ); 
end );

InstallMethod( ViewObj, "for more than one piece", true, 
    [ IsDoubleDomainWithObjects and IsPiecesRep ], 10,   
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
    [ IsDoubleDomainWithObjects and IsPiecesRep ], 0,   
function( dmwo )

    local i, pieces, np; 

    pieces := Pieces( dmwo ); 
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
#M  Display( <dmwo> ) . . . . . . . . . . . . . . display a magma with objects
##
InstallMethod( Display, "for a dmwo", true, [ IsDoubleDomainWithObjects ], 0, 
function( dmwo )
    
    local comp, c, i, m, len;

    if IsSinglePiece( dmwo ) then 
        if IsDirectProductWithCompleteDigraph( dmwo ) then 
            Print( "Single constituent magma with objects: " );
            if HasName( dmwo ) then
                Print( dmwo );
            fi;
            Print( "\n" ); 
            Print( "  objects: ", dmwo!.objects, "\n" );
            m := dmwo!.magma;
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
        comp := Pieces( dmwo );
        len := Length( comp );
        Print( "Magma with objects with ", len, " constituents:\n" );
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

InstallMethod( SinglePieceBasicDoubleGroupoid, "for a groupoid", true, 
    [ IsGroupoid ], 0, 
function( gpd ) 

    local dgpd; 

    dgpd := rec( groupoid := gpd, objects := ObjectList( gpd ) ); 
    ObjectifyWithAttributes( dgpd, IsDoubleGroupoidType, 
        IsSinglePiece, true, 
        IsAssociative, true, 
        IsCommutative, IsCommutative( gpd!.magma ), 
        IsBasicDoubleGroupoid, true ); 
    return dgpd; 
end ); 

############################################################################
##
#E double.gi . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  
