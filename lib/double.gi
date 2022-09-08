############################################################################# 
## 
#W  double.gi                 GAP4 package `groupoids'          Chris Wensley 
##
#Y  Copyright (C) 2000-2022, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file contains the declarations for doiuble groupoids 
##  

##################### DOUBLE DOMAIN WITH OBJECTS  ########################### 

InstallMethod( SinglePieceDoubleGroupoid, "for a groupoid and a group", true, 
    [ IsGroupoid, IsGroup ], 0, 
function( gpd, G ) 

    local dgpd; 

    dgpd := rec( groupoid := gpd, group := G ); 
    ObjectifyWithAttributes( dgpd, IsDoubleGroupoidType, 
        IsCommutative, IsCommutative( gpd!.magma ) and IsCommutative( G ) ); 
    return dgpd; 
end ); 


#####################  MULT SQUARE WITH OBJECTS  ############################ 

############################################################################# 
## 
#M  MultiplicativeSquareWithObjects(<dmwo>,<elt>,<down>,<left>,<up>,<right>) 
#M  MultiplicativeSquareWithObjectsNC(<isge>,<elt>,<down>,<left>,<up>,<right>) 
##
InstallMethod( MultiplicativeSquareWithObjectsNC, 
    "for boolean, element, down, left, up and right", true,  
    [ IsBool, IsMultiplicativeElement, IsObject, IsObject, IsObject, IsObject ], 
    0, 
function( b, e, d, l, u, r ) 

    local elt, fam;

    fam := IsGroupoidElementFamily; 
    elt := Objectify( IsDoubleGroupoidElementType, [ e, d, l, u, r ] );
    return elt; 
end ); 

InstallMethod( MultiplicativeSquareWithObjects, 
    "for double groupoid with objects, element, down, left, up and right", 
    true, [ IsDoubleMagmaWithObjects, IsMultiplicativeElement, 
            IsObject, IsObject, IsObject, IsObject ], 0, 
function( dmwo, e, d, l, u, r ) 

    local gpd, gp, piece, obs, fam, pwo, pos, homset, pose; 

    gpd := dmwo.groupoid; 
    gp := dmwo.group; 
    if IsSinglePiece( gpd ) then 
        piece := dmwo; 
    else 
        piece := PieceOfObject( gpd, TailOfArrow( d ) ); ### ??? 
    fi;
    gp := piece!.magma; 
    if not ( e in gp ) then 
        Error( "<e> not in group <gp>," ); 
    fi;
    obs := piece!.objects; 
    return MultiplicativeSquareWithObjectsNC( false, e, d, l, u, r ); 
end );

#############################################################################
## 
#M  ElementOfSquare
#M  DownArrow
#M  LeftArrow 
#M  UpArrow 
#M  DownArrow
##
InstallMethod( ElementOfSquare, "generic method for double groupoid element", 
    true, [ IsMultiplicativeSquareWithObjects ], 0, e -> e![1] ); 

InstallMethod( DownArrow, "generic method for double groupoid element", 
    true, [ IsMultiplicativeSquareWithObjects ], 0, e -> e![2] ); 

InstallMethod( LeftArrow, "generic method for double groupoid element", 
    true, [ IsMultiplicativeSquareWithObjects ], 0, e -> e![3] ); 

InstallMethod( UpArrow, "generic method for double groupoid element", 
    true, [ IsMultiplicativeSquareWithObjects ], 0, e -> e![4] ); 

InstallMethod( RightArrow, "generic method for double groupoid element", 
    true, [ IsMultiplicativeSquareWithObjects ], 0, e -> e![5] ); 

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj 
##  . . . . . . . . . . . . . . . . . . for elements in a magma with objects 
##
InstallMethod( String, "for an element in a magma with objects", true, 
    [ IsMultiplicativeElementWithObjects ], 0, 
function( e ) 
    return( STRINGIFY( "[", String( e![1] ), " : ", String( e![2] ), 
                       " -> ", String( e![3] ), "]" ) ); 
end );

InstallMethod( ViewString, "for an element in a magma with objects", true, 
    [ IsMultiplicativeElementWithObjects ], 0, String ); 

InstallMethod( PrintString, "for an element in a magma with objects", true, 
    [ IsMultiplicativeElementWithObjects ], 0, String ); 

InstallMethod( ViewObj, "for an element in a magma with objects", true, 
    [ IsMultiplicativeElementWithObjects ], 0, PrintObj ); 

InstallMethod( PrintObj, "for an element in a magma with objects",
    [ IsMultiplicativeElementWithObjects ],
function ( e )
    Print( "[", e![1], " : ", e![2], " -> ", e![3], "]" );
end );

InstallMethod( ViewObj, "for an element in a magma with objects",
    [ IsMultiplicativeElementWithObjects ], PrintObj );

############################################################################# 
## 
#M  UpDownProduct( dmwo, s1, s2 ) 
##      . . . . . . . . vertical composition of squares in a double groupoid 
## 
InstallMethod( UpDownProduct, "for two squares in a double groupoid", true, 
    [ IsDoubleMagmaWithObjects, IsMultiplicativeSquareWithObjects, 
      IsMultiplicativeSquareWithObjects], 0, 
function( dmwo, s1, s2 ) 

    local prod; 

    ## elements are composable? 
    if ( ( s1![2] = s2![4] ) and 
         ( FamilyObj( s1![1] ) = FamilyObj( s2![1] ) ) ) then 
        return MultiplicativeSquareWithObjectsNC( false, 
          s2![1]*s1![1]^s2![5], s2![2], s1![3]*s2![3], s1![4], s1![5]*s2![5] ); 
    else 
        return fail; 
    fi;  
end );

############################################################################# 
## 
#M  LeftRightProduct( dmwo, s1, s2 ) 
##      . . . . . . horizantalal composition of squares in a double groupoid 
## 
InstallMethod( LeftRightProduct, "for two squares in a double groupouid", true, 
    [ IsDoubleMagmaWithObjects, IsMultiplicativeSquareWithObjects, 
      IsMultiplicativeSquareWithObjects], 0, 
function( dmwo, s1, s2 ) 

    local prod; 

    ## elements are composable? 
    if ( ( s1![5] = s2![3] ) and 
         ( FamilyObj( s1![1] ) = FamilyObj( s2![1] ) ) ) then 
        return MultiplicativeSquareWithObjectsNC( false, 
          s1![1]^s2![2]*s2![1], s1![2]*s2![2], s1![3], s1![4]*s2![4], s2![5] ); 
    else 
        return fail; 
    fi;  
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
function( dmwo )
    Print( "#I  should be using special groupoid method!\n" ); 
    Print( "    group = ", dmwo!.group, "\n" ); 
    Print( "  objects = ", dmwo!.objects, "\n" ); 
end );

InstallMethod( PrintObj, "for a double groupoid", true, 
    [ IsDoubleGroupoid and IsSinglePiece ], 0, 
function( dmwo )
    Print( "#I  should be using special groupoid method!\n" ); 
    Print( "    group = ", dmwo!.group, "\n" ); 
    Print( "  objects = ", dmwo!.objects, "\n" ); 
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

##############################################################################
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

#############################################################################
##
#E  double.gi . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  
