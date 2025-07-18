############################################################################# 
## 
#W  mwo.gi                 GAP4 package `groupoids'             Chris Wensley 
##
##  This file contains the declarations of elements, magma, etc., and their 
##  families in the case of many objects.  So each algebraic structure comes 
##  with a set of objects, and each element $e$ has a tail object $te$ and a 
##  head object $he$, and multiplies partially if composable: on the left 
##  with those elements in its family which have head $te$, and on the right 
##  with those elements of its family which have tail $he$.  Non-composable 
##  elements return $fail$ when multiplied. 
##  

###########################  DOMAIN WITH OBJECTS  ########################### 

############################################################################# 
## 
#M  KindOfDomainWithObjects( <dwo> ) 
##
InstallMethod( KindOfDomainWithObjects, "for a list of domains with objects", 
    true, [ IsList ], 0, 
function( pieces ) 

    local kind; 

    ## kind:  1=gpd, 2=mon, 3=sgp, 4=mgm, 5=dom 
    kind := 0; 
    if ForAll( pieces, p -> ( FamilyObj(p) =  IsGroupoidFamily ) ) then 
        kind := 1; 
    elif ForAll( pieces, p -> ( ( FamilyObj(p) = IsMonoidWithObjectsFamily ) 
                             or ( FamilyObj(p) = IsGroupoidFamily ) ) ) then
        kind := 2; 
    elif ForAll( pieces, p -> ( ( FamilyObj(p) = IsSemigroupWithObjectsFamily ) 
                             or ( FamilyObj(p) = IsMonoidWithObjectsFamily ) 
                             or ( FamilyObj(p) = IsGroupoidFamily ) ) ) then 
        kind := 3; 
    elif ForAll( pieces, p -> 
        ( "IsMagmaWithObjects" in CategoriesOfObject(p) ) ) then 
        kind := 4; 
    elif ForAll( pieces, p -> 
        ( "IsDomainWithObjects" in CategoriesOfObject(p) ) ) then 
        kind := 5; 
    else 
        Info( InfoGroupoids, 2, "just an ordinary list?" ); 
    fi; 
    return kind; 
end );

#####################  MULT ELTS WITH OBJECTS  ############################## 

############################################################################# 
## 
#M  Arrow( <mwo>, <elt>, <tail>, <head> ) 
#M  ArrowNC( <mwo> <isgpdelt>, <elt>, <tail>, <head> ) 
##
InstallMethod( ArrowNC, 
    "for mwo, boolean, element, tail and head", true,  
    [ IsMagmaWithObjects, IsBool, IsMultiplicativeElement, IsObject, IsObject ],
    0, 
function( mwo, isge, e, t, h ) 

    local obs, elt, fam;

    Info( InfoGroupoids, 3, "standard method for ArrowNC" );
    if isge then 
        fam := IsGroupoidElementFamily; 
        elt := Objectify( IsGroupoidElementType, [ mwo, e, t, h ] );
    else 
        fam := IsMultiplicativeElementWithObjectsFamily; 
        elt := Objectify( IsMultiplicativeElementWithObjectsType,
                   [ mwo, e, t, h ] ); 
    fi; 
    return elt; 
end ); 

InstallMethod( Arrow, 
    "for general magma with objects, element, tail and head", true, 
    [ IsMagmaWithObjects, IsMultiplicativeElement, IsObject, IsObject ], 0, 
function( mwo, e, t, h ) 

    local piece, obs, fam, mag, pwo, pos, homset, pose; 

    if IsSinglePiece( mwo ) then 
        piece := mwo; 
    else 
        piece := PieceOfObject( mwo, t );
    fi;
    mag := piece!.magma; 
    if not ( e in mag ) then 
        Error( "<e> not in magma <mag>," ); 
    fi;
    obs := piece!.objects; 
    if not ( ( t in obs ) and ( h in obs ) ) then  
        Error( "<t> and <h> must be objects in <piece>," ); 
    fi;
    if not IsDirectProductWithCompleteDigraph( piece ) then 
        if not IsBound( piece!.table ) then 
            TryNextMethod(); 
        fi;
        pos := [ Position( obs, t ), Position( obs, h ) ]; 
        if not HasMultiplicationTable( mag ) then 
            Error( "expecting magma defined by multiplication table," ); 
        fi; 
        homset := piece!.table[pos[1]][pos[2]]; 
        pose := Position( GeneratorsOfMagma( mag ), e ); 
        if not ( pose in homset ) then 
            Error( "(e : t -> h) not an element in <piece>," ); 
        fi;   
    fi; 
    return ArrowNC( mwo, false, e, t, h ); 
end );

#############################################################################
## 
#M  ElementOfArrow
#M  TailOfArrow
#M  HeadOfArrow
#M  GroupoidOfArrow
##
InstallMethod( ElementOfArrow, "generic method for magma with objects element", 
    true, [ IsMultiplicativeElementWithObjects ], 0, e -> e![2] ); 

InstallMethod( TailOfArrow, "generic method for magma with objects element", 
    true, [ IsMultiplicativeElementWithObjects ], 0, e -> e![3] ); 

InstallMethod( HeadOfArrow, "generic method for magma with objects element", 
    true, [ IsMultiplicativeElementWithObjects ], 0, e -> e![4] ); 

InstallMethod( GroupoidOfArrow, "generic method for magma with objects element", 
    true, [ IsMultiplicativeElementWithObjects ], 0, e -> e![1] ); 

#############################################################################
##
#M  String, ViewString, PrintString, ViewObj, PrintObj 
##  . . . . . . . . . . . . . . . . . . for elements in a magma with objects 
##
InstallMethod( String, "for an element in a magma with objects", true, 
    [ IsMultiplicativeElementWithObjects ], 0, 
function( e ) 
    return( STRINGIFY( "[", String( e![2] ), " : ", String( e![3] ), 
                       " -> ", String( e![4] ), "]" ) ); 
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
    Print( "[", e![2], " : ", e![3], " -> ", e![4], "]" );
end );

InstallMethod( ViewObj, "for an element in a magma with objects",
    [ IsMultiplicativeElementWithObjects ], PrintObj );

#############################################################################
##
#M  \=( <e1>, <e2> ) . . . . . . equality of elements in a magma with objects
##
InstallMethod( \=, "for two multiplicative elements with objects", 
    IsIdenticalObj, [ IsMultiplicativeElementWithObjects, 
                      IsMultiplicativeElementWithObjects ], 0,
function( e1, e2 )
    return ForAll( [1..4], i -> ( e1![i] = e2![i] ) ); 
end );

#############################################################################
##
#M  \<( <e1>, <e2> ) . . . . . . equality of elements in a magma with objects
##
InstallMethod( \<, "for two multiplicative elements with objects", 
    IsIdenticalObj, [ IsMultiplicativeElementWithObjects, 
                      IsMultiplicativeElementWithObjects ], 0,
function( e1, e2 )
    if ( e1![1] <> e2![1] ) then
        Error( "e1, e2 belong to different mwos" );
    fi;
    if ( e1![3] < e2![3] ) then 
        return true; 
    elif ( (e1![3] = e2![3]) and (e1![4] < e2![4]) ) then 
        return true; 
    elif ( (e1![3] = e2![3]) and (e1![4] = e2![4]) and (e1![2] < e2![2]) ) then 
        return true; 
    else 
        return false; 
    fi;
end );

############################################################################# 
## 
#M  \*( e1, e2 ) . . . . . . composition of elements in a magma with objects 
## 
InstallMethod( \*, "for two elements in a magma with objects", IsIdenticalObj,
    [IsMultiplicativeElementWithObjects, IsMultiplicativeElementWithObjects], 
    0, 
function( e1, e2 ) 

    local prod; 

    if ( e1![1] <> e2![1] ) then
        Error( "e1, e2 belong to different mwos" );
    fi;
    ## elements are composable? 
    if ( e1![4] = e2![3] ) then 
        return ArrowNC( e1![1], false, e1![2]*e2![2], e1![3], e2![4] ); 
    else 
        return fail; 
    fi;  
end );

############################################################################# 
## 
#M  \^( e, p ) . . . . . . power (inverse) of element in a magma with objects 
## 
InstallMethod( \^, "for an element in a magma with objects and a PosInt", 
    true, [ IsMultiplicativeElementWithObjects, IsPosInt ], 0, 
function( e, p ) 
    ##  should be able to invert an identity element 
    ##  groupoids use their own method 
    if ( e![4] = e![3] ) then 
        return ArrowNC( e![1], false, e![2]^p, e![3], e![4] ); 
    else 
        return fail; 
    fi;  
end );

#############################################################################
##
#M  Order( <e> )  . . . . . . . . . . . . . . . . . . . . . of an mwo element
##
InstallOtherMethod( Order, "for a multiplicative element with objects", true,
    [ IsMultiplicativeElementWithObjects ], 0,
function( e ) 

    local ord;

    if not ( e![3] = e![4] ) then
        Error( "tail of e <> head of e," );
    fi;
    return Order( e![2] );
end );


################################  MAGMAS  ################################### 

############################################################################# 
## 
#F  MagmaWithObjects( <mag>, <obs> ) 
##
InstallGlobalFunction( MagmaWithObjects, function( arg ) 

    local obs, mag;

    # list of objects and a magma 
    if ( ( Length(arg) = 2 ) and IsMagma( arg[1] ) and IsSet( arg[2] ) ) then 
        mag := arg[1]; 
        obs := arg[2]; 
        if ( HasIsGeneratorsOfMagmaWithInverses( mag ) 
            and IsGeneratorsOfMagmaWithInverses( mag ) 
            and HasIsAssociative( mag ) and IsAssociative( mag ) ) then 
            Info( InfoGroupoids, 1, "SinglePieceGroupoid:-" );
            return SinglePieceGroupoid( mag, obs ); 
        elif ( HasIsMonoid( mag ) and IsMonoid( mag ) ) then 
            Info( InfoGroupoids, 1, "SinglePieceMonoidWithObjects:-" ); 
            return SinglePieceMonoidWithObjects( mag, obs ); 
        elif ( HasIsSemigroup( mag ) and IsSemigroup( mag ) ) then 
            Info( InfoGroupoids, 1, "SinglePieceSemigroupWithObjects:-" ); 
            return SinglePieceSemigroupWithObjects( mag, obs ); 
        else  ## it's just a magma 
            Info( InfoGroupoids, 1, "SinglePieceMagmaWithObjects:-" );
            return SinglePieceMagmaWithObjects( mag, obs ); 
        fi; 
    else 
        Error( "Current usage: MagmaWithObjects( <mag>, <obs> )," ); 
    fi; 
end ); 

############################################################################# 
## 
#M  SinglePieceMagmaWithObjects( <mag>, <obs> ) . . . . for magma and objects 
##
InstallMethod( SinglePieceMagmaWithObjects, 
    "for magma, objects", true, [ IsMagma, IsCollection ], 0, 
function( mag, obs ) 

    local cf, one, r, gens, mwo, fmwo, cmwo, isa, isc; 

    mwo := rec( objects := obs, magma := mag); 
    ObjectifyWithAttributes( mwo, IsMagmaWithObjectsType, 
        IsAssociative, IsAssociative( mag ), 
        IsCommutative, IsCommutative( mag ), 
        IsFinite, IsFinite( mag ), 
        IsSinglePieceDomain, true, 
        IsDirectProductWithCompleteDigraphDomain, true );
    gens := GeneratorsOfMagmaWithObjects( mwo );
    return mwo; 
end ); 

#############################################################################
##
#M  \=( <m1>, <m2> )  . . . . . . . test if two magmas with objects are equal
##
InstallMethod( \=, "for magmas with objects", IsIdenticalObj,
    [ IsMagmaWithObjects, IsMagmaWithObjects ], 0, 
function ( m1, m2 ) 

    local i, p1, p2;

    if ( IsSinglePiece(m1) and IsSinglePiece(m2) ) then 
        return ( ( m1!.objects=m2!.objects ) and ( m1!.magma=m2!.magma ) ); 
    elif ( IsSinglePiece(m1) or IsSinglePiece(m2) ) then 
        return false; 
    else 
        p1 := Pieces( m1 ); 
        p2 := Pieces( m2 ); 
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
#M  \in( <elt>, <mwo> ) . . . . test if an element is in a magma with objects 
##

InstallMethod( \in, "for mwo element and a standard magma with objects", true, 
    [ IsMultiplicativeElementWithObjects, 
      IsMagmaWithObjects and IsSinglePiece ], 0,
function( e, mwo ) 
    return ( e![1] = mwo );
end ); 

InstallMethod( \in, "for mwo element and a union of pieces", true, 
    [ IsMultiplicativeElementWithObjects, IsMagmaWithObjects and HasPieces ], 0,
function( e, mwo )
    return e in PieceOfObject( mwo, e![3] ); 
end );

#############################################################################
##
#M  Size 
##
InstallOtherMethod( Size, "generic method for a magma with objects", true,
    [ IsMagmaWithObjects ], 0,
function( mwo )

    local p, s;

    if ( HasIsDirectProductWithCompleteDigraph( mwo ) and 
            IsDirectProductWithCompleteDigraph( mwo ) ) then 
        return Size( mwo!.magma ) * Length( mwo!.objects )^2; 
    elif ( HasIsDiscreteDomainWithObjects( mwo ) and 
              IsDiscreteDomainWithObjects( mwo ) ) then 
        return Size( mwo!.magma ) * Length( mwo!.objects ); 
    elif ( HasIsSinglePieceDomain( mwo ) and 
              IsSinglePieceDomain( mwo ) ) then 
        return Size( mwo!.magma ) * Length( mwo!.objects )^2;
Print("reached here\n");
    elif HasPieces( mwo ) then 
        s := 0; 
        for p in Pieces( mwo ) do 
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
##  . . . . . . . . . . . . . . . . . . . . . . . . for a magma with objects 
##
InstallMethod( String, "for a magma with objects", true, 
    [ IsMagmaWithObjects ], 0, 
function( mwo ) 

    local kind; 

    kind := KindOfDomainWithObjects( [ mwo ] ); 
    if ( kind = 1 ) then 
        return( STRINGIFY( "groupoid" ) ); 
    elif ( kind = 2 ) then 
        return( STRINGIFY( "monoid with objects" ) ); 
    elif ( kind = 3 ) then 
        return( STRINGIFY( "semigroup with objects" ) ); 
    elif ( kind = 4 ) then 
        return( STRINGIFY( "magma with objects" ) ); 
    else 
        return( STRINGIFY( "domain with objects" ) ); 
    fi; 
end );

InstallMethod( ViewString, "for an element in a magma with objects", true, 
    [ IsMultiplicativeElementWithObjects ], 0, String ); 

InstallMethod( PrintString, "for an element in a magma with objects", true, 
    [ IsMultiplicativeElementWithObjects ], 0, String ); 

InstallMethod( ViewObj, "for an element in a magma with objects", true, 
    [ IsMultiplicativeElementWithObjects ], 0, PrintObj ); 

InstallMethod( ViewObj, "for a single piece magma with objects", true, 
    [ IsSinglePiece ], 0,   
function( mwo )

    local kind; 

    kind := KindOfDomainWithObjects( [ mwo ] ); 
    if ( kind = 1 ) or ( kind = 5 ) then 
        Print( "#I  should be using special groupoid method!\n" ); 
    elif ( kind = 2 ) then 
        Print( "monoid with objects :-\n" ); 
    elif ( kind = 3 ) then 
        Print( "semigroup with objects :-\n" ); 
    elif ( kind = 4 ) then 
        Print( "magma with objects :-\n" ); 
    else 
        Print( "not yet implemented for general domains with objects\n" ); 
    fi; 
    Print( "    magma = ", mwo!.magma, "\n" ); 
    Print( "  objects = ", mwo!.objects, "\n" ); 
end );

InstallMethod( PrintObj, "for a single piece magma with objects", true, 
    [ IsSinglePiece ], 0, 
function( mwo )

    local kind; 

    kind := KindOfDomainWithObjects( [ mwo ] ); 
    if ( kind = 1 ) or ( kind = 5 ) then 
        Print( "#I  should be using special groupoid method!\n" ); 
    elif ( kind = 2 ) then 
        Print( "monoid with objects :-\n" ); 
    elif ( kind = 3 ) then 
        Print( "semigroup with objects :-\n" ); 
    elif ( kind = 4 ) then 
        Print( "magma with objects :-\n" ); 
    else 
        Print( "not yet implemented for general domains with objects\n" ); 
    fi; 
    Print( "    magma = ", mwo!.magma, "\n" ); 
    Print( "  objects = ", mwo!.objects, "\n" ); 
end );

InstallMethod( ViewObj, "for more than one piece", true, 
    [ IsDomainWithObjects and IsPiecesRep ], 10,   
function( dwo )

    local i, pieces, np, kind; 

    pieces := Pieces( dwo ); 
    np := Length( pieces ); 
    kind := KindOfDomainWithObjects( Pieces( dwo ) ); 
    if (kind=1) then Print( "groupoid" );  
      elif (kind=2) then Print( "monoid with objects" ); 
      elif (kind=3) then Print( "semigroup with objects" ); 
      elif (kind=4) then Print( "magma with objects" ); 
      elif (kind=5) then Print( "double groupoid" ); 
      elif (kind=0) then Error( "invalid domain with objects," ); 
    fi;
    Print( " having ", np, " pieces :-\n" ); 
    for i in [1..np] do 
        Print( i, ": ", pieces[i] ); 
        if HasName( pieces[i] ) then 
            Print( "\n" ); 
        fi; 
    od; 
end ); 

InstallMethod( PrintObj, "for more than one piece", true, 
    [ IsMagmaWithObjects and IsPiecesRep ], 0,   
function( mwo )

    local i, pieces, np; 

    pieces := Pieces( mwo ); 
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
#M  Display( <mwo> ) . . . . . . . . . . . . . .  display a magma with objects
##
InstallMethod( Display, "for a mwo", true, [ IsMagmaWithObjects ], 0, 
function( mwo )
    
    local comp, c, i, m, len;

    if IsSinglePiece( mwo ) then 
        if IsDirectProductWithCompleteDigraph( mwo ) then 
            Print( "Single constituent magma with objects: " );
            if HasName( mwo ) then
                Print( mwo );
            fi;
            Print( "\n" ); 
            Print( "  objects: ", mwo!.objects, "\n" );
            m := mwo!.magma;
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
        comp := Pieces( mwo );
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

#############################################################################
##
#M  RootObject( <mwo> )      for a connected or many-piece magma with objects
##
InstallMethod( RootObject, "for a single piece mwo", true, 
    [ IsSinglePiece ], 0,
function( mwo )
    return mwo!.objects[1]; 
end ); 

InstallOtherMethod( RootObject, "for a mwo with pieces", true, 
    [ IsDomainWithObjects and IsPiecesRep ], 0,
function( mwo )
    Print( "#I only a single piece magma with objects has a root object\n" ); 
    return fail; 
end ); 

#############################################################################
##
#M  GeneratorsOfMagmaWithObjects( <mwo> )  for a connected magma with objects 
#M  GeneratorsOfSemigroupWithObjects( <swo> )
#M  GeneratorsOfMonoidWithObjects( <mwo> )
##
InstallMethod( GeneratorsOfMagmaWithObjects, "for a single piece mwo", 
    true, [ IsSinglePiece ], 0,
function( mwo )

    local obs, nobs, o1, m, mgens, kind, gens, i, j, k, g; 

    obs := mwo!.objects;
    nobs := Length( obs );
    o1 := obs[1];
    m := mwo!.magma; 
    kind := 1; 
    if ( "IsGroupoid" in CategoriesOfObject( mwo ) ) then 
        return GeneratorsOfGroupoid( mwo ); 
    fi;
    if not ( HasIsDirectProductWithCompleteDigraph( mwo ) and 
                IsDirectProductWithCompleteDigraph( mwo ) ) then 
        Info( InfoGroupoids, 1, "expecting product with complete graph" ); 
        return fail; 
    fi;
    if ( "IsMonoidWithObjects" in CategoriesOfObject( mwo ) ) then 
        return GeneratorsOfMonoidWithObjects( mwo ); 
    fi; 
    if ( "IsSemigroupWithObjects" in CategoriesOfObject( mwo ) ) then 
        kind := 2; 
        mgens := GeneratorsOfSemigroup( m ); 
    else 
        mgens := GeneratorsOfMagma( m ); 
    fi; 
    gens := ListWithIdenticalEntries( nobs * nobs * Length(mgens), 0 ); 
    k := 0; 
    for i in obs do 
        for j in obs do 
            for g in mgens do
                k := k+1; 
                gens[k] := ArrowNC( mwo, false, g, i, j ); 
            od;
        od;
    od;
    if ( kind = 2 ) then 
        SetGeneratorsOfSemigroupWithObjects( mwo, gens ); 
    fi;
    return gens; 
end );

InstallMethod( GeneratorsOfMagmaWithObjects, "for discrete domain",
    true, [ IsGroupoid and IsSinglePiece and IsDiscreteDomainWithObjects ], 0,
function( mwo ) 

    local o, ogens, gens;  

    gens := [ ];
    for o in mwo!.objects do 
        ogens := GeneratorsOfGroup( ObjectGroup( mwo, o ) ); 
        Add( gens, List( ogens, g -> GroupoidElement( mwo, g, o, o ) ) ); 
    od; 
    return gens; 
end ); 

InstallMethod( GeneratorsOfMagmaWithObjects, "for mwo with >1 piece",
    true, [ IsMagmaWithObjects ], 0,
function( mwo ) 
    return Concatenation( List( Pieces( mwo ), 
               GeneratorsOfMagmaWithObjects ) ); 
end );

InstallMethod( GeneratorsOfSemigroupWithObjects, "for a semigroup with objects", 
    true, [ IsSemigroupWithObjects ], 0, GeneratorsOfMagmaWithObjects );

InstallMethod( GeneratorsOfMonoidWithObjects, "for a monoid with objects", 
    true, [ IsMonoidWithObjects and IsSinglePiece], 0, 
function( mwo )

    local obs, nobs, o1, m, mgens, id, gens1, gens2, gens, i, k;

    obs := mwo!.objects;
    nobs := Length( obs );
    o1 := obs[1];
    m := mwo!.magma; 
    mgens := GeneratorsOfMonoid( m ); 
    id := One( m ); 
    gens1 := List( mgens, g -> ArrowNC( mwo, false, g, o1, o1 ) );
    gens2 := ListWithIdenticalEntries( (nobs-1)^2, 0 ); 
    k := 0;
    for i in [2..nobs] do 
        gens2[k+1] := ArrowNC( mwo, false, id, o1, obs[i] ); 
        gens2[k+2] := ArrowNC( mwo, false, id, obs[i], o1 ); 
        k := k+2; 
    od;
    gens := Immutable( Concatenation( gens1, gens2 ) ); 
    SetGeneratorsOfMonoidWithObjects( mwo, gens ); 
    return gens; 
end );



#############################  MORE THAN ONE PIECE  ######################### 

#############################################################################
##
#M  Pieces . . . . . . . . . . connected components of a domain with objects 
##
InstallMethod( Pieces, "for a single piece domain with objects",
    true, [ IsSinglePieceDomain ], 0,
function( dwo )
    return [ dwo ];
end );

#############################################################################
##
#M  ObjectList . . . . . . . . . . . . . . . . . . . for a magma with objects
##
InstallMethod( ObjectList, "for a magma with objects",
    true, [ IsMagmaWithObjects ], 0,
function( mwo )

    local obs; 

    if ( HasIsSinglePiece( mwo ) and IsSinglePiece( mwo ) ) then
        return mwo!.objects;
    else
        obs := Concatenation( 
            List( Pieces( mwo ), c -> c!.objects ) );
        if not IsDuplicateFree( obs ) then
            Error( "same object in more than one constituent," );
        fi;
        Sort( obs );
        return obs;
    fi;
end );

#############################################################################
##
#M  UnionOfPieces . . . . . . . . for a list of connected magmas with objects
#M  UnionOfPiecesOp 
##
InstallGlobalFunction( UnionOfPieces, 
function( arg )

    local L, npa, part, pieces, p, nco, obs, obp, i, gi, nobj;

    if IsList( arg[1] ) then 
        L := arg[1]; 
    else 
        L := arg; 
    fi; 
    npa := Length( L );
    pieces := [ ]; 
    if ( Length( L ) = 1 ) then 
        return L[1]; 
    fi; 
    for part in L do 
        if not IsDomainWithObjects( part ) then
            Info( InfoGroupoids, 1, "part ", part, "not an mwo" );
            return fail;
        fi;
        if ( HasIsSinglePiece(part) and IsSinglePiece(part) ) then
            Add( pieces, part );
        else
            Append( pieces, Pieces( part ) );
        fi;
    od; 
    obs := [ ]; 
    for p in pieces do 
        obp := ObjectList( p );
        if ( Intersection( obs, obp ) <> [ ] ) then
            Info( InfoGroupoids, 1, 
                  "pieces must have disjoint object sets" );
            return fail;
        fi;
        obs := Union( obs, obp ); 
    od;
    return UnionOfPiecesOp( pieces, pieces[1] );
end );

InstallMethod( UnionOfPiecesOp, "method for magmas with objects",
    true, [ IsList, IsDomainWithObjects ], 0,
function( comps, dom )

    local kind, len, pieces, L, fam, filter, mwo, i, obs, par;

    ## determine which kind:  1=gpd, 2=mon, 3=sgp, 4=mgm, 5=dgpd, 6=dom 
    if ForAll( comps, c -> "IsGroupoid" in CategoriesOfObject( c ) ) then 
        kind := 1; 
    elif ForAll( comps,
                 c -> "IsDoubleGroupoid" in CategoriesOfObject( c ) ) then
        kind := 5;
    elif ForAll( comps, 
                 c -> "IsMonoidWithObjects" in CategoriesOfObject( c ) ) then 
        kind := 2; 
    elif ForAll( comps, 
                 c -> "IsSemigroupWithObjects" in CategoriesOfObject( c ) ) then 
        kind := 3; 
    elif ForAll( comps, 
                 c -> "IsMagmaWithObjects" in CategoriesOfObject( c ) ) then 
        kind := 4;
    else 
        Print( "kind not in {1,2,3,4,5} so TryNextMethod()\n" ); 
        TryNextMethod(); 
    fi;
    Info( InfoGroupoids, 2, "kind = ", kind ); 
    ## order pieces by first object
    len := Length( comps ); 
    obs := List( comps, g -> g!.objects[1] );
    L := [1..len];
    SortParallel( obs, L );
    if ( L = [1..len] ) then 
        pieces := comps; 
    else 
        Info( InfoGroupoids, 2, "reordering pieces by first object" ); 
        pieces := List( L, i -> comps[i] );
    fi;
    if ( kind = 1 ) then 
        fam := IsGroupoidFamily; 
        filter := IsPiecesRep and IsGroupoid and IsAssociative; 
        mwo := Objectify( IsGroupoidPiecesType, rec() );
    elif ( kind = 5 ) then 
        fam := IsDoubleGroupoidFamily; 
        filter := IsPiecesRep and IsDoubleGroupoid and IsAssociative; 
        mwo := Objectify( IsDoubleGroupoidPiecesType, rec() ); 
    elif ( kind = 2 ) then 
        fam := IsMonoidWithObjectsFamily; 
        filter := IsPiecesRep and IsMonoidWithObjects; 
        mwo := Objectify( IsMonoidWOPiecesType, rec() );
    elif ( kind = 3 ) then 
        fam := IsSemigroupWithObjectsFamily; 
        filter := IsPiecesRep and IsSemigroupWithObjects; 
        mwo := Objectify( IsSemigroupWOPiecesType, rec() );
    elif ( kind = 4 ) then 
        fam := IsMagmaWithObjectsFamily; 
        filter := IsPiecesRep and IsMagmaWithObjects; 
        mwo := Objectify( IsMagmaWOPiecesType, rec() ); 
    else 
        ## ?? (23/04/10) fam := FamilyObj( [ pieces ] ); 
        Error( "union of unstructured domains not yet implemented," ); 
    fi; 
    SetIsSinglePieceDomain( mwo, false ); 
    SetIsDirectProductWithCompleteDigraphDomain( mwo, false ); 
    SetPieces( mwo, pieces ); 
    if HasParent( pieces[1] ) then 
        par := Ancestor( pieces[1] ); 
        if ForAll( pieces, c -> ( Ancestor( c ) = par ) ) then 
            SetParent( mwo, par ); 
        fi; 
    fi; 
    if ( kind = 1 ) then 
        if ForAll( pieces, p -> HasIsPermGroupoid(p) and IsPermGroupoid(p) )
             then SetIsPermGroupoid( mwo, true ); 
        elif ForAll( pieces, p -> HasIsPcGroupoid(p) and IsPcGroupoid(p) )
             then SetIsPcGroupoid( mwo, true ); 
        elif ForAll( pieces, p -> HasIsFpGroupoid(p) and IsFpGroupoid(p) )
             then SetIsFpGroupoid( mwo, true ); 
        fi; 
    fi; 
    return mwo; 
end );

#############################################################################
##
#M  MagmaWithSingleObject
##
##  Note that there is another method for [ IsGroup, IsObject ] in gpd.gi 
##
InstallMethod( MagmaWithSingleObject, "generic method for magma, object",
    true, [ IsMagma, IsObject ], 0,
function( mgm, obj ) 

    local o; 

    if ( IsList( obj ) and ( Length(obj) = 1 ) ) then
        Info( InfoGroupoids, 2, "object given as a singleton list" );
        o := obj[1]; 
    else
        o := obj;
    fi; 
    if not IsObject( o ) then 
        Error( "<obj> not a scalar or singleton list," ); 
    fi; 
    if ( HasIsAssociative( mgm ) and IsAssociative( mgm ) 
         and ( "IsMagmaWithInverses" in CategoriesOfObject( mgm ) ) 
         and IsMagmaWithInverses( mgm ) ) then 
        return SinglePieceGroupoidNC( mgm, [o] ); 
    elif ( HasIsMonoid( mgm ) and IsMonoid( mgm ) ) then 
        return SinglePieceMonoidWithObjects( mgm, [o] ); 
    elif ( HasIsSemigroup( mgm ) and IsSemigroup( mgm ) ) then 
        return SinglePieceSemigroupWithObjects( mgm, [o] ); 
    elif ( ( "IsMagma" in CategoriesOfObject(mgm) ) and IsMagma(mgm) ) then 
        return SinglePieceMagmaWithObjects( mgm, [o] ); 
    else 
        Error( "unstructured domains with objects not yet implemented," ); 
    fi; 
end );

############################################################################# 
## 
#M  PieceOfObject
## 
InstallMethod( PieceOfObject, "generic method for magma with objects", 
    true, [ IsDomainWithObjects, IsObject ], 0,
function( dwo, obj )

    local pieces, p, objp;

    if IsSinglePiece( dwo ) then
        if not ( obj in dwo!.objects ) then
            Error( "<obj> not an object of <dwo>," );
        else
            return dwo;
        fi;
    elif not ( obj in ObjectList( dwo ) ) then
        Info( InfoGroupoids, 1, "<obj> not an object of <dwo>" );
        return fail;
    fi;
    pieces := Pieces( dwo );
    for p in pieces do
        objp := p!.objects;
        if ( obj in objp ) then
            return p;
        fi;
    od;
    Info( InfoGroupoids, 1, "it appears that <obj> is not an object in <dwo>" );
    return fail;
end );

############################################################################# 
## 
#M  PieceNrOfObject
## 
InstallMethod( PieceNrOfObject, "generic method for domain with objects",
    true, [ IsDomainWithObjects, IsObject ], 0,
function( dwo, obj )

    local pieces, i, objp, np; 

    pieces := Pieces( dwo );
    for i in [1..Length( pieces )] do
        objp := pieces[i]!.objects;
        if ( obj in objp ) then
            return i;
        fi;
    od;
    Info( InfoGroupoids, 1, "it appears that <obj> is not an object in <dwo>" );
    return fail;
end );

#############################################################################
##
#M  IsDiscreteDomainWithObjects 
##
InstallMethod( IsDiscreteDomainWithObjects, "for a magma with objects", true,
    [ IsDomainWithObjects ], 0,
function( dwo )

    local p; 

    for p in Pieces( dwo ) do
        if not ( Length( p!.objects ) = 1 ) then
            return false;
        fi;
    od;
    return true;
end );

#############################################################################
##
#M  IsHomogeneousDomainWithObjects 
##
InstallMethod( IsHomogeneousDomainWithObjects, "for a magma with objects", 
    true, [ IsDiscreteDomainWithObjects ], 0,
function( dwo )

    local pieces, g, j, iso; 

    pieces := Pieces( dwo ); 
    g := pieces[1]!.magma; 
    return ForAll( [2..Length(pieces)], j -> ( g = pieces[j]!.magma ) ); 
end );

InstallMethod( IsHomogeneousDomainWithObjects, "for a magma with objects", 
    true, [ IsDomainWithObjects ], 0,
function( dwo )

    local pieces, sizes, g, j, iso; 

    pieces := Pieces( dwo ); 
    sizes := Set( List( pieces, Size ) ); 
    if not ( Length( sizes ) = 1 ) then 
        return false; 
    fi;
    g := pieces[1]!.magma; 
    return ForAll( [2..Length(pieces)], j -> ( g = pieces[j]!.magma ) ); 
end );


#################################  SUBDOMAINS  ############################## 

#############################################################################
##
#F  IsSubdomainWithObjects( <M>, <U> )
##
InstallMethod( IsSubdomainWithObjects, "for two domains with objects", true, 
    [ IsDomainWithObjects, IsDomainWithObjects ], 0, 
function( D, U )

    local compU, obj, genU, p, ok; 

    ##  insisting that a subdomain of a groupoid is a groupoid 
    ok := false; 
    if not ( IsMagmaWithObjects(D) and IsMagmaWithObjects(U) ) then
        Error( "not yet implemented for unstructured domains," ); 
    fi;
    if ( HasParentAttr(U) and ( ParentAttr(U) = D ) ) then 
        return true;
    fi;
    if not IsSubset( ObjectList(D), ObjectList(U) ) then
        return false;
    fi; 
    if ( IsSinglePiece(D) and IsSinglePiece(U) ) then
        obj := U!.objects[1];
        if ( IsGroupoid(D) and IsGroupoid(U) ) then 
            genU := GeneratorsOfGroupoid(U); 
            ok := ForAll( genU, g -> g in D ); 
##      elif ( IsMonoid(D) and IsMonoid(U) ) then 
##          ok := ( IsSubsemigroup( D!.magma, U!.magma ) 
##                  and IsMonoid( D!.magma ) ); 
##      elif ( IsSemigroup(D) and IsSemigroup(U) ) then 
##          ok := IsSubsemigroup( D!.magma, U!.magma ); 
        else 
            ok := IsSubset( GeneratorsOfMagma( D!.magma ), 
                            GeneratorsOfMagma( U!.magma ) ); 
        fi;
    elif IsSinglePiece(U) then
        obj := U!.objects[1];
        p := PieceOfObject( D, obj ); 
        ok := IsSubdomainWithObjects( p, U );
    else
        compU := Pieces(U);
        ok := ForAll( compU, p -> IsSubdomainWithObjects( D, p ) );
    fi;
    if ( ok and not HasParentAttr( U ) ) then 
        SetParent( U, D ); 
    fi; 
    return ok; 
end );

#############################################################################
##
#F  SubdomainWithObjects( <M>, <U> )
##
InstallGlobalFunction( SubdomainWithObjects, function( arg ) 

    local nargs, dwo, isgpd; 

    nargs := Length( arg ); 
    dwo := arg[1]; 
    isgpd := ( "IsGroupoid" in CategoriesOfObject( dwo ) ); 
    if isgpd then 
        if ( nargs = 2 ) then 
            return Subgroupoid( arg[1], arg[2] ); 
        elif ( nargs = 3 ) then 
            return Subgroupoid( arg[1], arg[2], arg[3] ); 
        elif ( nargs = 4 ) then 
            return Subgroupoid( arg[1], arg[2], arg[3], arg[4] ); 
        else 
            Error( "expecting 2, 3 or 4 arguments" ); 
        fi;
    else 
        Error( "SubdomainWithObjects needs more implementation" ); 
    fi; 
end );


################################  SEMIGROUPS  ############################### 

############################################################################# 
## 
#F  SemigroupWithObjects( <sgp>, <obs> ) 
##
InstallGlobalFunction( SemigroupWithObjects, function( arg ) 

    local obs, sgp; 

    # list of objects and a semigroup 
    if ( (Length(arg) = 2) and IsSemigroup( arg[1] ) and IsSet( arg[2] ) ) then 
        sgp := arg[1]; 
        obs := arg[2]; 
        if HasGeneratorsOfMagmaWithInverses( sgp ) then  
            return SinglePieceGroupoid( sgp, obs ); 
        elif ( HasIsMonoid( sgp ) and IsMonoid( sgp ) ) then 
            return SinglePieceMonoidWithObjects( sgp, obs ); 
        else  ## it's just a semigroup
            return SinglePieceSemigroupWithObjects( sgp, obs ); 
        fi; 
    else 
        Error( "Current usage: SemigroupWithObjects( <sgp>, <obs> )," ); 
    fi; 
end ); 

############################################################################# 
## 
#M  SinglePieceSemigroupWithObjects( <sgp>, <obs> ) . for semigroup, objects
##
InstallMethod( SinglePieceSemigroupWithObjects, 
    "for objects, semigroup", true, [ IsSemigroup, IsCollection ], 0, 
function( sgp, obs ) 

    local gens, swo; 

    swo := rec( objects := obs, magma := sgp ); 
    ObjectifyWithAttributes( swo, IsSemigroupWithObjectsType, 
        IsAssociative, IsAssociative( sgp ), 
        IsCommutative, IsCommutative( sgp ), 
        IsFinite, IsFinite( sgp ), 
        IsSinglePieceDomain, true, 
        IsDirectProductWithCompleteDigraphDomain, true ); 
    gens := GeneratorsOfSemigroupWithObjects( swo ); 
    return swo; 
end ); 


##################################  MONOIDS  ################################ 

############################################################################# 
## 
#F  MonoidWithObjects( <mon>, <obs> ) 
##
InstallGlobalFunction( MonoidWithObjects, function( arg ) 

    local obs, mon; 

    # list of objects and a monoid 
    if ( ( Length(arg) = 2 ) and IsMonoid( arg[1] )  and IsSet( arg[2] ) ) then 
        mon := arg[1]; 
        obs := arg[2]; 
        if HasGeneratorsOfMagmaWithInverses( mon ) then  
            return SinglePieceGroupoid( mon, obs ); 
        else  ## it's just a monoid
            return SinglePieceMonoidWithObjects( mon, obs ); 
        fi; 
    else 
        Error( "Current usage: MonoidWithObjects( <mon>, <obs> )," ); 
    fi; 
end ); 

############################################################################# 
## 
#M  SinglePieceMonoidWithObjects( <mon>, <sgp> ) . . . for semigroup, objects 
##
InstallMethod( SinglePieceMonoidWithObjects, 
    "for objects, monoid", true, [ IsMonoid, IsCollection ], 0, 
function( mon, obs ) 

    local gens, mwo; 

    mwo := rec( objects := obs, magma := mon ); 
    ObjectifyWithAttributes( mwo, IsMonoidWithObjectsType, 
        IsAssociative, IsAssociative( mon ), 
        IsCommutative, IsCommutative( mon ), 
        IsSinglePieceDomain, true, 
        IsDirectProductWithCompleteDigraphDomain, true, 
        IsFinite, HasIsFinite( mon ) and IsFinite( mon ) );
    gens := GeneratorsOfMonoidWithObjects( mwo ); 
    return mwo; 
end ); 


##################################  GROUPS  ################################# 
##  A *group with objects* is a magma with objects where the vertex magmas 
##  are groups, and so is a *groupoid* - see file gpd.gi.


#################################  SUBMAGMAS  ############################### 

############################################################################# 
## 
#M  IsSubmagmaWithObjectsGeneratingTable( <mag>, <A> ) 
##
InstallMethod( IsSubmagmaWithObjectsGeneratingTable, "for magma and array", 
    true, [ IsMagma, IsList ], 0, 
function( mag, A ) 

    local n, s, a, b, e;

    n := Length( A );    ## number of objects 
    s := Size( mag );  
    e := [1..s]; 
    for a in A do 
        if not ( IsList(a) and ( Length(a) = n ) ) then 
            return false; 
        fi; 
        for b in a do 
            if not IsSubset( e, b ) then 
                return false; 
            fi; 
        od; 
    od; 
    return true; 
end ); 

############################################################################# 
## 
#M  SubmagmaWithObjectsElementsTable( <mag>, <A> ) 
##
InstallMethod( SubmagmaWithObjectsElementsTable, "for magma and array", 
    true, [ IsMagma, IsList ], 0, 
function( mag, A ) 

    local t, done, B, C, T, n, s, i, j, k, Lij, Lik, Ljk, a, b, ab; 

    if not IsSubmagmaWithObjectsGeneratingTable( mag, A ) then 
        Error( "array A is not a generating table for a submagma over mag," ); 
    fi; 
    T := MultiplicationTable( mag ); 
    s := Size( mag ); 
    n := Length( A ); 
    done := false; 
    C := StructuralCopy( A ); 
    t := 0; 
    while not done do 
        t := t+1; 
        B := StructuralCopy( C );   ## C stores all the elements 
        for i in [1..n] do 
            for j in [1..n] do 
                Lij := C[i][j]; 
                for k in [1..n] do 
                    Ljk := C[j][k]; 
                    for a in Lij do 
                        for b in Ljk do 
                            ab := T[a][b]; 
                            C[i][k] := Union( C[i][k], [ab] ); 
                        od; 
                    od; 
                od; 
            od; 
        od; 
        done := ( B = C ); 
    od;
    return C;        
end ); 

############################################################################# 
## 
#M  SubmagmaWithObjectsByElementsTable( <mwo>, <A> ) . . for mwo and elements 
##
InstallMethod( SubmagmaWithObjectsByElementsTable, 
    "for objects, magma and table of generating elements", true, 
    [ IsSinglePiece, IsList ], 0, 
function( mwo, A ) 

    local C, mag, obs, cf, isa, isc, swo; 

    obs := mwo!.objects; 
    mag := mwo!.magma; 
    C := SubmagmaWithObjectsElementsTable( mag, A ); 
    ## ?? (23/04/10)  cf := mwo!.eltsfam; 
    cf := IsMagmaWithObjectsFamily; 
    isa := IsAssociative( mag ); 
    isc := IsCommutative( mag ); 
    swo := rec( objects := obs, magma := mag, table := C ); 
    ObjectifyWithAttributes( swo, 
        NewType( cf, IsSubmagmaWithObjectsTableRep ), 
        ParentAttr, mwo,
        IsAssociative, IsAssociative( mag ), 
        IsCommutative, IsCommutative( mag ), 
        IsFinite, IsFinite( mag ), 
        IsSinglePieceDomain, true ); 
    #? (13/10/08)  still need to set GeneratorsOfMagmaWithObjects ?? 
    return swo; 
end ); 

############################################################################# 
## 
#M  PrintObj( <swo> ) . . . . .  for submagma with objects and elements table
##

InstallMethod( PrintObj, "for a submagma with objects and elements table", 
    true, [ IsSubmagmaWithObjectsTableRep ], 0, 
function( M ) 
    Print( "submagma with objects:-\n" ); 
    Print( "objects = ", M!.objects, "\n" );
    Print( "  magma = ", M!.magma, "\n" );  
    Print( "  table = ", M!.table, "\n" ); 
end );


#################################  UTILITIES  ############################### 

############################################################################# 
## 
#M  Ancestor 
## 
InstallMethod( Ancestor, "method for a domain with objects", 
    true, [ IsDomainWithObjects ], 0,
function( dom ) 

    local found, d; 

    d := dom; 
    found := ( HasParent( d ) and ( Parent( d ) = d ) ); 
    while not found do 
        if not HasParent( d ) then 
            return fail; 
        else
            d := Parent( d ); 
            found := ( Parent( d ) = d ); 
        fi; 
    od; 
    return d; 
end );


#############################################################################
##
#M  Iterator( <mwo> ) . . . .  iterator for a single piece magma with objects
##
InstallMethod( Iterator, "for a single piece magma with objects", 
    [ IsSinglePiece ], 
function( mwo )
    return IteratorByFunctions( rec( 
        IsDoneIterator := function( iter )
            return ( IsDoneIterator( iter!.magmaIterator ) 
                     and ( iter!.tpos = iter!.len )
                     and ( iter!.hpos = iter!.len ) );
            end, 
        NextIterator := function( iter )
            if ( iter!.tpos = 0 ) then
                iter!.melt := NextIterator( iter!.magmaIterator );
                iter!.tpos := 1;
                iter!.hpos := 1;
            elif ((iter!.tpos = iter!.len) and (iter!.hpos = iter!.len)) then 
                iter!.melt := NextIterator( iter!.magmaIterator );
                iter!.tpos := 1;
                iter!.hpos := 1;
            elif ( iter!.hpos = iter!.len ) then
                iter!.hpos := 1;
                iter!.tpos := iter!.tpos + 1;
            else 
                iter!.hpos := iter!.hpos + 1;
            fi;
            if ( HasIsDirectProductWithCompleteDigraph( mwo ) 
                 and IsDirectProductWithCompleteDigraph( mwo ) ) then 
               return Arrow( mwo, iter!.melt, 
                   iter!.obs[iter!.tpos], iter!.obs[iter!.hpos] );
            else 
               return fail;
            fi; 
            end, 
        ShallowCopy := iter -> 
            rec( magmaIterator := ShallowCopy( iter!.magmaIterator ), 
                 melt := iter!.melt,
                 obs := iter!.obs,
                 len := iter!.len,
                 tpos := iter!.tpos,
                 hpos := iter!.hpos ),
        magmaIterator := Iterator( mwo!.magma ), 
        melt := 0, 
        obs := mwo!.objects,
        len := Length( mwo!.objects ),
        tpos := 0,
        hpos := 0 ) );
end );

InstallMethod( Iterator, "generic method for a magma with objects", 
    [ IsMagmaWithObjects ], 
function( mwo )
    return IteratorByFunctions( rec( 
        IsDoneIterator := function( iter )
            return ( IsDoneIterator( iter!.mwoIterator ) 
                     and ( iter!.cpos = iter!.len ) );
            end, 
        NextIterator := function( iter )
            if IsDoneIterator( iter!.mwoIterator ) then 
                iter!.cpos := iter!.cpos + 1;
                iter!.mwoIterator := 
                    Iterator( iter!.pieces[iter!.cpos] );
            fi;
            return NextIterator( iter!.mwoIterator );
            end,
        ShallowCopy := iter -> 
            rec( pieces := iter!.pieces,
                 len := iter!.len,
                 mwoIterator := ShallowCopy( iter!.mwoIterator ),
                 cpos := iter!.cpos ),
        pieces := Pieces( mwo ),
        len := Length( Pieces( mwo ) ),
        mwoIterator := Iterator( Pieces( mwo )[1] ),
        cpos := 1 ) );
end );

#############################################################################
##
#E  mwo.gi  . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  
