############################################################################# 
## 
#W  mwo.gi                    GAP4 package `Gpd'                Chris Wensley 
##
##  version 1.31, 17/12/2014 
##
#Y  Copyright (C) 2000-2014, Emma Moore and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
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
#M  TypeOfDomainWithObjects( <dwo> ) 
##
InstallMethod( TypeOfDomainWithObjects, "for a list of domains with objects", 
    true, [ IsList ], 0, 

    function( pieces ) 
    local  type; 
    ## type:  1=gpd, 2=mon, 3=sgp, 4=mgm, 5=dom 
    type := 0; 
    if ForAll( pieces, p -> ( FamilyObj(p) =  GroupoidFamily ) ) then 
        type := 1; 
    elif ForAll( pieces, p -> 
            HasIsMonoidWithObjects(p) and IsMonoidWithObjects(p) ) then 
        type := 2; 
    elif ForAll( pieces, p -> 
            HasIsSemigroupWithObjects(p) and IsSemigroupWithObjects(p) ) then 
        type := 3; 
    elif ForAll( pieces, p -> 
        ( "IsMagmaWithObjects" in CategoriesOfObject(p) ) 
                              and IsMagmaWithObjects(p) ) then 
        type := 4; 
    elif ForAll( pieces, p -> 
        ( "IsDomainWithObjects" in CategoriesOfObject(p) )  
                              and IsDomainWithObjects(p) ) then 
        type := 5; 
    else 
        Info( InfoGpd, 2, "just an ordinary list?" ); 
    fi; 
    return type; 
end );

#####################  MULT ELTS WITH OBJECTS  ############################## 

############################################################################# 
## 
#M  Arrow( <mwo>, <elt>, <tail>, <head> ) 
#M  ArrowNC( <isgpdelt>, <elt>, <tail>, <head> ) 
##
InstallMethod( ArrowNC, 
    "for boolean, element, tail and head", true,  
    [ IsBool, IsMultiplicativeElement, IsScalar, IsScalar ], 
    0, 
    function( isge, e, t, h ) 
    local  obs, elt, fam;
    ## ?? (30/04/10)  fam := mwo!.eltsfam; 
    ## ?? (21/09/10)  yet another attempt to make groupoid elements special! 
    if isge then 
        fam := GroupoidElementFamily; 
        elt := Objectify( 
            NewType( fam, IsGroupoidElement ), [ e, t, h ] ); 
    else 
        fam := MultiplicativeElementWithObjectsFamily; 
        elt := Objectify( 
            NewType( fam, IsMultiplicativeElementWithObjects ), [ e, t, h ] ); 
    fi; 
    return elt; 
end ); 

InstallMethod( Arrow, 
    "for general magma with objects, element, tail and head", true, 
    [ IsMagmaWithObjects, IsMultiplicativeElement, IsScalar, IsScalar ], 0, 
    function( mwo, e, t, h ) 
    local  piece, obs, fam, mag, pwo, pos, homset, pose; 
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
    if not IsDirectProductWithCompleteGraph( piece ) then 
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
    return ArrowNC( false, e, t, h ); 
end );

#############################################################################
## 
#M  ElementOfArrow
#M  TailOfArrow
#M  HeadOfArrow
##
InstallMethod( ElementOfArrow, "generic method for magma with objects element", 
    true, [ IsMultiplicativeElementWithObjects ], 0, e -> e![1] ); 

InstallMethod( TailOfArrow, "generic method for magma with objects element", 
    true, [ IsMultiplicativeElementWithObjects ], 0, e -> e![2] ); 

InstallMethod( HeadOfArrow, "generic method for magma with objects element", 
    true, [ IsMultiplicativeElementWithObjects ], 0, e -> e![3] ); 

#############################################################################
##
#M  PrintObj . . . . . . . . . . . . . . for elements in a magma with objects 
#M  ViewObj  . . . . . . . . . . . . . . for elements in a magma with objects 
##
InstallMethod( PrintObj, "for an element in a magma with objects",
    [ IsMultiplicativeElementWithObjects ],
function ( e )
    Print( "[", e![1], " : ", e![2], " -> ", e![3], "]" );
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
    return ForAll( [1..3], i -> ( e1![i] = e2![i] ) ); 
end );

############################################################################# 
## 
#M  \*( e1, e2 ) . . . . . . composition of elements in a magma with objects 
## 
InstallMethod( \*, "for two elements in a magma with objects", IsIdenticalObj,
    [IsMultiplicativeElementWithObjects, IsMultiplicativeElementWithObjects], 
    0, 
    function( e1, e2 ) 
    local  prod; 
    ## elements are composable? 
    if ( ( e1![3] = e2![2] ) and 
         ( FamilyObj( e1![1] ) = FamilyObj( e2![1] ) ) ) then 
        return ArrowNC( false, e1![1]*e2![1], e1![2], e2![3] ); 
    else 
        return fail; 
    fi;  
end );

############################################################################# 
## 
#M  \^( e, p ) . . . . . . power (inverse) of element in a magma with objects 
## 
InstallMethod( \^, "for an element in a magma with objects and a PosInt", 
    true, [ IsMultiplicativeElementWithObjects, IsPosInt ], 
    0, 
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
#M  Order( <e> )  . . . . . . . . . . . . . . . . . . . . . of an mwo element
##
InstallOtherMethod( Order, "for a multiplicative element with objects", true,
    [ IsMultiplicativeElementWithObjects ], 0,
function( e )
    local  ord;
    if not ( e![2] = e![3] ) then
        Error( "tail of e <> head of e," );
    fi;
    return Order( e![1] );
end );


################################  MAGMAS  ################################### 

############################################################################# 
## 
#F  MagmaWithObjects( <mag>, <obs> ) 
##
InstallGlobalFunction( MagmaWithObjects, function( arg ) 

    local  obs, mag;
    # list of objects and a magma 
    if ( ( Length( arg ) = 2 ) and IsMagma( arg[1] ) 
            and IsCollection( arg[2] ) and IsScalar( arg[2][1] ) ) then 
        mag := arg[1]; 
        obs := arg[2]; 
        if ( HasIsGeneratorsOfMagmaWithInverses( mag ) 
            and IsGeneratorsOfMagmaWithInverses( mag ) 
            and HasIsAssociative( mag ) and IsAssociative( mag ) ) then 
            Info( InfoGpd, 1, "SinglePieceGroupoid:-" );
            return SinglePieceGroupoid( mag, obs ); 
        elif ( HasIsMonoid( mag ) and IsMonoid( mag ) ) then 
            Info( InfoGpd, 1, "SinglePieceMonoidWithObjects:-" ); 
            return SinglePieceMonoidWithObjects( mag, obs ); 
        elif ( HasIsSemigroup( mag ) and IsSemigroup( mag ) ) then 
            Info( InfoGpd, 1, "SinglePieceSemigroupWithObjects:-" ); 
            return SinglePieceSemigroupWithObjects( mag, obs ); 
        else  ## it's just a magma 
            Info( InfoGpd, 1, "SinglePieceMagmaWithObjects:-" );
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
    local  cf, one, r, gens, mwo, fmwo, cmwo, isa, isc; 
    ## (23/04/10) commented out: 
    ##    cf := CollectionsFamily( FamilyObj( mag ) ); 
    ##  and replaced by: 
    cf := MagmaWithObjectsFamily; 
    r := obs[1];  ## root object  
    isa := IsAssociative( mag ); 
    isc := IsCommutative( mag ); 
#?  how to make the next command work ?? 
##  ObjectifyWithAttributes( mwo, NewType( cf, IsMWOSinglePieceRep ), 
##         rec( objects := obs, magma := mag, eltsfam := cf ), 
##         IsAssociative, isa, IsCommutative, isc ); 
    mwo:= Objectify( NewType( cf, IsMWOSinglePieceRep ), 
           rec( objects := obs, magma := mag ) ); 
#?  associative magmas are semigroups, so omit next line ?? 
    SetIsAssociative( mwo, IsAssociative( mag ) ); 
    SetIsCommutative( mwo, IsCommutative( mag ) ); 
    SetIsFinite( mwo, IsFinite( mag ) ); 
    SetIsSinglePieceDomain( mwo, true ); 
    SetIsDirectProductWithCompleteGraphDomain( mwo, true ); 
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
    local  i, p1, p2;
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
            #? (20/05/11) this assumes pieces must be in the same order ?? 
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
#? (20/05/11) this method only gets value 18, so does not get called!    ?? 
#?            which is why there is an IsArrowIn 

InstallMethod( \in, "for elements of a magma with objects", true,
    [ IsMultiplicativeElementWithObjects, IsMagmaWithObjects ], 0, 
function ( elt, mwo ) 

    local  obs; 
    obs := ObjectList( mwo ); 
    if not ( ( elt![2] in obs ) and ( elt![3] in obs ) ) then 
        return false; 
    fi; 
    if not IsDirectProductWithCompleteGraphDomain( mwo ) then 
        Error( "method now yet implemented," ); 
    fi; 
    return ( elt![1] in mwo!.magma ); 
end ); 

InstallMethod( IsArrowIn, 
    "for mwo element and a standard magma with objects", true, 
    [ IsMultiplicativeElementWithObjects, 
      IsMagmaWithObjects and IsSinglePiece ], 0,
function( e, mwo )
    local  obs, r1, r2;
    obs := mwo!.objects; 
    if not ( (e![2] in obs) and (e![3] in obs) ) then 
        return false; 
    fi; 
    if ( HasIsDirectProductWithCompleteGraph( mwo ) 
         and IsDirectProductWithCompleteGraph( mwo ) ) then 
        return (e![1] in mwo!.magma);
    else 
        Error( "mwo not a standard magma with objects" ); 
    fi; 
end );

InstallMethod( IsArrowIn, 
    "for mwo element and a union of constituents", true, 
    [ IsMultiplicativeElementWithObjects, IsMagmaWithObjects and HasPieces ], 0,
function( e, mwo )
    return IsArrowIn( e, PieceOfObject( mwo, e![2] ) ); 
end );

############################################################################# 
## 
#M  ViewObj( <mwo> ) . . . . . . . . . . . . . . . view a magma with objects 
#M  PrintObj( <mwo> ) . . . . . . . . . . . . . . print a magma with objects 
##

InstallMethod( ViewObj, "for a single piece magma with objects", true, 
    [ IsSinglePiece ], 0,   
    function( mwo )
    local  type; 
    type := TypeOfDomainWithObjects( [ mwo ] ); 
    if ( type = 1 ) then 
        Print( "#I  should be using special groupoid method!\n" ); 
    elif ( type = 2 ) then 
        Print( "monoid with objects :-\n" ); 
    elif ( type = 3 ) then 
        Print( "semigroup with objects :-\n" ); 
    elif ( type = 4 ) then 
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
    local  type; 
    type := TypeOfDomainWithObjects( [ mwo ] ); 
    if ( type = 1 ) then 
        Print( "#I  should be using special groupoid method!\n" ); 
    elif ( type = 2 ) then 
        Print( "monoid with objects :-\n" ); 
    elif ( type = 3 ) then 
        Print( "semigroup with objects :-\n" ); 
    elif ( type = 4 ) then 
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
    local  i, pieces, np, type; 
    pieces := Pieces( dwo ); 
    np := Length( pieces ); 
    type := TypeOfDomainWithObjects( Pieces( dwo ) ); 
    if (type=1) then Print( "groupoid" );  
      elif (type=2) then Print( "monoid with objects" ); 
      elif (type=3) then Print( "semigroup with objects" ); 
      elif (type=4) then Print( "magma with objects" ); 
      elif (type=5) then Print( "domain with objects" ); 
      elif (type=0) then Error( "invalid domain with objects," ); 
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
    [ IsDomainWithObjects and IsPiecesRep ], 10,   
    function( dwo )
    local  i, pieces, np; 
    pieces := Pieces( dwo ); 
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
InstallMethod( Display, "for a mwo", [ IsMagmaWithObjects ],
function( mwo )
    
    local  comp, c, i, m, len;

    if IsSinglePiece( mwo ) then 
        if IsDirectProductWithCompleteGraph( mwo ) then 
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
        Print( "Magma with objects with ", len, " constituents:\n" );
        for i in [1..len] do
            c := comp[i];
            if IsDirectProductWithCompleteGraph( c ) then 
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
#M  RootObject( <mwo> )                    for a connected magma with objects
#M  RootObject( <mwo> )                   for a many piece magma with objects
##
InstallMethod( RootObject, "for a single piece mwo", 
    true, [ IsSinglePiece ], 0,
function( mwo )
    return mwo!.objects[1]; 
end ); 

InstallOtherMethod( RootObject, "for a mwo with pieces", 
    true, [ IsDomainWithObjects and IsPiecesRep ], 0,
function( mwo )
    Print( "#I only a single piece magma with objects has a root object\n" ); 
    return fail; 
end ); 

#############################################################################
##
#M  GeneratorsOfMagmaWithObjects( <mwo> )  for a connected magma with objects
##
InstallMethod( GeneratorsOfMagmaWithObjects, "for a single piece mwo", 
    true, [ IsSinglePiece ], 0,
function( mwo )

    local  obs, nobs, o1, m, mgens, type, id, gens1, gens2, gens, rays, 
           i, j, k, g;

    obs := mwo!.objects;
    nobs := Length( obs );
    o1 := obs[1];
    m := mwo!.magma; 
    mgens := GeneratorsOfMagma( m ); 
    if HasGeneratorsOfMagmaWithInverses( m ) then 
        type := 4; 
    elif HasGeneratorsOfMagmaWithOne( m ) then 
        type := 3; 
    elif ( HasIsSemigroup( m ) and IsSemigroup( m ) ) then 
        type := 2; 
    else  ## just a magma 
        type := 1; 
    fi; 
    if ( type > 2 ) then 
        id := One( m ); 
        if ( type = 4 ) then ## a groupoid 
            gens1 := List( mgens, 
                g -> ArrowNC( true, g, o1, o1 ) ); 
            if ( HasIsDirectProductWithCompleteGraph( mwo ) 
                 and IsDirectProductWithCompleteGraph( mwo ) ) then 
                gens2 := List( obs{[2..nobs]}, 
                    o -> GroupoidElement( mwo, id, o1, o ) ); 
            elif IsSinglePieceRaysRep( mwo ) then  
                rays := mwo!.rays; 
                gens2 := List( [2..nobs], 
                    i -> GroupoidElement( mwo, rays[i], o1, obs[i] ) ); 
            fi; 
            gens := Immutable( Concatenation( gens1, gens2 ) ); 
            SetGeneratorsOfMagmaWithInverses( mwo, gens ); 
        else 
            gens1 := List( mgens, 
                g -> ArrowNC( false, g, o1, o1 ) );
            gens2 := ListWithIdenticalEntries( (nobs-1)^2, 0 ); 
            k := 0; 
            for i in obs do 
                for j in obs do 
                    if ( i <> j ) then 
                       k := k+1; 
                       gens2[k] := ArrowNC( false, id, i, j ); 
                    fi;
                od;
            od;
            gens := Immutable( Concatenation( gens1, gens2 ) ); 
            SetGeneratorsOfMagmaWithOne( mwo, gens ); 
        fi; 
    else  
    ## add in all possible generators? 
        gens := ListWithIdenticalEntries( nobs*nobs*Length(mgens), 0 ); 
        k := 0; 
        for i in obs do 
           for j in obs do 
              for g in mgens do 
                k := k+1; 
                gens[k] := ArrowNC( false, g, i, j );
              od;
           od; 
        od;
        SetGeneratorsOfMagma( mwo, gens ); 
    fi; 
    return gens; 
end );

InstallMethod( GeneratorsOfMagmaWithObjects, "for discrete domain",
    true, [ IsGroupoid and IsSinglePiece and IsDiscreteDomainWithObjects ], 0,
function( mwo )
    local  o, ogens, gens;  
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
    local  L; 
    L := Concatenation( List( Pieces( mwo ), 
             p -> GeneratorsOfMagmaWithObjects( p ) ) ); 
    return L; 
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

    local  obs; 

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
#M  UnionOfPiecesNC . . . . . . . for a list of connected magmas with objects
#M  UnionOfPieces
##
InstallMethod( UnionOfPiecesNC, "method for magmas with objects, and type",
    true, [ IsList, IsInt ], 0,
function( comps, type )

    local  len, pieces, L, fam, filter, mwo, i, obs, par;

    ## type:  1=gpd, 2=mon, 3=sgp, 4=mgm, 5=dom 

    ## order pieces by first object
    len := Length( comps ); 
    obs := List( comps, g -> g!.objects[1] );
    L := [1..len];
    SortParallel( obs, L );
    if ( L = [1..len] ) then 
        pieces := comps; 
    else 
        Info( InfoGpd, 2, "reordering pieces by first object" ); 
        pieces := List( L, i -> comps[i] );
    fi; 
    ## ?? (23/04/10) fam := FamilyObj( [ pieces ] ); 
    if ( type = 1 ) then 
        fam := GroupoidFamily; 
    else 
        fam := MagmaWithObjectsFamily; 
    fi;
    if ( type = 1 ) then 
        filter := IsPiecesRep and IsGroupoid and IsAssociative; 
    elif ( type = 2 ) then 
        filter := IsPiecesRep and IsMagmaWithObjectsAndOnes 
                              and IsAssociative; 
    elif ( type = 3 ) then 
        filter := IsPiecesRep and IsMagmaWithObjects and IsAssociative; 
    elif ( type = 4 ) then 
        filter := IsPiecesRep; 
    else 
        Error( "union of unstructured domains not yet implemented," ); 
    fi; 
    #?  should use ObjectifyWithAttributes here ??
    mwo := Objectify( NewType( fam, filter ), rec () );
    SetIsSinglePieceDomain( mwo, false ); 
    SetPieces( mwo, pieces ); 
    if HasParent( pieces[1] ) then 
        par := Ancestor( pieces[1] ); 
        if ForAll( pieces, c -> ( Ancestor( c ) = par ) ) then 
            SetParent( mwo, par ); 
        fi; 
    fi; 
    if ( type = 1 ) then 
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

InstallMethod( UnionOfPieces, "generic method for magmas with objects",
    true, [ IsList ], 0,
function( parts )

    local  npa, part, pieces, p, nco, obs, obp, i, gi, nobj, type;

    npa := Length( parts );
    pieces := [ ]; 
    if ( Length( parts ) = 1 ) then 
        Error( "only one part supplied," ); 
    fi; 
    for part in parts do 
        if not IsDomainWithObjects( part ) then
            Info( InfoGpd, 1, "part ", part, "not an mwo" );
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
            Info( InfoGpd, 1, "constituents must have disjoint object sets" );
            return fail;
        fi;
        obs := Union( obs, obp ); 
    od;
    type := TypeOfDomainWithObjects( pieces ); 
    Info( InfoGpd, 2, "union has type ", type ); 
    return UnionOfPiecesNC( pieces, type );
end );

#############################################################################
##
#M  DomainWithSingleObject
##
##  Note that there is another method for [ IsGroup, IsObject ] in gpd.gi 
##
InstallMethod( DomainWithSingleObject, "generic method for domain, object",
    true, [ IsDomain, IsObject ], 0,
function( dom, obj )
    local  o; 
    if ( IsList( obj ) and ( Length(obj) = 1 ) ) then
        Info( InfoGpd, 2, "object given as a singleton list" );
        o := obj[1]; 
    else
        o := obj;
    fi; 
    if not IsScalar( o ) then 
        Error( "<obj> not a scalar or singleton list," ); 
    fi; 
    if ( HasIsAssociative( dom ) and IsAssociative( dom ) 
         and ( "IsMagmaWithInverses" in CategoriesOfObject( dom ) ) 
         and IsMagmaWithInverses( dom ) ) then 
        return SinglePieceGroupoidNC( dom, [o] ); 
    elif ( HasIsMonoid( dom ) and IsMonoid( dom ) ) then 
        return SinglePieceMonoidWithObjects( dom, [o] ); 
    elif ( HasIsSemigroup( dom ) and IsSemigroup( dom ) ) then 
        return SinglePieceSemigroupWithObjects( dom, [o] ); 
    elif ( ( "IsMagma" in CategoriesOfObject(dom) ) and IsMagma(dom) ) then 
        return SinglePieceMagmaWithObjects( dom, [o] ); 
    else 
        Error( "unstructured domains with objects not yet implemented," ); 
    fi; 
end );

############################################################################# 
## 
#M  PieceOfObject
## 
InstallMethod( PieceOfObject, "generic method for magma with objects", 
    true, [ IsDomainWithObjects, IsScalar ], 0,
function( dwo, obj )

    local  pieces, p, objp;

    if IsSinglePiece( dwo ) then
        if not ( obj in dwo!.objects ) then
            Error( "<obj> not an object of <dwo>," );
        else
            return dwo;
        fi;
    elif not ( obj in ObjectList( dwo ) ) then
        Info( InfoGpd, 1, "<obj> not an object of <dwo>" );
        return fail;
    fi;
    pieces := Pieces( dwo );
    for p in pieces do
        objp := p!.objects;
        if ( obj in objp ) then
            return p;
        fi;
    od;
    Info( InfoGpd, 1, "it appears that <obj> is not an object in <dwo>" );
    return fail;
end );

############################################################################# 
## 
#M  PieceNrOfObject
## 
InstallMethod( PieceNrOfObject, "generic method for domain with objects",
    true, [ IsDomainWithObjects, IsScalar ], 0,
function( dwo, obj )

    local  pieces, i, objp, np;
    pieces := Pieces( dwo );
    for i in [1..Length( pieces )] do
        objp := pieces[i]!.objects;
        if ( obj in objp ) then
            return i;
        fi;
    od;
    Info( InfoGpd, 1, "it appears that <obj> is not an object in <dwo>" );
    return fail;
end );

#############################################################################
##
#M  IsDiscreteDomainWithObjects 
##
InstallMethod( IsDiscreteDomainWithObjects, "for a magma with objects", true,
    [ IsDomainWithObjects ], 0,
function( dwo )

    local  p;
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

    local  pieces, g, j, iso; 
    pieces := Pieces( dwo ); 
    g := pieces[1]!.magma; 
    for j in [2..Length(pieces)] do 
        iso := IsomorphismGroups( g, pieces[j]!.magma ); 
        if ( iso = fail ) then
            return false;
        fi;
    od;
    return true;
end );

InstallMethod( IsHomogeneousDomainWithObjects, "for a magma with objects", 
    true, [ IsDomainWithObjects ], 0,
function( dwo )

    local  pieces, sizes, g, j, iso; 
    pieces := Pieces( dwo ); 
    sizes := Set( List( pieces, p -> Size( p ) ) ); 
    if not ( Length( sizes ) = 1 ) then 
        return false; 
    fi;
    g := pieces[1]!.magma; 
    for j in [2..Length(pieces)] do 
        iso := IsomorphismGroups( g, pieces[j]!.magma ); 
        if ( iso = fail ) then
            return false;
        fi;
    od;
    return true;
end );


#################################  SUBDOMAINS  ############################## 

#############################################################################
##
#F  IsSubdomainWithObjects( <M>, <U> )
##
InstallMethod( IsSubdomainWithObjects, "for two domains with objects", true, 
    [ IsDomainWithObjects, IsDomainWithObjects ], 0, 
    function( D, U )

    local  compU, obj, p, ok; 

    #?  (21/11/08)  does any of this make sence ?? 
    #?  does a subdomeain of a groupoid have to be a groupoid ?? 
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
            #?  (21/11/08)  needs more testing here ?? 
            ok := IsSubgroup( ObjectGroup(D,obj), ObjectGroup(U,obj) ); 
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
#F  IsWide( <D>, <U> )
##
InstallMethod( IsWide, "for two domains with objects", true, 
    [ IsDomainWithObjects, IsDomainWithObjects ], 0, 
    function( D, U )

    return ( IsSubdomainWithObjects( D, U ) and 
             ObjectList( D ) = ObjectList( U ) ); 
end ); 


################################  SEMIGROUPS  ############################### 

############################################################################# 
## 
#F  SemigroupWithObjects( <sgp>, <obs> ) 
##
InstallGlobalFunction( SemigroupWithObjects, function( arg ) 

    local  obs, sgp;
    # list of objects and a semigroup 
    if ( ( Length( arg ) = 2 ) and IsSemigroup( arg[1] ) 
            and IsCollection( arg[2] ) and IsScalar( arg[2][1] ) ) then 
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

    local  cf, one, r, gens, swo, isa, isc; 

    ## ?? (23/04/10)  cf := CollectionsFamily( FamilyObj( sgp ) ); 
    cf := MagmaWithObjectsFamily; 
    r := obs[1];  ## root object  
    isa := IsAssociative( sgp ); 
    isc := IsCommutative( sgp ); 
    swo:= Objectify( NewType(  cf, IsMWOSinglePieceRep ), 
           rec( objects := obs, magma := sgp ) ); 
    SetIsAssociative( swo, IsAssociative( sgp ) ); 
    SetIsCommutative( swo, IsCommutative( sgp ) ); 
    SetIsFinite( swo, IsFinite( sgp ) ); 
    SetIsSinglePieceDomain( swo, true ); 
    SetIsDirectProductWithCompleteGraphDomain( swo, true ); 
    gens := GeneratorsOfMagmaWithObjects( swo ); 
    return swo; 
end ); 




##################################  MONOIDS  ################################ 

############################################################################# 
## 
#F  MonoidWithObjects( <mon>, <obs> ) 
##
InstallGlobalFunction( MonoidWithObjects, function( arg ) 

    local  obs, mon;
    # list of objects and a monoid 
    if ( ( Length( arg ) = 2 ) and IsMonoid( arg[1] ) 
            and IsCollection( arg[2] ) and IsScalar( arg[2][1] ) ) then 
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

    local  cf, one, r, gens, mwo, fmwo, cmwo, isa, isc; 

    ## ?? (23/04/10)  cf := CollectionsFamily( FamilyObj( mon ) ); 
    cf := MagmaWithObjectsFamily; 
    r := obs[1];  ## root object  
    isa := IsAssociative( mon ); 
    isc := IsCommutative( mon ); 
    mwo:= Objectify( NewType(  cf, IsMWOSinglePieceRep 
                               and IsMagmaWithObjectsAndOnes ), 
           rec( objects := obs, magma := mon ) ); 
    SetIsAssociative( mwo, IsAssociative( mon ) ); #? must be true?
    SetIsCommutative( mwo, IsCommutative( mon ) ); #? ditto?
    if ( HasIsFinite( mon ) and IsFinite( mon ) ) then 
        SetIsFinite( mwo, true );
    fi; 
    SetIsSinglePieceDomain( mwo, true ); 
    SetIsDirectProductWithCompleteGraphDomain( mwo, true ); 
    one := One( mon ); 
    gens := GeneratorsOfMagmaWithObjects( mwo ); 
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
    local  C, mag, obs, cf, isa, isc, swo; 
    obs := mwo!.objects; 
    mag := mwo!.magma; 
    C := SubmagmaWithObjectsElementsTable( mag, A ); 
    ## ?? (23/04/10)  cf := mwo!.eltsfam; 
    cf := MagmaWithObjectsFamily; 
    isa := IsAssociative( mag ); 
    isc := IsCommutative( mag ); 
    swo:= Objectify( NewType(  cf, IsSubmagmaWithObjectsTableRep ), 
           rec( objects := obs, magma := mag, table := C ) ); 
    SetParentAttr( swo, mwo );
    SetIsAssociative( swo, IsAssociative( mag ) ); 
    SetIsCommutative( swo, IsCommutative( mag ) ); 
    SetIsFinite( swo, IsFinite( mag ) ); 
    SetIsSinglePieceDomain( swo, true ); 
    #? (13/10/08)  still need to set GeneratorsOfMagma ?? 
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
    local  found, d; 

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
            if ( HasIsDirectProductWithCompleteGraph( mwo ) 
                 and IsDirectProductWithCompleteGraph( mwo ) ) then 
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
