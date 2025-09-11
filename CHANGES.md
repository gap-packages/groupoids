# CHANGES to the 'groupoids' package 

## Version 1.79 for GAP 4.14.0 (11/09/25)
 * (11/09/25) added operation GroupoidInnerAutomorphismNormalSubgroupoid
 * (10/09/25) added new release mechanism file .github/workflows/release.yml 
 * (22/07/25) revised code and examples of GroupoidInnerAutomorphism
 * (21/07/25) added GroupoidAutomorphismByNtuple(NC)
              added HomogeneousDiscreteSubgroupoid
 * (18/07/25) started work on groupoid actions

## Version 1.78 for GAP 4.14.0 (18/07/25) 
 * (17/07/25) documented some missing categories in the manual

## Version 1.77 for GAP 4.14.0 (03/07/25)
 * (03/07/25) made extensive changes to the examples in folder tst/manual 
              added operation IsFullSubgroupoid(gpd,subgpd)
              Subgroupoid now checks for SubgroupoidWithRays
              SubgroupoidByPieces now copes with mixed piece data
              added method for \= for two groupoid homomorphisms
              documented the operation PiecePositions
              revised HomomorphismByUnion - deals with many pieces

## Version 1.76 for GAP 4.13.1 (23/09/24) 
 * (23/09/24) fix error caused re immutable ExtRepOfObj by GAP pull #5808

## Version 1.74 for GAP 4.12.2 (09/02/23) 
 * (23/01/24) avoid trivial function wrappers; correct email address 

## Version 1.73 for GAP 4.12.2 (09/02/23) 
 * (09/02/23) first attempt at basic double groupoids 
 * (27/01/23) added RegularActionHomomorphismGroupoid and used in gpdhom.tst 
 * (23/12/22) changed email address and other personal details

## Version 1.71 for GAP 4.11.1 (07/08/22) 
 * (07/08/22) now requiring version 0.76 of Utils 
              major revision of left, right and double cosets 

## Version 1.69 for GAP 4.11.1 (15/11/21) 
 * (30/06/22) renamed GpdIsDigraph etc as IsGroupoidDigraph 
 * (15/11/21) moved package homepage to 
              https://github.com/gap-packages/groupoids 
 * (15/11/21) replaced IsDiscrete by IsDiscreteMagmaWithObjects 
 * (13/04/21) changed nicemap.tst to cope with alternative NiceMonomorphisms 
 * (08/04/21) Switch CI to use GitHub Actions 

## Version 1.68 for GAP 4.10.2 (04/09/19) 
 * (12/07/19) fixed errors in tests introduced in gapdev

## Version 1.67 for GAP 4.10.1 (17/06/19) 
 * (16/06/19) added Utils to the required packages 

## Version 1.66 for GAP 4.10.1 (29/05/19) 
 * (18/04/19) reorganised all .tst files into /tst/manual/ and /tst/extra/
 * (08/04/19) AutomorphismGroupoidOfGroupoid for homogeneous union 
 * (05/04/19) methods for IsInjective, IsSurjective for groupoid homs 
 * (03/04/19) methods for homomorphisms of IsGroupoidByIsomorphisms 
 * (26/03/19) added (Is)GroupoidByIsomorphisms with test and manual entry. 
 * (24/03/19) extended IsomorphismStandardGroupoid to a union of pieces
 * (21/03/19) added operation IsomorphismGroupoids  

## Version 1.65 for GAP 4.10.0 (05/03/19) 
 * (05/03/19) added attribute RightActionGroupoid 
 * (16/02/19) added License field in PackageInfo.g 

## 1.61 -> 1.63  (23/10/18) 
 * (23/10/18) Added ReducedImageElm for IsMappingToFreeProductWithAmalgamation
              IsFpaGroup now IsFreeProductWithAmalgamation; sim. IsHnnGroup 
 * (15/10/18) Undid previous commit; revised FreeProductWithAmalgamation(Op) 
              and made similar changes to HnnExtension(Op) inc. Embeddings 
              FpaInfo is now FreeProductWithAmalgamationInfo; sim. HnnInfo 

## 1.59 -> 1.61  (09/10/18) 
 * (09/10/18) Added 'GGRWS' to 'FreeProductWithAmalgamation', 'HnnExtension'

## 1.57 -> 1.59  (13/09/18) 
 * (13/09/18) GroupoidAutomorphismByObjectPerm method for groupoid with rays
 * (13/09/18) removed dependency on Utils - but still used by xtst/testextra.g
 * (12/09/18) removed attribute InverseOfIsomorphismFpSemigroup 
 * (11/09/18) added AutomorphismGroupoidOfGroupoid for homogeneous groupoids
 * (05/09/18) replaced SmallGroup(8,4) with QuaternionGroup(8)

## 1.55 -> 1.57  (28/08/18) 
 * (28/08/18) removed Semigroups from SuggestedOtherPackages (temporarily?) 
 * (10/05/18) added attribute PieceIsomorphisms for homogeneous dwos
 * (09/05/18) added attribute ParentMappingGroupoids for restricted mappings

## 1.54 -> 1.55  (02/02/18) 
 * (01/02/18) renamed test files in folder xtst/ 
 * (30/01/18) Rewrote to Left(Right)CosetRepresentatives(FromObject) 
 * (24/01/18) Corrections to methods for RightCoset and LeftCoset
 * (18/01/18) Removed HomsetCosetsGroupoidCoset + old IsHomsetCosetsType
 * (17/01/18) Operation RayElementsOfGroupoid now Attribute RaysOfGroupoid 
              and Attribute RaysOfGroupoid now Operation RayArrowsOfGroupoid 
 * (11/01/18) now using AutoDoc to build the manual 
 * (21/12/17) changed HomsetCosets from IsPositionalObjectRep 
              corrected SubgroupoidByPieces when rays are involved 
 * (19/12/17) add IsomorphismNewObjects method for hom discrete 
              methods for \in for AutomorphismGroupOfGroupoid 
 * (15/12/17) removed examples/*.g; added expt/; .xtst files not in xtst/ 
              split off autogroup functions from gpdhom.g{d,i} to gpdaut.g{d,i} 
 * (14/12/17) added method for \in for automorphisms of groupoids 

## 1.53 -> 1.54  (29/11/17) 
 * (29/11/17) revised manual Ch.5; added test to GroupoidHomomorphism; release!  
 * (27/11/17) added MappingGeneratorsImages method for hom discrete mappings
 * (18/10/17) fixed bug in DiscreteSubgroupoid; RestrictedMapGpd now in manual
 * (17/10/17) added other method for GroupoidInnerAutomorphism(R,S,r); 
 * (16/10/17) added method for RestrictedMappingGroupoids for homdisc 
 * (12/10/17) added DirectProductOp for groupoids; Projection & Embedding 
 * (11/10/17) added MappingWithObjectsByFunction and associated prop/attr and 
              then added new manual section on the inner automorphism group
 * (09/10/17) added operation GroupoidInnerAutomorphism 

## 1.52 -> 1.53  (04/10/17) 
 * (03/10/17) added section 5.5 to the manual: matrix reps of groupoids 
 * (29/09/17) test files back to original names and made independent 
 * (27/09/17) introduced lots of Types and adjusted Objectify commands 

## 1.51 -> 1.52  (21/09/17) 
 * (21/09/17) revised HomomorphismToSinglePiece and associated functions 
 * (21/09/17) SinglePieceMappingData now MappingToSinglePieceData 
 * (21/09/17) revised IsomorphismPermGroupoid and added IsomorphismPcGroupoid 
 * (20/09/17) revised RestrictedMappingGroupoids 
 * (13/09/17) removed (essentially unused) attribute HomsOfMapping
 * (13/09/17) now method for Size works for all magmas with objects 
 * (12/09/17) revised methods for GeneratorsOfMagmaWithObjects etc. 
 * (05/09/17) added lots of new default types, avoiding NewType(fam,filter)'s
 * (05/09/17) added properties IsMagmaWithObjectsInPieces etc 
 * (05/09/17) op TypeOfDomainWithObjects -> attribute KindOfDomainWithObjects
 * (04/09/17) revised the various category structures, so operations 
              IsArrowIn and IsElementInGroupoid now replaced by \in 
 * (27/08/17) GGHead/GGTail renamed Head/TailOfGraphOfGroupsWord 
 * (25/08/17) converted aut-test.g, hom-tst.g to .xtst; added tst/testextra.g
 * (18/08/17) provided a method for < for groupoid elements 
 * (14/08/17) redesign of homs proceeds : new InverseGeneralMapping for isos 
 * (11/09/17) PieceImages -> SinglePieceMappingData 
 * (08/08/17) testall.g copied to testing.g; testall now calls TestDirectory

## 1.46 -> 1.51  (06/08/17)     
 * (06/08/17) version 1.51 released - for GAP 4.8.8 
 * (03/07/17) README and CHANGES now in MarkDown format as .md files 
 * (14/06/17) added various methods for String, ViewString, PrintString 
 * (07/04/17) renamed the package 'groupoids' so most files needed editing 

## 1.45 -> 1.46  (21/02/17) 
 * (21/02/17) removed method for IsCommutative from `gpd.gi` 
              which was causing slowdown in one of the main GAP tests 

## 1.43 -> 1.45  (02/11/16) 
 * (02/11/16) improved RestrictedMappingGroupoids 
 * (01/11/16) added IsomorphismStandardGroupoid to `gpdhom.g{d,i}`; 
             GroupoidAutomorphismByRayImages->GroupoidAutomorphismByRayShifts; 
            SinglePieceGroupoidByGenerators->SinglePiceSubgroupoidByGenerators;
           improved RestrictedMappingGroupoids
 * (18/10/16) now using bibliography file `bib.xml` of type `bibxmlext.dtd`
 * (12/10/16) changed package releases to <https://gap-packages.github.io/gpd> 
 * (03/10/16) ImagesOfRays renamed ImageElementsOfRays 
 * (28/04/16) replaced Image(hom,x) with ImageElm(hom,x) throughout 
 * (28/04/16) moved AutomorphismGroup methods to AutomorphismGroupOfGroupoid
 * (27/04/16) added EmbeddingsInNiceObject 
 * (14/04/16) converted cases of \in to new InAutomorphismGroupOfGroupoid 

## 1.41 -> 1.43  (16/03/16) 
 * (16/03/16) dealt with new diffs in test files 
 * (18/02/16) removed date/version info from file headers 
 * (15/02/16) Added method for ImagesRepresentative( gpdhom, arrow ) 

## 1.36 -> 1.41  (09/02/16) 
 * (04/02/16) replacing test IsScalar for objects with IsObject/IsSet 
              added examples in section 4.1.1 where the objects are 
              free group generators or strings 
 * (12/01/16) renamed some ENTITYs in `PackageInfo.g` 

## 1.35 -> 1.36  (23/11/15) 
 * (23/11/15) new method for ObjectGroupHomomorphism 
 * (23/11/15) converted RootObject from an operation to an attribute 
 * (23/11/15) renamed IsDigraph etc as GpdIsDigraph as requested by James 
 * (16/11/15) RestrictionMappingGroupoids -> RestrictedMappingGroupoids 
 * (15/11/15) added method for WeightedAdjacencyMatrix for FpWeightedDigraphs 
 * (15/11/15) added PrintObj & Viewobj for groupoid right cosets 
 * (04/11/15) removed RestrictionMappingGroups: now GeneralRestrictedMapping
 * (28/10/15) added MathJax to `makedocrel.g` 
 * (01/09/15) major edits to `README`, including GitHub issues link 

## 1.34 -> 1.35  (24/08/15) 
 * (24/08/15) packed up version 1.35 prior to move from Bitbucket to Github 
 * (11/06/15) removed GroupoidHomomorphismByGroupHom and increased the 
              number of options recognised by GroupoidHomomorphism 
 * (10/06/15) fixed errors in NormalFormGGRWS for fpa- and hnn-groups 
 * (10/06/15) added methods for ReplaceOnePieceInUnion, and a new excample 
 * (10/06/15) made various improvements to the manual 
 * (10/06/15) renamed RootObjectHomomorphism as RootGroupHomomorphism 

## 1.32 -> 1.34  (05/06/15) 
 * (05/06/15) added method for GeneratorsOfMagma for a magma with objects 
 * (05/06/15) attempted fix of problem with NormalFormGGRWS 
 * (02/06/15) `PackageInfo.g` : 'gpd' is now an accepted package  
 * (02/06/15) converted the bibliography to BibXMLext format 
              and added an URL to Emma's thesis (`moore.ps.gz`) 
 * (02/06/15) Fixed typos in the manual reported by the referee 

## 1.31 -> 1.32  (03/02/15) 
 * (03/02/15) changed 'InversesIfNonzero' to 'Inverse' in category names 

## 1.23 -> 1.31  (17/12/14) 
 * (17/12/14) moved package homepage to <pages.bangor.ac.uk/~mas023/chda/gpd/> 
 * (26/11/14) changed Arrow to ArrowNC in GeneratorsOfMagmaWithObjects 

## 1.22 -> 1.23  (03/07/14) 
 * (03/07/14) updated chapter 6 in the manual: Technical Notes
 * (02/07/14) realised that rays are arrows so swapped the names: 
              RaysOfGroupoid with RayElementsOfGroupoid 
 * (02/07/14) added second Kd8method for IsHomogeneousDomainWithObjects
 * (01/07/14) renamed MultiplicativeElementWithObjects as Arrow, 
                      IsElementOfMagmaWithObjects as IsArrowIn, 
                      GroupoidElement no longer needed: use Arrow, 
                      IdentityElement as IdentityArrow, 
                      ConjugateGroupoidElement as ConjugateArrow.
              note that IsGroupoidElement still remains (not visible to users). 
 * (25/06/14) modified HomogeneousGroupoid(gpd,oblist) so that the initial gpd
              is no longer automatically included in the output 
 * (13/05/14) modified HomogeneousGroupoid and ReplaceOnePieceInUnion
 * (07/05/14) added IsMatrixGroupoid 
 * (27/11/13) added semigroups package to SuggestedOtherPackages (Needed...?) 

## 1.19 -> 1.22  (20/11/13) 
 * (20/11/13) Adjusted flip function in `mwohom.g` and `mwohom.tst`. 
 * (10/05/13) Added method for ImageElm for mwohoms and more than one piece. 
 * (09/05/13) Added IsElementOfMagmaWithObjects (in place of \in). 
 * (08/05/13) Renamed (again) Arrowelt -> ElementOfArrow, etc. 
 * (07/05/13) Started to fix a whole set of typos in the manual. 

## 1.17 -> 1.19  (11/03/13) 
 * (11/03/13) Minor modifications to test files to fix differences. 
 * (06/02/13) Changed IsDiscreteDomainWithObjects to be false when there 
              is only one object, and now there are diffs in tests! 
 * (05/02/13) rewrite of `testall.g` following Alex K's wedderga example 
 * (24/01/13) Brought back tests for graphs of groupoids and their words, 
              and added a section on these to `examples/ggraph.g`. 
 * (23/01/13) Made changes to PrintObj for groupoids. 
 * (22/01/13) Fixed problem with IsHomomorphismFromSinglePiece. 
              Made changes to PrintObj and Display methods. 

## 1.16 -> 1.17  (14/01/13) 
 * (12/01/13) Converted Vertices and Arcs back from Attributes to Operations 
              (otherwise there is a clash with the Grape package). 

## 1.15 -> 1.16  (09/01/13) 
 * (09/01/13) corrected output in test files (words in fp-groups are now 
              factorised where possible) and finalised version 1.16. 
 * (24/10/12) ??? Remove subgpds as input parameter for GraphOfGroupoids ??? 
 * (23/10/12) Added attribute WeightedAdjacencyMatrix for digraphs 
              and operation ArcsFromAdjacencyMatrix 
              Removed subgps as input parameter for GraphOfGroups, 
              so attribute SubgroupsOfGraphOfGroups is redundant. 
 * (27/06/12) Added operation EndoMappingToOne 

## 1.14 -> 1.15  (09/06/12) 
 * (09/06/12) Added test for trivial groups in InclusionMappingGroups 
 * (08/06/12) Removed IdentitySubgroup and used TrivialSubgroup instead, 
              following email from Max Horn 
              Similarly replaced Identity with Trivial in the operations 
              FullIdentitySubgroupoid and DiscreteIdentitySubgroupoid 

## 1.13 -> 1.14  (23/04/12) 
 * (20/04/12) Corrected ImageElm for single-piece groupoid elements. 
 * (18/04/12) Replaced ReadTest with Test in `testall.g` 
 * (12/01/12) Experimentally added method for IsCommutative to `gpd.gi` 
              for the case of a group handled by a nice monomorphism. 
 * (12/01/12) Edited examples in `gpdhom.g`, `gpdhom2`.g, `gpdhom.tst`, `gpdhom.xml`
 * (16/12/11) Fixed error in method for IsHomogeneousDomainWithObjects 

## 1.12 -> 1.13  (14/12/11) 
 * (09/12/11) Added AutomorphismGroup and NiceObjectAutoGroupGroupoid methods 
              for homogeneous discrete groupoids
 * (03/12/11) Added ObjectGroupHomomorphism 
              Renamed IsMappingWithObjectsRep as IsMappingWithPiecesRep 
              Improved methods for \= and Display for groupoid morphisms. 
 * (01/12/11) Added functions for homogeneous, discrete groupoids 
              and their morphisms. 
 * (30/11/11) IsDiscreteDomain now IsDiscreteDomainWithObjects. 
              Added IsHomogeneousDomainWithObjects. 
              GroupoidAutomorphismByRayImages now includes its order. 
 * (29/11/11) removed Emma's email address 
              now using package directory in the format `.../gpd-1.13/` 
              and archive files in the format  `gpd-1.13.tar` 

## 1.09 -> 1.12  (21/09/11) 
 * (20/09/11) new version of `makedocrel.g` for building the manual 
              added file `gpd/examples/readall.g` for testing purposes 

## 1.08 -> 1.09  (17/09/11) 
 * (17/09/11) Shortened the banner 
 * (16/09/11) Renamed subdirectory `gpd/gap` as `gpd/lib` 
              Status of package now specified as "submitted". 
 * (13/09/11) Added fga as a required package (used by `tst/gpd.tst`). 

## 1.07 -> 1.08  (06/09/11) 
 * (06/09/11) Changed IsGraphOfGroups to IsGraphOfGroupsRep 
              and introduced category IsGraphOfGroups and GraphOfGroupsFamily, 
              and similarly for IsGraphOfGroupoids. 
 * (04/09/11) Changed BIND_GLOBAL to BindGlobal, since it is 'safer' 
 * (16/08/11) Changed directory for archive to `.../chda/gap4r5/gpd/`

## 1.05 -> 1.07  (08/07/11) 
 * (08/07/11) Changed RootHomomorphism to RootObjectHomomorphism and 
              added new operation RootHomomorphism. 
 * (07/07/11) Extended GroupoidAutomorphismByObjectPerm to rays case. 
              Corrected \*  and ImageElm for gpd homs in the rays case. 
              Removed lists of known properties from test files 
              since the answers change after LoadAllPackages(); 
 * (06/07/11) Used RayImages instead of RayProducts for gpd homomorphisms. 
              Improved GroupoidAutomorphismByRayImages(NC). 
 * (26/06/11) Implemented easy case of SinglePieceGroupoidByGenerators, 
              and used this to rewrite ConjugateGroupoid. 
 * (22/06/11) Changed examples in `gpd.g`, `gpdhom.g` and the test versions. 
              Added GroupoidHomomorphismFromSinglePiece (non NC version).  
 * (15/06/11) Changed to ObjectifyWithAttributes in HomToSinglePieceNC. 
              Various edits in `mwohom.gi` and `gpdhom.gi`
 * (12/06/11) Improved InclusionMappingGroupoids. 
 * (10/06/11) Redid version and copyright lines in all files. 
 * (27/05/11) Extensive revisions and additions to chapter 4 of the manual. 
              Fixed bugs in FullSubgroupoid etc (swapping gps with obs). 
              Fixed some subgroupoid functions, using IsList instead of 
              IsHomogeneousList, since the groups may be of differing types.  
 * (20/05/11) Fixed bug in IsomorphismNewObjects, using Set(). 
               Rewrote the method for Ancestor. 
 * (16/05/11) Added method for IsWide in mwo.gi 
 * (12/05/11) Started tidying up a version to test with GAP 4.5 beta. 
 * (09/02/11) Fixed \= for groupoids with IsSinglePieceRaysRep. 
 * (28/01/11) Renamed GroupoidHom..ByObjectImages as ....ByObjectPerm. 
               Improved ImageElm for groupoid homomorphisms. 
               Improved InverseGeneralMapping for groupoid homomorphisms. 
 * (27/01/11) Big changes to groupoid homomorphisms, with the idea that 
               only simple functions should be provided, and more complicated 
               morphisms constructed by composition: 
               - added GroupoidHomomorphismByGroupHom, 
               - removed GroupoidHomomorphismFromSinglePiece, 
               - rewrote global function GroupoidHomomorphism. 
 * (26/01/11) Added operations RootGroup and RaysOfGroupoid.  
               Rewrote SubgroupoidWithRays. 
               Renamed LeftProdsOfRays as RayProducts, 
               and .rayConjugators as .rays .
 * (22/01/11) Changed AutomorphismGroup and NiceObjectAutoGroupGroupoid 
               by putting the group automorphisms before the object perms. 
 * (20/01/11) Finished new version of AutomorphismGroup for a groupoid: 
               includes function: NiceObjectAutoGroupGroupoid. 
 * (19/01/11) Renamed the 08.05 functions GroupoidMappingBy... 
               as GroupoidAutomorphismBy... and did major editing. 
 * (14/01/11) Constructed rep of the automorphism group of a groupoid 
               as a quotient of a semidirect product of perm groups 
               (i.e. implementing Prop.3.1 in Alp/Wensley 2010). 
 * (13/01/11) Corrected IdentityMapping for groupoids. 
               Fixed ParentAttr in FullSubgroupoid. 
 * (12/01/11) Checked the order for maps: [ src, rng, hom, imobs ].
               Implemented  m1*m2  when m1,m2 have several pieces. 
               Renamed ObjectPermOfGroupoidHomomorphism 
                    as ObjectTransformation OfGroupoidHomomorphism 
               and added MappingTransObjectsImages. 
               Added TestAllProductsUnderGroupoidHomomorphism. 
               Rename the following: 
                    MagmaHomomorphism..Piece.. -> Homomorphism..Piece.., 
                  IsHomomorphismWithObjectsRep -> IsMappingWithObjectsRep, 
                IsHomomorphismToSinglePieceRep -> IsMappingToSinglePieceRep
 * (02/01/11) Added checks in GroupoidHomomorphismFromSinglePiece. 
 * (21/09/10) Added operation RootObject. 
               Added LargerDirectProductGroupoid for IsSinglePieceRaysRep, 
               so a RaysRep groupoid is a subgroupoid of a direct product 
               groupoid, which may in turn have a larger parent. 
               Added operation ReplaceOnePieceInUnion. 
 * (20/09/10) Implemented ConjugateGroupoid and removed ConjugateSubgroupoid. 
               MultiplicativeElementWithObjectsNC how has first parameter 
               'isgpdelt', which is true for groupoid elements and, 
               when true, elements have GroupoidElementsFamily. 
               This allows special methods for elements of groupoids. 
 * (08/09/10) Deleted chunks of code into file `oldgpd.g{d,i}` :- 
               IsGroupoid, IsGroupoidElementRep, GroupoidElementNC. 
               Added ViewObj method for homs with more than one piece. 
               Added IsInjectiveWithObjects, IsSurjectiveWithObjects 
               methods for homs with more than one piece. 
 * (07/05/10) Added example of homomorphism S123 -> T123
 * (06/05/10) Reversed the pair in PieceImages: now  [ hom, imobs ]
 * (05/05/10) Added \^(e,p) for element in MonoidWithObjects and an Int  
 * (05/05/10) Changed GeneratorsOfMagmaWithObjects from Attribute to Operation 
               returning Attribute GeneratorsOfMagma(WithOne/Inverses). 
 * (05/05/10) Added sections on SemigroupsWithObjects and MonoidsWithObjects 
               to the manual. 
 * (30/04/10) Introduced MultiplicativeElementWithObjectsFamily 
               so now 'FamilyObj(e)!.mwo' fails to work. 
               Decided to try to do without saving the mwo with the element. 
               (For now avoid separate family for each groupoid.) 
               Also renamed IsMultiplicativeElementWithObjectsAndInverses 
               as IsGroupoidElement, and added the family of these. 
               Also added GroupoidFamily, GroupoidElementFamily. 
 * (30/04/10) IsGroupoidHomomorphism renamed IsGroupWithObjectsHomomorphism 
               and introduced new category IsGroupoidHomomorphism with 
               corresponding collection and family constructions. 
 * (26/04/10) Testing a trivial version of AutomorphismGroup( gpd ); 
 * (23/04/10) In mwo.gd added MagmaWithObjectsFamily, and used this family 
               in SinglePieceMagmaWithObjects when applying Objectify, 
               so that now IsMagmaWithObjectsCollection( [M78] ); is true! 
               Ditto GeneralMappingWithObjectsFamily in `mwohom.g{d,i}`. 
               IsGeneratorsOfMagmaWithInverses in `gpdaut.gi` now works. 
               ?? Can now get rid of all the .eltsfam's ?? probably not! 
 * (20/04/10) Copied method for IsGeneratorsOfMagmaWithInverses 
               from grpmat.gi, but does not work at present. 
 * (16/04/10) Started files  gpdaut.gd  and  gpdaut.gi  and introduced 
               IsAutomorphismGroupOfGroupoid, GroupoidActedUpon (??) 
 * (15/04/10) Renamed SinglePieceGroupoidWithRays as SubgroupoidWithRays 
 * (26/03/10) Moved Gpd development to IMac at home, and started v.1.07

## 1.04 -> 1.05  (21/11/2008)
 * (21/11/08) Introduced TypeOfDomainWithObjects.  
               Completed the change of parameter order to "<mag>,<obs>",
 * (19/11/08) GapDoc relegated to "suggested other packages".
 * (18/11/08) another massive change of function names (and filenames) 
               with Mapping -> Homomorphism (and ***map.* -> ***hom.*), 
 * (17/11/08) IsMappingSinglePieceSinglePiece -> IsMappingFromSinglePiece, 
               MagmaMappingWithCommonRange -> MappingToSinglePiece, 
               MappingOfSinglePieces -> MappingFromSinglePiece, 
               MappingFromSinglePiece -> MagmaMappingFromSinglePiece, 
               MagmaMappingByPieces -> MappingByUnion, 
               IsSinglePiece now (IsMagmaWithObjects and IsSinglePieceDomain), 
               and similarly for IsDiscrete 
               and IsDirectProductWithCompleteGraph, 
               added global function GroupoidMapping, 
 * (14/11/08) added InverseGeneralMapping method for 
                 IsGroupoidMapping and IsMappingSinglePieceSinglrPiece,
               omitted `morgraph.g{d,i}` and reinstated grpgraph, gpdgraph : 
               needed to replace \in by IsElementInGroupoid many times; 
               still a problem with \* for GraphOfGroupoidsWords

## 1.03 -> 1.04  (13/11/2008)
problem: need to redefine PieceImages in light of DefaultGroupoidMappingRep ? 
         need elements for this new submagma representation 

 * (13/11/08) deleted lots of examples which do not work at present 
               added GNU General Public License declaration,
               moved some XMod utilities to `gpd/gap/util.g{d,i},
 * (07/11/08) started chapters in the manual for magmas with objects; 
               got rid of the PosRep representation (not being used); 
               redefined IsSemigroupMappingWithObjects, 
               IsMonoidMappingWithObjects and IsGroupoidMapping; 
               added property IsConstantOnObjects for mappings 
               (this just returns true at the moment!)
 * (14/10/08) added new methods for MultiplicativeElementWithObjects; 
               added property IsMappingSinglePieceSinglePiece; 
 * (13/10/08) started to construct submagmas with objects using 
               a generating array of lists of elements:- 
               added representation IsSubmagmaWithObjectsTableRep, 
               and operations IsSubmagmaWithObjectsGeneratingTable, 
               SubmagmaWithObjectsElementsTable, 
               and SubmagmaWithObjectsByElementsTable; 
 * (10/10/08) Functions coming from preprint 08.05 :- 
               : IsDefaultGroupoidMappingRep, 
               : GroupoidMappingOfSinglePieces, 
               : MappingPermObjectsImages, 
               : GroupoidMappingByObjectImages, 
               : GroupoidMappingByGroupHom, 
               : GroupoidMappingByRayProds, 
 * (10/10/08) should IeEndomorphismWithObjects be IsEndoGeneralMapping ?? 
               introduced IsDefaultGroupoidMappingRep; 
               added attribute ImagesOfRays; 
               added operation MappingPermObjectsImages; 
               renamed MappingOfSinglePiecesNew 
               as MappingOfSinglePiecesGeneratorsImages  -  is it needed?
 * (09/10/08) found there is a problem in LeftCosetsNC  !! 
               converted "Morphism" to "Mapping" throughout: 
               this includes replacing "mor" by "map" in filenames;  
               replaced "IsMagmaMappingWithObjects" 
                   with "IsSemigroupMappingWithObjects"; 
               found lots of "Constituents" in the library, 
               so changed "Constituents" to "Pieces"; 
 * (06/10/08) added Display for MagmaWithObjects; 
               converted Iterator from Groupoid to MagmaWithObjects; 
               problems with Group(mor1a,mor1b) in mwomor.g
 * (03/10/08) added SubgroupoidBySubgroup; 
               rewrote attribute GeneratorsOfMagmaWithObjects 
               called by operation GeneratorsOfGroupoid; 
               removed test for U to be wide in G for (Right/Left)Coset, 
               and at the same time introduced attribute SuperDomain; 
               wondered whether to introduce category IsPermWithObjects? 
 * (25/09/08) new method for FullSubgroupoid; 
               changed IsSubgroupoid to an Operation from GlobalFunction 
               and split into a collection of methods; 
               fixed bug in \= for groupoids;  
 * (24/09/08) new category IsDomainWithObjects >= IsMagmaWithObjects 
               and global function DomainWithObjects 
               ApplyToSingleObject -> DomainWithSingleObject 
               ObjectMagma(s) -> ObjectDomain(s) . . . needs to be checked? 
               SubmagmaWithObjects -> SubdomainWithObjects, etc. 
 * (18/09/08) New function HomsetCosetsGroupoidCoset 
               used for RightCoset( sgpd, e ) and LeftCoset( sgpd, e )
               but more work needed when there are several constituents. 
 * (12/09/08) SingleConstituentGroupoidWithRays now a subgroupoid of an 
               IsDirectProductWithCompleteGraph and with the same objects. 
               RootGroup is now stored as .magma
               HomsetCosetsRep now has a sixth parameter - the rays. 
               ObjectStar, ObjectCostar, Homset all edited , 
               UnionHomsets removed as not needed. 
 * (03/09/08) SetParent for Submagma : Magma now AttributeStoring
 * (01/09/08) Converted old IsSubgroupoid to IsSubmagmaWithObjects etc. 
 * (04/04/08) Replaced GpdBuildManual() by file makedocrel.g
 * (20/03/08) Functions renamed as follows:
     : ObjectsOfGroupoid -> ObjectList
     : ConstituentsOfGroupoid -> Constituents
     : GroupoidByUnion(NC) -> UnionOfConstituents(NC)
     : GroupAsGroupoid -> ApplyToSingleObject 
     : IsGroupoidRep -> IsConstituentsRep 
     : IsStandardConnectedGroupoid -> IsDirectProductWithCompleteGraph
     : IsConnectedGroupoidRep -> IsGroupoidWithObjectsCompRep 
     : IsConnectedGroupoidWithRaysRep -> IsSingleConstituentRaysRep
     : ConnectedGroupoid(NC) -> SingleConstituentGroupoid 
     : ConnectedGroupoidWithRays(NC) -> SingleConstituentGroupoidWithRays(NC) 
     : ConnectedGroupoidByGenerators -> SingleConstituentGroupoidByGenerators 
     : GroupElement -> Arrow 
     : Tail -> Arrowtail 
     : Head -> Arrowhead
 * (14/03/08) Started new files: `mathcat.gd` and `mathcat.gi`
               initially with small bits taken from: `gpd.gd` and `gpd.gi`. 
               Tried to add some digraph functions there . . . 
               but these should be in files: `digraph.gd` and `digraph.gi`.
 * (13/03/08) Since 'component' is used in ComponentObjectRep, etc., 
               changed ComponentsOf... to ConstituentsOf... 
 * (11/03/08) MathCat now a Category, and not a Property 
 * (11/03/08) Revised: IsPermGroupoid, IsFpGroupoid, IsPcGroupoid 
 * (11/03/08) Revised: PrintObj and ViewObj 
 * (11/03/08) Replaced test for IsPcGroupoid with IsPolycyclicGroup(g) 
 * (06/03/08) Need: ConnectedGroupoidByGenerators( gens ) etc.
 * (06/03/08) Method for GeneratorsOfGroupoid in the non-connected case. 
 * (05/03/08) Expanded conjugation operator, e1^e2, following preprint 07.10 

## 1.01 -> 1.03  (08/10/2007)
 *  Started this `CHANGES` file. 
 *  Now using GAPDoc-1.0.
 *  Fixed some bugs in the example files. 
 *  New versions of MorphismToConnectedGroupoid, MorphismOfConnectedGroupoids
      have been started, but are not called and require further revision. 
 *  Created a test file:  `gpd_manual.tst`  called by  `testall.g` 
 *  Started to reintroduce connected groupoids with variable object groups: 
      added: IsGroupoidWithConstantGroup, ConnectedGroupoidWithRays(NC). 
      Note: many of the other functions do not yet work with these groupoids. 

# HISTORY up to version 1.01
 * 07/05/97  package `GraphGpd` started: tree and connected groupoids  
 * 12/12/00  version 1.001 published in Emma Moore's thesis
 * 30/01/04  version 1.002 prepared for GAP 4.4
 * 05/04/06  version 1.003 now has documentation in GAPDoc format,
             some names changed to avoid conflict with other packages.
 * 05/04/06  package extensively rewritten and renamed 'Gpd':
             version 1.01 submited as a deposited package
