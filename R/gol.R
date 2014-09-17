## *** NOTE: the development server has a newer tree graph DB (JWB) ***
otl_url <- function() { "http://devapi.opentreeoflife.org" }


##' @title Information about the graph of life
##' @description Basic information about the graph
##' @details Returns summary information about the entire graph database, including identifiers for the taxonomy and source trees used to build it.
##' @return A list of graph attributes:
##' \itemize{
##'	\item {graph_num_source_trees} {The number of unique source trees in the graph.}
##'	\item {graph_taxonomy_version} {The version of the taxonomy used to initialize the graph.}
##'	\item {graph_num_tips} {The number of terminal (tip) taxa in the graph.}
##'	\item {graph_root_name} {The taxonomic name of the root node of the graph.}
##'	\item {graph_root_node_id} {The node ID of the root node of the graph.}
##'	\item {graph_root_ott_id} {The OpenTree Taxonomy ID (ottID) of the root node of the graph.}
##' }
##' @examples
##' res <- gol_about()
##' @export
gol_about <- function() {
    res <- otl_POST(path="graph/about", body=list())
    cont <- httr::content(res)
    if (length(cont) < 1) {
        warning("Nothing returned")
    }
    return(cont)
}


##' @title Get reconstructed source tree
##' @description Returns a reconstructed source tree from the graph DB.
##' @details Reconstructs a source tree given identifiers: \code{study_id}, \code{tree_id}, and \code{git_sha}. The tree may differ from the original source tree in 2 ways: 1) it may contain fewer taxa (as duplicate taxa are pruned on tree ingestion), and 2) OpenTree Taxonomy IDs (ottIDs) are applied to all named internal nodes and as a suffix to all terminal node names.
##' @param study_id String. The study identifier. Will typically include a prefix ("pg_" or "ot_").
##' @param tree_id String. The tree identifier for a given study.
##' @param git_sha String. The git SHA identifying a particular source version.
##' @param format The name of the return format. The only currently supported format is newick.
##' @return a tree of class \code{"synth_sources"}
##' @examples
##' res <- gol_source_tree(study_id="ot_121", git_sha="a2c48df995ddc9fd208986c3d4225112550c8452", tree_id="7")
##' @export
gol_source_tree <- function(study_id=NULL, tree_id=NULL, git_sha=NULL) {
    if (any(is.null(c(study_id, tree_id, git_sha)))) {
    	    stop("Must supply all arguments: \'study_id\', \'tree_id\', \'git_sha\'")
    }
    q <- list(study_id=jsonlite::unbox(study_id), tree_id=jsonlite::unbox(tree_id), 
        git_sha=jsonlite::unbox(git_sha))
    
    res <- otl_POST(path="graph/source_tree", body=q)
    cont <- httr::content(res)
    phy <- collapse.singles(phytools::read.newick(text=(cont)[["newick"]])); # required b/c of "knuckles"
    return(phy)
}


##' @title Node info
##' @description Get summary information about a node in the graph
##' @details Summary information about a queried node, including 1) whether it is in the graph DB,
##' 2) whether it is in the synthetic tree, 3) supporting study sources, 4) number of 
##' descendant tip taxa, 5) graph node ID, and 6) taxonomic information (if it is a named
##' node in the graph), including: name, rank, OpenTree Taxonomy ID (ottID), and source taxonomy
##' IDs.
##' @param ott_id The OpenTree taxonomic identifier.
##' @param node_id The idenitifer of the node in the graph.
##' @param include_lineage Boolean. Whether to return the lineage of the node from the synthetic tree. Optional; default = FALSE.
##' @return A list of summary information about the queried node.
##' \itemize{
##'	\item {in_graph} {Boolean. Whether the queried node is present in the graph.}
##'	\item {node_id} {Numeric. The node ID of the queried node in the graph.}
##'	\item {in_synth_tree} {Boolean. Whether the queried node is present in the synthetic tree.}
##'	\item {rank} {String. The taxonomic rank of the queried node (if it is a named node).}
##'	\item {name} {String. The taxonomic name of the queried node (if it is a named node).}
##'	\item {ott_id} {Numeric. The OpenTree Taxonomy ID (ottID) of the queried node (if it is a named node).}
##'	\item {num_tips} {Numeric. The number of taxonomic tip descendants.}
##'	\item {num_synth_children} {Numeric . The number of synthetic tree tip descendants.}
##'	\item {tax_source} {String. Source taxonomy IDs (if it is a named node), e.g. "ncbi:9242,gbif:5289,irmng:104628".}
##'	\item {synth_sources} {A list of supporting synthesis source trees, each with:}
##' \itemize{
##' \item {study_id} {The study identifier. Will typically include a prefix ("pg_" or "ot_").}
##' \item {tree_id} {The tree identifier for a given study.}
##' \item {git_sha} {The git SHA identifying a particular source version.}
##' }
##'	\item {tree_sources} {A list of supporting source trees in the graph. May differ from \code{"synth_sources"}, if trees are in the graph, but were not used in constructing the synthetic tree. Each source has:}
##' \itemize{
##' \item {study_id} {The study identifier. Will typically include a prefix ("pg_" or "ot_").}
##' \item {tree_id} {The tree identifier for a given study.}
##' \item {git_sha} {The git SHA identifying a particular source version.}
##' }
##' }
##' @examples
##' res <- gol_node_info(ott_id=81461)
##' @export
gol_node_info <- function(node_id=NULL, ott_id=NULL, include_lineage=FALSE) {
    if (!is.null(node_id) && !is.null(ott_id)) {
        stop("Use only \'node_id\' OR \'ott_id\'")
    }
    if (is.null(node_id) && is.null(ott_id)) {
        stop("Must supply a \'node_id\' OR \'ott_id\'")
    }
    if (!is.logical(include_lineage)) {
		stop("Argument \'include_lineage\' should be logical")
	}
    if (!is.null(ott_id)) {
        q <- list(ott_id=jsonlite::unbox(ott_id), include_lineage=jsonlite::unbox(include_lineage))
    } else {
    	    q <- list(node_id=jsonlite::unbox(node_id), include_lineage=jsonlite::unbox(include_lineage))
    }
    res <- otl_POST(path="graph/node_info", body=q)
    cont <- httr::content(res)
    return(cont)
}



## Example that APE cannot handle:
# study_id="pg_420"; tree_id="522"; git_sha="a2c48df995ddc9fd208986c3d4225112550c8452"\
# "((((Tinamiformes_292467:1.0E-22,((Apteryx_241840:1.0E-22)Apterygiformes_816668:0.03818,((Dromaius_283193:1.0E-22)Dromaiidae_283194:0.01843,(Casuarius_589156:1.0E-22)Casuariidae_589161:0.014445)Casuariiformes_589166:0.030882):0.00232,Tinamidae_292469:0.091284,(((Crypturellus_870604:0.061151,Tinamus_402450:0.042862):0.01736,(Eudromia_292460:0.084637,Nothoprocta_292463:0.080725):0.00491,(Crypturellus_870604:0.061151,Tinamus_402450:0.042862):0.01736,(Eudromia_292460:0.084637,Nothoprocta_292463:0.080725):0.00491)Tinamidae_292469:1.0E-22,((Crypturellus_870604:0.061151,Tinamus_402450:0.042862):0.01736,(Eudromia_292460:0.084637,Nothoprocta_292463:0.080725):0.00491)Tinamidae_292469:1.0E-22)Tinamiformes_292467:1.0E-22):0.001643,(Rhea_857863:1.0E-22)Rheiformes_829553:0.08311):0.011185,(Struthio_292466:1.0E-22)Struthioniformes_857847:0.068069)Palaeognathae_81443:0.055986,(((((Crax_979429:1.0E-22)Cracidae_109893:0.090261,(((Colinus_204725:1.0E-22)Odontophoridae_594197:0.099855,((Coturnix_1098759:0.066402,(Gallus_153562:1.0E-22)Phasianinae_51353:0.03635):0.010595,Rollulus_352754:0.068711)Phasianidae_728070:0.014614):0.008216,(Numida_684050:1.0E-22)Numididae_684043:0.044719):0.06343):0.013369,(Alectura_570959:0.03048,Megapodius_837567:0.029367)Megapodiidae_620981:0.069087)Galliformes_837585:0.032204,(((Oxyura_88395:0.040265,((Malacorhynchus_436831:0.04328,Anser_190884:0.022753):0.003531,((Aythya_693334:0.011903,Anas_765185:0.012596):0.023881,Biziura_432080:0.034803):0.003654):0.002077)Anatidae_765193:0.054998,(Anseranas_714466:1.0E-22)Anseranatidae_732899:0.048445):0.008696,(Chauna_241847:1.0E-22)Anhimidae_241842:0.057828)Anseriformes_241841:0.008393):0.044868,(((((((Turnix_365489:1.0E-22)Turniciformes_810755:0.18873,((Larus_887695:1.0E-22)Laridae_887693:0.032647,(Dromas_960243:1.0E-22)Dromadidae_960242:0.025419):0.015026):0.00881,((((Thinocorus_628492:1.0E-22)Thinocoridae_628493:0.053497,(Pedionomus_163950:1.0E-22)Pedionomidae_163951:0.04395):0.016681,((Jacana_332001:1.0E-22)Jacanidae_331999:0.05956,(Rostratula_5275:1.0E-22)Rostratulidae_5274:0.055239):0.009395):0.01752,(Arenaria_821756:1.0E-22)Scolopacidae_887699:0.046052):0.024302):0.009831,((Burhinus_261316:1.0E-22)Burhinidae_261310:0.045568,((Haematopus_193407:1.0E-22)Haematopodidae_675126:0.032531,(Charadrius_112946:0.02814,Phegornis_214795:0.026739)Charadriidae_313123:0.014782):0.014788):0.003951):0.01301,(((Cariama_966327:1.0E-22)Cariamidae_966325:0.064557,(((Herpetotheres_438656:0.013273,Micrastur_1015202:0.015684):0.017691,(Daptrius_438662:0.025422,Falco_786441:0.042045):0.020204)Falconidae_212186:0.033545,((Psittacidae_1020130:1.0E-22,(:1.0E-22,(Psittacus_332937:0.044181,(((Alisterus_682886:0.025857,Psittacula_1020126:0.02314):0.002518,Micropsitta_989086:0.035173):0.003373,(Chalcopsitta_276608:0.035748,Platycercus_512910:0.031536):0.003623):0.005708,Psittacus_332937:0.044181,(((Alisterus_682886:0.025857,Psittacula_1020126:0.02314):0.002518,Micropsitta_989086:0.035173):0.003373,(Chalcopsitta_276608:0.035748,Platycercus_512910:0.031536):0.003623):0.005708):1.0E-22,:1.0E-22,(Psittacus_332937:0.044181,(((Alisterus_682886:0.025857,Psittacula_1020126:0.02314):0.002518,Micropsitta_989086:0.035173):0.003373,(Chalcopsitta_276608:0.035748,Platycercus_512910:0.031536):0.003623):0.005708,Psittacus_332937:0.044181,(((Alisterus_682886:0.025857,Psittacula_1020126:0.02314):0.002518,Micropsitta_989086:0.035173):0.003373,(Chalcopsitta_276608:0.035748,Platycercus_512910:0.031536):0.003623):0.005708):1.0E-22)Psittacidae_1020130:1.0E-22,(Cacatua_619340:1.0E-22)Cacatuidae_512919:0.03402,(Psittacus_332937:0.044181,(((Alisterus_682886:0.025857,Psittacula_1020126:0.02314):0.002518,Micropsitta_989086:0.035173):0.003373,(Chalcopsitta_276608:0.035748,Platycercus_512910:0.031536):0.003623):0.005708):0.007079)Psittaciformes_1020133:0.080722,((Acanthisitta_1085741:1.0E-22)Acanthisittidae_901940:0.104596,(((Menura_73933:1.0E-22)Menuridae_73954:0.059299,((Climacteris_531218:1.0E-22)Climacteridae_73972:0.077503,((Malurus_901831:1.0E-22)Maluridae_901832:0.083712,((Corvus_952596:1.0E-22)Corvoidea_635217:0.044225,((Picathartes_699624:1.0E-22)Picathartidae_176465:0.053895,(((Bombycilla_613883:1.0E-22)Bombycillidae_613878:0.069549,((Sylvia_463177:1.0E-22)Sylviidae_259942:0.055851,(Turdus_568571:1.0E-22)Turdidae_96286:0.063024):0.00352):3.93E-4,(((Fringilla_28336:1.0E-22)Fringillidae_839319:0.036294,(Passer_515158:1.0E-22)Passeridae_1011209:0.040818):0.008852,((Ploceus_370815:1.0E-22)Ploceidae_1031977:0.023436,(Vidua_507121:1.0E-22)Estrildidae_507124:0.027999):0.005305)Passeroidea_176458:0.013079):0.009812):0.006319):0.009903):0.012328):0.006066):0.026149,((((Smithornis_622872:1.0E-22)Eurylaimidae_622873:0.054061,(Pitta_44875:1.0E-22)Pittidae_44873:0.07916):0.002972,Sapayoa_29742:0.064324):0.032117,(((Tyrannus_463185:0.042419,Mionectes_683046:0.038317):0.011208,(Pipra_872124:1.0E-22)Pipridae_881043:0.037734):0.028602,((Thamnophilus_799167:1.0E-22)Thamnophilidae_799152:0.061805,((Grallaria_1041027:1.0E-22)Formicariidae_472037:0.058324,((Scytalopus_44860:1.0E-22)Rhinocryptidae_44868:0.055361,(Dendrocolaptes_390841:1.0E-22)Dendrocolaptidae_155232:0.055765):0.004101):0.004199):0.014647):0.013458):0.014098):0.011135)Passeriformes_1041547:0.05194):0.003214):0.001248):0.002413,((((Leptosomus_897862:1.0E-22)Leptosomidae_897861:0.08109,(Trogoniformes_539130:1.0E-22,((Trogon_1065611:0.03286,Pharomachrus_989089:0.03198):1.0E-22,(Trogon_1065611:0.03286,Pharomachrus_989089:0.03198):1.0E-22)Trogoniformes_539130:1.0E-22,((((((Dryocopus_733986:1.0E-22)Picidae_1020138:0.045771,(Indicator_467838:1.0E-22)Indicatoridae_467845:0.040802):0.019814,((Capito_815707:1.0E-22)Ramphastidae_489463:0.061571,(Megalaima_291638:1.0E-22)Megalaimidae_291635:0.070621):0.028918)Piciformes_472432:0.080422,((Bucco_483803:1.0E-22)Bucconidae_483802:0.097348,(Galbula_484896:1.0E-22)Galbulidae_484892:0.093829)Galbuliformes_484893:0.02902):0.012788,(((((Momotus_989084:1.0E-22)Momotidae_489432:0.084333,(Alcedo_549518:1.0E-22)Alcedinidae_938411:0.120941):0.009393,(Todus_284297:1.0E-22)Todidae_815966:0.096383):0.014113,((Brachypteracias_483804:1.0E-22)Brachypteraciidae_483798:0.059926,(Coracias_244695:1.0E-22)Coraciidae_244696:0.059306):0.025887):0.00185,(Merops_989088:1.0E-22)Meropidae_815968:0.113525):0.012257):0.004011,(((Phoeniculus_834690:1.0E-22)Phoeniculidae_834689:0.098169,(Upupa_412128:1.0E-22)Upupidae_291860:0.099034)Upupiformes_815967:0.100031,((Tockus_1097399:1.0E-22)Bucerotidae_489457:0.067023,(Bucorvus_991312:1.0E-22)Bucorvidae_991314:0.0305)Bucerotiformes_341907:0.062953):0.015769):0.006487,(Trogon_1065611:0.03286,Pharomachrus_989089:0.03198):0.086987):0.00136):0.003193,(((Strix_427905:0.019555,Athene_98071:0.04265):0.038003,(Tyto_1065605:0.02467,Phodilus_178684:0.021908)Tytonidae_402457:0.038841)Strigiformes_1028829:0.011322,(Urocolius_261312:1.0E-22)Coliiformes_815970:0.148957):0.001582):0.001889,((Cathartes_317003:0.008966,Sarcoramphus_819164:0.007934)Cathartidae_363021:0.028138,(((Buteo_119211:0.026466,Gampsonyx_767825:0.047509)Accipitrinae_786440:0.006245,(Pandion_509844:1.0E-22)Pandioninae_509843:0.039038)Accipitridae_1036185:0.009904,(Sagittarius_1036187:1.0E-22)Sagittariidae_1036188:0.048561):0.011531):0.001418):0.001111):0.005212):0.002734,((((Eupodotis_521835:0.024254,Choriotis_3600037:0.022179)Otididae_966318:0.063626,(((((Aramus_915651:1.0E-22)Aramidae_915649:0.034399,(Grus_414354:1.0E-22)Gruidae_446460:0.027902):0.016386,(Psophia_915642:1.0E-22)Psophiidae_915652:0.06858):0.007701,((Himantornis_383930:0.036588,Rallus_440612:0.035633):0.045303,(Sarothrura_399793:0.098432,(Heliornis_440620:1.0E-22)Heliornithidae_440606:0.081266):0.013724):0.041851):0.020906,(((Coua_787071:0.072867,(Centropus_1039514:1.0E-22)Centropidae_1040900:0.105747):0.00457,(Cuculus_1041429:0.052701,(Phaenicophaeus_1041421:0.024286,(Coccyzus_891428:1.0E-22)Coccyzidae_1041414:0.038539):0.034617):0.028485):0.010289,((Crotophaga_1041422:1.0E-22)Crotophagidae_1041423:0.06579,(Geococcyx_212185:1.0E-22)Neomorphidae_173050:0.075601):0.015668)Cuculiformes_212171:0.047155):0.001564):0.002641,(Musophagiformes_539139:1.0E-22,((Tauraco_539140:0.045392,Corythaeola_842352:0.040341)Musophagidae_539141:1.0E-22,(Tauraco_539140:0.045392,Corythaeola_842352:0.040341)Musophagidae_539141:1.0E-22)Musophagiformes_539139:1.0E-22,(Tauraco_539140:0.045392,Corythaeola_842352:0.040341)Musophagidae_539141:0.040747,((Gavia_803675:1.0E-22)Gaviiformes_70684:0.049699,((((((Eudocimus_689774:1.0E-22)Threskiornithidae_480157:0.053985,(Cochlearius_154816:0.025018,Ardea_860130:0.029045)Ardeidae_609781:0.032668):0.001574,(((Scopus_464703:1.0E-22)Scopidae_1057476:0.05511,(Balaeniceps_597797:1.0E-22)Balaenicipitidae_597796:0.033735):0.001343,(Pelecanus_316994:1.0E-22)Pelecanidae_452465:0.045666):0.010306):0.001878,((Fregata_108973:1.0E-22)Fregatidae_452471:0.036293,(((Phalacrocorax_322277:1.0E-22)Phalacrocoracidae_969838:0.051194,(Anhinga_443638:1.0E-22)Anhingidae_443646:0.039714):0.004943,(Morus_752683:1.0E-22)Sulidae_452462:0.031659):0.017862):0.005898):0.00116,(Ciconia_363012:1.0E-22)Ciconiidae_363013:0.043283):0.001253,((((Diomedea_379419:1.0E-22)Diomedeidae_85277:0.026207,(Oceanodroma_172642:0.032772,((Pelecanoides_904488:1.0E-22)Pelecanoididae_904487:0.024161,(Puffinus_1028843:1.0E-22)Procellariidae_1028841:0.015536):0.015313):0.002986):8.24E-4,Oceanites_656590:0.045177)Procellariiformes_452461:0.004445,(Eudyptula_388853:1.0E-22)Sphenisciformes_494366:0.044753):0.002742):0.001953):0.0014):0.001505):0.002031,(Opisthocomus_70726:1.0E-22)Opisthocomiformes_928359:0.089041):0.002382):0.001494,((((((Podargus_313122:1.0E-22)Podargidae_313130:0.031374,(Batrachostomus_275129:1.0E-22)Batrachostomatidae_275130:0.038779):0.051052,(((((Colibri_804122:0.036348,Phaethornis_174369:0.039041)Trochilidae_810751:1.0E-22)Trochiliformes_810738:1.0E-22,(Colibri_804122:0.036348,Phaethornis_174369:0.039041)Trochilidae_810751:0.086258,((Hemiprocne_697438:1.0E-22)Hemiprocnidae_882744:0.041301,(Aerodramus_534819:0.042417,Streptoprocne_697413:0.026977)Apodidae_609796:0.013721)Apodiformes_609798:0.037704):0.014326,(Aegotheles_540029:1.0E-22)Aegothelidae_540031:0.092918):0.012744,((Eurostopodus_726315:1.0E-22)Eurostopodidae_726316:0.042431,(Caprimulgus_275134:1.0E-22)Caprimulgidae_275127:0.045907):0.033167):0.001994):0.002299,(((Nyctibius_grandis_178249:0.017044,Nyctibius_bracteatus_178250:0.023877)Nyctibius_178253:1.0E-22)Nyctibiidae_178243:1.0E-22,(Nyctibius_grandis_178249:0.017044,Nyctibius_bracteatus_178250:0.023877)Nyctibius_178253:0.040279,(Steatornis_701547:1.0E-22)Steatornithidae_701556:0.075216):0.001377):0.003449,((Eurypyga_86682:1.0E-22)Eurypygidae_86683:0.049583,(Rhynochetos_86676:1.0E-22)Rhynochetidae_86678:0.045071):0.067821):0.002659,((((Phaethon_rubricauda_509057:0.002406,Phaethon_lepturus_855119:0.002726)Phaethon_330010:1.0E-22)Phaethontidae_452467:1.0E-22,(((((Columba_938415:0.033245,Geotrygon_449653:0.032737):0.009269,Treron_873964:0.035234):0.001206,(Otidiphaps_392964:0.023405,Columbina_935135:0.038143):0.002133)Columbiformes_363030:0.078976,(Monias_582942:0.023104,Mesitornis_966321:0.023582)Mesitornithidae_966320:0.083163):0.002749,(Syrrhaptes_880217:0.028131,Pterocles_244698:0.030308)Pteroclidae_244699:0.070212):0.00144,(Phaethon_rubricauda_509057:0.002406,Phaethon_lepturus_855119:0.002726)Phaethon_330010:0.075196):0.001028,((Phoenicopterus_443649:1.0E-22)Phoenicopteriformes_472435:0.036539,(Podiceps_329993:1.0E-22)Podicipediformes_452464:0.070732):0.019648):0.001701):0.002134):0.044959)Neognathae_241846:0.066177)Aves_81461;"

