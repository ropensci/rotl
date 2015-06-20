context("test tree_to_labels")

test_that("basic tree 1", {
    tree1 <- "((raccon:19.19959,bear:6.80041)InnerNode1:0.84600,((sea_lion:11.99700,seal:12.00300)InnerNode2:7.52973,((monkey:100.85930,cat:47.14069):20.59201,weasel:18.87953):2.09460):3.87382,dog:25.46154);"
    res_tree1 <- tree_to_labels(tree1)
    expect_equal(res_tree1$tip_label, c("raccon", "bear", "sea_lion", "seal", "monkey", "cat", "weasel", "dog"))
    expect_equal(res_tree1$edge_label, c("InnerNode1", "InnerNode2"))
})

test_that("basic tree 2", {
    tree2 <- "(Bovine:0.69395,(Gibbon:0.36079,(Orang:0.33636,(Gorilla:0.17147,(Chimp:0.19268, Human:0.11927):0.08386):0.06124):0.15057):0.54939,Mouse:1.21460):0.10;"
    res_tree2 <- tree_to_labels(tree2)
    expect_equal(res_tree2$tip_label, c("Bovine", "Gibbon", "Orang", "Gorilla", "Chimp", "Human", "Mouse"))
    expect_equal(res_tree2$edge_label, character(0))
})

test_that("basic tree 3", {
    tree3 <- "(Bovine:0.69395,(Hylobates:0.36079,(Pongo:0.33636,(G._Gorilla:0.17147, (P._paniscus:0.19268,H._sapiens:0.11927):0.08386):0.06124):0.15057):0.54939, Rodent:1.21460);"
    res_tree3 <- tree_to_labels(tree3)
    expect_equal(res_tree3$tip_label, c("Bovine", "Hylobates", "Pongo", "G._Gorilla", "P._paniscus", "H._sapiens", "Rodent"))
    expect_equal(res_tree3$edge_label, character(0))
})

test_that("only 1 tip", {
    tree_tip <- "A;"
    res_tree_tip <- tree_to_labels(tree_tip)
    expect_equal(res_tree_tip$tip_label, "A")
    expect_equal(res_tree_tip$edge_label, character(0))
})

test_that("only 1 tip with parentheses", {
    tree_tip <- "(A);"
    res_tree_tip <- tree_to_labels(tree_tip)
    expect_equal(res_tree_tip$tip_label, "A")
    expect_equal(res_tree_tip$edge_label, character(0))
})

test_that("only 1 tip and 1 internal", {
    tree_tip <- "(A)B;"
    res_tree_tip <- tree_to_labels(tree_tip)
    expect_equal(res_tree_tip$tip_label, "A")
    expect_equal(res_tree_tip$edge_label, "B")
})


test_that("tree with singletons", {
    tree_sing <- "(((((A)cats,B)dogs,(C,D)ducks)frogs)animals,E)fungi;"
    res_tree_sing <- tree_to_labels(tree_sing)
    expect_equal(res_tree_sing$tip_label, LETTERS[1:5])
    expect_equal(res_tree_sing$edge_label, c("cats", "dogs", "ducks", "frogs", "animals", "fungi"))
})
