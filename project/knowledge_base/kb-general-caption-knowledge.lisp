; Aug 15/12
;
; GENERAL KNOWLEDGE BASE FOR INITIAL CAPTION PROCESSING
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; The antecedents of the rules herein mostly depend on prior knowledge
; of gender (male.n or female.n) and relationships such as x being
; the grandmother (or friend, unle, son, etc.) of y. This knowledge
; gets added dynamically based on simple tests on constants or TTT 
; rules -- e.g. grandma-lillian.name refers to someone's grandmother, 
; and could also be referred to as lillian.name or, after Skolemization,
; something like grandma3.sk; and someone referred to as Mrs. 
; so-and-so is female and a young(ish) adult or middle-aged or 
; senior. ** We really also need caption-specific knowledge, over
; and above features/attributes, such as that certain individuals
; appear left to right or adjacent to each other in the image.
; Such information should eventually be supplied directly by the
; image processing.)
;
; The axioms here derive predications involving 'gray-haired.a', 
; 'mustachioed.a', 'mustacheless.a', 'racially-east-indian.a',
; etc., in accord with the choice of mappings from features/attributes
; to EPILOG predicates decided on for the phase2-q2 report. It would
; be desirable later to switch back to compositional ways of writing
; complex predicates, as in the "plan B" project and report, e.g.,
; [(hair-of.f X) gray.a], or, as a predicate (:l x [(hair-of.f X) gray.a]),
; or perhaps (have.v ((attr gray.a) hair.n)). This will link up more
; directly with linguistic (caption) input.
;
; As well, this version of the general KB shies away from modal
; formulations such as that the caption "says" such-and-such, or
; presupposes such-and-such, where we then make tentative truth
; inferences about the things said or presupposed.
;
; N.B., an important inferential limitation: As long as probabilities
;   can't be attached "externally" to propositions, but only through
;   explicit "with-certainty" modifiers, we need to allow arbitrary
;   "with-certainty" modifiers in *antecedents* of axioms that we expect
;   to use for inference chaining, i.e., with inferred certainty-modified
;   formulas feeding into their antecedents!

(in-package :epilog)
(store (x-is-predicate 'with-certainty) *lexicon-kb*)
(store (x-is-pred-mod 'classified-as) *lexicon-kb*)
(store (x-is-function 'times) *lexicon-kb*)
(store (x-is-function 'set-of) *lexicon-kb*); LFs for 'NP and NP' involve this
; Note: inferred certainties will be arithmetic expressions involving
;       'times' and (eventually) other arithmetic functions, so we'll
;       need to apply a Lisp evaluator to them to get the final numeric
;       data.

; Knowledge about degrees of certainty
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; maximum certainty is equivalent to factuality; however, for
; now these axioms need to be suppressed to avoid forward inference
; run-away on simple facts (however, with a forward inference strategy
; that avoids use of any rule more than once on an inference chain,
; the is might be ok):
;   (s '(all_wff w ((adv-s (with-certainty 1)) w) w))
;   (s '(all_wff w w ((adv-s (with-certainty 1)) w)))

; We might at some point try a rule for chaining certainties,
; to the effect that if w holds with certainty x, and ((w and w1) => w2)
; holds with certainty y, then (w1 => w2) holds with certainty x.y;
; and if w1 holds with certainty x and (w1 => w2) holds with certainty 
; y, then w2 holds with certainty x.y:
;   (s '(all_wff w 
;         (all_wff w1
;           (all_wff w2 
;             (all_term x 
;               (all_term y (((adv-s (with-certainty x)) w) and
;                            ((adv-s (with-certainty y))
;                             ((w and w1) => w2)))
;                           ((adv-s (with-certainty (times x y)))
;                            (w1 => w2))))))))
;   (s '(all_wff w1
;         (all_wff w2 
;           (all_term x 
;             (all_term y (((adv-s (with-certainty x)) w1) and
;                          ((adv-s (with-certainty y)) (w1 => w2)))
;                         ((adv-s (with-certainty (times x y)))
;                          w2))))))
; But it would be risky (in terms of computational explosions) to
; encode what should really be uncertain inference mechanisms as
; metaknowledge. So for now we need to use some specific axioms
; with certainty modifiers in the antecedent and consequent.

; Knowledge about gender and personhood
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Every person is a male or a female, but not both
    (s '(all x (x person.n) ((x male.n) or (x female.n))))
    (s '(all x (x male.n) (not (x female.n))))
; Every male and female is a person
    (s '(all x (x male.n) (x person.n)))
    (s '(all x (x female.n) (x person.n)))

; Knowledge about the connection between relational predicates
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; and their monadic form;
; ~~~~~~~~~~~~~~~~~~~~~~
;   (Could this be done with a metarule of some sort? Use ttt?
;   Will forward inference do anything with these? Probably not)
    (s '(all x ((x mother.n) <=> (some y (x mother-of.n y)))))
    (s '(all x ((x father.n) <=> (some y (x father-of.n y)))))
    (s '(all x ((x parent.n) <=> (some y (x parent-of.n y)))))
    (s '(all x ((x daughter.n) <=> (some y (x daughter-of.n y)))))
    (s '(all x ((x son.n) <=> (some y (x son-of.n y)))))
    (s '(all x ((x child.n) <=> (some y (x child-of.n y)))))
    (s '(all x ((x grandmother.n) <=> (some y (x grandmother-of.n y)))))
    (s '(all x ((x grandfather.n) <=> (some y (x grandfather-of.n y)))))
    (s '(all x ((x grandparent.n) <=> (some y (x grandparent-of.n y)))))
    (s '(all x ((x granddaughter.n) <=> (some y (x granddaughter-of.n y)))))
    (s '(all x ((x grandson.n) <=> (some y (x grandson-of.n y)))))
    (s '(all x ((x grandchild.n) <=> (some y (x grandchild-of.n y)))))
    (s '(all x ((x sister.n) <=> (some y (x sister-of.n y)))))
    (s '(all x ((x brother.n) <=> (some y (x brother-of.n y)))))
    (s '(all x ((x sibling.n) <=> (some y (x sibling-of.n y)))))
    (s '(all x ((x cousin.n) <=> (some y (x cousin-of.n y)))))
    (s '(all x ((x uncle.n) <=> (some y (x uncle-of.n y)))))
    (s '(all x ((x aunt.n) <=> (some y (x aunt-of.n y)))))
    (s '(all x ((x niece.n) <=> (some y (x niece-of.n y)))))
    (s '(all x ((x nephew.n) <=> (some y (x nephew-of.n y)))))
    (s '(all x ((x aunt.n) <=> (some y (x aunt-of.n y)))))
    (s '(all x ((x wife.n) <=> (some y (x wife-of.n y)))))
    (s '(all x ((x husband.n) <=> (some y (x husband-of.n y)))))
    (s '(all x ((x bride.n) <=> (some y (x bride-of.n y)))))
    (s '(all x ((x groom.n) <=> (some y (x groom-of.n y)))))
    (s '(all x ((x spouse.n) <=> (some y (x spouse-of.n y)))))
    (s '(all x ((x friend.n) <=> (some y (x friend-of.n y)))))
    (s '(all x ((x neighbor.n) <=> (some y (x neighbor-of.n y)))))
    (s '(all x ((x classmate.n) <=> (some y (x classmate-of.n y)))))
    (s '(all x ((x teammate.n) <=> (some y (x teammate-of.n y)))))
    (s '(all x ((x coach.n) <=> (some y (x coach-of.n y)))))
  
    (s '(all x (all y (x basketball-coach-of.n y) (x coach-of.n y))))
    (s '(all x (all y (x baseball-coach-of.n y) (x coach-of.n y))))
    (s '(all x (all y (x football-coach-of.n y) (x coach-of.n y))))
    (s '(all x (all y (x soccer-coach-of.n y) (x coach-of.n y))))
    (s '(all x (all y (x hockey-coach-of.n y) (x coach-of.n y))))
    (s '(all x (all y (x gymnastics-coach-of.n y) (x coach-of.n y))))
    (s '(all x (all y (x wrestling-coach-of.n y) (x coach-of.n y))))
    (s '(all x (all y (x tennis-coach-of.n y) (x coach-of.n y))))
    (s '(all x (all y (x swimming-coach-of.n y) (x coach-of.n y))))
    (s '(all x (all y (x volleyball-coach-of.n y) (x coach-of.n y))))

; Knowledge about interpersonal relations, gender, and age
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Brides are wives, grooms are husbands, both are spouses, and they
; are female and male respectively:
    (s '(all x (all y (x bride-of.n y) (x wife-of.n y))))
    (s '(all x (all y (x groom-of.n y) (x husband-of.n y))))
    (s '(all x (all y (x wife-of.n y) 
                      ((x female.n) and (x spouse-of.n y)))))
    (s '(all x (all y (x husband-of.n y) 
                      ((x male.n) and (x spouse-of.n y)))))
    (s '(all x (all y (x wife-of.n y) (y husband-of.n x))))
    (s '(all x (all y (x husband-of.n y) (y wife-of.n x))))

; Mothers are female parents; the inverse is 
; daughter/son/child; similarly for fathers
    (s '(all x (all y (x mother-of.n y) (x female.n))))
    (s '(all x (all y (x mother-of.n y) (x parent-of.n y))))
    (s '(all x (all y (x father-of.n y) (x male.n))))
    (s '(all x (all y (x father-of.n y) (x parent-of.n y))))
    
    (s '(all x (all y ((x parent-of.n y) and (x female.n))
                      (x mother-of.n y))))
    (s '(all x (all y ((x parent-of.n y) and (x male.n))
                      (x father-of.n y))))
    (s '(all x (all y ((x parent-of.n y) and (y female.n))
                      (y daughter-of.n x))))
    (s '(all x (all y ((x parent-of.n y) and (y male.n))
                      (y son-of.n x))))
    (s '(all x (all y (x parent-of.n y) (y child-of.n x))))
    (s '(all x (all y (x child-of.n y)
                      ((x daughter-of.n y) or (x son-of.n y)))))

; Grandmothers are female grandparents; the inverse is 
; grand{daughter/son/child}; similarly for grandfathers
; *** Many of these should really be equivalences ...
    (s '(all x (all y (x grandmother-of.n y) (x female.n))))
    (s '(all x (all y (x grandmother-of.n y) (x grandparent-of.n y))))
    (s '(all x (all y (x grandfather-of.n y) (x male.n))))
    (s '(all x (all y (x grandfather-of.n y) (x grandparent-of.n y))))
    
    (s '(all x (all y ((x grandparent-of.n y) and (x female.n))
                      (x grandmother-of.n y))))
    (s '(all x (all y ((x grandparent-of.n y) and (x male.n))
                      (x grandfather-of.n y))))
    (s '(all x (all y ((x grandparent-of.n y) and (y female.n))
                      (y granddaughter-of.n x))))
    (s '(all x (all y ((x grandparent-of.n y) and (y male.n))
                      (y grandson-of.n x))))
    (s '(all x (all y (x grandparent-of.n y) (y grandchild-of.n x))))
    (s '(all x (all y (x grandchild-of.n y)
                      ((x granddaughter-of.n y) or (x grandson-of.n y)))))
; Adults may be young adults, youngish adults, middle-aged, or seniors;
; they are not teens, children, toddlers or babies
; OF DUBIOUS CURRENT VALUE (yielding low probabilities that might
; "accidentally" be favored over more definite knowledge in a proof), 
; SO THEY ARE BLOCKED FOR NOW
;   (s '(all x (x adult.n)
;              ((adv-s (with-certainty .13)) (x young-adult.n))))
;   (s '(all x (x adult.n)
;              ((adv-s (with-certainty .25)) (x youngish-adult.n))))
;   (s '(all x (x adult.n)
;              ((adv-s (with-certainty .29)) (x middle-aged.a))))
;   (s '(all x (x adult.n)
;              ((adv-s (with-certainty .33)) (x senior.n))))
;   (s '(all x (x adult.n) 
;              ((adv-s (with-certainty .8)) (not (x teen.n)))))
;   (s '(all x (x adult.n) (not (x child.n))))
;   (s '(all x (x adult.n) (not (x baby-or-toddler.n))))
    
; Parents are adults
    (s '(all x (all y (x parent-of.n y)
                      ((adv-s (with-certainty .95)) (x adult.n)))))
; Grandparents are usually seniors
    (s '(all x (all y (x grandparent-of.n y)
                      ((adv-s (with-certainty .8)) (x senior.n)))))
; A coach is typically a youngish adult or middle-aged, and someone
; who has a coach is typically a teen or young adult
     (s '(all x (all y (x coach-of.n y)
                   ((adv-s (with-certainty .50)) (x youngish-adult.n)))))
     (s '(all x (all y (x coach-of.n y)
                   ((adv-s (with-certainty .45)) (x middle-aged.a)))))
     (s '(all x (all y (x coach-of.n y)
                   ((adv-s (with-certainty .45)) (y teen.n)))))
     (s '(all x (all y (x coach-of.n y)
                   ((adv-s (with-certainty .45)) (y young-adult.n)))))
; Grandparents may be middle-aged
; OF DUBIOUS CURRENT VALUE, SO BLOCKED FOR NOW (apt to be used, with 
; misleading results,  when the previous one would give the right result)
;   (s '(all x (all y (x grandparent-of.n y)
;                     ((adv-s (with-certainty .15)) (x middle-aged.a)))))
; Grandparents may very occasionally be youngish adults
;   (s '(all x (all y (x grandparent-of.n y)
;                     ((adv-s (with-certainty .05)) (x youngish-adult.n)))))
; Someone with a grandparent is usually in an age group from baby to 
; youngish adult, not a senior and not middle-aged
    (s '(all x (all y (x grandparent-of.n y)
                      ((adv-s (with-certainty .06)) (y baby-or-toddler.n)))))
    (s '(all x (all y (x grandparent-of.n y)
                      ((adv-s (with-certainty .3)) (y child.n)))))
    (s '(all x (all y (x grandparent-of.n y)
                      ((adv-s (with-certainty .2)) (y teen.n)))))
    (s '(all x (all y (x grandparent-of.n y)
                      ((adv-s (with-certainty .2)) (y young-adult.n)))))
    (s '(all x (all y (x grandparent-of.n y)
                      ((adv-s (with-certainty .15)) (y youngish-adult.n)))))
; Negative inferences are OF DUBIOUS CURRENT VALUE, SO BLOCKED FOR NOW 
;   (s '(all x (all y (x grandparent-of.n y)
;                     ((adv-s (with-certainty .5)) (not (y youngish-adult.n))))))
;   (s '(all x (all y (x grandparent-of.n y)
;                     ((adv-s (with-certainty .6)) (not (y middle-aged.a))))))
;   (s '(all x (all y (x grandparent-of.n y)
;                     ((adv-s (with-certainty .9)) (not (y senior.n))))))
; Uncles are adult males; the inverse is niece or nephew [**aunts to be added]
    (s '(all x (all y (x uncle-of.n y) (x male.n))))
    (s '(all x (all y ((x uncle-of.n y) and (y female.n))
                      (y niece-of.n x))))
    (s '(all x (all y ((x uncle-of.n y) and (y male.n))
                      (y nephew-of.n x))))
    (s '(all x (all y (x uncle-of.n y) 
                      ((y niece-of.n x) or (y nephew-of.n x)))))
    (s '(all x (all y (x uncle-of.n y)
                      ((adv-s (with-certainty .95)) (x adult.n)))))
; Sisters are female siblings; brothers are male siblings
    (s '(all x (all y (x sister-of.n y) (x female.n))))
    (s '(all x (all y (x brother-of.n y) (x sibling-of.n y))))
    (s '(all x (all y (x brother-of.n y) (x male.n))))
    (s '(all x (all y (x sister-of.n y) (x sibling-of.n y))))
    (s '(all x (all y (x sibling-of.n y) (y sibling-of.n x))))
    (s '(all x (all y ((x sibling-of.n y) and (x female.n))
                      (x sister-of.n y))))
    (s '(all x (all y ((x sibling-of.n y) and (x male.n))
                      (x brother-of.n y))))
; Siblings, friends and spouses are cohorts
    (s '(all x (all y (x sibling-of.n y) (x cohort-of.n y))))
    (s '(all x (all y (x friend-of.n y) (x cohort-of.n y))))
    (s '(all x (all y (x spouse-of.n y) (x cohort-of.n y))))
; Cohorts are typically in the same age group, or the next group up
; or down (for the young age groups) [Obviously, it would be nicer
; to use axioms that posit approximate age differences, along with
; ones that derive age group from age. Sticking to qualitative
; relations can be awkward!]
;
; lower-probability conclusions OF DUBIOUS CURRENT VALUE, SO BLOCKED FOR NOW
;   (s '(all x (all y ((x cohort-of.n y) and (y baby-or-toddler.n))
;                     ((adv-s (with-certainty .4)) (x baby-or-toddler.n)))))
    (s '(all x (all y ((x cohort-of.n y) and (y baby-or-toddler.n))
                      ((adv-s (with-certainty .5)) (x child.n)))))
    (s '(all x (all y ((x cohort-of.n y) and (y child.n))
                      ((adv-s (with-certainty .7)) (x child.n)))))
;   (s '(all x (all y ((x cohort-of.n y) and (y child.n))
;                     ((adv-s (with-certainty .1)) (x teen.n)))))
;   (s '(all x (all y ((x cohort-of.n y) and (y child.n))
;                     ((adv-s (with-certainty .1)) (x baby-or-toddler.n)))))
    (s '(all x (all y ((x cohort-of.n y) and (y teen.n))
                      ((adv-s (with-certainty .7)) (x teen.n)))))
;   (s '(all x (all y ((x cohort-of.n y) and (y teen.n))
;                     ((adv-s (with-certainty .1)) (x child.n)))))
;   (s '(all x (all y ((x cohort-of.n y) and (y teen.n))
;                     ((adv-s (with-certainty .1)) (x young-adult.n)))))
    (s '(all x (all y ((x cohort-of.n y) and (y young-adult.n))
                      ((adv-s (with-certainty .8)) (x young-adult.n)))))
    (s '(all x (all y ((x cohort-of.n y) and (y youngish-adult.n))
                      ((adv-s (with-certainty .8)) (x youngish-adult.n)))))
    (s '(all x (all y ((x cohort-of.n y) and (y middle-aged.a))
                      ((adv-s (with-certainty .8)) (x middle-aged.a)))))
    (s '(all x (all y ((x cohort-of.n y) and (y senior.n))
                      ((adv-s (with-certainty .9)) (x senior.n)))))
; Friendship goes both ways
    (s '(all x (all y (x friend-of.n y) (y friend-of.n x))))
; Babies are not friends (too young) [**will add age claims for spouses]
    (s '(all x (all y (x friend-of.n y) (not (x baby-or-toddler.n)))))
; Friends are typically in the same age group
    (s '(all x (all y ((x friend-of.n y) and (y child.n))
                      ((adv-s (with-certainty .7)) (x child.n)))))
    (s '(all x (all y ((x friend-of.n y) and (y teen.n))
                      ((adv-s (with-certainty .7)) (x teen.n)))))
    (s '(all x (all y ((x friend-of.n y) and (y young-adult.n))
                      ((adv-s (with-certainty .7)) (x young-adult.n)))))
    (s '(all x (all y ((x friend-of.n y) and (y youngish-adult.n))
                      ((adv-s (with-certainty .7)) (x youngish-adult.n)))))
    (s '(all x (all y ((x friend-of.n y) and (y middle-aged.a))
                      ((adv-s (with-certainty .7)) (x middle-aged.a)))))
    (s '(all x (all y ((x friend-of.n y) and (y senior.n))
                      ((adv-s (with-certainty .7)) (x senior.n)))))

; Knowledge about age, gender, and hair color/baldness
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Seniors, especially male ones, usually have gray hair; female
; seniors are somewhat likely to to brown-haired (we can omit red/blond).
; [As noted, as long as we can't associate probabilities "externally"
; with propositions (as in EPILOG 1), we have to allow "with-certainty"
; modifiers in the antecedents!]
    (s '(all x (x female.n)
         (all_term y ((adv-s (with-certainty y)) (x senior.n))
           ((adv-s (with-certainty (times .7 y))) (x gray-haired.a)))))
    (s '(all x (x female.n)
         (all_term y ((adv-s (with-certainty y)) (x senior.n))
           ((adv-s (with-certainty (times .2 y))) (x brown-haired.a)))))
    (s '(all x (x male.n)
         (all_term y ((adv-s (with-certainty y)) (x senior.n))
           ((adv-s (with-certainty (times .9 y))) (x gray-haired.a)))))
; Conversely, a gray-haired person is almost certainly a senior
    (s '(all x 
         (all_term y ((adv-s (with-certainty y)) (x gray-haired.a))
           ((adv-s (with-certainty (times .9 y))) (x senior.n)))))
; Youngish adults, young adults, teens, children, toddlers and babies
; are progressively more and more unlikely to be gray-haired.
; For middle-aged people, it seems best to be non-committal (no 
; hair-color axioms)
    (s '(all x
         (all_term y ((adv-s (with-certainty y)) (x youngish-adult.n))
           ((adv-s (with-certainty (times .7 y))) (not (x gray-haired.a))))))
    (s '(all x
         (all_term y ((adv-s (with-certainty y)) (x young-adult.n))
           ((adv-s (with-certainty (times .95 y))) (not (x gray-haired.a))))))
    (s '(all x
         (all_term y ((adv-s (with-certainty y)) (x teen.n))
           ((adv-s (with-certainty (times .99 y))) (not (x gray-haired.a))))))
    (s '(all x
         (all_term y ((adv-s (with-certainty y)) (x child.n))
           ((adv-s (with-certainty (times .9999 y))) (not (x gray-haired.a))))))
    (s '(all x
         (all_term y ((adv-s (with-certainty y)) (x baby-or-toddler.n))
           ((adv-s (with-certainty y)) (not (x gray-haired.a))))))
; Someone who is not gray-haired is likely to have one of the other colors
    (s '(all x (x female.n)
         (all_term y ((adv-s (with-certainty y)) (not (x gray-haired.a)))
           ((adv-s (with-certainty (times .3 (minus 1 y)))) (x brown-haired.a)))))
    (s '(all x (x female.n)
         (all_term y ((adv-s (with-certainty y)) (not (x gray-haired.a)))
           ((adv-s (with-certainty (times .25 (minus 1 y)))) (x dark-haired.a)))))
    (s '(all x (x female.n)
         (all_term y ((adv-s (with-certainty y)) (not (x gray-haired.a)))
           ((adv-s (with-certainty (times .2 (minus 1 y)))) (x blond-haired.a)))))
    (s '(all x (x female.n)
         (all_term y ((adv-s (with-certainty y)) (not (x gray-haired.a)))
           ((adv-s (with-certainty (times .2 (minus 1 y)))) (x red-haired.a)))))

    (s '(all x (x male.n)
         (all_term y ((adv-s (with-certainty y)) (not (x gray-haired.a)))
           ((adv-s (with-certainty (times .25 (minus 1 y)))) (x brown-haired.a)))))
    (s '(all x (x male.n)
         (all_term y ((adv-s (with-certainty y)) (not (x gray-haired.a)))
           ((adv-s (with-certainty (times .25 (minus 1 y)))) (x dark-haired.a)))))
    (s '(all x (x male.n)
         (all_term y ((adv-s (with-certainty y)) (not (x gray-haired.a)))
           ((adv-s (with-certainty (times .15 (minus 1 y)))) (x blond-haired.a)))))
    (s '(all x (x male.n)
         (all_term y ((adv-s (with-certainty y)) (not (x gray-haired.a)))
           ((adv-s (with-certainty (times .15 (minus 1 y)))) (x red-haired.a)))))
; A senior male may well be bald
    (s '(all x (x male.n)
         (all_term y ((adv-s (with-certainty y)) (x senior.n))
           ((adv-s (with-certainty (times .5 y))) (x bald-headed.a)))))
; Conversely, a bald-headed person is male, and may well be a senior,
; or perhaps middle-aged (ignoring babies for now)
    (s '(all x
         (all_term y ((adv-s (with-certainty y)) (x bald-headed.a))
           ((adv-s (with-certainty y)) (x male.n)))))
    (s '(all x
         (all_term y ((adv-s (with-certainty y)) (x bald-headed.a))
           ((adv-s (with-certainty (times .6 y))) (x senior.n)))))
; OF DUBIOUS CURRENT VALUE, SO BLOCKED FOR NOW
;   (s '(all x 
;        (all_term y ((adv-s (with-certainty y)) (x bald-headed.a))
;          ((adv-s (with-certainty (times .4 y))) (x middle-aged.n)))))

; Knowledge about age, gender, and facial hair
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Females don't have beards or mustaches [some awkwardness results
; from the use of "negative predicates" like 'beardless']
    (s '(all x (x female.n) (not (x bearded.a))))
    (s '(all x (not (x bearded.a)) (x beardless.a)))
    (s '(all x (x female.n) ((adv-s (with-certainty .95))
                             (not (x mustachioed.a)))))
    (s '(all x 
         (all_term y ((adv-s (with-certainty y))
                      (not (x mustachioed.a)))
                     ((adv-s (with-certainty y))
                      (x mustacheless.a)))))
; Babies and children don't have beards or mustaches
    (s '(all x (x baby-or-toddler.n) (not (x bearded.a))))
    (s '(all x (x baby-or-toddler.n) (not (x bearded.a))))
    (s '(all x (x child.n) (not (x mustachioed.a))))
; Males from young adults upward may be bearded, and may have mustaches
    (s '(all x ((x male.n) and (x adult.n))
                ((adv-s (with-certainty .1)) (x bearded.a))))
    (s '(all x ((x male.n) and (x adult.n))
               ((adv-s (with-certainty .1)) (x mustachioed.a))))
; The probability of not being bearded is complementary to that of
; being bearded, and similarly for 'mustachioed.a':
    (s '(all x (all y ((adv-s (with-certainty y)) (x bearded.a))
                      ((adv-s (with-certainty (minus 1 y)))
                       (x beardless.a)))))
    (s '(all x (all y ((adv-s (with-certainty y)) (x mustachioed.a))
                      ((adv-s (with-certainty (minus 1 y)))
                       (x mustacheless.a)))))
         
; Knowledge about age and glasses
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Older persons tend to have glasses, but the probability goes down
; for younger people
    (s '(all x (all_term y ((adv-s (with-certainty y)) (x senior.n))
                    ((adv-s (with-certainty (times .8 y)))
                     (x wearing-glasses.a)))))
    (s '(all x (all_term y ((adv-s (with-certainty y)) (x middle-aged.a))
                    ((adv-s (with-certainty (times .6 y)))
                     (x wearing-glasses.a)))))
; OF DUBIOUS CURRENT VALUE, SO BLOCKED FOR NOW
;   (s '(all x (all_term y ((adv-s (with-certainty y)) (x youngish-adult.n))
;                   ((adv-s (with-certainty (times .3 y)))
;                    (x wearing-glasses.a)))))
;   (s '(all x (all_term y ((adv-s (with-certainty y)) (x young-adult.n))
;                   ((adv-s (with-certainty (times .2 y)))
;                    (x wearing-glasses.a)))))
;   (s '(all x (all_term y ((adv-s (with-certainty y)) (x teen.n))
;                   ((adv-s (with-certainty (times .1 y)))
;                    (x wearing-glasses.a)))))
; The probability of not wearing glasses is complementary to that of
; wearing glasses (not used for the reason below):
;     (s '(all x (all_term y ((adv-s (with-certainty y)) (x wearing-glasses.a))
;                            ((adv-s (with-certainty (minus 1 y)))
;                             (x not-wearing-glasses.a)))))
; Hmm, there's a problem: Suppose the age probabilities for person X are
; BABY-OR-TODDLER.N .05, CHILD.N .05, TEEN.N .1, YOUNG-ADULT.N .1,
; YOUNGISH-ADULT.N .1, MIDDLE-AGED.A 0.2, SENIOR.N 0.4 (these sum to 1).
; Then the probability of X wearing glasses will come out to (.4)(.8) =
; .32, and so the complement would be .68 -- i.e., the probability of
; no-glasses is very high! One problem here is of course that the actual
; probability of wearing glasses should sum over the possible age groups.
; But even then we may have a counterintuitive result: Suppose we have
; unit probability of a senior wearing glasses and 0 probability of a
; non-senior wearing glasses. Then we'll end up with probability .4
; of X wearing glasses and .6 of not wearing glasses; so this would lead
; to an apparent clash with image-derived information that X *is*
; wearing glasses! So rather than using an across-the-board complement
; rule for not-wearing-glasses, it seems best to introduce a complement 
; rule for each age category:
; Low-probability items are OF DUBIOUS CURRENT VALUE, SO BLOCKED FOR NOW
;   (s '(all x (all_term y ((adv-s (with-certainty y)) (x senior.n))
;                   ((adv-s (with-certainty (times .1 y)))
;                    (x not-wearing-glasses.a)))))
;   (s '(all x (all_term y ((adv-s (with-certainty y)) (x middle-aged.a))
;                   ((adv-s (with-certainty (times .4 y)))
;                    (x not-wearing-glasses.a)))))
    (s '(all x (all_term y ((adv-s (with-certainty y)) (x youngish-adult.n))
                    ((adv-s (with-certainty (times .6 y)))
                     (x not-wearing-glasses.a)))))
    (s '(all x (all_term y ((adv-s (with-certainty y)) (x young-adult.n))
                    ((adv-s (with-certainty (times .7 y)))
                     (x not-wearing-glasses.a)))))
    (s '(all x (all_term y ((adv-s (with-certainty y)) (x teen.n))
                    ((adv-s (with-certainty (times .8 y)))
                     (x not-wearing-glasses.a)))))
    (s '(all x (all_term y ((adv-s (with-certainty y)) (x child.n))
                    ((adv-s (with-certainty (times .95 y)))
                     (x not-wearing-glasses.a)))))
    (s '(all x (all_term y ((adv-s (with-certainty y)) (x baby-or-toddler.n))
                    ((adv-s (with-certainty (times .9999 y)))
                     (x not-wearing-glasses.a)))))


; Activities indicating age
; ~~~~~~~~~~~~~~~~~~~~~~~~~
; One who plays is probably a child
    (s '(all x (all e ((x play.v) @ e) 
                      ((adv-s (with-certainty .6)) (x child.n)))))
; Nonepisodic version (often obtained in tenseless captions):
    (s '(all x (x play.v)
               ((adv-s (with-certainty .6)) (x child.n))))
; Someone who graduates is probably a teen or young adult
    (s '(all x (all e ((x graduate.v) @ e)
                      ((adv-s (with-certainty .6)) (x teen.n)))))
    (s '(all x (all e ((x graduate.v) @ e)
                      ((adv-s (with-certainty .4)) (x young-adult.n)))))
; Nonepisodic versions:
    (s '(all x (x graduate.v)
               ((adv-s (with-certainty .6)) (x teen.n))))
    (s '(all x (x graduate.v)
               ((adv-s (with-certainty .4)) (x young-adult.n))))
; Someone who has a graduation party (just) graduated; a rough axiom
    (s '(all x (all y ((y ((nn graduation.n) party.n)) and (y pertain-to x))
                      (some e ((x graduate.v) @ e)))))
; If something (an event) is the graduation of something (a person),
; then the latter is very likely a teen or young-adult:
    (s '(all x (all y (x graduation-of.n y)
               ((adv-s (with-certainty .6)) (y teen.n)))))
    (s '(all x (all y (x graduation-of.n y)
               ((adv-s (with-certainty .4)) (y young-adult.n)))))
; We could add that someone who graduates graduates *from* some high school
; or college/university

; Someone who has a graduation party is a central character in a photo;
; central characters tend to be the object of implicit relation like
; that in "Grandmother Lillian". (The same could be said about brides, 
; grooms, and also the picture-captioner; e.g., suppose a picture of 
; just one person is captioned "Grandmother" -- this probably entails 
; it's the picture-captioner's grandmother). Being mentioned first in
; a caption also tend to indicate centrality (this metainformation may
; best be handled by TTT foreward inference rules).
    (s '(all x (all y ((y ((nn graduation.n) party.n)) and (y pertain-to x))
                      (x central-character.n))))
; Someone at camp is probably a child or teen
    (s '(all x (all e ((x at.p (K camp.n)) @ e)
                      ((adv-s (with-certainty .6)) (x child.n)))))
    (s '(all x (all e ((x at.p (K camp.n)) @ e)
                      ((adv-s (with-certainty .3)) (x teen.n)))))

; Descriptors implying age or gender
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; A kid is most likely child, or perhaps a teen
    (s '(all x (x kid.n) ((adv-s (with-certainty .8)) (x child.n))))
    (s '(all x (x kid.n) ((adv-s (with-certainty .2)) (x teen.n))))
; Someone who is little is very likely a child
    (s '(all x (x little.a) ((adv-s (with-certainty .7)) (x child.n))))
; Someone who is in a dress is a female
    (s '(all x (x person.n) 
               (all y (y dress.n) 
                      (all e ((x in.p y) @ e) (x female.n)))))

; Likely relatonships for central characters
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; (There's some risk in these pragmatic rules: E.g., we may infer
; that Grandma Lillian is a mother, and this might lead to the
; tentative conclusion that she is the mother of the *central*
; character -- surely a mistake. That really suggests that
; a switch to TTT rules that use the canonicalized LF to posit
; such relations. Also we want to connect the y that a grandmother
; may be inferred to be the grandmother *of* to the central
; character.)
    (s '(all x (x mother.n) 
         (all y (y central-character.n)
          ((adv-s (with-certainty .6)) (x mother-of.n y)))))
    (s '(all x (x grandmother.n) 
         (all y (y central-character.n)
          ((adv-s (with-certainty .6)) (x grandmother-of.n y)))))
    (s '(all x (x uncle.n) 
         (all y (y central-character.n)
          ((adv-s (with-certainty .6)) (x uncle-of.n y)))))
    ; could write more rules, but see equivocation above...
                                   
