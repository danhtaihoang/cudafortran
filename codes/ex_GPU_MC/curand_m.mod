V27 0x4 curand_m
12 curand_m.cuf S582 0
11/28/2013  08:49:49
use iso_c_binding private
enduse
D 56 18 20
D 58 24 615 8 614 7
D 64 24 617 8 616 7
D 76 18 80
D 78 21 8 1 3 0 0 0 0 1 0
 0 0 3 3 0 81
D 81 18 82
D 83 21 9 1 3 0 0 0 0 1 0
 0 0 3 3 0 83
D 86 18 84
S 582 24 0 0 0 8 1 0 4658 10005 0 A 0 0 0 0 0 0 0 0 0 0 0 0 58 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 curand_m
S 583 6 4 0 0 6 585 582 4667 80000c 0 A 0 0 0 0 0 0 0 0 0 0 0 0 711 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 curand_rng_pseudo_default
S 584 3 0 0 0 6 0 1 0 0 0 A 0 0 0 0 0 0 0 0 0 100 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 585 6 4 0 0 6 587 582 4693 80000c 0 A 0 0 0 0 0 4 0 0 0 0 0 0 711 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 curand_rng_pseudo_xorwow
S 586 3 0 0 0 6 0 1 0 0 0 A 0 0 0 0 0 0 0 0 0 101 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 587 6 4 0 0 6 589 582 4718 80000c 0 A 0 0 0 0 0 8 0 0 0 0 0 0 711 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 curand_rng_quasi_default
S 588 3 0 0 0 6 0 1 0 0 0 A 0 0 0 0 0 0 0 0 0 200 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 589 6 4 0 0 6 1 582 4743 80000c 0 A 0 0 0 0 0 12 0 0 0 0 0 0 711 0 0 0 0 0 0 0 0 0 0 582 0 0 0 0 curand_rng_quasi_sobol
S 590 3 0 0 0 6 0 1 0 0 0 A 0 0 0 0 0 0 0 0 0 201 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 591 19 0 0 0 8 1 582 4766 4000 0 A 0 0 0 0 0 0 0 0 0 592 0 0 0 0 0 0 9 1 0 0 0 0 0 582 0 0 0 0 curandcreategenerator
O 591 1 592
S 592 14 5 0 0 0 1 582 4766 0 18000 A 0 0 0 0 0 0 0 1 2 591 0 0 0 0 0 0 0 0 0 0 0 10 0 582 0 0 596 0 curandcreategenerator
F 592 2 593 594
S 593 1 3 0 0 7 1 592 4788 2004 2000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 generator
S 594 1 3 0 0 6 1 592 4798 2004 6000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 rng_type
S 595 3 0 0 0 6 0 1 0 0 0 A 0 0 0 0 0 0 0 0 0 21 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 596 3 0 0 0 56 0 1 0 0 0 A 0 0 0 0 0 0 0 0 4807 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 21 63 75 72 61 6e 64 43 72 65 61 74 65 47 65 6e 65 72 61 74 6f 72
R 614 25 6 iso_c_binding c_ptr
R 615 5 7 iso_c_binding val c_ptr
R 616 25 8 iso_c_binding c_funptr
R 617 5 9 iso_c_binding val c_funptr
R 650 6 42 iso_c_binding c_null_ptr$ac
R 652 6 44 iso_c_binding c_null_funptr$ac
R 653 26 45 iso_c_binding ==
R 655 26 47 iso_c_binding !=
S 686 19 0 0 0 8 1 582 5584 4000 0 A 0 0 0 0 0 0 0 0 0 687 0 0 0 0 0 0 10 1 0 0 0 0 0 582 0 0 0 0 curandsetpseudorandomgeneratorseed
O 686 1 687
S 687 14 5 0 0 0 1 582 5584 0 18000 A 0 0 0 0 0 0 0 15 2 686 0 0 0 0 0 0 0 0 0 0 0 20 0 582 0 0 691 0 curandsetpseudorandomgeneratorseed
F 687 2 688 689
S 688 1 3 0 0 7 1 687 4788 2004 6000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 generator
S 689 1 3 0 0 7 1 687 5619 2004 6000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 seed
S 690 3 0 0 0 6 0 1 0 0 0 A 0 0 0 0 0 0 0 0 0 34 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 691 3 0 0 0 76 0 1 0 0 0 A 0 0 0 0 0 0 0 0 5624 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 34 63 75 72 61 6e 64 53 65 74 50 73 65 75 64 6f 52 61 6e 64 6f 6d 47 65 6e 65 72 61 74 6f 72 53 65 65 64
S 692 19 0 0 0 8 1 582 5659 4000 0 A 0 0 0 0 0 0 0 0 0 693 0 0 0 0 0 0 12 2 0 0 0 0 0 582 0 0 0 0 curandgenerateuniform
O 692 2 699 693
S 693 14 5 0 0 0 1 582 5659 0 18000 A 0 0 0 0 0 0 0 17 3 692 0 0 0 0 0 0 0 0 0 0 0 30 0 582 0 0 697 0 curandgenerateuniform
F 693 3 694 695 696
S 694 1 3 0 0 7 1 693 4788 2004 6000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 generator
S 695 7 3 0 0 78 1 693 5681 80a104 2000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 odata
S 696 1 3 0 0 7 1 693 5687 2004 6000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 numele
S 697 3 0 0 0 56 0 1 0 0 0 A 0 0 0 0 0 0 0 0 5694 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 21 63 75 72 61 6e 64 47 65 6e 65 72 61 74 65 55 6e 69 66 6f 72 6d
S 698 6 1 0 0 6 1 693 5716 40800006 2000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_0
S 699 14 5 0 0 0 1 582 5722 0 18000 A 0 0 0 0 0 0 0 20 3 0 0 0 0 0 0 0 0 0 0 0 0 39 0 582 0 0 704 0 curandgenerateuniformdouble
F 699 3 700 701 702
S 700 1 3 0 0 7 1 699 4788 2004 6000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 generator
S 701 7 3 0 0 83 1 699 5681 80a104 2000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 odata
S 702 1 3 0 0 7 1 699 5687 2004 6000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 numele
S 703 3 0 0 0 6 0 1 0 0 0 A 0 0 0 0 0 0 0 0 0 27 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 704 3 0 0 0 81 0 1 0 0 0 A 0 0 0 0 0 0 0 0 5750 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 27 63 75 72 61 6e 64 47 65 6e 65 72 61 74 65 55 6e 69 66 6f 72 6d 44 6f 75 62 6c 65
S 705 6 1 0 0 6 1 699 5778 40800006 2000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 z_b_1
S 706 19 0 0 0 8 1 582 5784 4000 0 A 0 0 0 0 0 0 0 0 0 707 0 0 0 0 0 0 13 1 0 0 0 0 0 582 0 0 0 0 curanddestroygenerator
O 706 1 707
S 707 14 5 0 0 0 1 582 5784 0 18000 A 0 0 0 0 0 0 0 23 1 706 0 0 0 0 0 0 0 0 0 0 0 51 0 582 0 0 710 0 curanddestroygenerator
F 707 1 708
S 708 1 3 0 0 7 1 707 4788 2004 6000 A 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 generator
S 709 3 0 0 0 6 0 1 0 0 0 A 0 0 0 0 0 0 0 0 0 22 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 6
S 710 3 0 0 0 86 0 1 0 0 0 A 0 0 0 0 0 0 0 0 5807 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 18 22 63 75 72 61 6e 64 44 65 73 74 72 6f 79 47 65 6e 65 72 61 74 6f 72
S 711 11 0 0 0 8 663 582 5830 40800008 805000 A 0 0 0 0 0 16 0 0 583 589 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 _curand_m$8
A 12 2 0 0 0 6 584 0 0 0 12 0 0 0 0 0 0 0 0 0
A 13 1 0 0 0 6 583 0 0 0 0 0 0 0 0 0 0 0 0 0
A 14 2 0 0 0 6 586 0 0 0 14 0 0 0 0 0 0 0 0 0
A 15 1 0 0 0 6 585 0 0 0 0 0 0 0 0 0 0 0 0 0
A 16 2 0 0 0 6 588 0 0 0 16 0 0 0 0 0 0 0 0 0
A 17 1 0 0 0 6 587 0 0 0 0 0 0 0 0 0 0 0 0 0
A 18 2 0 0 0 6 590 0 0 0 18 0 0 0 0 0 0 0 0 0
A 19 1 0 0 0 6 589 0 0 0 0 0 0 0 0 0 0 0 0 0
A 20 2 0 0 0 6 595 0 0 0 20 0 0 0 0 0 0 0 0 0
A 76 1 0 0 0 58 650 0 0 0 0 0 0 0 0 0 0 0 0 0
A 79 1 0 0 0 64 652 0 0 0 0 0 0 0 0 0 0 0 0 0
A 80 2 0 0 0 6 690 0 0 0 80 0 0 0 0 0 0 0 0 0
A 81 1 0 0 14 6 698 0 0 0 0 0 0 0 0 0 0 0 0 0
A 82 2 0 0 0 6 703 0 0 0 82 0 0 0 0 0 0 0 0 0
A 83 1 0 0 0 6 705 0 0 0 0 0 0 0 0 0 0 0 0 0
A 84 2 0 0 0 6 709 0 0 0 84 0 0 0 0 0 0 0 0 0
Z
J 149 1 1
V 76 58 7 0
S 0 58 0 0 0
A 0 6 0 0 1 2 0
J 150 1 1
V 79 64 7 0
S 0 64 0 0 0
A 0 6 0 0 1 2 0
Z
