
(*
(*:History:
      		26 June 2005: Created.
      		02 Aug	2005: Last modified
  *)

(*
:Summary:
This file contains definitions of K-bessel functions. K3, k12 and k21 do not \
compute but their definitions are correct: Mathematica simply cannot compute \
them accurately.

In order to import it into Mathematica, save this notebook in package format. \
You might need to open the .m file and delete the comment marks around \
BeginPackage, EndPackage, and the usage sections. The .m files included in \
this directory are ready for use, though.
*)

*)

BeginPackage["KBessel`"]

(*
\!\(\*
  RowBox[{"\[IndentingNewLine]", 
    RowBox[{\(MatrixGamma::usage = "\<MatrixGamma[n, s] is the matrix gamma function defined in Terras 2, page 41. n is  a positive integer. s is a list of complex numbers sarisfying Re(s_j+...+s_n)>(j-1)/2, for j=1,...,n.\>"\), "\[IndentingNewLine]", "\[IndentingNewLine]", \(PowerFunction::usage = "\<PowerFunction[s, Y] is the power function for Y positive definite and s in the corresponding multi-dimensional complex plane (Terras II,p.38). Y is a matrix and s is a list of complex numbers.\>"\), "\[IndentingNewLine]", "\[IndentingNewLine]", \(InverseIwasawa::usage = "\<InverseIwasawa takes a two  n x n matrices, A and B, as input and returns Transpose[B].A.B\>"\), "\[IndentingNewLine]", "\[IndentingNewLine]", \(BabyK::usage = \[IndentingNewLine]"\<BabyK takes input,s and z which are in the upper half complex plane.Terras1,p.136.\>"\), "\[IndentingNewLine]", "\[IndentingNewLine]", \(K1::usage = \[IndentingNewLine]"\<K1[s, a, b]. s is a complex number;a and b are real and greater than 0 (Terras 2, p.52).\>"\), "\[IndentingNewLine]", "\[IndentingNewLine]", \(K2::usage = \[IndentingNewLine]"\<(s, t) is a number in the 2-dimensional complex plain. In other words, s and t are complex. A and B are 2 X 2 positive-definite matrices.\>"\), "\[IndentingNewLine]", "\[IndentingNewLine]", 
      RowBox[{\(SingularK2::usage\), "=", "\[IndentingNewLine]", "\"\<s is in 2-dim complex plane. A and B and D are positive numbers. s is in the 2-dim complex plane and has the following restriction. Let a and b be complex numbers such that s_1 + s_2 = a and s_2 - 1/2 = b; Re(a) must be sufficiently large for the convergence of\!\(TraditionalForm\`\(\(\\\ \)\(\(MatrixGamma\)\(.\)\)\)\)\>\""}], "\[IndentingNewLine]", "\[IndentingNewLine]", \(K3::usage = \[IndentingNewLine]\*"\"\<This is from Terras2, p. 51. s is in \!\(\[DoubleStruckCapitalC]\^3\)i.e. a list of 3 complex numbers and V, W  are in P_3, or W is singular with s satisfying s is a list of complex numbers sarisfying Re(s_j+...+s_n)>(j-1)/2, for j=1,...,n.\>\""\), "\[IndentingNewLine]", "\[IndentingNewLine]", \(SingularK3::usage = \[IndentingNewLine]"\<SingularK3[a, b, c, A, B, D] is K3 for singular argument where s is in the 3-dim complex plane, A  is real positive, and B and D are  2 x 2 matrices.\>"\), "\[IndentingNewLine]", "\[IndentingNewLine]", \(Babyk::usage = \[IndentingNewLine]"\<Babyk[s, z, a]. s, z  are complex. z is in the upper half complex plane and Re(s)>0. a is real. (Terras 1, p.137)\>"\), "\[IndentingNewLine]", "\[IndentingNewLine]", \(k11::usage = \[IndentingNewLine]"\<k11[s, Y, n]. s is a complex number in the suitable half-plane, Y is 2 x 2 positive definite, and n is a real number.\>"\), "\[IndentingNewLine]", "\[IndentingNewLine]", \(k11Determinant1::usage = \[IndentingNewLine]"\<k11Determinant1[s, y, a]. s is suitably restricted complex number, y is a positive real , and is plain real. It is a synonym for k11[{s, 0}, <det 1 matrix with y at one corner>, a] Terras2 p.52\>"\), "\[IndentingNewLine]", "\[IndentingNewLine]", \(k12::usage = \[IndentingNewLine]"\<k12 takes a number in the 3-dimensional complex plane (a list of complex numbers), a 3 x 3 matrix, and a 1 x 2 matrix as input. It returns a complex number. Doesn't work.\>"\), "\[IndentingNewLine]", "\[IndentingNewLine]", 
      RowBox[{\(k12BasedOnK1::usage\), "=", "\[IndentingNewLine]", "\"\<k12BasedOnK1[s_,  V_, W_, A_]. s is complex, V is 1 x 1 and W is 2 x 2. A is 1 x 2. This is  a synonym for k12[s, 0, 0|(\!\(\*GridBox[{
{V, 0},
{0, W}
}]\)), A]\>\""}], "\[IndentingNewLine]", \(k21::usage = \[IndentingNewLine]"\<k21[s, Y, N]. s is in the 3-dimensional complex plane (a list of complex numbers), Y is a 3 x 3 matrix and N is a 2 x 1 matrix. There are conditions for convergence listed in Bengtson, p. 67. Doesn't work.\>"\), "\[IndentingNewLine]", "\[IndentingNewLine]", \(k21IdentityArgument::usage = "\<[s_, a_]. s is complex with Re(s)>0; a is real (p.52, Terras 2).\>"\), "\[IndentingNewLine]", "\[IndentingNewLine]", \(k21basedonK2::usage = "\<[s_, V_, W_, A_]. s is in the 2-dim complex plane and the coordinates of s lie in a suitable half-plane; V and W are positive-definite; A is a 1x2 matrix with real entries. (Terras2, p. 54)\>"\)}]}]\)
*)
Begin["`Private`"]
(*
Global`Private`
*)
\!\(\(\(MatrixGamma[n_, \ s_] := \ Module[{f}, \[IndentingNewLine]f[elmt_, \ i_] := If[i[\([1]\)] >= j, \ elmt, \ 0]; \[IndentingNewLine]Pi^\((n \((n - 1)\)/4)\) \(\[Product]\+\(j = 1\)\%n Gamma[Apply[\ Plus, MapIndexed[f, s]\ ] - \ \((j - 1)\)\/2]\)]\)\(\[IndentingNewLine]\)
  \)\)
\!\(\(\(PowerFunction[s_, \ Y_] := Module[{upperJ},  (*upperN\ returns\ the\ j\ X\ j\ upper\ left\ hand\ corner\ of\ \(\(S\)\(.\)\)*) upperJ[j_, \ S_] := Take[S, \ j, \ j]; \[IndentingNewLine]\[Product]\+\(j = 1\)\%\(Length[Y]\)Det[\ upperJ[j, \ Y]\ ]\^s[\([\ j\ ]\)]]\)\(\[IndentingNewLine]\)
  \)\)
InverseIwasawa[A_, B_] := Transpose[B].A.B

\!\(\(\(BabyK[s_, \ z_] := \(1\/2\) \(\[Integral]\_0\%\[Infinity] Exp[\(\(-z\)\/2\) \((t + 1\/t)\)] \(t\^\(s - 1\)\) \[DifferentialD]t\)\)\(\[IndentingNewLine]\)
  \)\)
\!\(\(\( (*\ Note, \ we' re\ using\ the\ indirect\ definition\ based\ on\ \(\(babyK\)\(.\)\)\ *) \)\(\[IndentingNewLine]\)\(K1[s_, \ a_, \ b_] := \(\(2 \(\((b\/a)\)\^\(s\/2\)\) BabyK[s, \ 2  Sqrt[a*b]]\)\(\[IndentingNewLine]\)
    \)\)\)\)
\!\(\(\(K2[s_, t_, \ A_, \ B_] := NIntegrate[\(v\^\(s + t - 1/2\)\) \(w\^\(t - 3/2\)\) Exp[\(-v\)\ A[\([1, \ 1]\)] - 2\ v\ *x\ A[\([1, \ 2]\)] - \((w + v\ *x\^2)\)\ A[\([2, \ 2]\)] - \(\((w + v\ *x\^2)\) B[\([1, \ 1]\)]\)\/\(v\ *w\) + \(2\ x*\ B[\([1, \ 2]\)]\)\/w - B[\([2, \ 2]\)]\/w], \ {w, \ 0, \ \[Infinity]}, \ {v, \ 0, \ \[Infinity]}, \ {x, \ \(-\[Infinity]\), \ \[Infinity]}]\)\(\[IndentingNewLine]\)
  \)\)
(* This is K2 with singular argument. *)
\!\(\(\(SingularK2[\ s, \ A_, \ B_, \ D_] := \(Pi\^0.5\) \(Det[B]\^\(-0.5\)\) \(A\^\(-a\)\) MatrixGamma[1, \ s[\([1]\)]\  + \ s[\([2]\)]] K1[\ s[\([2]\)] - 0.5, \ B, \ D]\)\(\[IndentingNewLine]\)
  \)\)
\!\(\(\(K3[s_, \ V_, \ W_] := \[Integral]\_\(-\[Infinity]\)\%\[Infinity]\(\[Integral]\_\(-\[Infinity]\)\%\[Infinity]\(\[Integral]\_0\%\[Infinity]\(\[Integral]\_0\%\[Infinity]\(\[Integral]\_0\%\[Infinity]\(\[Integral]\_0\%\[Infinity]\( u\^s[\([1]\)]\) \(Det[V]\^\(s[\([2]\)] - 2\)\) \(\((\(-x\)\ v\^2 + x\ u\ w)\)\^s[\([3]\)]\) Exp[\(-u\)\ V[\([1, \ 1]\)] - 2\ v\ V[\([1, \ 2]\)] - 2\ \((u\ y + v\ z)\)\ V[\([1, \ 3]\)] - w\ V[\([2, \ 2]\)] - 2\ \((v\ y + w\ z)\)\ V[\([2, \ 3]\)] - \((w + y\ \((u\ y + v\ z)\) + z\ \((v\ y + w\ z)\))\)\ V[\([3, \ 3]\)] - \(\((w\^2 - v\^2\ y\^2 + u\ w\ y\^2)\)\ W[\([1, \ 1]\)]\)\/\(\(-v\^2\)\ w + u\ w\^2\) - \(2\ \((\(-v\)\ w - v\^2\ y\ z + u\ w\ y\ z)\)\ W[\([1, \ 2]\)]\)\/\(\(-v\^2\)\ w + u\ w\^2\) - \(2\ \((v\^2\ y - u\ w\ y)\)\ W[\([1, \ 3]\)]\)\/\(\(-v\^2\)\ w + u\ w\^2\) - \(\((u\ w - v\^2\ z\^2 + u\ w\ z\^2)\)\ W[\([1, \ 2]\)]\)\/\(\(-v\^2\)\ w + u\ w\^2\) - \(2\ \((v\^2\ z - u\ w\ z)\)\ W[\([2, \ 3]\)]\)\/\(\(-v\^2\)\ w + u\ w\^2\) - \(\((\(-v\^2\) + u\ w)\)\ W[\([3, \ 3]\)]\)\/\(\(-v\^2\)\ w + u\ w\^2\)] \[DifferentialD]u \[DifferentialD]v \[DifferentialD]w \[DifferentialD]x \[DifferentialD]y \[DifferentialD]z\)\)\)\)\)\)\(\[IndentingNewLine]\)
  \)\)

SingularK3[a_, b_, c_, A_, B_, D_] := 
  Pi Det[B]^(-.5) A^(-a - b - c) MatrixGamma[1, a + b + c] K2[b, c - .5, B, 
      D]
  
\!\(\(Babyk[s_, \ z_, \ 0] := \(Im[z]\^\(1 - s\)\) Gamma[1\/2] Gamma[s - 1\/2]/Gamma[s];\)\[IndentingNewLine]
  Babyk[s_, \ z_, \ a_] := Exp[2  I*a*Pi*Re[z]] 2 \( Pi\^\(1/2\)\) \(Gamma[s]\^\(-1\)\) \(Abs[a*Pi]\^\(s - 1/2\)\) \(Im[z]\^\(1/2\)\) BabyK[s - 1/2, \ 2  Abs[Pi*a*Im[z]]]\[IndentingNewLine]
  \)
\!\(\*
  RowBox[{\(k11[s_, Y_, a_]\), ":=", 
    RowBox[{\(\@\(2\ Pi\)\), " ", 
      RowBox[{"FourierTransform", "[", 
        RowBox[{
          RowBox[{"PowerFunction", "[", 
            RowBox[{\(-s\), ",", " ", 
              RowBox[{"InverseIwasawa", "[", 
                RowBox[{\(Inverse[Y]\), ",", 
                  RowBox[{"(", GridBox[{
                        {"1", "0"},
                        {"x", "1"}
                        }], ")"}]}], "]"}]}], "]"}], ",", "x", ",", \(2\ Pi\ a\)}], "]"}], " "}]}]\)
\!\(k11Determinant1[s_, \ y_, \ 0] := \(y\^\(1 - s\)\) Gamma[1\/2] Gamma[s - 1\/2]/Gamma[s]\)
\!\(\(\(k11Determinant1[s_, \ y_\ , \ a_] := 2 \( Pi\^\(1/2\)\) \(Gamma[s]\^\(-1\)\) \(Abs[Pi*a]\^\(s - 1/2\)\) \(y\^\(1/2\)\) BabyK[s - 1/2, \ 2  Pi\ Abs[a*y]]\)\(\[IndentingNewLine]\)
  \)\)
\!\(\*
  RowBox[{
    RowBox[{\(k12[s_, \ Y_, \ N_]\), ":=", 
      RowBox[{"NIntegrate", "[", 
        RowBox[{
          RowBox[{
            RowBox[{"PowerFunction", "[", 
              RowBox[{\(-s\), ",", 
                RowBox[{"InverseIwasawa", "[", 
                  RowBox[{\(Inverse[Y]\), ",", " ", 
                    RowBox[{"(", GridBox[{
                          {"1", "0", "0"},
                          {\(x\_1\), "1", "0"},
                          {\(x\_2\), "0", "1"}
                          }], ")"}]}], "]"}]}], "]"}], \(Exp[2  I*Pi\ \((N[\([1, \ 1]\)] x\_1\  + \ N[\([1, \ \ 2]\)] x\_2)\)]\)}], ",", " ", \({x\_1, \ \(-\[Infinity]\), \ \[Infinity]}\), ",", " ", \({x\_2, \ \(-\[Infinity]\), \ \[Infinity]}\)}], "]"}]}], "\[IndentingNewLine]"}]\)
sstar[s_, m_] := Append[Reverse[Take[s, m - 1]] , -Apply[Plus, Take[s, m]]]
ssharp[s_, m_, n_] := ReplacePart[-s, -Last[s] + (n - m)/2, Length[s]]
\!\(k12BasedOnK1[s_, \ \ V_, \ W_, \ A_] := Pi\ \(Det[W]\^\(1/2\)\) K1[\(ssharp[{s}, \ 1, \ 2]\)[\([1]\)], \ \(InverseIwasawa[W, \ Pi\ Transpose[A]]\)[\([1, \ 1]\)], \ \(Inverse[V]\)[\([1, \ 1]\)]]\)
\!\(\*
  RowBox[{\( (*\ Doesn' t\ work\ *) \), "\[IndentingNewLine]", 
    RowBox[{
      RowBox[{\(k21[s_, \ Y_, \ N_]\), ":=", 
        RowBox[{"NIntegrate", "[", 
          RowBox[{
            RowBox[{
              RowBox[{"PowerFunction", "[", 
                RowBox[{\(-s\), ",", 
                  RowBox[{"InverseIwasawa", "[", 
                    RowBox[{\(Inverse[Y]\), ",", " ", 
                      RowBox[{"(", GridBox[{
                            {"1", "0", "0"},
                            {"0", "1", "0"},
                            {\(x\_1\), \(x\_2\), "1"}
                            }], ")"}]}], "]"}]}], "]"}], 
              RowBox[{"Exp", "[", 
                RowBox[{"Tr", "[", 
                  RowBox[{"2", "I", " ", "*", "Pi", "*", 
                    RowBox[{\(Transpose[N]\), ".", 
                      RowBox[{"(", GridBox[{
                            {\(x\_1\)},
                            {\(x\_2\)}
                            }], ")"}]}]}], "]"}], "]"}]}], ",", " ", \({x\_1, \ \(-\[Infinity]\), \ \[Infinity]}\), ",", " ", \({x\_2, \ \(-\[Infinity]\), \ \[Infinity]}\)}], "]"}]}], "\[IndentingNewLine]"}]}]\)
B[p_, q_] := Gamma[p]Gamma[q]/Gamma[p + q]
\!\(k21IdentityArgument[s_, \ a_] := k11Determinant1[s - 1\/2, \ 1, \ a] B[1\/2, \ s - 1\/2]\)
(* These s functions are only for k21 *)

sstar[s_] := Append[Take[s, 1] , -Apply[Plus, Take[s, 2]]]
ssharp[s_] := ReplacePart[-s, -Last[s] + 1/2, Length[s]]
\!\(\(\( (*\ Do\ not\ make\ the\ second\ entry\ of\ A\ 0\ otherwise\ you' ll\ get\ matrices\ that\ are\ not\ in\ \(P\_2\) being\ passed\ to\ \(\(K2\)\(.\)\)\ *) \)\(\[IndentingNewLine]\)\(k21basedonK2[s_, \ V_, \ W_, \ A_] := Pi\ Det[W]\ K2[ssharp[s], \ InverseIwasawa[W, \ Pi\ Transpose[A]], \ Inverse[V]]/MatrixGamma[2, \ \(-sstar[s]\)]\)\)\)

End[ ]
EndPackage[ ]

