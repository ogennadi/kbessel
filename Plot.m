
(*
(*:History:
		    24 June 2005: Created
		    02 Aug	2005: Last modified
*)

(*
:Summary:
This file contains definitions of functions for conveniently plotting complex \
functions i.e. plotting the real, imaginary, and modulus graphs side-by-side.

In order to import it into Mathematica, save this notebook in package format. \
You might need to open the .m file and delete the comment marks around \
BeginPackage, EndPackage, and the usage sections. The .m files included in \
this directory are ready for use, though.
*)

*)

BeginPackage["ComplexPlot`"]


ComplexPlot::usage = "Plot[a, r, axeslabels] Plots the real, imaginary, and modulus graphs of the 2D expression a, using the list r as the range over which to plot, and axeslabels as the AxesLabels."
\

ComplexPlot3D::usage = "Plot[a, r, axeslabels] Plots the real, imaginary, and modulus graphs of the 3D expression a, using the list r as the range over which to plot, and axeslabels as the AxesLabels"
\

ComplexContourPlot::usage =
  "ComplexContourPlot takes an expression, an x range and a y range (lists), the number of plot points to use, and a list of axis labels, and displays an array of contour plots of the given function: one real, one imaginary, and one of the modulus."
\

RealComplexPlot3D::usage =
  "Plots a 3D graph of the real part of the expression given. It takes an expression, an x range and a y range (lists), the number of plot points to use, and a list of axis labels."
\

ImaginaryComplexPlot3D::usage =
  "Plots a 3D graph of the imaginary part of the expression given. It takes an expression, an x range and a y range (lists), the number of plot points to use, and a list of axis labels."
\

ModulusComplexPlot3D::usage =
  "Plots a 3D graph of the modulus of the expression given. It takes an expression, an x range and a y range (lists), the number of plot points to use, and a list of axis labels."
\

ComplexPlotAnimate::usage = "[a_,xrange_,yrange_, plotpoints_, plotrange_, axeslabels_]. This is exactly the same as ComplexPlot3D except that plotrange is a list of 3 plotranges: one for Re, Im, and Abs respectively. (The animate is in the name because fixing the plotrange keeps the plot frame steady for animation)"
\

ComplexContourPlotAnimate::usage = "[a_,xrange_,yrange_, plotpoints_, plotrange_, axeslabels_]. This is exactly the same as ComplexContourPlot except that plotrange is a list of 3 plotranges: one for Re, Im, and Abs respectively."
\


Begin["`Private`"]



ComplexPlot[a_, range_, plotpoints_, axeslabels_] :=
  Show[GraphicsArray[{
        Plot[Re[Evaluate[a]], range, DisplayFunction -> Identity, 
          AxesLabel -> axeslabels, PlotLabel -> "Re"],
        Plot[Im[Evaluate[a]], range, DisplayFunction -> Identity, 
          AxesLabel -> axeslabels, PlotLabel -> "Im"],
        Plot[Abs[Evaluate[a]], range, DisplayFunction -> Identity, 
          AxesLabel -> axeslabels, PlotLabel -> "Abs"]},
      
      ImageSize -> 600]]

ComplexPlot3D[a_, xrange_, yrange_, plotpoints_, axeslabels_] :=
  
  Show[GraphicsArray[{Plot3D[Re[Evaluate[a]], xrange, yrange, 
          DisplayFunction -> Identity, AxesLabel -> axeslabels , 
          PlotLabel -> "Re"],
         Plot3D[Im[Evaluate[a]], xrange, yrange, DisplayFunction -> Identity, 
          AxesLabel -> axeslabels , PlotLabel -> "Im"],
        Plot3D[Abs[Evaluate[a]], xrange, yrange, DisplayFunction -> Identity, 
          AxesLabel -> axeslabels , PlotLabel -> "Abs"]},
      
      ImageSize -> 600]]

ComplexContourPlot[a_, xrange_, yrange_, plotpoints_, axeslabels_] :=
  
  Show[GraphicsArray[{ContourPlot[Re[Evaluate[a]], xrange, yrange, 
          DisplayFunction -> Identity, AxesLabel -> axeslabels , 
          ContourLines -> False, PlotLabel -> "Re"],
         ContourPlot[Im[Evaluate[a]], xrange, yrange, 
          DisplayFunction -> Identity, AxesLabel -> axeslabels , 
          ContourLines -> False, PlotLabel -> "Im"],
        ContourPlot[Abs[Evaluate[a]], xrange, yrange, 
          DisplayFunction -> Identity, AxesLabel -> axeslabels , 
          ContourLines -> False, PlotLabel -> "Abs"]},
      
      ImageSize -> 600]]

RealComplexPlot3D[a_, xrange_, yrange_, plotpoints_, axeslabels_] := 
  Plot3D[Evaluate[Re[a]], xrange, yrange, AxesLabel -> axeslabels, 
    ColorFunction -> (Hue[(1 - #)/2] &)]

ImaginaryComplexPlot3D[a_, xrange_, yrange_, plotpoints_, axeslabels_] := 
  Plot3D[Evaluate[Im[a]], xrange, yrange, AxesLabel -> axeslabels, 
    ColorFunction -> (Hue[(1 - #)/2] &)]

ModulusComplexPlot3D[a_, xrange_, yrange_, plotpoints_, axeslabels_] := 
  Plot3D[Evaluate[Abs[a]], xrange, yrange, AxesLabel -> axeslabels, 
    ColorFunction -> (Hue[(1 - #)/2] &)]

ComplexPlotAnimate[a_, xrange_, yrange_, plotpoints_, plotrange_, 
    axeslabels_] :=
  
  Show[GraphicsArray[{Plot3D[Re[Evaluate[a]], xrange, yrange, 
          DisplayFunction -> Identity, AxesLabel -> axeslabels , 
          PlotRange -> plotrange[[1]],  PlotLabel -> "Re"],
         Plot3D[Im[Evaluate[a]], xrange, yrange, DisplayFunction -> Identity, 
          AxesLabel -> axeslabels , PlotRange -> plotrange[[2]],  
          PlotLabel -> "Im"],
        Plot3D[Abs[Evaluate[a]], xrange, yrange, DisplayFunction -> Identity, 
          AxesLabel -> axeslabels , PlotRange -> plotrange[[3]],  
          PlotLabel -> "Abs"]},
      
      ImageSize -> 600]]

ComplexContourPlotAnimate[a_, xrange_, yrange_, plotpoints_, plotrange_, 
    axeslabels_] :=
  
  Show[GraphicsArray[{ContourPlot[Re[Evaluate[a]], xrange, yrange, 
          DisplayFunction -> Identity, AxesLabel -> axeslabels , 
          PlotRange -> plotrange[[1]],  PlotLabel -> "Re"],
         ContourPlot[Im[Evaluate[a]], xrange, yrange, 
          DisplayFunction -> Identity, AxesLabel -> axeslabels , 
          PlotRange -> plotrange[[2]],  PlotLabel -> "Im"],
        ContourPlot[Abs[Evaluate[a]], xrange, yrange, 
          DisplayFunction -> Identity, AxesLabel -> axeslabels , 
          PlotRange -> plotrange[[3]],  PlotLabel -> "Abs"]},
      
      ImageSize -> 600]]

End[ ]

EndPackage[]

