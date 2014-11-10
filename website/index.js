/*
 * Broke away from index.html: 5 aug, 2005
 * last modified: 5 aug, 2005
 *
 *
 * If you want to add new functions to the list or make changes to existing
 * ones,  simply modify the entries in var info below. The following is an
 * explanation of the fields for the entries in var info.
 *
 * name:		Name of the function that appearsin the list
 * numFrames:	The number of frames the animation has
 * timeName:	A list of the names to display at each tick of the clock.
 * prefix3D, prefixContour:	This script attaches a frame number to this prefix
 * 		and then adds .gif to find the picture to display at each time step.
 * text:		The URL to link to in for more information.
*/
 
var NUM_PANES = 2
var IMG_POSTFIX = ".gif"

var ticks = [0, 0]
var paused = [true, false]
var paneFunction = [1, 0] // contains the indices of the functions being displayed
var currentPane = 0 	// pane to display newly selected functions
var info = [{name:			"Blank",
			 timeName:		["Blank"],
			 numFrames:		1,
			 prefix3D:		"blank",
			 prefixContour:	"blankcontour",
			 text:			[""] },

			{name:			"K[s+z*i, 1+t*i]",
			 timeName:		["K[s+z*i, 1<em class='changer'>-5i</em>, 1]",
							 "K[s+z*i, 1<em class='changer'>-4i</em>, 1]",
   							 "K[s+z*i, 1<em class='changer'>-3i</em>, 1]",
							 "K[s+z*i, 1<em class='changer'>-2i</em>, 1]",
							 "K[s+z*i, 1<em class='changer'>-1i</em>, 1]",
							 "K[s+z*i, 1<em class='changer'>-0i</em>, 1]",
							 "K[s+z*i, 1<em class='changer'>+i</em>, 1]",
							 "K[s+z*i, 1<em class='changer'>+2i</em>, 1]",
							 "K[s+z*i, 1<em class='changer'>+3i</em>, 1]",
							 "K[s+z*i, 1<em class='changer'>+4i</em>, 1]"],
			 numFrames:		10,
			 prefix3D:		"BabyK-s-real-s-imag-z-imag/3D",
			 prefixContour:	"BabyK-s-real-s-imag-z-imag/contour",
			 text:			"BabyK/index.html"},

			{name:			"K1[t+2*i, a, b]",
			 timeName:		["K1[<em class='changer'>1.0</em>+2*i, a, b]",
							 "K1[<em class='changer'>1.5</em>+2*i, a, b]",
							 "K1[<em class='changer'>2.0</em>+2*i, a, b]",
							 "K1[<em class='changer'>2.5</em>+2*i, a, b]",
							 "K1[<em class='changer'>3.0</em>+2*i, a, b]",
							 "K1[<em class='changer'>3.5</em>+2*i, a, b]",
							 "K1[<em class='changer'>4.0</em>+2*i, a, b]",
							 "K1[<em class='changer'>4.5</em>+2*i, a, b]",
							 "K1[<em class='changer'>5.0</em>+2*i, a, b]"],
			 numFrames:		9,
			 prefix3D:		"K1-s-real-a-b/3D",
			 prefixContour: "K1-s-real-a-b/contour",
			 text:			"MatrixK/index.html"},

			{name:			"K2",
			 timeName:		["K2[{1+I, 1+I}, A, B]"],
			 numFrames:		1,
			 prefix3D:		"K2/3D",
			 prefixContour:	"K2/contour",
			 text:			["MatrixK/index.html"] },

			{name:			"K3 singular argument",
			 timeName:		["K3[{0, 0, 1+I}, 1, A, B]"],
			 numFrames:		1,
			 prefix3D:		"K3-singular/3D",
			 prefixContour:	"K3-singular/contour",
			 text:			["MatrixK/index.html"] },

			{name:			"k[s+z*i, 1+t*i, 1]",
			 timeName:		["k[s+z*i, 1+<em class='changer'>i</em>, 1]",
							 "k[s+z*i, 1+<em class='changer'>2i</em>, 1]",
   							 "k[s+z*i, 1+<em class='changer'>3i</em>, 1]",
							 "k[s+z*i, 1+<em class='changer'>4i</em>, 1]",
							 "k[s+z*i, 1+<em class='changer'>5i</em>, 1]",
							 "k[s+z*i, 1+<em class='changer'>6i</em>, 1]",
							 "k[s+z*i, 1+<em class='changer'>7i</em>, 1]",
							 "k[s+z*i, 1+<em class='changer'>8i</em>, 1]",
							 "k[s+z*i, 1+<em class='changer'>9i</em>, 1]",
							 "k[s+z*i, 1+<em class='changer'>10i</em>, 1]"],
			 numFrames:		10,
			 prefix3D:		"Babyk-s-real-s-imag-z-imag/3D",
			 prefixContour:	"Babyk-s-real-s-imag-z-imag/contour",
			 text:			"Babyk/index.html"},

			{name:			"k11[{1+i, i}, {t, x}, y]",
			 timeName:		["k11[{1+i, i}, {<em class='changer'>-5</em>, x}, y]",
							 "k11[{1+i, i}, {<em class='changer'>-4</em>, x}, y]",
							 "k11[{1+i, i}, {<em class='changer'>-3</em>, x}, y]",
							 "k11[{1+i, i}, {<em class='changer'>-2</em>, x}, y]",
							 "k11[{1+i, i}, {<em class='changer'>-1</em>, x}, y]",
							 "k11[{1+i, i}, {<em class='changer'>0</em>, x}, y]",
							 "k11[{1+i, i}, {<em class='changer'>1</em>, x}, y]",
							 "k11[{1+i, i}, {<em class='changer'>2</em>, x}, y]",
							 "k11[{1+i, i}, {<em class='changer'>3</em>, x}, y]",
							 "k11[{1+i, i}, {<em class='changer'>4</em>, x}, y]"],
			 numFrames:		10,
			 prefix3D:		"k11-x1-x2-y/3D",
			 prefixContour: "k11-x1-x2-y/contour",
			 text:			"Matrixk/index.html"},

			{name:			"k11[{1+i, t*i}, a, b]",
			 timeName:		["k11[1+i, <em class='changer'>-5i</em>, a, b]",
							 "k11[1+i, <em class='changer'>-4i</em>, a, b]",
							 "k11[1+i, <em class='changer'>-3i</em>, a, b]",
							 "k11[1+i, <em class='changer'>-2i</em>, a, b]",
							 "k11[1+i, <em class='changer'>-1i</em>, a, b]",
							 "k11[1+i, <em class='changer'>0</em>, a, b]",
							 "k11[1+i, <em class='changer'>1i</em>, a, b]",
							 "k11[1+i, <em class='changer'>2i</em>, a, b]",
							 "k11[1+i, <em class='changer'>3i</em>, a, b]",
							 "k11[1+i, <em class='changer'>4i</em>, a, b]"],
			 numFrames:		10,
			 prefix3D:		"k11-s2-imag-x-y/3D",
			 prefixContour: "k11-s2-imag-x-y/contour",
			 text:			"Matrixk/index.html"	},

			{name:			"k12[{{a, 0, 0},{0, b, 0},  {0, 0, t}}, (1, 1)]",
			 timeName:		["k12[{{a, 0, 0}, {0, b, 0},  {0, 0, <em class='changer'>9</em>}}, (1, 1)]",
							 "k12[{{a, 0, 0}, {0, b, 0},  {0, 0, <em class='changer'>10</em>}}, (1, 1)]",
							 "k12[{{a, 0, 0}, {0, b, 0},  {0, 0, <em class='changer'>11</em>}}, (1, 1)]",
							 "k12[{{a, 0, 0}, {0, b, 0},  {0, 0, <em class='changer'>12</em>}}, (1, 1)]",
							 "k12[{{a, 0, 0}, {0, b, 0},  {0, 0, <em class='changer'>13</em>}}, (1, 1)]",
							 "k12[{{a, 0, 0}, {0, b, 0},  {0, 0, <em class='changer'>14</em>}}, (1, 1)]",
							 "k12[{{a, 0, 0}, {0, b, 0},  {0, 0, <em class='changer'>15</em>}}, (1, 1)]",
							 "k12[{{a, 0, 0}, {0, b, 0},  {0, 0, <em class='changer'>16</em>}}, (1, 1)]",
							 "k12[{{a, 0, 0}, {0, b, 0},  {0, 0, <em class='changer'>17</em>}}, (1, 1)]",
							 "k12[{{a, 0, 0}, {0, b, 0},  {0, 0, <em class='changer'>18</em>}}, (1, 1)]"],
			 numFrames:		10,
			 prefix3D:		"k12-V-W11-W22/3D",
			 prefixContour: "k12-V-W11-W22/contour",
			 text:			"Matrixk/index.html"},

			{name:			"k12[{{a, 0, 0},{0, 1, 0},  {0, 0, b}}, (1, t)]",
			 timeName:		["k12[{{a, 0, 0},{0, 1, 0},  {0, 0, b}}, (1, <em class='changer'>4</em>)]",
							 "k12[{{a, 0, 0},{0, 1, 0},  {0, 0, b}}, (1, <em class='changer'>5</em>)]",
							 "k12[{{a, 0, 0},{0, 1, 0},  {0, 0, b}}, (1, <em class='changer'>6</em>)]",
							 "k12[{{a, 0, 0},{0, 1, 0},  {0, 0, b}}, (1, <em class='changer'>7</em>)]",
							 "k12[{{a, 0, 0},{0, 1, 0},  {0, 0, b}}, (1, <em class='changer'>8</em>)]",
							 "k12[{{a, 0, 0},{0, 1, 0},  {0, 0, b}}, (1, <em class='changer'>9</em>)]",
							 "k12[{{a, 0, 0},{0, 1, 0},  {0, 0, b}}, (1, <em class='changer'>10</em>)]",
							 "k12[{{a, 0, 0},{0, 1, 0},  {0, 0, b}}, (1, <em class='changer'>11</em>)]",
							 "k12[{{a, 0, 0},{0, 1, 0},  {0, 0, b}}, (1, <em class='changer'>12</em>)]",
							 "k12[{{a, 0, 0},{0, 1, 0},  {0, 0, b}}, (1, <em class='changer'>13</em>)]"],
			 numFrames:		10,
			 prefix3D:		"k12-V-W22-A2/3D",
			 prefixContour: "k12-V-W22-A2/contour",
			 text:			["Matrixk/index.html"]},

			{name:			"k21[t+a*i, b]",
			 timeName:		["k21[<em class='changer'>1.0</em>+a*i, b]",
							 "k21[<em class='changer'>1.2</em>+a*i, b]",
							 "k21[<em class='changer'>1.4</em>+a*i, b]",
							 "k21[<em class='changer'>1.6</em>+a*i, b]",
							 "k21[<em class='changer'>1.8</em>+a*i, b]",
							 "k21[<em class='changer'>2.0</em>+a*i, b]",
							 "k21[<em class='changer'>2.2</em>+a*i, b]",
							 "k21[<em class='changer'>2.4</em>+a*i, b]",
							 "k21[<em class='changer'>2.6</em>+a*i, b]",
							 "k21[<em class='changer'>2.8</em>+a*i, b]"],
			 numFrames:		10,
			 prefix3D:		"k21-sreal-simag-a/3D",
			 prefixContour: "k21-sreal-simag-a/contour",
			 text:			["Matrixk/index.html"]},

]

/*
 * gotten from http://www.htmlcodetutorial.com/linking/linking_famsupp_72.html
 * 5 August 2005
 */
function popup(mylink, windowname)
{
	if (! window.focus)return true;
	var href;
	if (typeof(mylink) == 'string')
	href=mylink;
	else
	href=mylink.href;
	window.open(href, windowname, 'width=700,height=500,scrollbars=yes');
	return false;
}

/*
 * Pauses or unpauses the currently selected pane
 */
function pause(){
	paused[currentPane] = !paused[currentPane]
}

/*
 * This is called every time "the second hand" moves
 */
function onTick(){
	for(var i = 0; i < NUM_PANES; i++){
		if(!paused[i]){
			ticks[i] = (ticks[i] + 1) % info[paneFunction[i]].numFrames
		}
	}

	displayFunction()
}

/*
 * num		integer
 *
 * Changes the current pane
 */
function changeCurrentPane(num){
	currentPane = num
	document.getElementById("label" + num).setAttribute("class", "selected")
	document.getElementById( "label" + ((num+1)%2) ).setAttribute("class", "unselected")
}
 
/* 
 * list 		Listbox (i.e. <select>)
 *
 * populates the listbox with functions
 */
function populateList(list){
	for(var i = 0; i < info.length; i++){
		list.options[i] = new Option(info[i].name, info[i].name)
	}
}

/*
 * id	number
 * 
 * Places the selected function into a pane of its own.
 */
function selectFunction( id ){
	paneFunction[currentPane] = id

	// now reset everything for the currentPane
	ticks[currentPane] = 0
	paused[currentPane] = true
	
	displayFunction()
}
 
/*
 * func		integer
 *
 * displays the the selected function's visualizations and stuff in the current
 * pane.
 */
function displayFunction(){

	// change the data one pane at a time
	for(var i = 0; i < NUM_PANES; i++){
		
		var currFunc = info[paneFunction[i]]
		var currTick = ticks[i]

		document.getElementById("label" + i).innerHTML = currFunc.timeName[currTick]
		
		document.getElementById("3D" + i).src = currFunc.prefix3D + currTick +
IMG_POSTFIX
IMG_POSTFIX 

		document.getElementById("contour" + i).src = currFunc.prefixContour +
currTick + IMG_POSTFIX
		
		document.getElementById("text" + i).href = currFunc.text
	}//for
}

/* This is called when the document loads */
function main(){
	populateList(document.getElementById("functions"))
	displayFunction()
	setInterval("onTick()", 1000)
}

