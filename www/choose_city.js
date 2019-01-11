document.getElementsByClassName("navbar-nav")[0].children[0].onclick = function(){
	document.getElementsByClassName("nonmobile")[0].children[0].children[0].classList.add("techimage4")
	document.getElementsByClassName("nonmobile")[0].children[0].children[0].classList.remove("techimage4reverse")
}

document.getElementsByClassName("navbar-nav")[0].children[1].onclick=function(){
	document.getElementsByClassName("nonmobile")[0].children[0].children[0].classList.remove("techimage4")
	document.getElementsByClassName("nonmobile")[0].children[0].children[0].classList.add("techimage4reverse")
}








window.onscroll = function() {myFunction()};

var sticky_bottom_first = document.getElementsByClassName("tab-pane")[0].getElementsByClassName("finalsection")[0].offsetTop; //This is to set the end for float just above the Select 5 cities
var nav_sticky_offset = document.getElementsByClassName("navbar")[0].offsetTop; //This is to define the main navigation offset
var i = 0

function myFunction() {
	var first_tab_panel = document.getElementsByClassName("tab-pane")[0];
	var second_tab_panel = document.getElementsByClassName("tab-pane")[1];
	if(second_tab_panel.className == "tab-pane active"){
		if(i==0){
			window.sticky_bottom_second = document.getElementsByClassName("tab-pane")[1].getElementsByClassName("finalsection")[0].offsetTop;
			i++
		}
		var filler_sel_2 = document.getElementsByClassName("filler_sel_2")[0];
		var cma_select = second_tab_panel.getElementsByClassName("selectize-metro")[0];
		var final_section = second_tab_panel.getElementsByClassName("finalsection")[0];
		var height_second_tab = cma_select.offsetHeight;
		var start_height_second_tab = document.getElementsByClassName("row")[17].offsetTop + document.getElementsByClassName("row")[17].offsetHeight;
		if(filler_sel_2.style.height=="0px"){
			var start_height_second_tab = start_height_second_tab + height_second_tab
		}
		else{
			var start_height_second_tab = start_height_second_tab + filler_sel_2.offsetHeight
		}

		if (window.pageYOffset > start_height_second_tab && window.pageYOffset < window.sticky_bottom_second *1.1) {
  			cma_select.getElementsByClassName("form-group")[0].classList.add("sticky-selection");
  			cma_select.getElementsByClassName("form-group")[0].style.width="70%";
  			if(window.innerWidth>=1680){
  				cma_select.getElementsByClassName("form-group")[0].style.width="80%";
  			}
  			cma_select.getElementsByClassName("form-group")[0].style.borderBottom="3px solid #e24585";
  			cma_select.getElementsByClassName("form-group")[0].style.maxWidth="1152px";
  			cma_select.getElementsByClassName("form-group")[0].getElementsByClassName("control-label")[0].style.display="table-cell";
  			cma_select.getElementsByClassName("form-group")[0].childNodes[1].style.display="table-cell";
  			cma_select.getElementsByClassName("form-group")[0].childNodes[1].style.width="60%";
  			filler_sel_2.style.height=String(height_second_tab)+"px";

  		} 
  		else {
  			cma_select.getElementsByClassName("form-group")[0].classList.remove("sticky-selection");
  			cma_select.getElementsByClassName("form-group")[0].style.width="300px";
  			cma_select.getElementsByClassName("form-group")[0].style.borderBottom="unset";
  			filler_sel_2.style.height="0px";
  		}
	}
	//First selection city tab codes
	if(first_tab_panel.className == "tab-pane active"){
		var start_height_first_tab = document.getElementsByClassName("row")[0].offsetTop + document.getElementsByClassName("row")[0].offsetHeight
		var cma_select = first_tab_panel.getElementsByClassName("selectize-city")[0]; //Get the seleciton element base
		var filler_sel_1 = document.getElementsByClassName("filler_sel_1")[0];
		var height_first_tab = cma_select.offsetHeight; //Calculate the height so filler can be changed for seamless transition
		if(filler_sel_1.style.height=="0px"){
			var start_height_first_tab = start_height_first_tab + height_first_tab;
		}
		else{
			var start_height_first_tab = start_height_first_tab + filler_sel_1.offsetHeight;
		}
		if(window.pageYOffset > start_height_first_tab && window.pageYOffset < sticky_bottom_first - 200){ //Check proper offset
			cma_select.classList.add("sticky-selection"); //Add the sticky seleciton class
			cma_select.style.width="70%"; //Set the width - needed to override prior class
			if(window.innerWidth>=1680){
  				cma_select.style.width="80%";
  			}
			cma_select.style.borderBottom="3px solid #e24585"; //Set border for distinction
			cma_select.style.maxWidth="1152px"; //Set maximum width in case
			filler_sel_1.style.height=String(height_first_tab) + "px"; //Set filler height

		}
		else{
			cma_select.classList.remove("sticky-selection") //Remove the sticky selection class
			cma_select.style.width="unset"; //Unset width
			cma_select.style.borderBottom="none"; //Unset border
			filler_sel_1.style.height="0px"; //Unset filler height
		}
	}
	var maximum_top_nav = document.getElementsByClassName("row")[36].offsetTop; //Setting the maximum height for nav. It's the about the visualization line
	var nav_bar = document.getElementsByClassName("navbar")[0];
	var filler = document.getElementsByClassName("filler")[0];
	if(window.pageYOffset > nav_sticky_offset){ //Check if y offset on the windows is more than the navigation bar
		nav_bar.classList.add("sticky-selection-nav");
		nav_bar.style.paddingLeft="0";
		nav_bar.style.paddingRight="0";
		filler.style.height="72px";
		var height_nav_limit = nav_bar.offsetTop + nav_bar.offsetHeight;
		if(window.pageYOffset > maximum_top_nav-300){
			nav_bar.classList.remove("sticky-selection-nav")
			nav_bar.style.paddingLeft="0";
			nav_bar.style.paddingRight="0";
			filler.style.height="0px";
		}

	}
	else{
		nav_bar.classList.remove("sticky-selection-nav")
		nav_bar.style.paddingLeft="0";
		nav_bar.style.paddingRight="0";
		filler.style.height="0px";
	}

}