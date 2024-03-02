/**
* Name: elevationtest
* Based on the internal empty template. 
* Author: Gamaliel Palomo
* Tags: 
*/


model elevationtest


global{
	grid_file dem_file <- file("../includes/mdt.tif");
	field terrain <- field(dem_file) ;
	field flow <- field(terrain.columns,terrain.rows);
	geometry shape <- envelope(dem_file);
}


experiment main type:gui{
	output{
		display main_display type:opengl{
			
		}
	}
}