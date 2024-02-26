/**
* Name: absorption
* This model simulates how water absorption is affected by the buildings result of implementing master plans  
* Author: Gamaliel Palomo and Alejandro Lepe
* Tags: 
*/


model absorption

global{
	file limits_shp <- file("../includes/area_estudio.shp");
	file limits_squared_shp <- file("../includes/area_estudio_squared.shp");
	file study_area <- file("../includes/AEPH.shp");
	file building_shp <- file("../includes/desplante.shp");
	shape_file watershed_shp <- shape_file("../includes/microcuencas_2022.shp");
	grid_file dem_file <- file("../includes/mdt_scaled.tif");
	image_file satellite_image_file <- image_file("../includes/satellite.png");
	image_file satellite2_image_file <- image_file("../includes/satellite_grayscale.png");

	//DEM
	field terrain <- field(dem_file) ;
	field valid_dem <- field(terrain.columns,terrain.rows);	
	
	geometry shape <- envelope(dem_file);
	string scenario parameter:current_scenario <- "current" among:["current","2011"];
	bool show_satellite parameter:satellite <- true;
	bool grayscale_satellite parameter:grayscale_satellite <- false;
	bool show_watershed parameter:watershed <- false;
	string last_scenario <- scenario;
	list<building> active_buildings;
	list<building> new_buildings;
	
	
	//indicators
	float permeability <- 1.0;
	int permeability_per <- 100 update:int(permeability*100);
	string flooding_risk <- "bajo" among:["bajo","moderado","alto"];
	map<string,float> flooding_risk_float <- ["bajo"::0.1,"moderado"::0.4,"alto"::0.8];
	
	init{
		create area from:limits_shp with:[type::"limits"];
		create area from:study_area with:[type::"intervention area"];
		create area from:limits_squared_shp with:[type::"limits_squared"];
		create area from:watershed_shp with:[type::"watershed"];
		
		
		do compute_indicators;
		do filter_valid_pixels;
		create building from:building_shp with:[from_scenario::"current"]{
			drawable <- true;
			add self to:active_buildings;
		}
		ask building{
			do fix_height;
		}
		create building number:200 with:[from_scenario::"2011"]{
			do init_random;
			add self to:new_buildings;
		}
	}
	reflex listen_changes{
		if scenario != last_scenario{
			do switch_scenario;
			last_scenario <- scenario;
		}
	}
	action switch_scenario{
		if scenario = "current"{
			ask active_buildings{
				if from_scenario = "2011"{
					drawable <- false;
					remove self from:active_buildings;
				}
			}
		}
		else{
			ask new_buildings{
				drawable <- true;
				add self to:active_buildings;
			}
		}
		do compute_indicators;
	}
	action compute_indicators{
		area my_study_area <- first(area where(each.type="intervention area"));
		permeability <-1- sum(active_buildings collect(each.shape.area))/my_study_area.shape.area;
		if permeability >0.9{flooding_risk <- "bajo";}
		else if permeability >0.75{flooding_risk <- "moderado";}
		else {flooding_risk <- "alto";}
	}
	 //Actions related to DEM
	 action filter_valid_pixels{
	 	area the_area <- first(area where(each.type="limits"));
	 	list<point> valid_points <- valid_dem points_in the_area-1;
	 	loop pt over:valid_points{
	 		valid_dem[pt] <- terrain[pt]>2092?0:(terrain[pt]<919?0:terrain[pt]);
	 	}
	 } 
}

species building{
	bool drawable <- false;
	string from_scenario;
	int width;
	int length;
	int heigth <- rnd(4,50);
	action fix_height{
		location <- {location.x,location.y,valid_dem[{location.x,location.y}]};
	}
	action init_random{
		location <- any_location_in(one_of(area where(each.type="intervention area")));
		do fix_height;
		width <- rnd(20,100);
		length <- rnd(20,100);
		shape <- rectangle(width,length);
		from_scenario <- "2011";
	}
	aspect default{
		if drawable{
			draw shape color:rgb (97, 203, 201, 255) depth:heigth;
		}
		
	}
}

species area{
	string type;
	aspect default{
		if type = "intervention area"{
			draw shape wireframe:true border:#red width:2.0;
		}
		else if type = "limits"{
			draw shape wireframe:true border:#gray width:2.0;
		}
		else if type="limits_squared" and show_satellite{
			if grayscale_satellite{
				draw shape texture:satellite2_image_file;
			}
			else{
				draw shape texture:satellite_image_file;
			}
		}
		else if type = "watershed" and show_watershed{
			draw shape color:rgb (28, 12, 222, 120) border:#white width:3.0 at:{location.x,location.y,5};
		}
	}
}

experiment main type:gui{
	output{
		display main_display type:opengl background:#black axes:false{
			overlay size:{0,0} position:{0.05,0.05} transparency:0.5 background:#black{
				draw "Escenario: "+scenario at:{70#px,70#px} color:#white font: font("Arial", 75,#bold);
				draw "Área de filtración: "+permeability_per+"%" at:{70#px,125#px} color:#white font:font("Arial", 35,#bold);
				draw "Riesgo de inundación: "+flooding_risk at:{70#px,165#px} color:#white font:font("Arial", 35,#bold);
			}
			camera 'default' location: {1797.5449,38492.9671,12417.5757} target: {12996.2918,17431.1874,0.0};
			mesh valid_dem scale: 1 triangulation: true  color: palette([#black, #saddlebrown, #darkgreen, #green]) refresh: false smooth: true;
			//species area aspect:default;
			species building aspect:default;
		}
	}
}