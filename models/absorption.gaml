/**
* Name: absorption
* This model simulates how water absorption is affected by the buildings result of implementing master plans  
* Author: Gamaliel Palomo and Alejandro Lepe
* Tags: 
*/


model absorption

global{
	file limits_shp <- file("../includes/cuenca_rancho_contento.shp");
	file limits_squared_shp <- file("../includes/area_estudio_squared.shp");
	file study_area <- file("../includes/AEPH.shp");
	file building_shp <- file("../includes/desplante.shp");
	shape_file watershed_shp <- shape_file("../includes/microcuencas_2022.shp");
	grid_file dem_file <- file("../includes/mdt_scaled_clipped.tif");
	//image_file satellite_image_file <- image_file("../includes/satellite.png");
	//image_file satellite2_image_file <- image_file("../includes/satellite_grayscale.png");

	//Scenario geometry
	geometry shape <- envelope(dem_file);
	
	//DEM and water diffusion stuff
	field terrain <- field(dem_file) ;
	field valid_terrain <- field(terrain.columns, terrain.rows);
	field flow <- field(terrain.columns,terrain.rows);	
	field hf <- field(terrain.columns,terrain.rows);
	list<point> points <- flow points_in shape;
	list<point> valid_points;
	map<point, list<point>> neighbors;
	
	map<point, float> h;
	map<point, float> heights <- [];
	list<point> source_cells <- [];
	map<point, bool> done;
	
	int frequence_input <- 1;
	float diffusion_rate <- 0.8;
	float input_water<-0.001 min:0.0 max:1.0 parameter:"rain";
	
	string scenario;
	string rain_area <- "high_areas";
	bool grayscale_satellite;
	bool show_satellite;
	bool show_watershed;
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
		
		//do compute_indicators;
		
		do filter_valid_pixels;
		
		//Init maps that depend of valid_points
		h <- valid_points as_map (each::valid_terrain[each]);
		neighbors  <- valid_points as_map (each::(valid_points closest_to (each,8)));
		
		
		loop c from:0 to:flow.columns{
			loop r from: 0 to:flow.rows{
				flow[{c,r}]<- -1.0;
				hf[{c,r}]<- -1.0;
			}
		}
		loop pt over:points-valid_points{
			terrain[pt]<- -1.0;
			flow[pt]<- -1.0;
		}
		loop pt over: valid_points  {
			flow[pt] <- 0.0;//valid_terrain[pt];
		}
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
		
		source_cells <- valid_points where(valid_terrain[each]>1800);
		
	}
	reflex listen_changes{
		if scenario != last_scenario{
			do switch_scenario;
			last_scenario <- scenario;
		}
	}
	
	reflex rain_high_areas when: every(frequence_input#cycle) and rain_area="high_areas"{
		loop p over:source_cells{
			flow[p] <- flow[p] + input_water;
		}
	}
	
	//Reflex to add water to all the cells
	reflex rain when: every(frequence_input#cycle) and rain_area="overall"{
		loop p over: valid_points {
			flow[p] <- flow[p] + input_water;
		}
	}
	float height (point c) {
		return h[c] + flow[c];
	}
	
	//Reflex to flow the water according to the altitude and the obstacle
	reflex flowing {
		done <- valid_points as_map (each::false);
		heights <- valid_points as_map (each::height(each));
		list<point> water <- valid_points where (flow[each] > 0) sort_by (heights[each]);
		loop p over: valid_points - water {
			done[p] <- true;
		}
		int tmp_c <- 0;
		loop p over: water {
			list<point> selected_nb <- neighbors[p] where(done[each] and height(p) > heights[each]) sort_by heights[each];
			loop flow_cell over:  selected_nb{ //{
				float water_flowing <- max(0.0, min((height(p) - heights[flow_cell]), flow[p] * diffusion_rate));
				flow[p] <- flow[p] - water_flowing;
				flow[flow_cell] <- flow[flow_cell] + water_flowing;
				heights[p] <- height(p) ;
				hf[p] <- heights[p];
				heights[flow_cell] <- height(flow_cell) ;
			}
			done[p] <- true;
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
	 	geometry valid_area <- first(limits_shp);
	 	valid_points <- points where(each overlaps valid_area);
	 	loop pt over:valid_points{
	 		valid_terrain[pt] <- terrain[pt]>2092?2092:(terrain[pt]<1621?-1.0:terrain[pt]);
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
		location <- {location.x,location.y,terrain[{location.x,location.y}]};
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
		/*else if type="limits_squared" and show_satellite{
			if grayscale_satellite{
				draw shape texture:satellite2_image_file;
			}
			else{
				draw shape texture:satellite_image_file;
			}
		}*/
		else if type = "watershed" and show_watershed{
			draw shape color:rgb (28, 12, 222, 120) border:#white width:3.0 at:{location.x,location.y,5};
		}
	}
}

experiment main type:gui parent:physical_world{
	
	parameter "watershed" var:show_watershed init:false;
	parameter "show satellite" var:show_satellite init:false;
	parameter "current scenario" var:scenario init:"current" among:["current","2011"];
	parameter "type of rain" var:rain_area init:"high_areas" among:["high_areas","overall"];
	output{
		layout #split;
		display main_display type:opengl background:#black axes:false{
			overlay size:{0,0} position:{0.05,0.05} transparency:0.5 background:#black{
				draw "Escenario: "+scenario at:{70#px,70#px} color:#white font: font("Arial", 75,#bold);
				draw "Área de filtración: "+permeability_per+"%" at:{70#px,125#px} color:#white font:font("Arial", 35,#bold);
				draw "Riesgo de inundación: "+flooding_risk at:{70#px,165#px} color:#white font:font("Arial", 35,#bold);
			}
			//camera 'default' location: {1894.7121,15588.966,8311.7077} target: {5926.385,3180.7526,0.0};
			mesh valid_terrain scale: 1 triangulation: true  color: palette([#black, #saddlebrown, #darkgreen, #green]) refresh: false smooth: true no_data:-1.0;
			mesh hf scale: 1 triangulation: true color: palette(reverse(brewer_colors("Blues"))) transparency: 0.7 no_data:-1.0 ;
			//species area aspect:default;
			species building aspect:default;
		}
		display 2d type:java2D{
			mesh valid_terrain scale: 1 triangulation: true  color: palette([#black, #saddlebrown, #darkgreen, #green]) refresh: false smooth: true no_data:-1.0;
			mesh flow scale: 1 triangulation: true color: palette(reverse(brewer_colors("Blues"))) transparency: 0.5 no_data:-1.0 ;
		}
	}
}