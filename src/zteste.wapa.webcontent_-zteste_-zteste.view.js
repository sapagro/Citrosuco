sap.ui.jsview("zteste.zteste", {                                                                                                                                                                                                                               
                                                                                                                                                                                                                                                               
	/** Specifies the Controller belonging to this View.&nbsp;                                                                                                                                                                                                    
	* In the case that it is not implemented, or that "null" is returned, this View does not have a Controller.                                                                                                                                                   
	* @memberOf zteste.zteste                                                                                                                                                                                                                                     
	*/&nbsp;                                                                                                                                                                                                                                                      
	getControllerName : function() {                                                                                                                                                                                                                              
		return "zteste.zteste";                                                                                                                                                                                                                                      
	},                                                                                                                                                                                                                                                            
                                                                                                                                                                                                                                                               
	/** Is initially called once after the Controller has been instantiated. It is the place where the UI is constructed.&nbsp;                                                                                                                                   
	* Since the Controller is given to this method, its event handlers can be attached right away.&nbsp;                                                                                                                                                          
	* @memberOf zteste.zteste                                                                                                                                                                                                                                     
	*/&nbsp;                                                                                                                                                                                                                                                      
	createContent : function(oController) {                                                                                                                                                                                                                       
 		return new sap.m.Page({                                                                                                                                                                                                                                     
			title: "Title",                                                                                                                                                                                                                                             
			content: [                                                                                                                                                                                                                                                  
			                                                                                                                                                                                                                                                            
			]                                                                                                                                                                                                                                                           
		});                                                                                                                                                                                                                                                          
	}                                                                                                                                                                                                                                                             
                                                                                                                                                                                                                                                               
});                                                                                                                                                                                                                                                            