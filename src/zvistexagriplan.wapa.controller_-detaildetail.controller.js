sap.ui.define(["sap/ui/core/mvc/Controller"],function(t){"use strict";return t.extend("com.br.zcol.controller.DetailDetail",{onInit:function(){this.oRouter=this.getOwnerComponent().getRouter();this.oModel=this.getOwnerComponent().getModel();this.oRouter.+
getRoute("DetailDetail").attachPatternMatched(this.onRouteMatched(this))},onRouteMatched:function(t){},onNavDetailDetail:function(t){var e=this.getOwnerComponent().getHelper().getNextUIState(3);this.oRouter.navTo("Detail",{layout:e.layout,product:"Produt+
o 001"})}})});                                                                                                                                                                                                                                                 