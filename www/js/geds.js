// main js for gsexpr

shinyjs.init = function(){
  // all checked after initiation
  // $("button.btn.checkbtn.btn-default").removeClass("active").addClass("active");
  $('input[type="checkbox"]').prop("checked", true);
};

shinyjs.example_gene_set = function(params){
  var defaultParams = {id: null};
  params = shinyjs.getParams(params, defaultParams);
  var selector = $("#" + params.id);
  selector.val("AKT FOXM1 PRKAA1");
};
shinyjs.example_protein_set = function(params){
  var defaultParams = {id: null};
  params = shinyjs.getParams(params, defaultParams);
  var selector = $("#" + params.id);
  selector.val("AKT FOXM1 PRKAA1");
};
shinyjs.example_miRNA_set = function(params){
  var defaultParams = {id: null};
  params = shinyjs.getParams(params, defaultParams);
  var selector = $("#" + params.id);
  selector.val("LET7a-3p let7b-3p let-7c3p let-7D-3p");
};


shinyjs.checkall = function() {
  $("button.btn.checkbtn.btn-default").removeClass("active").addClass("active");
  $('input[type="checkbox"]').prop("checked", true);
};


shinyjs.uncheckall = function() {
  $("button.btn.checkbtn.btn-default").removeClass("active");
  $('input[type="checkbox"]').prop("checked", false);
};

shinyjs.switch = function(params){
  var defaultParams = {id: null};
  params = shinyjs.getParams(params, defaultParams);
  var selector = $("#" + params.id);
  selector.change();
};

shinyjs.openTab = function(params){
  var defaultParams = {id: null};
  params = shinyjs.getParams(params, defaultParams);
  $('a', $('.sidebar')).each(function(){
    if(this.getAttribute('data-value') == params.id){
      this.click();
      var help = 'div#help_' + params.id;
      setTimeout(function() {$(help).collapse("show");}, 1000);
    }
  });
}