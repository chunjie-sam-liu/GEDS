// main js for gsexpr

shinyjs.init = function(){
  // all checked after initiation
  // $("button.btn.checkbtn.btn-default").removeClass("active").addClass("active");
  $('input[type="checkbox"]').prop("checked", true);
};

shinyjs.example_mRNA_set = function(params){
  var defaultParams = {id: null};
  params = shinyjs.getParams(params, defaultParams);
  var selector = $("#" + params.id);
  selector.val("TP53 FOXM1 PTEN MYB");
};
shinyjs.example_protein_set = function(params){
  var defaultParams = {id: null};
  params = shinyjs.getParams(params, defaultParams);
  var selector = $("#" + params.id);
  selector.val("AKT1");
};
shinyjs.example_miRNA_set = function(params){
  var defaultParams = {id: null};
  params = shinyjs.getParams(params, defaultParams);
  var selector = $("#" + params.id);
  selector.val("hsa-let-7b-5p hsa-miR-9-5p hsa-miR-9-3p");
};

shinyjs.TCGAmRNAselectall = function() {
  $("div#select_mRNA_TCGA button.btn.checkbtn.btn-primary:not(.active)").click();
};
shinyjs.TCGAmRNAunselectall = function() {
  $("div#select_mRNA_TCGA button.btn.checkbtn.btn-primary.active").click();
};
shinyjs.GTEXmRNAselectall = function() {
  $("div#select_mRNA_GTEX button.btn.checkbtn.btn-primary:not(.active)").click();
};
shinyjs.GTEXmRNAunselectall = function() {
  $("div#select_mRNA_GTEX button.btn.checkbtn.btn-primary.active").click();
};
shinyjs.CCLEmRNAselectall = function() {
  $("div#select_mRNA_CCLE button.btn.checkbtn.btn-primary:not(.active)").click();
};
shinyjs.CCLEmRNAunselectall = function() {
  $("div#select_mRNA_CCLE button.btn.checkbtn.btn-primary.active").click();
};
shinyjs.TCGAproteinselectall = function() {
  $("div#select_protein_TCGA button.btn.checkbtn.btn-primary:not(.active)").click();
};
shinyjs.TCGAproteinunselectall = function() {
  $("div#select_protein_TCGA button.btn.checkbtn.btn-primary.active").click();
};
shinyjs.MCLPproteinselectall = function() {
  $("div#select_protein_MCLP button.btn.checkbtn.btn-primary:not(.active)").click();
};
shinyjs.MCLPproteinunselectall = function() {
  $("div#select_protein_MCLP button.btn.checkbtn.btn-primary.active").click();
};
shinyjs.TCGAmiRNAselectall = function() {
  $("div#select_miRNA_TCGA button.btn.checkbtn.btn-primary:not(.active)").click();
};
shinyjs.TCGAmiRNAunselectall = function() {
  $("div#select_miRNA_TCGA button.btn.checkbtn.btn-primary.active").click();
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
  $('a', $('.navbar')).each(function(){
    if(this.getAttribute('data-value') == params.id){
      this.click();
      var help = 'div#help_' + params.id;
      setTimeout(function() {$(help).collapse("show");}, 1000);
    }
  });
}