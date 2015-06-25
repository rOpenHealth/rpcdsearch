require(devtools)
load_all()

medical_table <- read.delim("~/hspc//CPRD2014//Lookups//medical.txt", fileEncoding="latin1", stringsAsFactors = FALSE)
drug_table <- read.delim("~/hspc//CPRD2014/Lookups/product.txt", fileEncoding="latin1", stringsAsFactors = FALSE)

def <- MedicalDefinition(terms = list("peripheral vascular disease", "peripheral gangrene", "-wrong answer",
                      "intermittent claudication", "thromboangiitis obliterans",
                      "thromboangiitis obliterans", "diabetic peripheral angiopathy",
                      c("diabetes", "peripheral angiopathy"),
                      c("diabetes", "peripheral angiopathy"),
                      c("buerger",  "disease presenile_gangrene"),
                      "thromboangiitis obliterans",
                      "-rubbish",
                      c("percutaneous_transluminal_angioplasty", "artery"),
                      c("bypass", "iliac_artery"),
                      c("bypass", "femoral_artery"),
                      c("femoral_artery" , "occlusion"),
                      c("popliteal_artery", "occlusion"),
                      "dissecting_aortic_aneurysm", "peripheral_angiopathic_disease",
                      "acrocyanosis", "acroparaesthesia", "erythrocyanosis",
                      "erythromelalgia", "ABPI",
                      c("ankle", "brachial"),
                      c("ankle", "pressure"),
                      c("left", "brachial"),
                      c("left", "pressure"),
                      c("right", "brachial"),
                      c("right", "pressure")),
         codes = list("G73"),
         tests = NULL,
         drugs = list("insulin", "diabet", "aspirin"))

def2 <- import_definition_lists("example_search.csv")

def <- match_list$PAD
def_name <- "terms"
input <- def$terms
a <- build_definition_lists(def, medical_table,drug_table = drug_table)
b <- build_definition_lists(def2, medical_table,drug_table = drug_table)

