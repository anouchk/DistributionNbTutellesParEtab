library(data.table)

#set working directory
setwd('/Users/analutzky/Desktop/UMR_CNRS/dep1_dotations')

Table_UMR=fread('UMR_tableau_global.csv')

var.names=colnames(Table_UMR)
colnames(Table_UMR)=make.names(var.names)


#### COMPTER LE NOMBRE DE TUTELLES ET DE PARTENAIRES INSTITUTIONNELS PAR UNITE DE RECHERCHE
# Créons une colonne où serait renseigné le nombre de tutelles par UMR, auquel on rajoute 1 pour compter le CNRS 
Table_UMR[,NbTutelle:=sum(Type.partenariat=='Tutelle')+1L,by=Code.Unité.au.31.12.2018]

# Décortiquons :
# Table_UMR[,NbTutelle] on crée un colonne nommée NbTutelle. Si elle existait déjà ça la remplacerait
# NbTutelle:= On assigne à la variable NbTutelle l'opétation suivante
# Type.partenariat=='Tutelle' c'est une variable bouléenne qui vaut true ou false
# Donc la somme c'est le nb de fois où c'est égal à true, c'est-à-dire où lacondition est vérifiée
# by=Code.Unité.au.31.12.2018 par UMR distincte

# pourquoi 1L au lieu de 1 ?
# Type of RHS ('double') must match LHS ('integer'). To check and coerce would impact performance too much for the fastest cases. Either change the type of the target column, or coerce the RHS of := yourself (e.g. by using 1L instead of 1)
# En fait R ne t'embête pas avec les types, mais data.table ne fait pas le boulot pour aller plus vite. Donc il faut l'aider.

# Idem pour le nombre de partenaires institutionnels
Table_UMR[,NbPartenaireInstitutionel:=sum(Type.partenariat=='Partenaire institutionnel'),by=Code.Unité.au.31.12.2018]


##### FAIRE UN TABLEAU AVEC, PAR ETABLISSEMENT, LE NOMBRE D'UNITES DONT IL EST CO-TUTELLE AVEC LE CNRS et aux côtés de combien d'autres co-tutélaires (2 tutelles, 3, voire 9 !)
# Subtilité 1 : On ne prend que les UMR, USR, UPR, ERL et FRE (on laisse de côté des UMS, UMI, FR)
# Subtilité 2 : on ne regarde que les tutelles (pas les partenariats institutionnels, dits aussi "tutelles secondaires")
DetailEtablissement = Table_UMR[type.d.unité%in%c('UMR','UPR','USR','ERL','FRE') & Type.partenariat=='Tutelle',.(count=length(Code.Unité.au.31.12.2018)),by=.(Etablissement,NbTutelle)]
# Avec les noms cours c'est plus parlant
DetailEtablissement = Table_UMR[type.d.unité%in%c('UMR','UPR','USR','ERL','FRE') & Type.partenariat=='Tutelle',.(count=length(Code.Unité.au.31.12.2018)),by=.(nom.court,NbTutelle)]
write.csv2(as.data.frame(DetailEtablissement),file='Detail_melt_tutelles_par_etab',fileEncoding = "UTF8")



# On met tout ça dans l'ordre alphabétique des établissements, et l'ordre décroissant du nb de tutelles je crois :
DetailEtablissement[order(Etablissement,NbTutelle)]
DetailEtablissement[order(nom.court,NbTutelle)]
# Puis on met une colonne par nombre de tutelles possibles (de 1 à 9 dans notre cas), pour avoir, pour chaque ligne, ie établissement, la répartition
DetailEtablissement = dcast(DetailEtablissement,Etablissement~NbTutelle,fill=0)
DetailEtablissement = dcast(DetailEtablissement,nom.court~NbTutelle,fill=0)


write.csv2(as.data.frame(DetailEtablissement),file='Detail_tutelles_par_etablissement',fileEncoding = "UTF8")

##### FAIRE UN TABLEAU AVEC, PAR TYPE D'UNITE, LE NOMBRE D'UNITES DONT IL EST CO-TUTELLE AVEC LE CNRS 
# On aimerait avoir cette info par type d'unité de recherche
# il faut commencer par dédoublonner :
TablE_UMR_dedoublonnée = unique(Table_UMR, by='Code.Unité.au.31.12.2018')
# Puis on fait pareil que pour les établissements :
DetailTypeUnité = TablE_UMR_dedoublonnée[,.(count=length(Code.Unité.au.31.12.2018)),by=.(type.d.unité,NbTutelle)]
DetailTypeUnité = dcast(DetailTypeUnité,type.d.unité~NbTutelle,fill=0)


write.csv2(as.data.frame(DetailTypeUnité),file='Detail_tutelles_par_type_unite',fileEncoding = "UTF8")