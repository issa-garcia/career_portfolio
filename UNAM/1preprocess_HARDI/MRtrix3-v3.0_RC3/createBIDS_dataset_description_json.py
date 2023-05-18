import json
from collections import OrderedDict

data= OrderedDict()
#General fields, shared with MRI BIDS and MEG BIDS:
#Required fields:
#name of the dataset
data['Name'] = 'SUDMEX_TMS Multimodal dwipreproc'

#The version of the BIDS standard that was used
data['BIDSVersion'] = '1.6.1-dev'

#Recommended fields:
#The interpretation of the dataset. MUST be one of "raw" or "derivative". For backwards compatibility, the default value is "raw".
data['DatasetType'] = 'derivative'

#what license is this dataset distributed under? The use of license name abbreviations is suggested for specifying a license. A list of common licenses with suggested abbreviations can be found in appendix III.
data['License'] = 'CC0'


#List of individuals who contributed to the creation/curation of the dataset
data['Authors'] = ['Issa-Garcia V','Gonzalez-Escamilla G','Garza-Villarreal E']

#who should be acknowledged in helping to collect the data
data['Acknowledgements'] = 'We acknowledge the authors of SUDMEX_TMS dataset: Ruth Alcala-Lozano, Sofia Fernandez-Lozano, Erick Morelos-Santana, Alan Davalos, Viviana Villicana and Eduardo A. Garza-Villarreal. We would like to thank Alejandra Torres, Daniela Guerrero Leon, Ernesto Reyes Zamorano, Eden Sanchez Rosas, Hugo Garcia Cantu and Isabel Espinoza Luna for their assistance in conducting the clinical trial. Major thanks to Michael D. Fox and Molly Schineller for providing the normative connectivity maps from fucidals. We also thank the Laboratorio Nacional de Visualizacion Cientifica Avanzada (LAVIS) for the use of their computer cluster and the Laboratorio Nacional de Imagenologia por Resonancia Magnetica (LANIREM). Victor Issa-Garcia and Eduardo Garza-Villarreal would like to thank Direccion General de Calidad y Educacion en Salud, Secretaria de Salud, Mexico for the scholarship support provided to Victor.'

#Instructions how researchers using this dataset should acknowledge the original authors. This field can also be used to define a publication that should be cited in publications that use the dataset
data['HowToAcknowledge'] = ''

#sources of funding (grant numbers)
data['Funding'] = ['CONACYT FOSISS No. 0260971','CONACYT No. 253072','PAPIIT-UNAM IA202120']

#List of ethics committee approvals of the research protocols and/or protocol identifiers.
data['EthicsApprovals'] = ['CEI/C/070/2016']

#a list of references to publication that contain information on the dataset, or links.
data['ReferencesAndLinks'] = ['','','']

#the Document Object Identifier of the dataset (not the corresponding paper).
data['DatasetDOI'] = ''

#Used to specify provenance of the derived dataset. See table below for contents of each object.
data['GeneratedBy'] = [{"Name": 'MRtrix3', "Version": '3.0_RC3', "CodeURL": 'file://./code/script_sgearray_1nomcf.sge'},{"Name": 'FSL', "Version": '6.0.3', "CodeURL": 'file://./code/script_sgearray_1nomcf.sge'},{"Name": 'ANTs', "Version": '2.3.4', "CodeURL": 'file://./code/script_sgearray_1nomcf.sge'}]

#Used to specify the locations and relevant attributes of all source datasets. Valid keys in each object include URL, DOI (see URI), and Version with string values.
data['SourceDatasets'] = [{"URL": 'https://openneuro.org/datasets/ds003037/versions/1.0.1',"DOI": 'doi:10.18112/openneuro.ds003037.v1.0.1',"Version": '1.0.1'}]

root_dir = '/mnt/MD1200B/egarza/public/addimex_tms/derivatives/'
project_label = 'MRtrix3-v3.0_RC3'

dataset_json_folder = root_dir+project_label
dataset_json_name=dataset_json_folder+'/'+'dataset_description.json'

with open(dataset_json_name, 'w') as ff:
    json.dump(data, ff,sort_keys=False, indent=4)
