# SOCRATex
SOCRATex: Staged Optimization of Curation, Regularization, and Annotation of clinical Text

## Description
SOCRATex is a natural language processing system which can work on OMOP formated database.  
The system supports unsupervised text clustering to suggest topics on the text documents, and provides text editors for the annotation. 
Using the annotated data or existinf JSON docuemtns, information retrieval of the documents using Elasticsearch is possible.  

## Getting Started
To install and execute SOCRATex, please follow the codes below:

```R
# Following two packages are recommended to install before SOCRATex
install.packages("devtools")
devtools::install_github("OHDSI/DatabaseConnector")
devtools::install_github("OHDSI/SqlRender")

# Install SOCRATex
devtools::install_github("ABMI/SOCRATex")
library(SOCRATex)
SOCRATex()
```
# SOCRATex video
[![Video Label](http://img.youtube.com/vi/F7vR6hkugY4/0.jpg)](https://youtu.be/F7vR6hkugY4)


# Building Elastic Stack
[Elasticsearch](https://www.elastic.co/kr/downloads/elasticsearch) and [Kibana](https://www.elastic.co/kr/downloads/kibana) are recommended to be installed.  
If you want to handle your JSON documents in other ways, [Logstash](https://www.elastic.co/kr/downloads/logstash) or [Beats](https://www.elastic.co/kr/downloads/beats) could be your options
