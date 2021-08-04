endpoint="https://www.bu.edu/dbin/cnsb/CETSA/api.php"

list.projects = function(){
	res=httr::GET(endpoint,query=list(q="project"))
	httr::content(res)
}

list.experiments = function(projectid){
	res=httr::GET(endpoint,query=list(q="experiment",id=projectid))
	httr::content(res)
}

demo.project = function(){
	list(name="new project",lab="some lab",project_type="what is a type?")
}

add.project = function(data){
	if(is.null(data)){
		return(list(status="error",code="empty data"))
	}
	res=httr::POST(endpoint,body=list(q="project",data=data),encode="json")
	#print(res)
	httr::content(res)
}

demo.experiment = function(){
	list(projectID=1,experiment_type="what is a type?",done_by="Ryan",num_samples=2,num_replicates=2,assay_type="what is this?",concentration_range="1:10",temperature_range="1:10",organism="Human",run_date="2020-06-29") # TODO: proteingroups / drug
}

add.experiment = function(data){
	if(is.null(data)){
		return(list(status="error",code="empty data"))
	}
	res=httr::POST(endpoint,body=list(q="experiment",data=data),encode="json")
	#print(res)
	httr::content(res)
}
