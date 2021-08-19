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
	list(projectID=1,experiment_type="what is a type?",done_by="Ryan",num_samples=2,num_replicates=2,assay_type="what is this?",concentration_range="1:10",temperature_range="1:10",organism="Human",run_date="2020-06-29",
		results=data.frame(uniprot=c("Q13547","P69905"), drugbankID=c("DB06603","BUCMD001"), drugname=c("","CMD Alpha"), fold_change=c("0,0.004,-0.024,0.041,0.3,0.652,0.729,0.8,0.827,1","0,0.11,0.22,0.33,0.44,0.55,0.66,0.77,0.88,1.0"), conditions=c("cond a","cond b"), min_x=c(1,1), max_x=c(10,10), pEC50=c(7.468661,8.25465), slope=c(1.612325,2.16473),stringsAsFactors=F))
}

add.result = function(data){
	if(is.null(data)){
		return(list(status="error",code="empty data"))
	}
	res=httr::POST(endpoint,body=list(q="result",data=data),encode="json")
	httr::content(res)
}

add.experiment = function(data){
	results=NULL
	if(is.null(data)){
		return(list(status="error",code="empty data"))
	}
	if(!is.null(data$results)){
		results=data$results
	}
	exp_res=httr::POST(endpoint,body=list(q="experiment",data=data),encode="json")
	exp_content=httr::content(exp_res)

	if(exp_content$status=="ok" && is.data.frame(results)){
		results$experimentID=exp_content$ID

		drugbank=read.csv(system.file("extdata", "drugbank.tsv", package="RcetsaAPI"),sep="\t",stringsAsFactors=F)
		results=merge(results,drugbank,by.x="drugbankID",by.y="id",all.x=T)
		i=!is.na(results$drugbankname)
		results$drugname[i]=results$drugbankname[i]

		res_content=lapply(1:nrow(results),FUN=function(i) add.result(as.list(results[i,])))
		status=unlist(sapply(res_content,"[","status"))
		err_i=which(status!="ok")
		if(length(err_i)>0){
			print(res_content)
			exp_content$status=paste0("error in ",length(err_i),"/",length(status))
			exp_content$errors=res_content[err_i]
		}
	}

	exp_content
}
