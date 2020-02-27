# Roxygen documentation

#' Upload a file into our AWS S3 account and return its  public URL.
#'
#' @return An AWS url.
#' @references www.globaltradealert.org
#' @author GTA


# Function infos and parameters  --------------------------------------------

gta_upload_to_aws = function(upload.file.name=NULL,
                             upload.file.path=NULL,
                             upload.destination="gtaupload/Uploads/files/",
                            credential.path="setup/keys/as3.R"){
  
  ## For how to interact with AWS from the command line, see https://docs.aws.amazon.com/cli/latest/userguide/install-cliv2.html
  
  source(credential.path)
  file.name.nogap=gsub("\\s+","_",upload.file.name)
  
  upload.location=gsub("/+","/",paste0(upload.file.path,"/",file.name.nogap))
  
  file.copy(gsub("/+","/",paste0(upload.file.path,"/",upload.file.name)), 
                   upload.location)
  
  system(paste0(aws.cred," aws s3 cp '",upload.location,"' s3://",upload.destination," --acl public-read"))
  
  file.remove(upload.location)
  
  aws.url=paste0("http://s3-eu-west-1.amazonaws.com/",gsub("/+","/",paste0(upload.destination,"/",file.name.nogap)))
  
  
  return(aws.url)
}



