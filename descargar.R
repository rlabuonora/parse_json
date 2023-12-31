# Descargar y descomprimir todos los archivos de compras anuales

links <- c(
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/703fc3f1-c251-4f43-a1d5-11380fa741f6/download/ocds-2022.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/16f2facf-1b13-4e71-82f1-13fae7a25cab/download/ocds-2021.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/1ebf3367-321e-4171-a524-0105897e1230/download/ocds-2020.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/b52ec670-bf0d-4607-8a66-92c3b94a6459/download/ocds-2019.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/60c70a33-2515-4f7f-8ae0-850a0e8c822c/download/ocds-2018.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/cfd2f909-9660-4f20-bd68-663557175f9d/download/ocds-2017.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/405b0144-c143-49ea-88cd-5cf19151a24f/download/ocds-2016.zip",  
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/3bfb4fa7-a554-4ff7-b4dd-6d112ece356a/download/ocds-2015.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/1700fb11-1a0c-4829-ade8-7c1891e48b9b/download/ocds-2014.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/3a82f4f6-2d4b-4cdb-8d85-0a25ccc1be59/download/ocds-2013.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/f5731ca1-1a3d-4803-a08b-ed9d7ef0983a/download/ocds-2012.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/27a7ceb7-5a14-4548-9bce-799d51570921/download/ocds-2011.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/02c7c86c-dc34-4471-a916-93799c863fd8/download/ocds-2010.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/b16d7fae-bb86-4ca2-b00a-538d6305410d/download/ocds-2009.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/a7a0bd88-bf19-45eb-a569-0744da10af52/download/ocds-2008.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/0f59e788-0c2f-45b8-9664-d46fd838a3d1/download/ocds-2007.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/1891fc0f-4183-4a90-8463-f9a4b549a40c/download/ocds-2006.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/d422ac87-3ee3-44e8-96e2-d4dc607a492d/download/ocds-2005.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/9c59c983-5f81-479c-9cfd-40eefaa432d3/download/ocds-2004.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/fb8fb9cd-b33f-41bb-8ab7-7355fd082d92/download/ocds-2003.zip",
  "https://catalogodatos.gub.uy/dataset/5203d170-e9ec-44d3-b09c-c0ef02270cc9/resource/eae57d58-1e39-453d-90f7-54bca4a4a33c/download/ocds-2002.zip"
  
)

download_and_extract <- function(url) {
  
  dest <- glue("./json/{path_file(url)}")
  download.file(url, destfile = dest)
  utils::unzip(dest, exdir='./json')
  
}

# Crear directorio
dir.create('./json')
# Descargar y descomprimir jsons
as.list(links) %>% 
  purrr::walk(download_and_extract)