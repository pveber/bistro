open Core_kernel.Std
open Types
open Bistro.EDSL_sh

type output = [`deseq2_output] directory

let wrapper_package = workflow [
    mkdir_p (dest // "bin") ;
    wget
      "https://raw.githubusercontent.com/pveber/compbio-scripts/master/deseq2-wrapper/0.0.1/deseq2-wrapper.R"
      ~dest:(dest // "bin/deseq2-wrapper.R") () ;
    cmd "chmod" [
      string "u+x" ;
      (dest // "bin/deseq2-wrapper.R")
    ]
  ]

let wrapper factors samples =
  let factors = opt "--factors" (list string ~sep:",") factors in
  let samples = List.map samples ~f:(fun (factor_vals, counts) ->
      seq [ list string ~sep:"," factor_vals ; string "," ; dep counts ]
    )
  in
  let outdir = opt "--outdir" ident dest in
  workflow [
    cmd ~path:[wrapper_package] "deseq2-wrapper.R" (outdir :: factors :: samples) ;
  ]


let wrapper factors samples =
  Bistro.Workflow.make ~descr:"deseq2.wrapper" [%R {|
factor_names <- c({{ list (string % quote ~using:'"') ~sep:"," factors }})
sample_files <- c({{ list (snd % dep % quote ~using:'"') ~sep:"," samples }})
description <- as.data.frame(
                 matrix(
                   c({{ list (fst % list (string % quote ~using:'"') ~sep:",") ~sep:"," samples }}),
                   ncol = {{ int (List.length factors) }},
                   byrow = F))
colnames(description) <- factor_names
rownames(description) <- NULL

### DATA PROCESSING
loadCounts <- function(sample_files) {
    loadFile <- function(fn) {
        d <- read.table(fn,header=F,sep='\t')
        d[1:(dim(d)[1] - 5),2]
    }
    sapply(sample_files,loadFile)
}

loadIds <- function(samples_files) {
    d <- read.table(sample_files[1],header=F,sep='\t')
    d[1:(dim(d)[1] - 5),1]
}

differentialAnalysis <- function(counts, description) {
    DESeq(DESeqDataSetFromMatrix(countData=counts,
                                 colData=description,
                                 design=as.formula(paste("~", paste(colnames(description),sep=" + ")))),
          fitType='local')
}

### OUTPUT
outputForAllCoeffs <- function(ids, dds) {
    for(name in resultsNames(dds)) {
        fn <- paste(outdir,paste(name,"tsv",sep="."),sep='/')
        res <- cbind(data.frame(id = ids), results(dds,name=name))
        write.table(res,file=fn,row.names=F,sep='\t')
    }
}

html_li <- function(x) paste("<li>", x, "</li>")

html_ul <- function(xs) {
    paste(
        "<ul>", 
        sapply(xs, html_li),
        "</ul>"
    )
}

html_a <- function(href,text) {
    paste("<a href=",
          href,
          ">",
          text,
          "</a>")
}

html_append <- function(x) {
    cat(x,
        file = HTMLGetFile(),
        append = T,
        sep = ""
        )
}


htmlIndex <- function(dds) {
    HTMLStart(outdir=outdir, file="index", extension="html", echo=F, HTMLframe=F, Title="DESeq report")
    HTML.title("DESeq report", HR=1)
    HTML.title("Differentially expressed genes", HR=3)
    html_append(
        html_ul(sapply(resultsNames(dds), function(x) html_a(paste(x,"tsv",sep="."),x)))
    )
    HTMLStop()
}

main <- function() {
    ids <- loadIds(sample_files)
    counts <- loadCounts(sample_files)
    dds <- differentialAnalysis(counts, description)
    system(paste("mkdir -p", outdir))
    outputForAllCoeffs(ids, dds)
    htmlIndex(dds)
}


#main()
q("no",1)
|}]



let index_of_wrapper_output o = Bistro.Workflow.extract o [ "index.html" ]
