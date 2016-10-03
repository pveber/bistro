open Core_kernel.Std
open Bistro.Std
open Defs
open Bistro.EDSL

class type table = object
  inherit [ < header : [`yes] ; .. > ] tsv
end

type output =
  <
    comparison_summary : table workflow ;
    comparisons : ((string * string * string) * table workflow) list ;
    effect_table : table workflow ;
    normalized_counts : table workflow ;
    sample_clustering : svg workflow ;
    sample_pca : svg workflow ;
  >

let env = docker_image ~account:"pveber" ~name:"bioconductor" ~tag:"3.3" ()

let wrapper_script = {|
library(DESeq2)
library(gplots)
library(RColorBrewer)

### DATA PROCESSING
loadCounts <- function(sample_files) {
    loadFile <- function(fn) {
        d <- read.table(fn,header=F,sep='\t')
        d[1:(dim(d)[1] - 5),2]
    }
    sapply(sample_files,loadFile)
}

loadIds <- function(sample_files) {
    d <- read.table(sample_files[1],header=F,sep='\t')
    d[1:(dim(d)[1] - 5),1]
}

differentialAnalysis <- function(counts, description) {
    DESeq(DESeqDataSetFromMatrix(countData=counts,
                                 colData=description,
                                 design=as.formula(paste("~", paste(colnames(description),collapse=" + ")))),
          fitType='local')
}

my.summary.results <- function(object) {
    alpha <- 0.1
    notallzero <- sum(object$baseMean > 0)
    up <- sum(object$padj < alpha & object$log2FoldChange > 0,
              na.rm = TRUE)
    down <- sum(object$padj < alpha & object$log2FoldChange <
                    0, na.rm = TRUE)
    filt <- sum(!is.na(object$pvalue) & is.na(object$padj))
    outlier <- sum(object$baseMean > 0 & is.na(object$pvalue))
    c(notallzero, up, down, filt, outlier)
}

### OUTPUT
outputForAllComparisons <- function(description, outdir, factor_names, ids, dds) {
    recap <- data.frame(gene = ids)
    stats <- data.frame(comparison = character(0),
                        expressed = integer(0),
                        up = integer(0),
                        down = integer(0),
                        filt = integer(0),
                        outlier = integer(0), stringsAsFactors=F)
    for(f in factor_names) {
        l <- unique(description[,f])
        for(i in 1:length(l))
            for(j in if(i+1 > length(l)) c() else (i + 1):length(l)) {
                label <- paste0(f,"_",l[i],"_",l[j])

                res <- results(dds,contrast=c(f,as.character(l[i]),as.character(l[j])))

                fn <- paste0(outdir,"/results_",label,".tsv")
                write.table(cbind(data.frame(id = ids), res),file=fn,row.names=F,sep='\t',quote=F)

                recap[,paste0(label,"_l2fc")] <- res[,"log2FoldChange"]
                recap[,paste0(label,"_padj")] <- res[,"padj"]

                stats[dim(stats)[1]+1,] <- c(label,my.summary.results(res))

                svg(paste0(outdir,"/MA_plot_",label,".svg"), width=7, height=3.5)
                plotMA(res,main=label)
                dev.off()
            }
    }

    write.table(recap, file = paste0(outdir,"/recap.tsv"),row.names=F,sep='\t',quote=F)
    write.table(stats, file = paste0(outdir,"/summary.tsv"),row.names=F,sep='\t',quote=F)
}


generalPlots <- function(description, outdir, factor_names, ids, dds) {
    rld <- rlog(dds)
    rldMat <- assay(rld)
    rldDist <- dist(t(rldMat))
    mat <- as.matrix(rldDist)
    rownames(mat) <- colnames(mat) <- apply(description, 1, paste, collapse = ":")
    hc <- hclust(rldDist)
    hmcol <- colorRampPalette(brewer.pal(9,"GnBu"))(100)

    svg(paste0(outdir,"/sample_clustering.svg"))
    heatmap.2(mat, Rowv = as.dendrogram(hc), symm = TRUE, trace = "none", col = rev(hmcol), margin = c(13,13))
    dev.off()

    svg(paste0(outdir,"/sample_pca.svg"))
    print(plotPCA(rld, intgroup = factor_names))
    dev.off()

    counts <- cbind(ids,as.data.frame(counts(dds,normalized=T)))
    write.table(counts, file = paste0(outdir,"/normalized_counts.tsv"),row.names=F,sep='\t',quote=F,col.names=F)
}

main <- function(outdir, factor_names, sample_files, conditions) {
    description <- as.data.frame(conditions)
    colnames(description) <- factor_names
    rownames(description) <- NULL
    ids <- loadIds(sample_files)
    counts <- loadCounts(sample_files)
    dds <- differentialAnalysis(counts, description)
    system(paste("mkdir -p", outdir))
    outputForAllComparisons(description, outdir, factor_names, ids, dds)
    generalPlots(description, outdir, factor_names, ids, dds)
}
|}

let app fn args =
  seq ~sep:"" [
    string fn ; string "(" ;
    seq ~sep:"," args ;
    string ")" ;
  ]

let app' fn args =
  let arg (k, v) = seq ~sep:"=" [ string k ; v ] in
  seq ~sep:"" [
    string fn ; string "(" ;
    list arg ~sep:"," args
  ]

let wrapper factors samples =
  let script_path = tmp // "script.R" in
  let script = seq ~sep:"\n" [
      string wrapper_script ;
      app "main" [
        quote dest ~using:'"' ;
        app "c" (List.map factors ~f:(string % quote ~using:'"')) ;
        app "c" (List.map samples ~f:(snd % dep % quote ~using:'"')) ;
        app' "matrix" [
          "data", app "c" (List.map samples ~f:fst
                           |> List.concat
                           |> List.map ~f:(string % quote ~using:'"')) ;
          "ncol", int (List.length factors) ;
          "byrow", string "T" ;
        ]
      ]
    ]
  in
  workflow ~descr:"deseq2.wrapper" [
    dump ~dest:script_path script ;
    shcmd "Rscript" ~env [ script_path ] ;
  ]

(*
   remove duplicates *and* keep original order
   not tail-recursive and quadratic complexity
*)
let unique xs =
  let rec aux seen = function
    | [] -> []
    | h :: t ->
      if List.mem seen h then
        aux seen t
      else
        h :: aux (h :: seen) t
  in
  aux [] xs

let rec fold_2sets xs ~init ~f =
  match xs with
  | [] -> init
  | h :: t ->
    let next = List.fold t ~init ~f:(fun accu g -> f accu h g) in
    fold_2sets t ~init:next ~f

let factor_levels factor_names conditions =
  List.fold_right
    conditions
    ~init:(List.map factor_names ~f:(const []))
    ~f:(fun cond accu -> List.map2_exn cond accu ~f:(fun c cs -> c :: cs))
  |> List.map ~f:unique

let comparisons factor_names conditions =
  let factor_levels = factor_levels factor_names conditions in
  List.map2_exn factor_names factor_levels ~f:(fun name levels ->
      fold_2sets levels ~init:[] ~f:(fun accu l1 l2 ->
          (name, l1, l2) :: accu
        )
      |> List.rev
    )
  |> List.concat


let main_effects factors samples =
  let o = wrapper factors samples in
  let sel p = o / selector p in
  object
    method sample_clustering = sel [ "sample_clustering.svg" ]
    method sample_pca = sel [ "sample_pca.svg" ]
    method normalized_counts = sel [ "normalized_counts.tsv" ]
    method comparison_summary = sel [ "summary.tsv" ]
    method effect_table = sel [ "recap.tsv" ]
    method comparisons =
      let conditions = List.map samples ~f:fst in
      List.map (comparisons factors conditions) ~f:(fun ((name, l1, l2) as comp) ->
          comp, sel [ sprintf "results_%s_%s_%s.tsv" name l1 l2 ]
        )
  end
