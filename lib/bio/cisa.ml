open Core_kernel
open Bistro
open Bistro.Shell_dsl

let img = [ docker_image ~account:"pveber" ~name:"cisa" ~tag:"20140304" () ]

let merge ?(min_length = 100) xs =
  let config_line (label, fa) =
    [
      string "data=" ; dep fa ;
      string ",title=" ; string label ;
    ]
  in
  let config_file = file_dump (
      List.intersperse ~sep:[string "\n"] (
        [ string "count=" ; int (List.length xs) ]
        :: List.map xs ~f:config_line
        @
        [string "Master_file=" ; dest]
        :: [string "min_length=" ; int min_length]
        :: []
      )
      |> List.concat
      |> seq ~sep:""
    )
  in
  Workflow.shell ~descr:"cisa.Merge" ~img [
    mkdir_p tmp ;
    cmd "Merge.py" [ config_file ] ;
  ]

let cisa ~genome_size contigs =
  let ( := ) var expr = seq ~sep:"" [string var ; string "=" ; expr ] in
  let script = file_dump (
      seq ~sep:"\n" [
        "TMP" := tmp // "output" ;
        "GENOMESIZE" := int genome_size ;
        "CONTIGS" := dep contigs ;
        "DEST" := dest ;
        string {|
NUCMER=`which nucmer`
CISA=$(dirname $(readlink -f $(which CISA.py)))
MAKEBLASTDB=`which makeblastdb`
BLASTN=`which blastn`
CONFIG=$TMP/cisa.config

mkdir -p $TMP
mkdir -p $TMP/CISA1
cd $TMP

cat > $CONFIG <<__HEREDOC__
genome=$GENOMESIZE
infile=$CONTIGS
outfile=$DEST
nucmer=$NUCMER
R2_Gap=0.95
CISA=$CISA
makeblastdb=$MAKEBLASTDB
blastn=$BLASTN
__HEREDOC__

yes | CISA.py $CONFIG
|}
      ]
    )
  in
  Workflow.shell ~descr:"cisa" ~img [
    mkdir_p tmp ;
    cmd "bash" [ script ] ;
  ]
