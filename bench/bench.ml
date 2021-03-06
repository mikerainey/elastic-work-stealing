open Xlib
open XBase
open Params

let system = XSys.command_must_succeed_or_virtual

(*****************************************************************************)
(** Parameters *)

let arg_virtual_run = XCmd.mem_flag "virtual_run"
let arg_virtual_build = XCmd.mem_flag "virtual_build"
let arg_nb_runs = XCmd.parse_or_default_int "runs" 1
let arg_nb_seq_runs = XCmd.parse_or_default_int "seq_runs" 1
let arg_force_get = XCmd.mem_flag "force_get"
let arg_mode = Mk_runs.mode_from_command_line "mode"
let arg_skips = XCmd.parse_or_default_list_string "skip" []
let arg_onlys = XCmd.parse_or_default_list_string "only" []
let arg_problems = XCmd.parse_or_default_list_string "problems" []
let arg_numa_alloc_interleaved = XCmd.parse_or_default_bool "numa_alloc_interleaved" true
let arg_input_files_folder = XCmd.parse_or_default_string "infile_folder" "/var/tmp/infiles/"
let arg_input_real_world_graphs_folder = XCmd.parse_or_default_string "graphs_folder" arg_input_files_folder
let arg_steal_policy = XCmd.parse_or_default_string "steal_policy" "coupon"
let arg_proc = 
  let cmdline_proc = XCmd.parse_or_default_int "proc" 0 in
  let default =
    if cmdline_proc > 0 then
      cmdline_proc
    else
      let _ = system "get-nb-cores.sh > nb_cores" false in
      let chan = open_in "nb_cores" in
      let str = try input_line chan
      with End_of_file -> (close_in chan; "1")
      in
      int_of_string str
  in
  let default = default - 1 in (* doing this to avoid core 0 *)
  XCmd.parse_or_default_int "proc" default
let arg_proc_step = XCmd.parse_or_default_int "proc_step" arg_proc

let procs =
  let rec gen p =
    if p >= arg_proc then
      [arg_proc]
    else
      p :: gen (p + arg_proc_step)
  in
  let procs = gen arg_proc_step in
  match procs with
  | 1 :: _ -> procs
  | _ -> 1 :: procs

let par_run_modes =
  Mk_runs.([
    Mode arg_mode;
    Virtual arg_virtual_run;
    Runs arg_nb_runs; ])

let seq_run_modes =
  Mk_runs.([
    Mode arg_mode;
    Virtual arg_virtual_run;
    Runs arg_nb_seq_runs; ])

let gen_inputs_run_modes =
  Mk_runs.([
    Mode arg_mode;
    Virtual arg_virtual_run;
    Runs 1; ])

(*****************************************************************************)
(** Steps *)

let select make run check plot =
   let arg_skips =
      if List.mem "run" arg_skips && not (List.mem "make" arg_skips)
         then "make"::arg_skips
         else arg_skips
      in
   Pbench.execute_from_only_skip arg_onlys arg_skips [
      "make", make;
      "run", run;
      "check", check;
      "plot", plot;
      ]

let nothing () = ()

(*****************************************************************************)
(** Files and binaries *)

let build path bs is_virtual =
   system (sprintf "make -C %s -j %s" path (String.concat " " bs)) is_virtual

let file_results exp_name =
  Printf.sprintf "results_%s.txt" exp_name

let file_results_par exp_name =
  Printf.sprintf "results_par_%s.txt" exp_name

let file_results_seq exp_name =
  Printf.sprintf "results_seq_%s.txt" exp_name

let file_plots exp_name =
  Printf.sprintf "plots_%s.pdf" exp_name

let file_tables_src exp_name =
  Printf.sprintf "tables_%s.tex" exp_name

let file_tables exp_name =
  Printf.sprintf "tables_%s.pdf" exp_name
  
(** Evaluation functions *)

let eval_exectime = fun env all_results results ->
   Results.get_mean_of "exectime" results

let eval_sleeppct = fun env all_results results ->
  let idle = Results.get_mean_of "total_idle_time" results in
  let sleep = Results.get_mean_of "total_sleep_time" results in
  (sleep /. idle)

(*****************************************************************************)
(** Benchmark settings *)

let mk_proc = mk int "proc" 

let mk_n n =
    mk int "n" n
  & (mk string "!pretty_name" ("$n="^string_of_int n^"$"))
    
let string_of_millions v =
   let x = v /. 1000000. in
     if x >= 10. then sprintf "%.0f" x
     else if x >= 1. then sprintf "%.1f" x
     else if x >= 0.1 then sprintf "%.2f" x
     else sprintf "%.3f" x 

let mk_microseq_n n =
    mk int "n" n
  & (mk string "!pretty_name" ("$n="^string_of_millions (float_of_int n)^"$"))

let dflt_microseq_n = 100000000

let mk_all f xs =
  let rec g xs =
    match xs with
    | [] -> Params.mk_unit
    | [x] -> f x
    | x::xs -> f x ++ g xs
  in
  g xs

let string_of_percentage_value v =
    let x = 100. *. v in
    let sx = sprintf "%.0f" x in
    sx

let string_of_percentage ?(show_plus=false) v =
   match classify_float v with
   | FP_subnormal | FP_zero | FP_normal ->
      sprintf "%s%s%s"
        (if v > 0. && show_plus then "+" else "")
        (string_of_percentage_value v) "%"
   | FP_infinite -> "$+\\infty$"
   | FP_nan -> "na"

let string_of_percentage_change ?(show_plus=false) vold vnew =
  string_of_percentage ~show_plus:show_plus (vnew /. vold -. 1.0)

(*****************************************************************************)
(** Input generation *)

module ExpGenInputs = struct

let name = "gen-inputs"

let input_file_path_of fname =
  arg_input_files_folder ^ "/" ^ fname  

let ipfs_get hash fpath is_virtual =
  system (sprintf "ipget -o %s %s" fpath hash) is_virtual

let ipfs_get_if_needed hash outfile force_get is_virtual =
  let outfile_path = input_file_path_of outfile in
  if force_get || not (Sys.file_exists outfile_path) then (
    printf "downloading %s to \n" outfile_path;
    ipfs_get hash outfile_path is_virtual)
  else
    ()

let ipfs_get_files table force_get is_virtual =
  List.iter (fun (h, p) -> ipfs_get_if_needed h p force_get is_virtual) table

let infiles_by_hash = [
    "QmUet78nvvDXwDtQPC8bQqULmFdk4uerW9WieMNiiBy1Zk", "com-orkut.ungraph.txt_SJ";
    "QmNedH2Hr1Dc2rgnefCx6Q2P2CkL2Y43R41NG6xvjAdV5K", "twitter_SJ";
  ]

let row_of_infile infile =
  let h, _ = List.find (fun (_, f) -> f = infile) infiles_by_hash in
  (h, infile)

let fetch_infiles_of infiles =
  let table = List.map row_of_infile infiles in
  ipfs_get_files table arg_force_get arg_virtual_run

let path_of_outfile f = arg_input_files_folder ^ "/" ^ f
                      
let mk_outfile f =
  mk string "outfile" (path_of_outfile f)

let mk_microseq_input = mk_microseq_n dflt_microseq_n

let mk_rand_seq item_ty n =
  let outfile = Printf.sprintf "rand-seq-%s-%d" item_ty n in
  let pretty_name = Printf.sprintf "random $%s \cdot 10^6$ %ss"
                      (string_of_millions (float_of_int n)) item_ty
  in
    (mk_prog "run-randomSeq")
  & (mk int "n" n)
  & (mk_outfile outfile)
  & (mk string "type" item_ty)
  & (mk string "!pretty_name" pretty_name)

let mk_almost_sorted_seq item_ty n =
  let outfile = Printf.sprintf "almost-sorted-seq-%s-%d" item_ty n in
  let pretty_name = Printf.sprintf "almost sorted $%s \cdot 10^6$ %ss"
                      (string_of_millions (float_of_int n)) item_ty
  in
    (mk_prog "run-almostSortedSeq")
  & (mk int "n" n)
  & (mk_outfile outfile)
  & (mk string "type" item_ty)
  & (mk string "!pretty_name" pretty_name)

let mk_expt_seq item_ty n =
  let outfile = Printf.sprintf "expt-seq-%s-%d" item_ty n in
  let pretty_name = Printf.sprintf "exponential $%s \cdot 10^6$ %ss"
                      (string_of_millions (float_of_int n)) item_ty
  in
    (mk_prog "run-exptSeq")
  & (mk int "n" n)
  & (mk_outfile outfile)
  & (mk string "type" item_ty)
  & (mk string "!pretty_name" pretty_name)

let dflt_nb_items = 100000000

let mk_rand_seq = mk_rand_seq "double" dflt_nb_items
let mk_almost_sorted_seq = mk_almost_sorted_seq "double" dflt_nb_items
let mk_expt_seq = mk_expt_seq "double" dflt_nb_items
let mk_seq_inputs = mk_rand_seq ++ mk_almost_sorted_seq ++ mk_expt_seq
let mk_quicksort_inputs = mk_seq_inputs (* & mk_list int "cutsize" [1024; dflt_nb_items;] *)
  
let mk_rand_points distribution dims n =
  let outfile = Printf.sprintf "rand-points-%s-%d-%d" distribution dims n in
  let pretty_name = Printf.sprintf "%s, $%s \cdot 10^6$ points"
                      distribution (string_of_millions (float_of_int n))
  in
    (mk_prog "run-randPoints")
  & (mk string "distribution" distribution)
  & (mk int "dims" dims)
  & (mk int "n" n)
  & (mk_outfile outfile)
  & (mk string "!pretty_name" pretty_name)

let dflt_nb_pts = 10000000
  
let mk_rndpts_insphere_2d = mk_rand_points "in-sphere" 2 dflt_nb_pts
let mk_rndpts_kuzmin_2d = mk_rand_points "kuzmin" 2 dflt_nb_pts
let mk_rndpts_2d = mk_rndpts_insphere_2d ++ mk_rndpts_kuzmin_2d

let mk_rmat_graph n impl =
  let outfile = Printf.sprintf "rMat-%s-%d" impl n in
  let pretty_name = Printf.sprintf "rMat"
                      (* by default, m=10*n in rMat generator *)
                                   (*                      (string_of_millions (float_of_int (10*n)))*)
  in
    (mk_prog "run-rMatGraph")
  & (mk int "n" n)
  & (mk_outfile outfile)
  & (mk string "!pretty_name" pretty_name)
  & (mk int "source" 0)
  & (mk string "impl" impl)

let mk_parallel_paths_graph n p impl =
  let outfile = Printf.sprintf "parPaths-%d-%d" n p in
  let pretty_name = outfile in
    (mk_prog "run-parallelPathsGraph")
  & (mk int "n" n)
  & (mk int "p" p)
  & (mk_outfile outfile)
  & (mk string "!pretty_name" pretty_name)  
  & (mk int "source" 0)
  & (mk string "impl" impl)

let mk_real_world_graph n pretty_name source =
    (mk_prog "run-getRealWorldGraph")
  & (mk_outfile n)
  & (mk string "!pretty_name" pretty_name)
  & (mk string "!file_name" n)
  & (mk int "source" source)
  
let mk_rmat_graph_pure = mk_rmat_graph 10000000 "pure"
let mk_chain = mk_parallel_paths_graph 10000000 1 "pure"
let mk_parallel_paths_8 = mk_parallel_paths_graph 10000000 8 "pure"
let mk_parallel_paths_72 = mk_parallel_paths_graph 10000000 72 "pure"
let mk_orkut_graph = mk_real_world_graph "com-orkut.ungraph.txt_SJ" "orkut" 1
let mk_twitter_graph = mk_real_world_graph "twitter_SJ" "twitter" 1
let mk_bfs_pure_inputs =
  mk_chain ++ mk_parallel_paths_8 ++ mk_parallel_paths_72 ++
    (*mk_rmat_graph_pure ++ *) mk_orkut_graph (* ++ mk_twitter_graph *)

(*
let mk_rmat_graph_ligra = mk_rmat_graph 20000000 "ligra"
let mk_bfs_ligra_inputs = mk_rmat_graph_ligra ++ mk_orkut_graph ++ mk_twitter_graph
 *)

let mk_grep_input nb_rows row_len nb_occurrences pat_str impl =
  let outfile = Printf.sprintf "grep-%d-%d-%d-%s" nb_rows row_len nb_occurrences pat_str in
  let pretty_name = Printf.sprintf
                      "$%s \cdot 10^6$ lines; %s matches"
                      (string_of_millions (float_of_int nb_rows))
                      (if nb_occurrences = 10 then "10" else "$10^5$")
  in
  let bd_problem = "grep" in
    (mk_prog "text.pbbscpp.bin" & mk_proc arg_proc)
  & (mk int "nb_rows" nb_rows)
  & (mk int "row_len" row_len)
  & (mk int "nb_occurrences" nb_occurrences)
  & (mk string "pattern" pat_str)
  & (mk_outfile outfile)
  & (mk string "!pretty_name" pretty_name)

let mk_grep_input1 = mk_grep_input 100000 1000 100 "foobarbaz" "delay"
let mk_grep_input2 = mk_grep_input 1000000 100 100000 "foobar" "delay"
(*let mk_grep_input3 = mk_grep_input 10000 10000 100 "foobarbaz" "delay"*) (* this input causes mpl to crash *)

let mk_grep_inputs = mk_grep_input1 ++ mk_grep_input2 (* ++ mk_grep_input3*)

let mk_suffixarray_input n pretty_name url =
    (mk_prog "run-getWebFile")
  & (mk_outfile n)
  & (mk string "!pretty_name" pretty_name)
  & (mk string "!file_name" n)
  & (mk string "url" url)

let mk_suffixarray_inputs =
  let chr22_url = "https://github.com/zfy0701/Parallel-LZ77/raw/master/testData/chr22.dna" in
  mk_suffixarray_input "chr22.dna" "chr22.dna" chr22_url

let input_descriptions_of mk_outputs params =
  ~~ List.map (Params.to_envs mk_outputs) (fun e ->
      List.map (Env.get_as_string e) params)

let input_descriptions_of mk_outputs params =
  ~~ List.map (Params.to_envs mk_outputs) (fun e ->
      List.map (Env.get_as_string e) params)

let mk_inputs_to_download = mk_orkut_graph ++ mk_twitter_graph
  
let file_to_download_of mk_inputs =
  let input_descriptions = input_descriptions_of mk_inputs ["!file_name";] in
  let f [file_name;] = file_name in
  List.map f input_descriptions
  
let make() =
  ()

let run() =
  let _ = system ("mkdir -p " ^ arg_input_files_folder) arg_virtual_run in
  let _ =  fetch_infiles_of (file_to_download_of mk_inputs_to_download) in
  Mk_runs.(call (gen_inputs_run_modes @ [
    Output (file_results name);
    Timeout 4000;
    Args (    mk_rndpts_2d
           ++ mk_seq_inputs
           ++ mk_grep_inputs
           ++ mk_suffixarray_inputs              
           ++ mk_bfs_pure_inputs (*
           ++ mk_bfs_ligra_inputs *) )]))

let check = nothing  (* do something here *)
  
let plot() = ()

let all () = select make run check plot

end

(*****************************************************************************)
(** Benchmark data *)

let pretty_name = "!pretty_name" 
let mk_pretty_name = mk string pretty_name

let mk_inputs_from_outputs mk_outputs =
  let input_descriptions = ExpGenInputs.input_descriptions_of mk_outputs ["outfile"; pretty_name;]
  in
  let f [outfile; pretty_name;] =
    (mk string "infile" outfile) & (mk_pretty_name pretty_name)
  in
  mk_all f input_descriptions

let mk_textsearch_inputs_from_outputs mk_outputs =
  let input_descriptions =
    ExpGenInputs.input_descriptions_of mk_outputs ["outfile"; pretty_name; "pattern";]
  in
  let f [outfile; pretty_name; pattern;] =
    (mk string "infile" outfile) & (mk string "pattern" pattern) & (mk_pretty_name pretty_name)
  in
  mk_all f input_descriptions

let mk_graph_inputs_from_outputs mk_outputs =
  let input_descriptions =
    ExpGenInputs.input_descriptions_of mk_outputs ["outfile"; pretty_name; "source";]
  in
  let f [outfile; pretty_name; source;] =
    (mk string "infile" outfile) & (mk_pretty_name pretty_name) &
      (mk int "source" (int_of_string source))
  in
  mk_all f input_descriptions

type benchmark_descr = {
    bd_problem : string;
    bd_mk_input : Params.t;
}

let benchmarks : benchmark_descr list = [

    { bd_problem = "tabulate";
      bd_mk_input = ExpGenInputs.mk_microseq_input; };

    { bd_problem = "reduce";
      bd_mk_input = ExpGenInputs.mk_microseq_input; };

    { bd_problem = "scan";
      bd_mk_input = ExpGenInputs.mk_microseq_input; };

    { bd_problem = "filter";
      bd_mk_input = ExpGenInputs.mk_microseq_input; };

    { bd_problem = "bfs";
      bd_mk_input = mk_graph_inputs_from_outputs ExpGenInputs.mk_bfs_pure_inputs; };

    { bd_problem = "fib";
      bd_mk_input = mk_n 38; };

    { bd_problem = "quicksort";
      bd_mk_input = mk_inputs_from_outputs ExpGenInputs.mk_quicksort_inputs; };

    { bd_problem = "samplesort";
      bd_mk_input = mk_inputs_from_outputs ExpGenInputs.mk_seq_inputs; };

    { bd_problem = "quickhull";
      bd_mk_input = mk_inputs_from_outputs ExpGenInputs.mk_rndpts_2d; };

    { bd_problem = "grep";
      bd_mk_input = mk_textsearch_inputs_from_outputs ExpGenInputs.mk_grep_inputs; };

    { bd_problem = "suffixarray";
      bd_mk_input = mk_inputs_from_outputs ExpGenInputs.mk_suffixarray_inputs; };

]

let benchmarks =
  if arg_problems = [] then
    benchmarks
  else
    List.filter (fun b -> List.exists (fun a -> a = b.bd_problem) arg_problems) benchmarks

let mk_impl = mk string "impl"
let mk_problem = mk string "problem"

let mk_prog_of_bd bd =
  (mk_prog "run") & (mk_problem bd.bd_problem)

let mk_runs_of_bd (bd : benchmark_descr) =
  (mk_prog_of_bd bd) & bd.bd_mk_input

let mk_steal_policy = mk string "steal_policy"
let steal_policy_once = "once"
let steal_policy_coupon = "coupon"

let elastic_policy_semaphore = "semaphore"
let elastic_policy_disabled = "off"
let elastic_policy_spinsleep = "spinsleep"
let mk_elastic_policy = mk string "elastic_policy"

let pretty_mcsl_configname = "!pretty_mcsl"
let mk_pretty_mcsl_config = mk string pretty_mcsl_configname

let pretty_elastic_disabled_name = "Nonelastic"

let mk_mcsl_config_elastic_disabled =
  (mk_elastic_policy elastic_policy_disabled) &
  (mk_pretty_mcsl_config pretty_elastic_disabled_name)

let mk_mcsl_config_elastic_dflt =
  (mk_elastic_policy elastic_policy_semaphore) &
  (mk_pretty_mcsl_config "Elastic")

let mk_mcsl_config_steal_once =
  (mk_elastic_policy elastic_policy_semaphore) &
  (mk_steal_policy steal_policy_once) &
  (mk_pretty_mcsl_config "Steal-1")

let mk_mcsl_config_spinsleep =
  (mk_elastic_policy elastic_policy_spinsleep) &
  (mk_pretty_mcsl_config "Spin-sleep")

let mk_pin_core_dense =
  (mk bool "pinning_enabled" true) & (mk string "resource_packing" "dense") & (mk string "resource_binding" "by_core")

let mk_mcsl_config_cpu_pin =
  (mk_elastic_policy elastic_policy_semaphore) &
  mk_pin_core_dense &
  (mk_pretty_mcsl_config "Pinned")

let mk_mcsl_elastic_configs =
  mk_mcsl_config_elastic_dflt ++
  mk_mcsl_config_steal_once ++
  mk_mcsl_config_spinsleep ++
  mk_mcsl_config_cpu_pin

(*****************************************************************************)
(** Baseline experiment *)

module ExpBaseline = struct

let name = "baseline"

let mk_all_seq_impls =
  (mk_impl "opt") ++ (mk_impl "cilk")

let mk_mcsl_config = mk_elastic_policy elastic_policy_disabled

let mk_all_par_impls =
  ((mk_impl "opt") & mk_mcsl_config) ++ (mk_impl "cilk")

let mk_all_runs =
  mk_all mk_runs_of_bd benchmarks

let make () = ()

let run() = (
  Mk_runs.(call (seq_run_modes @ [
    Output (file_results_seq name);
    Timeout 4000;
    Args (mk_all_runs & mk_all_seq_impls & (mk_proc 1))]));
  Mk_runs.(call (par_run_modes @ [
    Output (file_results_par name);
    Timeout 4000;
    Args (mk_all_runs & mk_all_par_impls & (mk_proc arg_proc))])))

let check () = ()

let plot() = 
  let tex_file = file_tables_src name in
  let pdf_file = file_tables name in
  let nb_procs = List.length procs in
  let base_impl = "cilk" in
  let nb_cols_per_proc = 2 in
  let nb_cols = nb_cols_per_proc * nb_procs in
  Mk_table.build_table tex_file pdf_file (fun add ->
      let hdr =
        let ls = String.concat "|" (XList.init nb_cols (fun _ -> "c")) in
        Printf.sprintf "|l|%s|" ls
      in
      add (Latex.tabular_begin hdr);
      (* Proc header *)
      Mk_table.cell ~escape:true ~last:false add "";
      ~~ List.iteri procs (fun proc_i proc ->
          let last = proc_i+1 = nb_procs in
          Mk_table.cell ~escape:true ~last:last add (Latex.tabular_multicol nb_cols_per_proc "c|" (Printf.sprintf "$P$ = %d" proc))
        );
      add Latex.tabular_newline;
      (* Binary header *)
      Mk_table.cell ~escape:true ~last:false add "";
      ~~ List.iteri procs (fun proc_i proc ->
          let last = proc_i+1 = nb_procs in
          let _ = Mk_table.cell ~escape:true ~last:false add base_impl in
          Mk_table.cell ~escape:true ~last:last add (Printf.sprintf "\shortstack{ours\\\\ vs\\\\ %s}" "Cilk")
        );
      add Latex.tabular_newline;
      (* Benchmarks *)
      ~~ List.iteri benchmarks (fun bd_i bd ->
          let inputs = ~~ List.map (Params.to_envs bd.bd_mk_input) (fun e ->
                           (Env.get_as_string e pretty_name))
          in
          ~~ List.iteri inputs (fun input_i pn ->
            let bench_str = Printf.sprintf "%s (%s)" bd.bd_problem pn in
            Mk_table.cell ~escape:true ~last:false add bench_str;
            ~~ List.iteri procs (fun proc_i proc ->
                let results_all =
                  let f = if proc = 1 then file_results_seq else file_results_par in
                  Results.from_file (f name)
                in
                let last = proc_i+1 = nb_procs in
                let exectime_of mk_p =
                  let [col] = (( mk_p & (mk_pretty_name pn) ) & (mk_proc proc)) Env.empty in
                  let results = Results.filter col results_all in
                  Results.get_mean_of "exectime" results
                in
                let pretty_exectime_of =  Printf.sprintf "%.3f" in
                let base_time = exectime_of (mk_prog_of_bd bd & mk_impl "cilk") in
                let base_pretty = pretty_exectime_of base_time in
                let _ = Mk_table.cell ~escape:true ~last:false add base_pretty in
                if proc = 1 then
                  let impl_time = exectime_of (mk_prog_of_bd bd & mk_impl "opt") in
                  let pct = string_of_percentage_change ~show_plus:true base_time impl_time in
                  Mk_table.cell ~escape:true ~last:false add pct
                else
                  let impl_time = exectime_of (mk_prog_of_bd bd & mk_mcsl_config & mk_impl "opt") in
                  let pct = string_of_percentage_change ~show_plus:true base_time impl_time in
                  Mk_table.cell ~escape:true ~last:last add pct
            );
        add Latex.tabular_newline));
      add Latex.tabular_end;)
  
let all () = select make run check plot

end

(*****************************************************************************)
(** Elastic running time experiment *)

module ExpElasticExectime = struct

let name = "elastic_exectime"

let procs = List.filter (fun p -> p <> 1) procs

let mk_all_par_impls =
  (mk_impl "opt") & (mk_mcsl_config_elastic_disabled ++ mk_mcsl_elastic_configs)

let mk_all_runs =
  mk_all mk_runs_of_bd benchmarks

let make () = ()

let run() = 
  Mk_runs.(call (par_run_modes @ [
    Output (file_results name);
    Timeout 4000;
    Args (mk_all_runs & mk_all_par_impls & (mk_proc arg_proc))]))

let check () = ()

let plot() = 
  let tex_file = file_tables_src name in
  let pdf_file = file_tables name in
  let nb_procs = List.length procs in
  let mcsl_elastic_configs = ~~ List.map (Params.to_envs mk_mcsl_elastic_configs) (fun e ->
                           (Env.get_as_string e pretty_mcsl_configname))
  in
  let nb_mcsl_elastic_configs = List.length mcsl_elastic_configs in
  let nb_cols_per_proc = nb_mcsl_elastic_configs + 1 in
  let nb_cols = nb_cols_per_proc * nb_procs in
  Mk_table.build_table tex_file pdf_file (fun add ->
      let hdr =
        let ls = String.concat "|" (XList.init nb_cols (fun _ -> "c")) in
        Printf.sprintf "|l|%s|" ls
      in
      add (Latex.tabular_begin hdr);
      (* Proc header *)
      Mk_table.cell ~escape:true ~last:false add "";
      ~~ List.iteri procs (fun proc_i proc ->
          let last = proc_i+1 = nb_procs in
          Mk_table.cell ~escape:true ~last:last add (Latex.tabular_multicol nb_cols_per_proc "c|" (Printf.sprintf "$P$ = %d" proc))
        );
      add Latex.tabular_newline;
      (* Binary header *)
      Mk_table.cell ~escape:true ~last:false add "";
      ~~ List.iteri procs (fun proc_i proc ->
          let last = proc_i+1 = nb_procs in
          let _ = Mk_table.cell ~escape:true ~last:false add pretty_elastic_disabled_name in
          ~~ List.iteri mcsl_elastic_configs (fun mcsl_elastic_config_i mcsl_elastic_config ->
              let last = mcsl_elastic_config_i+1 = nb_mcsl_elastic_configs && last in
              Mk_table.cell ~escape:true ~last:last add mcsl_elastic_config)
        );
      add Latex.tabular_newline;
      (* Benchmarks *)
      let results_all = Results.from_file (file_results name) in
      ~~ List.iteri benchmarks (fun bd_i bd ->
          let inputs = ~~ List.map (Params.to_envs bd.bd_mk_input) (fun e ->
                           (Env.get_as_string e pretty_name))
          in
          ~~ List.iteri inputs (fun input_i pn ->
            let bench_str = Printf.sprintf "%s (%s)" bd.bd_problem pn in
            Mk_table.cell ~escape:true ~last:false add bench_str;
            ~~ List.iteri procs (fun proc_i proc ->
                let last = proc_i+1 = nb_procs in
                let exectime_of mk_p =
                  let [col] = ( mk_p & (mk_pretty_name pn) & (mk_proc proc) & (mk_prog_of_bd bd) & (mk_impl "opt")) Env.empty in
                  let results = Results.filter col results_all in
                  Results.get_mean_of "exectime" results
                in
                let pretty_exectime_of =  Printf.sprintf "%.3f" in
                let base_time = exectime_of (mk_pretty_mcsl_config pretty_elastic_disabled_name)  in
                let base_pretty = pretty_exectime_of base_time in
                let _ = Mk_table.cell ~escape:true ~last:false add base_pretty in
                ~~ List.iteri mcsl_elastic_configs (fun mcsl_elastic_config_i mcsl_elastic_config ->
                    let last = mcsl_elastic_config_i+1 = nb_mcsl_elastic_configs && last in
                    let impl_time = exectime_of (mk_pretty_mcsl_config mcsl_elastic_config) in
                    let pct = string_of_percentage_change ~show_plus:true base_time impl_time in
                    Mk_table.cell ~escape:true ~last:last add pct)
            );
        add Latex.tabular_newline));
      add Latex.tabular_end;)
  
let all () = select make run check plot

end

(*****************************************************************************)
(** Sleep-time experiment *)

module ExpSleeptime = struct

let name = "sleeptime"

let procs = List.filter (fun p -> p <> 1) procs

let mk_impl = mk string "impl"
let mk_problem = mk string "problem"

let mk_all_runs =
  (mk_impl "sta") & mk_all mk_runs_of_bd benchmarks

let make () = ()

let file_results_baseline exp_name =
  Printf.sprintf "results_baseline_%s.txt" exp_name

let file_results exp_name =
  Printf.sprintf "results_%s.txt" exp_name

let file_tables_src_impl impl_name exp_name =
  Printf.sprintf "tables_%s_%s.tex" impl_name exp_name

let file_tables_impl impl_name exp_name =
  Printf.sprintf "tables_%s_%s.pdf" impl_name exp_name

let mcsl_elastic_configs = ~~ List.map (Params.to_envs mk_mcsl_elastic_configs) (fun e ->
                               (Env.get_as_string e pretty_mcsl_configname))
let run() = (
  Mk_runs.(call (par_run_modes @ [
    Output (file_results_baseline name);
    Timeout 4000;
    Args (mk_all_runs & mk_mcsl_config_elastic_disabled & (mk_list int "proc" procs))]));
  Mk_runs.(call (par_run_modes @ [
    Output (file_results name);
    Timeout 4000;
    Args (mk_all_runs & mk_mcsl_elastic_configs & (mk_list int "proc" procs))])))


let check () = ()

let plot_impl impl_name =
  let tex_file = file_tables_src_impl impl_name name in
  let pdf_file = file_tables_impl impl_name name in
  let results_baseline = Results.from_file (file_results_baseline name) in
  let results_impl = Results.from_file (file_results name) in
  let baseline_name = pretty_elastic_disabled_name in
  let nb_procs = List.length procs in
  let nb_cols_for_baseline = 4 in
  let nb_cols_per_impl = 6 in
  let nb_cols_per_proc = nb_cols_for_baseline + nb_cols_per_impl in
  let nb_cols = nb_cols_per_proc * nb_procs in
  Mk_table.build_table tex_file pdf_file (fun add ->
      let hdr =
        let ls = String.concat "|" (XList.init nb_cols (fun _ -> "c")) in
        Printf.sprintf "|l|%s|" ls
      in
      add (Latex.tabular_begin hdr);
      (* Proc header *)
      Mk_table.cell ~escape:true ~last:false add "";
      ~~ List.iteri procs (fun proc_i proc ->
          let last = proc_i+1 = nb_procs in
          Mk_table.cell ~escape:true ~last:last add (Latex.tabular_multicol nb_cols_per_proc "c|" (Printf.sprintf "$P$ = %d" proc))
        );
      add Latex.tabular_newline;
      (* Nonelastic/elastic header *)
      Mk_table.cell ~escape:true ~last:false add "";
      ~~ List.iteri procs (fun proc_i proc ->
          let last = proc_i+1 = nb_procs in
          let _ = Mk_table.cell ~escape:true ~last:false add (Latex.tabular_multicol nb_cols_for_baseline "c|" baseline_name) in
          let _ = Mk_table.cell ~escape:true ~last:last add (Latex.tabular_multicol nb_cols_per_impl "c|" impl_name) in
          ()
        );
      add Latex.tabular_newline;
      (* Binary header *)
      Mk_table.cell ~escape:true ~last:false add "";
      ~~ List.iteri procs (fun proc_i proc ->
          let last = proc_i+1 = nb_procs in
          let _ = Mk_table.cell ~escape:true ~last:false add "work (s)" in
          let _ = Mk_table.cell ~escape:true ~last:false add "idle (s)" in
          let _ = Mk_table.cell ~escape:true ~last:false add "nb. steals" in
          let _ = Mk_table.cell ~escape:true ~last:false add "utilization" in
          let _ = Mk_table.cell ~escape:true ~last:false add "work" in
          let _ = Mk_table.cell ~escape:true ~last:false add "idle" in
          let _ = Mk_table.cell ~escape:true ~last:false add "work+idle" in
          let _ = Mk_table.cell ~escape:true ~last:false add "nb. steals" in
          let _ = Mk_table.cell ~escape:true ~last:false add "nb. sleeps" in
          let _ = Mk_table.cell ~escape:true ~last:last add "utilization" in
          ()
        );
      add Latex.tabular_newline;
      (* Benchmarks *)
      ~~ List.iteri benchmarks (fun bd_i bd ->
          let inputs = ~~ List.map (Params.to_envs bd.bd_mk_input) (fun e ->
                           (Env.get_as_string e pretty_name))
          in
          ~~ List.iteri inputs (fun input_i pn ->
            let bench_str = Printf.sprintf "%s (%s)" bd.bd_problem pn in
            Mk_table.cell ~escape:true ~last:false add bench_str;
            ~~ List.iteri procs (fun proc_i proc ->
                let last = proc_i+1 = nb_procs in
                let stat_of impl_name stat =
                  let results = if impl_name = baseline_name then results_baseline else results_impl in
                  let [col] = (((mk_impl "sta") & (mk_prog_of_bd bd) & (mk_pretty_name pn) ) & (mk_proc proc) & (mk_pretty_mcsl_config impl_name)) Env.empty in
                  let results = Results.filter col results in
                  Results.get_mean_of stat results
                in
                let total_idle_time_ne = stat_of baseline_name "total_idle_time" in
                let total_work_time_ne = stat_of baseline_name "total_work_time" in
                let nb_steals_ne = stat_of baseline_name "nb_steals" in
                let utilization_ne = stat_of baseline_name "utilization" in
                let total_time_ne = total_idle_time_ne +. total_work_time_ne in
                let total_idle_time_e = stat_of impl_name "total_idle_time" in
                let total_work_time_e = stat_of impl_name "total_work_time" in
                let launch_time_e = stat_of impl_name "launch_duration" in
                let nb_sleeps_e = stat_of impl_name "nb_sleeps" in
                let nb_steals_e = stat_of impl_name "nb_steals" in                
                let total_time_e = total_idle_time_e +. total_work_time_e in
                let utilization_e = stat_of impl_name "utilization" in
                let _ = Mk_table.cell ~escape:true ~last:false add (Printf.sprintf "%.3f" total_work_time_ne) in
                let _ = Mk_table.cell ~escape:true ~last:false add (Printf.sprintf "%.3f" total_idle_time_ne) in
                let _ = Mk_table.cell ~escape:true ~last:false add (Printf.sprintf "%.0f" nb_steals_ne) in
                let _ = Mk_table.cell ~escape:true ~last:false add (string_of_percentage utilization_ne) in
                let _ = Mk_table.cell ~escape:true ~last:false add (string_of_percentage_change ~show_plus:true total_work_time_ne total_work_time_e) in
                let _ = Mk_table.cell ~escape:true ~last:false add (string_of_percentage_change ~show_plus:true total_idle_time_ne total_idle_time_e) in
                let _ = Mk_table.cell ~escape:true ~last:false add (string_of_percentage_change ~show_plus:true total_time_ne total_time_e) in
                let _ = Mk_table.cell ~escape:true ~last:false add (Printf.sprintf "%.0f" nb_steals_e) in
                let _ = Mk_table.cell ~escape:true ~last:false add (Printf.sprintf "%.0f" nb_sleeps_e) in
                let _ = Mk_table.cell ~escape:true ~last:last add (string_of_percentage utilization_e) in
                ()
              );
        add Latex.tabular_newline));
      add Latex.tabular_end;)

let plot() =
  ~~List.iter mcsl_elastic_configs plot_impl
  
let all () = select make run check plot

end

(*****************************************************************************)
(** Main *)

let _ =
  let arg_actions = XCmd.get_others() in
  let bindings = [ 
      "gen-inputs", ExpGenInputs.all;
      "baseline", ExpBaseline.all;
      "elastic_exectime", ExpElasticExectime.all;
      "sleeptime", ExpSleeptime.all;
  ]
  in
  Pbench.execute_from_only_skip arg_actions [] bindings;
  ()
    
