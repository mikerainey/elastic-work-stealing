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

let mk_n n = mk int "n" n

let string_of_millions v =
   let x = v /. 1000000. in
     if x >= 10. then sprintf "%.0f" x
     else if x >= 1. then sprintf "%.1f" x
     else if x >= 0.1 then sprintf "%.2f" x
     else sprintf "%.3f" x 

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

let ipfs_get hash outfile is_virtual =
  system (sprintf "ipget -o %s/%s %s" arg_input_files_folder outfile hash) is_virtual

let ipfs_get_if_needed hash outfile force_get is_virtual =
  if force_get || not (Sys.file_exists outfile) then
    ipfs_get hash outfile is_virtual
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
let mk_quicksort_inputs = mk_seq_inputs & mk_list int "cutsize" [1024; dflt_nb_items;]
  
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

let mk_parallel_paths_graph n p =
  let outfile = Printf.sprintf "parPaths-%d-%d" n p in
  let pretty_name = outfile in
    (mk_prog "run-parallelPathsGraph")
  & (mk int "n" n)
  & (mk int "p" p)
  & (mk_outfile outfile)
  & (mk string "!pretty_name" pretty_name)  

let mk_real_world_graph n pretty_name source =
    (mk_prog "run-getRealWorldGraph")
  & (mk_outfile n)
  & (mk string "!pretty_name" pretty_name)
  & (mk string "!file_name" n)
  & (mk int "source" source)
  
let mk_rmat_graph_pure = mk_rmat_graph 20000000 "pure"
let mk_chain = mk_parallel_paths_graph 10000000 1
let mk_parallel_paths_8 = mk_parallel_paths_graph 10000000 8
let mk_parallel_paths_72 = mk_parallel_paths_graph 10000000 72
let mk_orkut_graph = mk_real_world_graph "com-orkut.ungraph.txt_SJ" "orkut" 1
let mk_twitter_graph = mk_real_world_graph "twitter_SJ" "twitter" 1
let mk_bfs_pure_inputs =
  mk_chain ++ mk_parallel_paths_8 ++ mk_parallel_paths_72 ++
    mk_rmat_graph_pure ++ mk_orkut_graph ++ mk_twitter_graph

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

]

let benchmarks =
  if arg_problems = [] then
    benchmarks
  else
    List.filter (fun b -> List.exists (fun a -> a = b.bd_problem) arg_problems) benchmarks

(*****************************************************************************)
(** Running-time experiment *)

module ExpExectime = struct

let name = "exectime"

let mk_impl = mk string "impl"
let mk_problem = mk string "problem"

let mk_prog bd =
  (mk_prog "run-CPP") & (mk_problem bd.bd_problem)

let mk_runs_of_bd (bd : benchmark_descr) =
  (mk_prog bd) & bd.bd_mk_input

let mk_all_impls =
  ((mk_impl "opt") & (mk_list string "steal_policy" ["once"; "coupon";])) ++ (mk_impl "cilk")

let mk_all_runs =
  mk_all mk_runs_of_bd benchmarks

let make () = ()

let run() = (
  Mk_runs.(call (par_run_modes @ [
    Output (file_results name);
    Timeout 4000;
    Args (mk_all_runs & mk_all_impls & (mk_proc arg_proc))])))

let check () = ()

let formatter =
     Env.format (Env.(
       [ ("prog", Format_custom (fun s -> ""));
         ("!pretty_name", Format_custom (fun s -> ""));
         ("problem", Format_custom (fun s -> s));
       ]))

let plot() =
  Mk_bar_plot.(call ([
      Bar_plot_opt Bar_plot.([
         X_titles_dir Vertical;
         Y_axis [Axis.Lower (Some 0.)] ]);
      Formatter formatter;
      Charts mk_unit;
      Series mk_all_impls;
      X mk_all_runs;
      Input (file_results name);
      Output (file_plots name);
      Y_label "exectime";
      Y eval_exectime;
  ]))
  
let all () = select make run check plot

end

(*****************************************************************************)
(** Sleep-time experiment *)

module ExpSleeptime = struct

let name = "sleeptime"

let mk_impl = mk string "impl"
let mk_problem = mk string "problem"

let mk_prog bd =
  (mk_prog "run-CPP") & (mk_problem bd.bd_problem)

let mk_runs_of_bd (bd : benchmark_descr) =
  (mk_prog bd) & bd.bd_mk_input

let mk_all_impls =
  (mk_impl "sta") & (mk_list string "steal_policy" ["once"; "coupon";])

let mk_all_runs =
  mk_all mk_runs_of_bd benchmarks

let make () = ()

let run() = (
  Mk_runs.(call (par_run_modes @ [
    Output (file_results name);
    Timeout 4000;
    Args (mk_all_runs & mk_all_impls & (mk_proc arg_proc))])))

let check () = ()

let formatter =
     Env.format (Env.(
       [ ("prog", Format_custom (fun s -> ""));
         ("!pretty_name", Format_custom (fun s -> ""));
         ("problem", Format_custom (fun s -> s));
       ]))

let plot() = (
  Mk_bar_plot.(call ([
      Bar_plot_opt Bar_plot.([
         X_titles_dir Vertical;
         Y_axis [Axis.Lower (Some 0.)] ]);
      Formatter formatter;
      Charts mk_unit;
      Series mk_all_impls;
      X mk_all_runs;
      Input (file_results name);
      Output (file_plots name);
      Y_label "sleep pct. of idle time";
      Y eval_sleeppct;
  ]));
  Mk_bar_plot.(call ([
      Bar_plot_opt Bar_plot.([
         X_titles_dir Vertical;
         Y_axis [Axis.Lower (Some 0.)] ]);
      Formatter formatter;
      Charts mk_unit;
      Series mk_all_impls;
      X mk_all_runs;
      Input (file_results name);
      Output (file_plots (name^"sleeps"));
      Y_label "nb sleeps";
      Y (fun env all_results results ->
          Results.get_mean_of "nb_sleeps" results);
  ])))
  
let all () = select make run check plot

end

(*****************************************************************************)
(** Main *)

let _ =
  let arg_actions = XCmd.get_others() in
  let bindings = [ 
      "gen-inputs", ExpGenInputs.all;
      "exectime", ExpExectime.all;
      "sleeptime", ExpSleeptime.all;
  ]
  in
  Pbench.execute_from_only_skip arg_actions [] bindings;
  ()
    
