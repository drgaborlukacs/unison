(* Unison file synchronizer: src/hardlinks.ml *)

let featHardlinks =
  Features.register "Sync: Hardlink propagation" ~arcFormatChange:true None

let featureEnabled () = Features.enabled featHardlinks

let checkhardlinks =
  Prefs.createBool "checkhardlinks" false
    ~category:(`Advanced `Sync)
    ~local:true
    ~send:featureEnabled
    "warn when synchronized paths share an inode (hardlink aliases)"
    "When this preference is set, Unison's update scanner emits a \
     warning if two or more synchronized paths in the same replica \
     refer to the same inode (i.e., are hardlinks of one another).  \
     This is independent of \\verb|synchardlinks|: warnings are \
     emitted whether or not the topology is being propagated."

let synchardlinks =
  Prefs.createBool "synchardlinks" false
    ~category:(`Advanced `Sync)
    ~send:featureEnabled
    "propagate hardlink topology between replicas (experimental)"
    "When this preference is set, Unison records hardlink groups during \
     update detection and recreates them on the destination replica \
     during propagation, instead of copying the file contents \
     independently for each alias.  The destination's hardlink \
     topology is made to mirror the source's; aliases are never \
     introduced on the destination unless they exist on the source.  \
     This feature is experimental and may change in incompatible \
     ways in future versions.  The default is \\verb|false|."

let syncing () = featureEnabled () && Prefs.read synchardlinks

(***** Scan-side state *****)

(* (dev, ino).  We deliberately do NOT use [Fileinfo.info.inode], which is
   truncated to 31 bits as a per-path change-detection stamp.  Reading
   [st_ino] directly gives full-width inodes (63-bit on 64-bit OCaml;
   on 32-bit OCaml the Unix module itself caps at 31 bits, which is an
   external limitation we cannot fix here). *)
module Key = struct
  type t = int * int
  let equal (a, b) (c, d) = a = c && b = d
  let hash (a, b) = (a * 31) lxor b
end

module Tbl = Hashtbl.Make (Key)

(* keyTable : (dev, ino) -> deduped sorted list of paths
   pathIndex : path -> (dev, ino) (for quick groupOf lookup) *)
let keyTable : Path.local list Tbl.t = Tbl.create 16
module PathHash = struct
  type t = Path.local
  let equal a b = String.equal (Path.toString a) (Path.toString b)
  let hash p = Hashtbl.hash (Path.toString p)
end
module PathTbl = Hashtbl.Make (PathHash)
let pathIndex : (int * int) PathTbl.t = PathTbl.create 16

let recording () = Prefs.read checkhardlinks || Prefs.read synchardlinks

let beginScan () =
  Tbl.clear keyTable;
  PathTbl.clear pathIndex

let record path stats =
  if recording ()
     && stats.Unix.LargeFile.st_kind = Unix.S_REG
     && stats.Unix.LargeFile.st_nlink > 1
  then begin
    let key = (stats.Unix.LargeFile.st_dev, stats.Unix.LargeFile.st_ino) in
    let prev = try Tbl.find keyTable key with Not_found -> [] in
    if not (List.exists
              (fun q -> Path.toString q = Path.toString path) prev) then begin
      Tbl.replace keyTable key (path :: prev);
      PathTbl.replace pathIndex path key
    end
  end

let groupOf path =
  match PathTbl.find_opt pathIndex path with
  | None -> None
  | Some key ->
      match Tbl.find_opt keyTable key with
      | None | Some [] | Some [_] -> None
      | Some paths ->
          Some (List.sort
                  (fun a b ->
                    String.compare (Path.toString a) (Path.toString b))
                  paths)

let primaryOf path =
  match groupOf path with
  | None -> None
  | Some (p :: _) -> Some p
  | Some [] -> None

let endScan fspath =
  if Prefs.read checkhardlinks then begin
    let groups =
      Tbl.fold
        (fun _ paths acc -> if List.length paths > 1 then paths :: acc else acc)
        keyTable []
    in
    List.iter (fun paths ->
      let sorted =
        List.sort_uniq (fun a b ->
          String.compare (Path.toString a) (Path.toString b)) paths
      in
      if List.length sorted < 2 then () else
      let body =
        String.concat "\n  " (List.map Path.toString sorted)
      in
      Util.warn (Printf.sprintf
        "In replica %s the following synchronized paths share one inode \
         (hardlink aliases)%s:\n  %s\n"
        (Fspath.toPrintString fspath)
        (if syncing () then "" else
           ".  Unison will sync them as independent files")
        body)
    ) groups
  end
