(* Unison file synchronizer: src/os.ml *)
(* Copyright 1999-2020, Benjamin C. Pierce

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*)


(* This file attempts to isolate operating system specific details from the  *)
(* rest of the program.                                                      *)

let debug = Util.debug "os"

(* Assumption: Prefs are not loaded on server, so clientHostName is always *)
(* set to myCanonicalHostName. *)

let localCanonicalHostName =
  try System.getenv "UNISONLOCALHOSTNAME"
  with Not_found -> Unix.gethostname()

let clientHostName : string Prefs.t =
  Prefs.createString "clientHostName" localCanonicalHostName
    ~category:(`Advanced `Remote)
    "set host name of client"
    ("When specified, the host name of the client will not be guessed " ^
     "and the provided host name will be used to find the archive.")

let serverHostName = localCanonicalHostName

let myCanonicalHostName () =
  if !Trace.runningasserver then serverHostName else Prefs.read clientHostName

let inplaceRename =
  Prefs.createBool "inplaceRename" false
    ~category:(`Advanced `General)
    "preserve destination inode when replacing an existing regular file"
    ("When set, Os.rename copies contents into the existing destination "
   ^ "regular file (preserving its inode) instead of using rename(2) to "
   ^ "atomically swap.  Only the blocks that actually differ between source "
   ^ "and destination are written, which lets snapshotting filesystems (ZFS, "
   ^ "btrfs) retain only the changed blocks instead of the whole file.  "
   ^ "Atomicity of the replacement is given up in exchange.  Falls through "
   ^ "to the normal rename for symlinks, directories, and the case where "
   ^ "the destination does not yet exist.  Disabled by default.")

let inplaceBlockSize =
  Prefs.create "inplaceBlockSize" 16
    ~category:(`Advanced `General)
    "block size in KiB for inplaceRename (must be a power of 2)"
    ("Block size in KiB used by \\verb|inplaceRename| when comparing source "
   ^ "and destination to skip writes for unchanged blocks.  Must be a "
   ^ "positive power of 2 (1, 2, 4, 8, 16, 32, ...).  For best snapshot "
   ^ "behavior, set this to match the destination filesystem's record size "
   ^ "(e.g. 16 for a ZFS dataset with \\verb|recordsize=16K|, 128 for the "
   ^ "ZFS default of 128K).  Has no effect when \\verb|inplaceRename| is "
   ^ "false.")
    (fun _ s ->
       let k =
         try int_of_string s
         with Failure _ ->
           raise (Prefs.IllegalValue
                    (Printf.sprintf
                       "inplaceBlockSize: %S is not an integer" s))
       in
       if k <= 0 || k land (k - 1) <> 0 then
         raise (Prefs.IllegalValue
                  (Printf.sprintf
                     "inplaceBlockSize: %d is not a positive power of 2" k));
       k)
    (fun k -> [string_of_int k])
    Umarshal.int

let tempFilePrefix = ".unison."
let tempFileSuffixFixed = ".unison.tmp"
let tempFileSuffix = ref tempFileSuffixFixed
let includeInTempNames s =
  (* BCP: Added this in Jan 08.  If (as I believe) it never fails, then this tricky
     stuff can be deleted. *)
  assert (s<>"");
  tempFileSuffix :=
    if s = "" then tempFileSuffixFixed
    else "." ^ s ^ tempFileSuffixFixed

let isTempFile file =
  Util.endswith file tempFileSuffixFixed &&
  Util.startswith file tempFilePrefix

(*****************************************************************************)
(*                      QUERYING THE FILESYSTEM                              *)
(*****************************************************************************)

let exists fspath path =
  Fileinfo.getType false fspath path <> `ABSENT

let readLink fspath path =
  Util.convertUnixErrorsToTransient
  "reading symbolic link"
    (fun () ->
       let abspath = Fspath.concat fspath path in
       let l = Fs.readlink abspath in
       if Sys.win32 || Sys.cygwin then
         Fileutil.backslashes2forwardslashes l
       else
         l
    )

let rec isAppleDoubleFile file =
  Prefs.read Osx.rsrc &&
  String.length file > 2 && file.[0] = '.' && file.[1] = '_'

(* Assumes that (fspath, path) is a directory, and returns the list of       *)
(* children, except for '.' and '..'.                                        *)
let allChildrenOf fspath path =
  Util.convertUnixErrorsToTransient
  "scanning directory"
    (fun () ->
      let rec loop children directory =
        let newFile = try directory.Fs.readdir () with End_of_file -> "" in
        if newFile = "" then children else
        let newChildren =
          if newFile = "." || newFile = ".." then
            children
          else
            Name.fromString newFile :: children in
        loop newChildren directory
      in
      let absolutePath = Fspath.concat fspath path in
      let directory =
        try
          Some (Fs.opendir absolutePath)
        with Unix.Unix_error (Unix.ENOENT, _, _) ->
          (* FIX (in Ocaml): under Windows, when a directory is empty
             (not even "." and ".."), FindFirstFile fails with
             ERROR_FILE_NOT_FOUND while ocaml expects the error
             ERROR_NO_MORE_FILES *)
          None
      in
      match directory with
        Some directory ->
          begin try
            let result = loop [] directory in
            directory.Fs.closedir ();
            result
          with Unix.Unix_error _ as e ->
            begin try
              directory.Fs.closedir ()
            with Unix.Unix_error _ -> () end;
            raise e
          end
      | None ->
          [])

(* Assumes that (fspath, path) is a directory, and returns the list of       *)
(* children, except for temporary files and AppleDouble files.               *)
let rec childrenOf fspath path =
  List.filter
    (fun filename ->
       let file = Name.toString filename in
       if isAppleDoubleFile file then
         false
(* does it belong to here ? *)
(*          else if Util.endswith file backupFileSuffix then begin *)
(*             let newPath = Path.child path filename in *)
(*             removeBackupIfUnwanted fspath newPath; *)
(*             false *)
(*           end  *)
       else if isTempFile file then begin
         if Util.endswith file !tempFileSuffix then begin
           let p = Path.child path filename in
           let i = Fileinfo.getBasic false fspath p in
           let secondsinthirtydays = 2592000.0 in
           if Props.time i.Fileinfo.desc +. secondsinthirtydays < Util.time()
           then begin
             debug (fun()-> Util.msg "deleting old temp file %s\n"
                      (Fspath.toDebugString (Fspath.concat fspath p)));
             delete fspath p
           end else
             debug (fun()-> Util.msg
                      "keeping temp file %s since it is less than 30 days old\n"
                      (Fspath.toDebugString (Fspath.concat fspath p)));
         end;
         false
       end else
         true)
    (allChildrenOf fspath path)

(*****************************************************************************)
(*                        ACTIONS ON FILESYSTEM                              *)
(*****************************************************************************)

(* Deletes a file or a directory, but checks before if there is something    *)
and delete fspath path =
  Util.convertUnixErrorsToTransient
    "deleting"
    (fun () ->
      let absolutePath = Fspath.concat fspath path in
      match Fileinfo.getType false fspath path with
        `DIRECTORY ->
          begin try
            Fs.chmod absolutePath 0o700
          with Unix.Unix_error _ -> () end;
          Safelist.iter
            (fun child -> delete fspath (Path.child path child))
            (allChildrenOf fspath path);
          Fs.rmdir absolutePath
      | `FILE ->
          if not Sys.unix then begin
            try
              Fs.chmod absolutePath 0o600;
            with Unix.Unix_error _ -> ()
          end;
          Fs.unlink absolutePath;
          if Prefs.read Osx.rsrc then begin
            let pathDouble = Fspath.appleDouble absolutePath in
            if Fs.file_exists pathDouble then
              Fs.unlink pathDouble
          end
      | `SYMLINK ->
           (* Note that chmod would not do the right thing on links *)
          Fs.unlink absolutePath
      | `ABSENT ->
          ())

(* Copy the contents of [source] into the existing regular file [target]
   while preserving [target]'s inode.  Only blocks that actually differ
   between [source] and [target] at the same offset are written, so that
   snapshotting filesystems retain only changed blocks.  [target] is
   truncated to [source]'s length when shorter. *)
let inplaceUpdate source target =
  let block = Prefs.read inplaceBlockSize * 1024 in
  let src_fd = Fs.openfile source [Unix.O_RDONLY; Unix.O_CLOEXEC] 0 in
  Util.finalize (fun () ->
    let dst_fd = Fs.openfile target [Unix.O_RDWR; Unix.O_CLOEXEC] 0 in
    Util.finalize (fun () ->
      let src_buf = Bytes.create block in
      let dst_buf = Bytes.create block in
      let rec read_chunk fd buf pos n =
        if n = 0 then pos
        else
          let r = Unix.read fd buf pos n in
          if r = 0 then pos
          else read_chunk fd buf (pos + r) (n - r)
      in
      let rec write_chunk fd buf pos n =
        if n = 0 then ()
        else
          let w = Unix.single_write fd buf pos n in
          write_chunk fd buf (pos + w) (n - w)
      in
      let prefix_equal a b n =
        let rec aux i =
          i >= n
          || (Bytes.unsafe_get a i = Bytes.unsafe_get b i && aux (i + 1))
        in
        aux 0
      in
      let rec loop offset =
        let n_src = read_chunk src_fd src_buf 0 block in
        if n_src = 0 then offset
        else begin
          let _ = Unix.LargeFile.lseek dst_fd
                    (Int64.of_int offset) Unix.SEEK_SET in
          let n_dst = read_chunk dst_fd dst_buf 0 n_src in
          if n_dst <> n_src
             || not (prefix_equal src_buf dst_buf n_src) then begin
            let _ = Unix.LargeFile.lseek dst_fd
                      (Int64.of_int offset) Unix.SEEK_SET in
            write_chunk dst_fd src_buf 0 n_src
          end;
          loop (offset + n_src)
        end
      in
      let total = loop 0 in
      Unix.LargeFile.ftruncate dst_fd (Int64.of_int total);
      Unix.fsync dst_fd)
      (fun () -> Unix.close dst_fd))
    (fun () -> Unix.close src_fd);
  (* Defense in depth: confirm target now matches source byte-for-byte
     before we let the caller drop the source.  If the comparison or copy
     logic ever produced a wrong result, this raises and the source is
     preserved; unison's next sync will recover via its own fingerprint
     check on the half-written target. *)
  let srcFp = Fingerprint.file source Path.empty in
  let dstFp = Fingerprint.file target Path.empty in
  if not (Fingerprint.equal srcFp dstFp) then
    raise (Util.Transient
             (Printf.sprintf
                "inplaceUpdate: post-write fingerprint mismatch on %s"
                (Fspath.toPrintString target)))

let rename ?exdev fname sourcefspath sourcepath targetfspath targetpath =
  let source = Fspath.concat sourcefspath sourcepath in
  let source' = Fspath.toPrintString source in
  let target = Fspath.concat targetfspath targetpath in
  let target' = Fspath.toPrintString target in
  if source = target then
    raise (Util.Transient ("Rename ("^fname^"): identical source and target " ^ source'));
  Util.convertUnixErrorsToTransient ("renaming " ^ source' ^ " to " ^ target')
    (fun () ->
      debug (fun() -> Util.msg "rename %s to %s\n" source' target');
      let useInplace =
        Prefs.read inplaceRename
        && (try
              (Fs.lstat target).Unix.LargeFile.st_kind = Unix.S_REG
            with Unix.Unix_error _ -> false)
      in
      begin
        if useInplace then begin
          debug (fun() -> Util.msg "rename: inplace update of %s\n" target');
          inplaceUpdate source target;
          Fs.unlink source
        end else
        try
          Fs.rename source target
        with Unix.Unix_error (Unix.EXDEV, _, _) as e ->
          (* We need to handle EXDEV when rename is the primary
             transport action. [Util.convertUnixErrorsToTransient]
             loses the original errno, so this is the workaround. *)
          begin match exdev with
          | Some f -> f ()
          | None -> raise e
          end
      end;
      if Prefs.read Osx.rsrc then begin
        let sourceDouble = Fspath.appleDouble source in
        let targetDouble = Fspath.appleDouble target in
        if Fs.file_exists sourceDouble then
          Fs.rename sourceDouble targetDouble
        else if Fs.file_exists targetDouble then
          Fs.unlink targetDouble
      end)

let symlink =
  if Fs.hasSymlink () then
    fun fspath path l ->
      Util.convertUnixErrorsToTransient
      "writing symbolic link"
      (fun () ->
         let abspath = Fspath.concat fspath path in
         Fs.symlink l abspath)
  else
    fun fspath path l ->
      raise (Util.Transient
               (Format.sprintf
                  "Cannot create symlink \"%s\": \
                   symlinks are not supported on this system%s"
                  (Fspath.toPrintString (Fspath.concat fspath path))
                  (if Sys.win32 || Sys.cygwin then
                     " or elevated privileges may be required"
                  else "")
               ))

(* Create a new directory, using the permissions from the given props        *)
let createDir fspath path perms =
  Util.convertUnixErrorsToTransient
  "creating directory"
    (fun () ->
       let absolutePath = Fspath.concat fspath path in
       Fs.mkdir absolutePath perms)

(*****************************************************************************)
(*                              FINGERPRINTS                                 *)
(*****************************************************************************)

type fullfingerprint = Fingerprint.t * Fingerprint.t

let mfullfingerprint = Umarshal.(prod2 Fingerprint.m Fingerprint.m id id)

let fingerprint fspath path typ =
  (Fingerprint.file fspath path,
   Osx.ressFingerprint fspath path typ)

let pseudoFingerprint path size =
  (Fingerprint.pseudo path size, Fingerprint.dummy)

let isPseudoFingerprint (fp,rfp) =
  Fingerprint.ispseudo fp

(* FIX: not completely safe under Unix                                       *)
(* (with networked file system such as NFS)                                  *)
let safeFingerprint fspath path info optFp =
    let rec retryLoop count info optFp optRessFp =
      if count = 0 then
        raise (Util.Transient
                 (Printf.sprintf
                    "Failed to fingerprint file \"%s\": \
                     the file keeps on changing"
                    (Fspath.toPrintString (Fspath.concat fspath path))))
      else
        let fp =
          match optFp with
            None     -> Fingerprint.file fspath path
          | Some fp -> fp
        in
        let ressFp =
          match optRessFp with
            None      -> Osx.ressFingerprint fspath path info.Fileinfo.typ
          | Some ress -> ress
        in
        let (info', dataUnchanged, ressUnchanged) =
          Fileinfo.unchanged fspath path info in
        if dataUnchanged && ressUnchanged then
          (info', (fp, ressFp))
        else
          retryLoop (count - 1) info'
            (if dataUnchanged then Some fp else None)
            (if ressUnchanged then Some ressFp else None)
    in
    retryLoop 10 info (* Maximum retries: 10 times *)
      (match optFp with None -> None | Some (d, _) -> Some d)
      None

let fullfingerprint_to_string (fp,rfp) =
  Printf.sprintf "(%s,%s)" (Fingerprint.toString fp) (Fingerprint.toString rfp)

let reasonForFingerprintMismatch (fpdata,fpress) (fpdata',fpress') =
  if fpdata = fpdata' then "resource fork"
  else if fpress = fpress' then "file contents"
  else "both file contents and resource fork"

let fullfingerprint_dummy = (Fingerprint.dummy,Fingerprint.dummy)

let fullfingerprintHash (fp, rfp) =
  Fingerprint.hash fp + 31 * Fingerprint.hash rfp

let fullfingerprintEqual (fp, rfp) (fp', rfp') =
  Fingerprint.equal fp fp' && Fingerprint.equal rfp rfp'


(*****************************************************************************)
(*                           UNISON DIRECTORY                                *)
(*****************************************************************************)

(* Make sure archive directory exists                                        *)
let createUnisonDir() =
  try ignore (System.stat Util.unisonDir)
  with Unix.Unix_error(_) ->
    Util.convertUnixErrorsToFatal
      (Printf.sprintf "creating unison directory %s"
         Util.unisonDir)
      (fun () ->
         ignore (System.mkdir Util.unisonDir 0o700))

(*****************************************************************************)
(*                           TEMPORARY FILES                                 *)
(*****************************************************************************)

(* Truncate a filename to at most [l] bytes, making sure of not
   truncating an UTF-8 character.  Assumption: [String.length s > l] *)
let rec truncate_filename s l =
  if l > 0 && Char.code s.[l] land 0xC0 = 0x80 then
    truncate_filename s (l - 1)
  else
    String.sub s 0 l

(* We need to be careful not to use longer temp-file names than the
   file system permits.  eCryptfs has the lowest file name length
   limit we know of, at 143 bytes. *)
let maxFileNameLength = 143

(* Generates an unused fspath for a temporary file.                          *)
let genTempPath fresh fspath path prefix suffix =
  let rec f i =
    let s =
      if i=0 then suffix
      else Printf.sprintf "..%03d.%s" i suffix in
    let tempPath =
      match Path.deconstructRev path with
        None ->
          assert false
      | Some (name, parentPath) ->
          let name = Name.toString name in
          let nameLen = String.length name in
          let prefixLen = String.length prefix in
          let suffixLen = String.length s in
          let maxLen = maxFileNameLength - prefixLen - suffixLen in
          let name =
            if nameLen <= maxLen then name else
              let nameDigest = Digest.MD5.to_hex (Digest.MD5.string name) in
              let nameDigestLen = String.length nameDigest in
              let maxLen = maxLen - nameDigestLen in
              assert (maxLen>0);
              (truncate_filename name maxLen ^ nameDigest)
          in
          Path.child parentPath (Name.fromString (prefix ^ name ^ s))
    in
    if fresh && exists fspath tempPath then f (i + 1) else tempPath
  in f 0

let tempPath ?(fresh=true) fspath path =
  genTempPath fresh fspath path tempFilePrefix !tempFileSuffix
