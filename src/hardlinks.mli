(* Unison file synchronizer: src/hardlinks.mli *)

(* Phase 1: detection.  When [checkhardlinks] is set, the update scanner
   warns about (dev,ino) seen at two or more synchronized paths.

   Phase 3: propagation.  When [synchardlinks] is set and the
   [hardlinks-1] feature is negotiated, the scanner emits a [Hardlink]
   updateContent for the non-canonical members of each group, so that
   the receiving replica can recreate the alias instead of copying the
   contents independently. *)

val checkhardlinks : bool Prefs.t
val synchardlinks : bool Prefs.t

val featureEnabled : unit -> bool
(** True iff [hardlinks-1] is in the negotiated feature set. *)

val syncing : unit -> bool
(** True iff [synchardlinks] is set and the feature is negotiated. *)

(***** Scan-side state *****)

val beginScan : unit -> unit
(** Reset the per-scan group table.  Call once before scanning a replica. *)

val endScan : Fspath.t -> unit
(** Emit detection warnings (if [checkhardlinks]) for any groups recorded
    during the scan.  The table is retained for queries until the next
    [beginScan]. *)

val record : Path.local -> Unix.LargeFile.stats -> unit
(** Record a stat result.  No-op when neither [checkhardlinks] nor
    [synchardlinks] is set, when the file is not a regular file, or
    when its link count is not greater than one. *)

val groupOf : Path.local -> Path.local list option
(** [groupOf p] returns the list of all paths sharing an inode with [p],
    sorted by [Path.toString], or [None] if [p] was not recorded as part
    of a group during the most recent scan. *)

val primaryOf : Path.local -> Path.local option
(** [primaryOf p] returns the canonical (smallest by [Path.toString])
    path of the group containing [p], or [None] if [p] is not in a
    group.  When [primaryOf p = Some q] and [q <> p], [p] is a
    "secondary" alias of [q]. *)
